	PROGRAM NH2E
C
C $Id: nh2e_4dyr.f,v 1.1 2002/10/15 20:15:51 janine Exp $
C
C
C NH2E CONVERTS A "NEW" HOURLY QCF FILE TO AN EBUFR FILE.
C
C Synopsis: nh2e [-d destination_directory] [<qcf_data_file>]
C
C Written by: John J. Allison / john@ofps.ucar.edu
C             UCAR/OFPS
C             19 May 1992
C
C Modification history:
C -- Nov 1994 by John J. Allison
C  now expects 15 char station names
C 05 Aug 1992 by John J. Allison
C  another QC fix: if all data values are Missing, then don't write
C  an EBUFR record
C 30 Jun 1992 by John J. Allison
C  made NH2E from H2E, in order to handle the New hourly QCF, which
C  has the nominal date/time prepended to the record
C 29 May 1992 by John J. Allison
C  changed Q2E into H2E for handling nominal times in hourly data
C  Q2E and H2E are now 2 separate programs
C
C  15 Sep 1999, ds
C    Added CENT variable to hold the century number (e.g. 1900 or 2000).
C
C  19 Sep 2002, JAG
C    Modified code to read in new 4-digit year hourly QCF data.  Eliminated
C    CENT variable.
C
C Description:
C  H2E reads a QCF (ASCII) file, converts the data to appropriate units,
C and writes an EBUFR file, using the Generic EBUFR Encoder Routines (GER).
C See the documentation for QCF and the Generic Encoder for more info.
C  H2E is different from Q2E in that it handles hourly data only.
C The nominal time is placed in the EBUFR header. The time of observation
C is entered as the first 6 data elements. Note that Q2E does not have
C the time as data elements, but places the observation time in the header.
C  Note that NH2E reads in the nominal time from the data record,
C and so doesn't need to compute it. Because of this, it ignores any
C -n or +n cmd line args.
C  The command line arguments -n and +n change the values for the
C before and after the hour time ranges, respectively. The defaults are
C 15 minutes before the hour and 15 minutes after the hour. These values
C should be the same values given to MKNOM (whose defaults are also
C -15 and +15). H2E does not drop any values outside the time range,
C as MKNOM does. Rather, if a value is outside the range, then the
C nominal time remains equal to the observation time. A warning message
C is printed to stderr.
C  For example, data with a time field of 92/02/01 00:45 would be encoded,
C and the nominal time placed in the header would be 92/02/01 01:00.
C  When H2E is called with no arguments, the QCF data is read from stdin
C and the EBUFR data is written to stdout. This makes it easy to use
C compressed data files, for example:
C   % zcat asos5_201.qcf.Z | h2e | compress > asos5_201.ebufr.Z
C Here is a handy csh script:
C --------cut here--------
C #!/bin/csh
C foreach i (*.qcf.Z)
C set j=$i:r
C zcat $i | h2e | compress > $j:r.ebufr.Z
C end
C --------cut here--------
C  BE VERY VERY CAREFUL: GER output ALL their error messages to stdout.
C NOTHING is output to stderr. If no errors occur, then your EBUFR file
C will be just fine. But, if any GER decides to get talkative, you're
C doomed and you may not know it until you try to use the EBUFR file.
C  This program is intended to be quick, simple, and straightforward to
C write. Whenever possible, execution speed has been opted for over
C space or elegance. This program is not intended to be extendible to
C a wide range of applications, just something to quickly convert QCF to
C EBUFR. As such, some things are hard-coded (e.g. unit conversion) and
C non-modularized, so the only subroutine calls are to GER.
C  But, one should also note that not much can be done within Q2E to
C achieve a speedup. GER is the bottleneck.
C  Also, this program is not completely ANSI standard. Note especially
C the use of include and records for the QCF data structure. Also,
C the 'D' statements may be Sun specific? And the command line argument
C stuff is Sun specific.
C
C ******* NOTE *******
C Note the WRITE statements just below line 198. They write the number of
C records processed to stderr. You may want to make these line debug
C lines (with 'D' in 1st column) or comments, and then recompile.
C 
C Input:
C  QCF data file (<qcf_data_file>).
C
C Output:
C  EBUFR file.
C  When compiled with "-xld" option, also prints out progress messages.
C  These are marked in the code with a 'D' in the 1st column.
C
C Files:
C  QCF data file - the file to be converted
C    argv[1] taken to be QCF (hourly) data file
C  EBUFR output file - contains the converted data
C    named same as QCF file, with ".ebufr" extension
C  Control file - tells GER what type of data is coming
C    assumed to be named "control.txt"
C  Class description file - contains list of class descriptions
C    assumed to be named "class_file"
C  Parameter description file - contains param. descriptions
C    assumed to be named "desc_file"
C  Code/Flag table files - one for each code or flag table
C    must be named according to EMPRESS and GER conventions
C  H2E opens and closes all files. Code/Flag table files are
C    opened/closed by GER via calls to GTFXYU and CLSUNI. GER writes
C    to EBUFR output file, and reads from the rest (execpt QCF file).
C
C Subroutines and Functions
C  GER RDCONF,WRTHDR,ENCOBS
C  GEXT GTFXYU,CLSUNI
C  FORTRAN ICHAR,IARGC,GETARG
C

C Functions to be sent to GER WRTHDR
	EXTERNAL GTFXYU
	INTEGER GTFXYU
	EXTERNAL CLSUNI
C FORTRAN function for cmd line args
	INTEGER IARGC

C Include QCF data structure
	INCLUDE 'qcf.inc'

C Constant Definitions

	REAL QRMISS
	INTEGER QIMISS
	PARAMETER (QRMISS=-999.99,QIMISS=-999)
	INTEGER ISIZ,RSIZ
	PARAMETER (ISIZ=31,RSIZ=14)

C Internal Variables
C QREC is the QCF data
C IDATA,RDATA,CDATA,DFLAG,CLIBYT,LATLON get sent to GER ENCOBS
C TIME is the nominal time, sent to ENCOBS separately
C *FIL and *UNI are the filenames and unit numbers
C QCTAB is a lookup table for QC flags
C everything else is just temporary stuff
	RECORD /QCF/ QREC
	INTEGER IDATA(ISIZ),IMISS,QCTAB(256)
	INTEGER TIME(6),DUMTIM(6)
	INTEGER CLIBYT,ARGCT,I,LOC,DLOC
	INTEGER QCFUNI,OUTUNI,CTLUNI,CLAUNI,PRMUNI
	INTEGER RECCT,ENCCT,MISCT
	LOGICAL DFLAG,DATSEN
	REAL RDATA(RSIZ),LATLON(2),RMISS
	CHARACTER*25 CDATA
	CHARACTER*256 QCFFIL,OUTFIL,CTLFIL,CLAFIL,PRMFIL,ARGV
	CHARACTER*256 DIRECT
	CHARACTER CH

C Must get RMISS set to all 1s
	EQUIVALENCE (IMISS,RMISS)

C Initialize Variables
C Missing value for integer == all 1s for 32 bits
	IMISS = 2147483647
	DFLAG = .FALSE.
	TIME(6) = 0
	IDATA(6) = 0
	DUMTIM(1) = 99
	DUMTIM(2) = 12
	DUMTIM(3) = 31
	DUMTIM(4) = 23
	DUMTIM(5) = 59
	DUMTIM(6) = 59
	I = 1
C Remember: GTFXYU uses unit 10.
	QCFUNI = 11
	OUTUNI = 12
	CTLUNI = 13
	CLAUNI = 14
	PRMUNI = 15

	CTLFIL = 'control_hrly.txt'
	CLAFIL = 'class_file'
	PRMFIL = 'desc_file'
	DIRECT = './'
	DLOC = 0

C Lookup table for quality control code values
	QCTAB(85) = 0
	QCTAB(71) = 1
	QCTAB(66) = 2
	QCTAB(68) = 3
	QCTAB(78) = 4
	QCTAB(88) = 5
	QCTAB(69) = 6
	QCTAB(67) = 7
	QCTAB(84) = 8
	QCTAB(73) = 9
	QCTAB(77) = 15


	ARGCT = IARGC()
C If no arguments, use stdin and stdout
	IF (ARGCT .EQ. 0) THEN
	  QCFFIL = 'stdin'
	  QCFUNI = 5
	  OUTFIL = 'stdout'
	  OUTUNI = 6
D	  WRITE (0,*) 'ARGCT .EQ. 0'
	  WRITE (0,*) 'Warning: Generic Encode Routines print ',
&	              'error messages to stdout.'
	  GO TO 50
	END IF

C Loop to run program once on each file named
	DO WHILE (I .LE. ARGCT)

C Get QCF filename from command line
	CALL GETARG(I,ARGV)

	IF (ARGV(1:2) .EQ. '-d') THEN
	  I = I + 1
	  IF (I .GT. ARGCT) GO TO 300
	  CALL GETARG(I,ARGV)
	  DLOC = INDEX(ARGV,' ')
	  DLOC = DLOC - 1
	  DIRECT(1:DLOC) = ARGV(1:DLOC)
	  IF (DIRECT(DLOC:DLOC) .NE. '/') THEN
	    DLOC = DLOC + 1
	    DIRECT(DLOC:DLOC) = '/'
	  END IF
	  GO TO 300
	END IF

	QCFFIL = ARGV
C QCF filename is in form of file.qcf or file
C EBUFR output file is in form of file.ebufr
C *** assumes that the first period in the filename is also the last!!!
	LOC = INDEX(ARGV,'.')
	IF (LOC .EQ. 0) THEN
	  LOC = INDEX(ARGV,' ')
	  LOC = LOC - 1
	  IF (DLOC .GT. 0) THEN
	    OUTFIL = DIRECT(1:DLOC) // ARGV(1:LOC) // '.ebufr'
	  ELSE
	    OUTFIL = ARGV(1:LOC) // '.ebufr'
	  END IF
	ELSE
	  IF (DLOC .GT. 0) THEN
	    OUTFIL = DIRECT(1:DLOC) // ARGV(1:LOC) // 'ebufr'
	  ELSE
	    OUTFIL = ARGV(1:LOC) // 'ebufr'
	  END IF
	END IF

C A 'D' in the 1st column means only compile this statement
C  when the -xld flag is given to f77. For debugging purposes.
D	WRITE (0,*) 'QCF filename: ',QCFFIL
D	WRITE (0,*) 'EBUFR output filename: ',OUTFIL
D	WRITE (0,*) 'Control filename: ',CTLFIL
D	WRITE (0,*) 'Class descripton filename: ',CLAFIL
D	WRITE (0,*) 'Parameter description filename: ',PRMFIL

C Open Files

	OPEN (FILE=QCFFIL, UNIT=QCFUNI, STATUS='OLD', ERR=9990)
	OPEN (FILE=OUTFIL, UNIT=OUTUNI, STATUS='NEW', ERR=9990)
50	CONTINUE
	OPEN (FILE=CTLFIL, UNIT=CTLUNI, STATUS='OLD', ERR=9990)
	OPEN (FILE=CLAFIL, UNIT=CLAUNI, STATUS='OLD', ERR=9990)
	OPEN (FILE=PRMFIL, UNIT=PRMUNI, STATUS='OLD', ERR=9990)

D	WRITE (0,*) 'Files open.'

	RECCT = 0
	ENCCT = 0
	MISCT = 0

C Call GER RDCONF to read control file

	REWIND (CTLUNI)
	CALL RDCONF (CTLUNI)
	CLOSE (UNIT=CTLUNI)

D	WRITE (0,*) 'GER RDCONF returned.'

C Call GER WRTHDR to write EBUFR header data
	  CALL WRTHDR (OUTUNI,CLAUNI,PRMUNI,GTFXYU,CLSUNI,DUMTIM,1)
	  CLOSE (UNIT=CLAUNI)
	  CLOSE (UNIT=PRMUNI)

D	  WRITE (0,*) 'GER WRTHDR returned.'


C Read header lines from QCF file
	READ (QCFUNI, '(A1)') CH
	REWIND (QCFUNI)
	IF (CH .EQ. 'N') THEN
	  READ (QCFUNI,'(//)')
	END IF


C Data processing loop:
C  Read data
C  Convert data into correct units
C  Call GER ENCOBS to write EBUFR data

C oh no, mr. bill! you're gonna use a goto!
C YEAH, BOY, IT WAS EASIER TO THINK OF THAN SOME FANCY DOWHILE.
C LOOK AT HOW THE END=198 IN READ WORKS.

C Also, note that on ERR, we goto 198 as well. This is a quick
C fix since the QCF file does not just end. The last line is
C "**** END OF FILE ****" or something. This produces an error
C with this FORMAT statement. One way to handle the situation
C might be to read the record into a CHARACTER*236 buffer,
C check if it .EQ. "**** END OF FILE ****", then either quit or
C read the buffer using the 101 FORMAT statement.
C  Also note that some hourly data does not have the END OF FILE
C line, but just ends. This works fine for that, too.

100	CONTINUE

C Read data
C23456789012345678901234567890123456789012345678901234567890123456789012
      READ (QCFUNI,101,END=198,ERR=198)
     * TIME(1),TIME(2),TIME(3),TIME(4),TIME(5),
     * IDATA(1),IDATA(2),IDATA(3),
     * IDATA(4),IDATA(5),CDATA(1:10),CDATA(11:25),
     * LATLON(1),LATLON(2),CLIBYT,QREC.STAELV,
     * QREC.STAPRS,QREC.STAFLG,QREC.SEAPRS,QREC.SEAFLG,QREC.CMPSEA,
     * QREC.CMPFLG,QREC.TEMP,QREC.TMPFLG,QREC.DEWPNT,QREC.DEWFLG,
     * QREC.WNDSPD,QREC.SPDFLG,QREC.WNDDIR,QREC.DIRFLG,QREC.PRECIP,
     * QREC.PRCFLG,QREC.SG,QREC.SQUALL,QREC.SQLFLG,QREC.PRSWEA,
     * QREC.PWFLG,QREC.VISIB,QREC.VISFLG,QREC.CELHT1,IDATA(20),
     * QREC.C1FLG,IDATA(22),QREC.CA1FLG,QREC.CELHT2,IDATA(24),
     * QREC.C2FLG,IDATA(26),QREC.CA2FLG,QREC.CELHT3,IDATA(28),
     * QREC.C3FLG,IDATA(30),QREC.CA3FLG

C Same format used to write QCF data
C23456789012345678901234567890123456789012345678901234567890123456789012
101   FORMAT (2(I4,2('/',I2),X,I2,':',I2,X),
     * A10,X,A15,X,F10.5,X,F11.5,X,I3,
     * X,F7.2,X,8(F7.2,X,A1,X),A1,X,F7.2,X,A1,X,I4,X,A1,X,F8.2,X,A1,
     * 3(X,F7.2,X,I2,X,A1,X,I2,X,A1))

	DATSEN = .FALSE.
	RECCT = RECCT + 1
D	WRITE (0,'(A,I,A)') 'QCF record ',RECCT,' read.'

102	CONTINUE

C Convert data into correct units, and
C   store in IDATA,RDATA,CDATA arrays for GER
C Some values already stored in proper array from READ
C See "h2e.doc" for the matchings between QCF fields and the ?DATA arrays

C BUFR_value = QCF_value * (10^Scale) - Reference
C parameter (qcf_units bufr_units scale reference)
C BUT, GER ENCOBS does the scaling and referencing
C  itself. All that we have to do is convert units (e.g. mb->Pa).

C station elevation (m m 0 -400)
	IF (QREC.STAELV .EQ. QRMISS) THEN
	  RDATA(1) = RMISS
	ELSE
	  RDATA(1) = QREC.STAELV
	  DATSEN = .TRUE.
	END IF

C In order to compensate for some errors in one version of the '.0qc'
C  and '.qcf' files, we do not check the field for .EQ. QRMISS if there
C  is a QC flag. Instead, we check if the QC flag .EQ. 'M', in which
C  case we ignore the field and insert the appropriate missing values.
C  This is because there are 2 possible missing values for some fields.
C  (all the real fields, plus present weather).
C However, if the flag .NE. 'M' then the field could still contain a
C  missing value and must be checked against QRMISS, i.e. the bug is
C  such that "-999.99 U" could also show up.

C station pressure (mb Pa -1 0)
C QC flag for station pressure
	IF (QREC.STAFLG .EQ. 'M') THEN
C Set both to missing; ignore the value in STAPRS
	  IDATA(7) = 15
	  RDATA(2) = RMISS
	ELSE
C Code table and Flag table values have missing values built in
C  so just use a handy reference table for conversion
	  IDATA(7) = QCTAB(ICHAR(QREC.STAFLG))
C Data value could still be missing, leave QC flag alone
	  IF (QREC.STAPRS .EQ. QRMISS) THEN
	    RDATA(2) = RMISS
	  ELSE
	    RDATA(2) = QREC.STAPRS * 100.0
	    DATSEN = .TRUE.
	  END IF
	END IF

C sea level pressure (mb Pa -1 0)
C QC flag for sea level pressure
	IF (QREC.SEAFLG .EQ. 'M') THEN
	  IDATA(8) = 15
	  RDATA(3) = RMISS
	ELSE
	  IDATA(8) = QCTAB(ICHAR(QREC.SEAFLG))
	  IF (QREC.SEAPRS .EQ. QRMISS) THEN
	    RDATA(3) = RMISS
	  ELSE
	    RDATA(3) = QREC.SEAPRS * 100.0
	    DATSEN = .TRUE.
	  END IF
	END IF

C computed sea level pressure (mb Pa -1 0)
C QC flag for computed sea level
	IF (QREC.CMPFLG .EQ. 'M') THEN
	  IDATA(9) = 15
	  RDATA(4) = RMISS
	ELSE
	  IDATA(9) = QCTAB(ICHAR(QREC.CMPFLG))
	  IF (QREC.CMPSEA .EQ. QRMISS) THEN
	    RDATA(4) = RMISS
	  ELSE
	    RDATA(4) = QREC.CMPSEA * 100.0
	    DATSEN = .TRUE.
	  END IF
	END IF

C dry bulb temperature (C K 1 0)
C QC flag for dry bulb temp
	IF (QREC.TMPFLG .EQ. 'M') THEN
	  IDATA(10) = 15
	  RDATA(5) = RMISS
	ELSE
	  IDATA(10) = QCTAB(ICHAR(QREC.TMPFLG))
	  IF (QREC.TEMP .EQ. QRMISS) THEN
	    RDATA(5) = RMISS
	  ELSE
	    RDATA(5) = QREC.TEMP + 273.15
	    DATSEN = .TRUE.
	  END IF
	END IF

C dew point temperature (C K 1 0)
C QC flag for dew point temp
	IF (QREC.DEWFLG .EQ. 'M') THEN
	  IDATA(11) = 15
	  RDATA(6) = RMISS
	ELSE
	  IDATA(11) = QCTAB(ICHAR(QREC.DEWFLG))
	  IF (QREC.DEWPNT .EQ. QRMISS) THEN
	    RDATA(6) = RMISS
	  ELSE
	    RDATA(6) = QREC.DEWPNT + 273.15
	    DATSEN = .TRUE.
	  END IF
	END IF

C wind speed (m/s m/s 1 0)
C QC flag for wind speed
	IF (QREC.SPDFLG .EQ. 'M') THEN
	  IDATA(12) = 15
	  RDATA(7) = RMISS
	ELSE
	  IDATA(12) = QCTAB(ICHAR(QREC.SPDFLG))
	  IF (QREC.WNDSPD .EQ. QRMISS) THEN
	    RDATA(7) = RMISS
	  ELSE
	    RDATA(7) = QREC.WNDSPD
	    DATSEN = .TRUE.
	  END IF
	END IF

C wind direction (dg_true dg_true 0 0)
C  (original QCF include file said degrees azimuth)
C QC flag for wind direction
	IF (QREC.DIRFLG .EQ. 'M') THEN
	  IDATA(13) = 15
	  RDATA(8) = RMISS
	ELSE
	  IDATA(13) = QCTAB(ICHAR(QREC.DIRFLG))
	  IF (QREC.WNDDIR .EQ. QRMISS) THEN
	    RDATA(8) = RMISS
	  ELSE
	    RDATA(8) = QREC.WNDDIR
	    DATSEN = .TRUE.
	  END IF
	END IF

C total precipitation (mm kg/m^2 1 -1)
C QC flag for total precip
	IF (QREC.PRCFLG .EQ. 'M') THEN
	  IDATA(14) = 15
	  RDATA(9) = RMISS
	ELSE
	  IDATA(14) = QCTAB(ICHAR(QREC.PRCFLG))
	  IF (QREC.PRECIP .EQ. QRMISS) THEN
	    RDATA(9) = RMISS
	  ELSE
	    RDATA(9) = QREC.PRECIP
	    DATSEN = .TRUE.
	  END IF
	END IF

C squall/gust indicator
	IF (QREC.SG .EQ. 'S') THEN
	  IDATA(15) = 1
	ELSE IF (QREC.SG .EQ. 'Q') THEN
	  IDATA(15) = 1
	ELSE IF (QREC.SG .EQ. 'G') THEN
	  IDATA(15) = 0
	ELSE
	  IDATA(15) = 3
	END IF

C squall/gust speed (m/s m/s 1 0)
C QC flag for squall/gust speed
	IF (QREC.SQLFLG .EQ. 'M') THEN
	  IDATA(16) = 15
	  RDATA(10) = RMISS
	ELSE
	  IDATA(16) = QCTAB(ICHAR(QREC.SQLFLG))
	  IF (QREC.SQUALL .EQ. QRMISS) THEN
	    RDATA(10) = RMISS
	  ELSE
	    RDATA(10) = QREC.SQUALL
	    DATSEN = .TRUE.
	  END IF
	END IF

C present weather (idata(11))
C  almost same code table as BUFR, but be careful of Missing value!
C QC flag for present weather (idata(12))
	IF (QREC.PWFLG .EQ. 'M') THEN
	  IDATA(18) = 15
	  IDATA(17) = 511
	ELSE
	  IDATA(18) = QCTAB(ICHAR(QREC.PWFLG))
	  IF (QREC.PRSWEA .EQ. QIMISS) THEN
	    IDATA(17) = 511
	  ELSE
	    IDATA(17) = QREC.PRSWEA
	    DATSEN = .TRUE.
	  END IF
	END IF

C (horizontal) visibility (m m -1 0)
C QC flag for visibility
	IF (QREC.VISFLG .EQ. 'M') THEN
	  IDATA(19) = 15
	  RDATA(11) = RMISS
	ELSE
	  IDATA(19) = QCTAB(ICHAR(QREC.VISFLG))
	  IF (QREC.VISIB .EQ. QRMISS) THEN
	    RDATA(11) = RMISS
	  ELSE
	    RDATA(11) = QREC.VISIB
	    DATSEN = .TRUE.
	  END IF
	END IF

C ceiling flag and cloud amount codes are read in directly to idata
C  for each of the 3 layers (see READ statement above)
C they also have missing values built in that do not need to be
C  corrected for with their QC flags

C ceiling height-1st layer (100ft ft -2 0)
C QC flag for ceiling height/flag
	IF (QREC.C1FLG .EQ. 'M') THEN
	  IDATA(21) = 15
	  RDATA(12) = RMISS
	ELSE
	  IDATA(21) = QCTAB(ICHAR(QREC.C1FLG))
	  IF (QREC.CELHT1 .EQ. QRMISS) THEN
	    RDATA(12) = RMISS
	  ELSE
	    RDATA(12) = QREC.CELHT1 * 100.0
	    DATSEN = .TRUE.
	  END IF
	END IF

C QC flag for cloud amount
	IDATA(23) = QCTAB(ICHAR(QREC.CA1FLG))

C ceiling height-2nd layer (100ft ft -2 0)
C QC flag for ceiling height/flag
	IF (QREC.C2FLG .EQ. 'M') THEN
	  IDATA(25) = 15
	  RDATA(13) = RMISS
	ELSE
	  IDATA(25) = QCTAB(ICHAR(QREC.C2FLG))
	  IF (QREC.CELHT2 .EQ. QRMISS) THEN
	    RDATA(13) = RMISS
	  ELSE
	    RDATA(13) = QREC.CELHT2 * 100.0
	    DATSEN = .TRUE.
	  END IF
	END IF

C QC flag for cloud amount
	IDATA(27) = QCTAB(ICHAR(QREC.CA2FLG))

C ceiling height-3rd layer (100ft ft 0 0)
C QC flag for ceiling height/flag
	IF (QREC.C3FLG .EQ. 'M') THEN
	  IDATA(29) = 15
	  RDATA(14) = RMISS
	ELSE
	  IDATA(29) = QCTAB(ICHAR(QREC.C3FLG))
	  IF (QREC.CELHT3 .EQ. QRMISS) THEN
	    RDATA(14) = RMISS
	  ELSE
	    RDATA(14) = QREC.CELHT3 * 100.0
	    DATSEN = .TRUE.
	  END IF
	END IF

C QC flag for cloud amount
	IDATA(31) = QCTAB(ICHAR(QREC.CA3FLG))

D	WRITE (0,*) 'QCF data converted.'


C If we've seen data (i.e. not everything is Missing), then
C   Call GER ENCOBS to write EBUFR data

	IF (DATSEN) THEN
	  CALL ENCOBS (OUTUNI,1,TIME,LATLON,CLIBYT,DFLAG,
&	    IDATA,RDATA,CDATA)
	  ENCCT = ENCCT + 1
D	  WRITE (0,*) 'GER ENCOBS returned.'
D	  WRITE (0,'(A,I)') ' ENCCT = ',ENCCT
	ELSE
	  MISCT = MISCT + 1
D	  WRITE (0,*) 'Missing record, not encoded.'
D	  WRITE (0,'(A,I)') ' MISCT = ',MISCT
	END IF
D	WRITE (0,'(A,I)') ' RECCT = ',RECCT


C Repeat data processing loop

190	GO TO 100

C Loop exit place when EOF

198	CONTINUE

C May want to 'D' following
	WRITE (0,'(A20,A)') QCFFIL,': reached EOF.'
	WRITE (0,'(I,A)') RECCT,' QCF records read.'
	WRITE (0,'(I,A)') ENCCT,' records encoded.'
	WRITE (0,'(I,A)') MISCT,' records with all missing.'

C Close files that are still open
C  unless they're stdin,stdout
	IF (QCFUNI .NE. 5) THEN
	  CLOSE (QCFUNI)
	  CLOSE (OUTUNI)
	END IF

300	CONTINUE
C End of main DO loop for processing cmd line args
	I = I + 1
	END DO

C Quit

D	WRITE (0,*) 'End of nh2e.'
	GO TO 9999

C Error messages for general use, written to stderr

9990	WRITE (0,*) 'Error opening a file.'
	GO TO 9999

9999	CONTINUE

C Fix for -fnonstd flag in -fast compiler option
	CALL STANDARD_ARITHMETIC()

	STOP

	END
