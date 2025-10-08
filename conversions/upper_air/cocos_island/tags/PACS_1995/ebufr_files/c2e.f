C
C $Id: c2e.f,v 1.10 1996/04/09 17:19:40 john Exp $
C
	PROGRAM C2E
C
C C2E CONVERTS A CLASS FILE TO AN EBUFR FILE.
C
C Synopsis: c2e [<class_data_file>]
C
C Written by: John J. Allison
C             UCAR/OFPS
C             27 May 1992
C
C Modification history:
C  22 Jan 1993 by John J. Allison
C   + finished up encoding of network and site ids
C   + changed descriptors for time, altitude, added time signif
C  09 Dec 1992 by John J. Allison
C   + nominal time now appears in the file on Header Line 12
C   + also allowed for composite dayfiles to be processed
C     (these are simply concatenations of several single soundings)
C  23 Feb 1994 by John J. Allison
C   + made a little bit more ANSI
C     still non-ANSI: DO WHILE ; DEBUG comments; TABS (?)
C
C Description:
C  C2E reads a STORM CLASS Format (SCF) file (ASCII), converts the data
C to appropriate units, and writes an EBUFR file, using the Generic
C EBUFR Encoder Routines (GER). See the documentation for CLASS and
C the Generic Encoder for more information.
C  This program is meant to simple, quick, and easy. Note that the
C speed bottleneck is in GER, though. Also, this program is not
C completely ANSI standard. The 'D' statements may be Sun specific?
C And the command line argument stuff is Sun specific.
C
C Input:
C  CLASS data file (<class_data_file>).
C
C Output:
C  EBUFR file.
C  When compiled with "-xld" option, also prints out progress messages.
C  These are marked in the code with a 'D' in the 1st column.
C
C Files:
C  SCF data file - the STORM CLASS Format file to be converted
C    argv[1] taken to be SCF data file
C  EBUFR output file - contains the converted data
C    named same as SCF file, with ".ebufr" extension
C  Control file - tells GER what type of data is coming
C    assumed to be named "control_scf.txt"
C  Class description file - contains list of class descriptions
C    assumed to be named "class_file"
C  Parameter description file - contains param. descriptions
C    assumed to be named "desc_file"
C  Code/Flag table files - one for each code or flag table
C    must be named according to EMPRESS and GER conventions
C  C2E opens and closes all files. Code/Flag table files are
C    opened/closed by GER via calls to GTFXYU and CLSUNI. GER writes
C    to EBUFR output file, and reads from the rest (execpt SCF file).
C
C Subroutines and Functions:
C  GER RDCONF,WRTHDR,ENCOBS
C  GEXT GTFXYU,CLSUNI
C  FORTRAN ICHAR,IARGC,GETARG
C
C23456789012345678901234567890123456789012345678901234567890123456789012

C Functions to be sent to GER WRTHDR
	EXTERNAL GTFXYU
	INTEGER GTFXYU
	EXTERNAL CLSUNI
C FORTRAN function for cmd line args
	INTEGER IARGC

C Constant Definitions
C S?MISS are the SCF missing values
C ISIZ,RSIZ are array sizes for IDATA,RDATA
C IMUL,RMUL are the number of int or real items to replicate
C  and are used in computing the proper index to IDATA,RDATA
	REAL S5MISS,S4MISS,S3MISS,S2MISS
	PARAMETER (S5MISS=99999.0,S4MISS=9999.0)
	PARAMETER (S3MISS=999.0,S2MISS=99.0)
	INTEGER ISIZ,RSIZ,YEAR,IMUL,RMUL
	PARAMETER (ISIZ=25007,RSIZ=45001,YEAR=1992,IMUL=5,RMUL=10)

C Internal Variables
C IDATA,RDATA,CDATA,DFLAG,LATLON,TIME are passed to GER ENCOBS
C *FIL and *UNI are for filenames and unit numbers
C QCTAB is a lookup table for QC flags
C IMISS,RMISS are missing values for BUFR
C CTR is the number of records processed so far,
C  which ends up being the replication factor
C NUMOHD is the number of optional header records,
C  which ends up being the replication factor
C everything else is temporary
	INTEGER IDATA(ISIZ),IMISS,I,ARGCT,LOC,TIME(6),IZERO,ITMP
	INTEGER CTR,SCFUNI,OUTUNI,CTLUNI,CLAUNI,PRMUNI,NUMOHD
	INTEGER QCTAB(99),J,IOS,K,UNCCT
	REAL SDATA(21),RDATA(RSIZ),LATLON(2),RMISS
	CHARACTER*850 CDATA
	CHARACTER*130 HDRSTR,TMPSTR
	CHARACTER*256 ARGV,SCFFIL,OUTFIL,CTLFIL,CLAFIL,PRMFIL
	LOGICAL DFLAG,LSTFLG,FSTFLG

C Must get RMISS set to all 1s
	EQUIVALENCE (IMISS,RMISS)

C Initialize Variables
C Missing value for integer == all 1s for 32 bits
	IMISS = 2147483647
	DFLAG = .FALSE.
	TIME(6) = 0
	I = 1
	IZERO = ICHAR('0')
	CDATA(1:45) = '                    '
&	      // '                         '
C Remember: GTFXYU uses unit 10.
	SCFUNI = 11
	OUTUNI = 12
	CTLUNI = 13
	CLAUNI = 14
	PRMUNI = 15
	CTLFIL = 'control_scf.txt'
	CLAFIL = 'class_file'
	PRMFIL = 'desc_file'
C Table for QC flag lookup
	QCTAB(1) = 1
	QCTAB(2) = 2
	QCTAB(3) = 3
	QCTAB(4) = 8
	QCTAB(9) = 15
	QCTAB(99) = 0

C Get number of cmd line args
	ARGCT = IARGC()
C If no arguments, error: can't use stdin/stdout
	IF (ARGCT .EQ. 0) THEN
	  WRITE (0,*) ' Usage: c2e <input_file>'
	  GO TO 999
	END IF

C Loop to run program once on each file named
	DO WHILE (I .LE. ARGCT)

C Get SCF filename from command line
	CALL GETARG(I,ARGV)
	SCFFIL = ARGV
C SCF filename is in form of file.class or file
C EBUFR output file is in form of file.ebufr
C *** assumes that the first period in the filename is also the last!!!
	LOC = INDEX(ARGV,'.')
	IF (LOC .EQ. 0) THEN
	  LOC = INDEX(ARGV,' ')
	  LOC = LOC - 1
	  OUTFIL = ARGV(1:LOC) // '.ebufr'
	ELSE
	  OUTFIL = ARGV(1:LOC) // 'ebufr'
	END IF

C A 'D' in the 1st column means only compile this statement
C  when the -xld flag is given to f77. For debugging purposes.
D	WRITE (0,'(A)') 'SCF filename: ',SCFFIL
D	WRITE (0,'(A)') 'EBUFR output filename: ',OUTFIL
D	WRITE (0,'(A)') 'Control filename: ',CTLFIL
D	WRITE (0,'(A)') 'Class descripton filename: ',CLAFIL
D	WRITE (0,'(A)') 'Parameter description filename: ',PRMFIL

C Open Files

	OPEN (FILE=SCFFIL, UNIT=SCFUNI, STATUS='OLD', ERR=990)
	OPEN (FILE=OUTFIL, UNIT=OUTUNI, STATUS='NEW', ERR=990)
	OPEN (FILE=CTLFIL, UNIT=CTLUNI, STATUS='OLD', ERR=990)
	OPEN (FILE=CLAFIL, UNIT=CLAUNI, STATUS='OLD', ERR=990)
	OPEN (FILE=PRMFIL, UNIT=PRMUNI, STATUS='OLD', ERR=990)

D	WRITE (0,*) ' Files open.'


C Call GER RDCONF to read control file

	REWIND (CTLUNI)
	CALL RDCONF (CTLUNI)
	CLOSE (UNIT=CTLUNI)

D	WRITE (0,*) ' GER RDCONF returned.'


C Put in something for TIME to WRTHDR. This TIME only appears in
C  the EBUFR type 1 record, and is irrelevant to this data set.
	TIME(1) = 99
	TIME(2) = 12
	TIME(3) = 31
	TIME(4) = 23
	TIME(5) = 59

C Call GER WRTHDR to write EBUFR header info
	CALL WRTHDR(OUTUNI,CLAUNI,PRMUNI,GTFXYU,CLSUNI,TIME,1)
        CLOSE (UNIT=CLAUNI)
	CLOSE (UNIT=PRMUNI)

C ****** This was the OLD way. Now we use Header Line 12.
C Fill in TIME field from filename: this is the Nominal Time
C  and is used as the time of the observation. Launch Time is
C  encoded as a set of descriptors.
C Filename MUST be in one of the following forms:
C   sssmddhh.ext
C   sssmddhhmm.ext
C where sss is the 3-character Station ID, m is 1-char month,
C dd is 2-char day, hh is 2-char hour, mm is 2-char minute,
C and ext the extension, which is ignored.
C	TIME(1) = YEAR
C	TIME(5) = 0
C	READ (SCFFIL,'(A3,I1,I2,I2)') CDATA(1:3),TIME(2),TIME(3),TIME(4)
C	IF (SCFFIL(9:9) .NE. '.') THEN
C	  READ (SCFFIL(9:10),'(I2)') TIME(5)
C	END IF
C Right here is where something may need to go to turn TIME into
C   a nominal time before being passed to ENCOBS.

C Read SCF header lines
C  Skip 1st 2 lines, then get Site ID from 3rd line

C what happens is that when an error occurs trying to read a number,
C  we assume that a new sounding has started and go to 12
c since the first line has already been read, we have to backspace to
c  read it again. If this is the first sounding, then backspace shouldn't
c  hurt since it can't go back past the beginning of the file

12	CONTINUE
	UNCCT = 0
	BACKSPACE (SCFUNI)
	READ (SCFUNI,'(A130)') HDRSTR
C FSTFLG = .TRUE. if site id is in the first 3 chars
C LSTFLG = .TRUE. if site id is in the last 3 chars instead of first 3
C both are .FALSE. if we use the entire string as the site id
	FSTFLG = .FALSE.
	LSTFLG = .FALSE.
C set network ID appropriately
	IF (HDRSTR(36:38) .EQ. 'AES') THEN
	  CDATA(1:10) = 'AES       '
	ELSE IF (HDRSTR(36:45) .EQ. 'NCAR CLASS') THEN
	  CDATA(1:10) = 'CLASS     '
	  LSTFLG = .TRUE.
	ELSE IF (HDRSTR(36:50) .EQ. 'CLASS 10 SECOND') THEN
	  CDATA(1:10) = 'CLASS     '
	  LSTFLG = .TRUE.
	ELSE IF (HDRSTR(36:44) .EQ. 'Flatlands') THEN
	  CDATA(1:10) = 'Flatlands '
	ELSE IF (HDRSTR(36:44) .EQ. 'Fort Sill') THEN
	  CDATA(1:10) = 'Fort Sill '
	ELSE IF (HDRSTR(36:51) .EQ. 'National Weather') THEN
	  CDATA(1:10) = 'NWS       '
	ELSE IF (HDRSTR(36:47) .EQ. 'Picket Fence') THEN
	  CDATA(1:10) = 'Picket Fen'
	ELSE IF (HDRSTR(36:46) .EQ. 'Air Weather') THEN
	  CDATA(1:10) = 'AWS       '
	ELSE IF (HDRSTR(36:44) .EQ. 'NCAR L2D2') THEN
	  CDATA(1:10) = 'L2D2      '
	  LSTFLG = .TRUE.
	ELSE IF (HDRSTR(36:45) .EQ. 'TOGA-COARE') THEN
	  CDATA(1:10) = 'TOGA-COARE'
	ELSE IF (HDRSTR(36:43) .EQ. 'Sounding') THEN
	  CDATA(1:10) = 'TOGA-COARE'
	ELSE IF (HDRSTR(36:42) .EQ. 'BoM-GTS') THEN
	  CDATA(1:10) = 'BoM-GTS   '
	ELSE IF (HDRSTR(36:44) .EQ. 'ECMWF-GTS') THEN
	  CDATA(1:10) = 'ECMWF-GTS '
	ELSE IF (HDRSTR(36:44) .EQ. 'ETA MOLTS') THEN
	  CDATA(1:10) = 'ETA MOLTS '
	ELSE
	  CDATA(1:10) = 'Sounding  '
	END IF

	READ (SCFUNI,'(A130)') HDRSTR
	READ (SCFUNI,'(A130)') HDRSTR

C Set Site ID
	CDATA(11:45) = '          '
&	 // '                         '

	IF (LSTFLG) THEN
C last 3 non-blank chars
	  DO 15, J = 130,1,-1
	    IF (HDRSTR(J:J) .NE. ' ') THEN
	      GO TO 16
	    END IF
15	  CONTINUE
16	  CONTINUE
	  CDATA(11:13) = HDRSTR(J-2:J)
	ELSE IF (FSTFLG) THEN
C first 3 chars
C	  CDATA(11:13) = HDRSTR(36:38)
C first word
	  DO 17, J = 36,45
	    IF (HDRSTR(J:J) .EQ. ' ') THEN
	      GO TO 18
	    END IF
C J-25 is because we want J-35+10
C  (35 for header field name from HDRSTR, then +10 for network already in CDATA)
	    CDATA(J-25:J-25) = HDRSTR(J:J)
17	  CONTINUE
18	  CONTINUE
	ELSE
C use it all
	  CDATA(11:45) = HDRSTR(36:70)
	END IF

C Skip the ddd mm.mm'W lonlat stuff --- it's harder to read, and it's
C  not any better (it was derived from the decimal values)
C Note latlon is in lonlat order, also this way in the data fields
C	READ (SCFUNI,'(A59,2F,A)') HDRSTR,LATLON(2),LATLON(1),TMPSTR
	READ (SCFUNI,'(A130)') HDRSTR
	J = 0
	K = 130
	DO WHILE ((J .LT. 3) .AND. (K .GT. 0))
	  IF (HDRSTR(K:K) .EQ. ',') THEN
	    J = J + 1
	  END IF
	  K = K - 1
	END DO
	IF (K .LE. 0) THEN
	  WRITE (0,*) ' cannot find latlonalt in hdr line'
	  LATLON(1) = 0.0
	  LATLON(2) = 0.0
	  TMPSTR(1:2) = '0 '
	END IF
C Next mess is since some numbers are printed as integers,
C  some as floats. So if the string has '.' then assume float
C Must traverse around in HDRSTR using ',' as field separators
	K=K+2
	J=K
	DO WHILE (HDRSTR(J:J) .NE. ',')
	  J=J+1
	END DO
	J=J-1
	IF (INDEX(HDRSTR(K:J),'.') .EQ. 0) THEN
	  READ (HDRSTR(K:J),'(I)') ITMP
	  LATLON(2) = REAL(ITMP)
	ELSE
	  READ (HDRSTR(K:J),'(F)') LATLON(2)
	END IF
	K=J+2
	J=K
	DO WHILE (HDRSTR(J:J) .NE. ',')
	  J=J+1
	END DO
	J=J-1
	IF (INDEX(HDRSTR(K:J),'.') .EQ. 0) THEN
	  READ (HDRSTR(K:J),'(I)') ITMP
	  LATLON(1) = REAL(ITMP)
	ELSE
	  READ (HDRSTR(K:J),'(F)') LATLON(1)
	END IF
	K=J+2
	J=K
	DO WHILE (HDRSTR(J:J) .NE. ',')
	  J=J+1
	END DO
	J=J-1
	IF (INDEX(HDRSTR(K:J),'.') .EQ. 0) THEN
	  READ (HDRSTR(K:J),'(I)') ITMP
	  RDATA(1) = REAL(ITMP)
	ELSE
	  READ (HDRSTR(K:J),'(F)') RDATA(1)
	END IF

C Read time of launch
	READ (SCFUNI,'(A35,3I,3(X,I2))') HDRSTR,
     .   IDATA(1),IDATA(2),IDATA(3),IDATA(4),IDATA(5),IDATA(6)

C If present, these header lines go into the "Additional Info/Comments" fields
	NUMOHD = 0
	DO 20, J = 6,11
	  READ(SCFUNI,'(A130)') HDRSTR
	  IF ((HDRSTR(1:1) .NE. '/') .AND. (HDRSTR(1:1) .NE. ' ')) THEN
	    IF (HDRSTR(1:8) .EQ. 'CAUTION-') THEN
C XXX fix for messed up comment line in TOGA-COARE
	      CDATA(NUMOHD*130+46:NUMOHD*130+80) =
     .         'CAUTION:                           '
	      CDATA(NUMOHD*130+81:NUMOHD*130+175) = HDRSTR(10:104)
	    ELSE
	      CDATA(NUMOHD*130+46:NUMOHD*130+175) = HDRSTR
	    END IF
	    NUMOHD = NUMOHD + 1
	  END IF
20	CONTINUE
	IDATA(7) = NUMOHD

C Read Nominal time of launch
	READ (SCFUNI,'(A130)') TMPSTR
C	READ (SCFUNI,'(A35,3I,3(X,I2))',IOSTAT=IOS) HDRSTR,
	READ (TMPSTR,'(A35,3I,3(X,I2))',IOSTAT=IOS) HDRSTR,
     .    TIME(1),TIME(2),TIME(3),TIME(4),TIME(5),TIME(6)
        IF ((IOS .NE. 0) .OR. (TMPSTR(36:36) .EQ. ' ')) THEN
C Error reading nominal time so use Launch time
          WRITE (0,'(A,A20,A)')
     .     'c2e: ', SCFFIL(1:20),
     .     ' Warning: cannot read Nominal time from Hdr Line 12'
          WRITE (0,'(A,A,A3,X,I2)')
     .     '     using Launch time as Nominal time;',
     .     ' stn/hr: ',CDATA(11:13),IDATA(4)
          TIME(1) = IDATA(1)
          TIME(2) = IDATA(2)
          TIME(3) = IDATA(3)
          TIME(4) = IDATA(4)
          TIME(5) = IDATA(5)
          TIME(6) = IDATA(6)
        END IF

C  Rest of header lines ignored
	DO 22, J = 13,15
	  READ(SCFUNI,'(A130)') HDRSTR
22	CONTINUE

C CTR is the number of records seen so far
	CTR = 0

C Loop top for data read
25	CONTINUE

	IF ((CTR * 5 + 5) .GT. (ISIZ + 7)) THEN
	  GO TO 991
	END IF
	IF ((CTR * 9 + 9) .GT. (RSIZ + 1)) THEN
	  GO TO 992
	END IF

C Begin replication
C
C  Note how the index to IDATA,RDATA is computed
C  The first time through, CTR==0 so time increment goes in RDATA(2)
C  There are RMUL number of integer values to be replicated, so on
C  the CTRth iteration, there are CTR*RMUL values to skip over

C Read SCF data
	READ (SCFUNI,*,IOSTAT=IOS) SDATA
	IF (IOS .NE. 0) THEN
C error (we have hit a new sounding) or end of file
	  GO TO 85
	END IF

C Convert SCF data
C Time increment (s s)
	IF (SDATA(1) .EQ. S4MISS) THEN
	  RDATA(CTR*RMUL+2) = RMISS
	ELSE
	  RDATA(CTR*RMUL+2) = SDATA(1)
	END IF

C Time significance (code, always set to 3)
	IDATA(CTR*IMUL+9) = 3

C We're going to assume here that QC flags match the data correctly,
C  i.e. if the data has a missing value then the QC flag is 9.0
C  and if the QC flag is 9.0 then the data has a missing value
C This is unlike the QCF->EBUFR stuff where the initial data sets
C  had some messed up missing flag/value pairs (see "q2e.f")

C Pressure (mb Pa)
	IF (SDATA(2) .EQ. S4MISS) THEN
	  RDATA(CTR*RMUL+3) = RMISS
	ELSE
	  RDATA(CTR*RMUL+3) = SDATA(2) * 100.0
	END IF

C QC flag for pressure
	IDATA(CTR*IMUL+10) = QCTAB(INT(SDATA(16)))

C Altitude (m m)
	IF (SDATA(15) .EQ. S5MISS) THEN
	  RDATA(CTR*RMUL+4) = RMISS
	ELSE
	  RDATA(CTR*RMUL+4) = SDATA(15)
	END IF

C
C Temp/dewpt conversions:
C *** Use 273.1, not 273.15 since CLASS format only has 1 decimal place
C

C Dry bulb temperature (C K)
	IF (SDATA(3) .EQ. S3MISS) THEN
	  RDATA(CTR*RMUL+5) = RMISS
	ELSE
	  RDATA(CTR*RMUL+5) = SDATA(3) + 273.1
	END IF

C QC flag for temperature
	IDATA(CTR*IMUL+11) = QCTAB(INT(SDATA(17)))

C Dew point (C K)
	IF (SDATA(4) .EQ. S3MISS) THEN
	  RDATA(CTR*RMUL+6) = RMISS
	ELSE
	  RDATA(CTR*RMUL+6) = SDATA(4) + 273.1
	END IF

C Humidity (% %)
	IF (SDATA(5) .EQ. S3MISS) THEN
	  RDATA(CTR*RMUL+7) = RMISS
	ELSE
	  RDATA(CTR*RMUL+7) = SDATA(5)
	END IF

C QC flag for dew point and humidity
	IDATA(CTR*IMUL+12) = QCTAB(INT(SDATA(18)))

C Wind speed (m/s m/s)
	IF (SDATA(8) .EQ. S3MISS) THEN
	  RDATA(CTR*RMUL+8) = RMISS
	ELSE
	  RDATA(CTR*RMUL+8) = SDATA(8)
	END IF

C Wind direction (deg deg_true)
	IF (SDATA(9) .EQ. S3MISS) THEN
	  RDATA(CTR*RMUL+9) = RMISS
	ELSE
	  RDATA(CTR*RMUL+9) = SDATA(9)
	END IF

C QC flag for wind; assuming that wind data match these flags correctly
	IF ((SDATA(19) .EQ. S2MISS) .OR. (SDATA(20) .EQ. S2MISS)) THEN
C Unchecked
	  UNCCT = UNCCT + 1
C	  GO TO 999
	  IDATA(CTR*IMUL+13) = 0
	ELSE IF ((SDATA(19) .EQ. 9.0) .OR. (SDATA(20) .EQ. 9.0)) THEN
C Data missing
	  IDATA(CTR*IMUL+13) = 15
	ELSE IF ((SDATA(19) .EQ. 3.0) .OR. (SDATA(20) .EQ. 3.0)) THEN
C Bad
	  IDATA(CTR*IMUL+13) = 3
	ELSE IF ((SDATA(19) .EQ. 2.0) .OR. (SDATA(20) .EQ. 2.0)) THEN
C Dubious (or "maybe")
	  IDATA(CTR*IMUL+13) = 2
	ELSE IF ((SDATA(19) .EQ. 4.0) .OR. (SDATA(20) .EQ. 4.0)) THEN
C Estimated
	  IDATA(CTR*IMUL+13) = 8
	ELSE IF ((SDATA(19) .EQ. 1.0) .AND. (SDATA(20) .EQ. 1.0)) THEN
C Good (both must be good)
	  IDATA(CTR*IMUL+13) = 1
	ELSE
C Unknown value --- should never ever get here
	  WRITE (0,'(1X,A50,A5,I2,A7,I5,A6,F5.1,F5.1)')
     .      'c2e: wind QC flags have unknown value for stn/hr: ',
     .      CDATA(11:15),TIME(4),
     .      ' level#',CTR+1,'flags=',SDATA(19),SDATA(20)
	  GO TO 999
	END IF

C Line 30 is here for historical reasons
30	CONTINUE


C Longitude (deg deg)
	IF ((SDATA(11) .EQ. S4MISS) .OR.
     .      (SDATA(11) .EQ. S3MISS)) THEN
	  RDATA(CTR*RMUL+10) = RMISS
	ELSE
	  RDATA(CTR*RMUL+10) = SDATA(11)
	END IF

C Latitude (deg deg)
	IF (SDATA(12) .EQ. S3MISS) THEN
	  RDATA(CTR*RMUL+11) = RMISS
	ELSE
	  RDATA(CTR*RMUL+11) = SDATA(12)
	END IF


C Increment # records seen
	CTR = CTR + 1

C Continue read loop
	GO TO 25

C Loop exit place for end of sounding
85	CONTINUE

C Replication factor == # of records
	IDATA(8) = CTR

C Call GER ENCOBS to encode data
C  That's right! One entire sounding is put into one single (E)BUFR record
	CALL ENCOBS(OUTUNI,1,TIME,LATLON,0,DFLAG,IDATA,RDATA,CDATA)

	IF (UNCCT .GT. 0) THEN
	 WRITE (0,'(X,A,A20,X,A,A3,X,I2,X,A,I,A)')
     .     'c2e: ', SCFFIL(1:20),
     .     'stn/hr: ',CDATA(11:13),TIME(4),
     .     ': QC flag for wind is 99.0 (unchecked) for ',
     .     UNCCT, ' records.'
	END IF

C End of File
	IF (IOS .LT. 0) THEN
	  GO TO 90
	END IF

C Continue with the next sounding in the file
	GO TO 12

C Loop exit place on EOF
90	CONTINUE

C Close in/out files
	CLOSE (UNIT=SCFUNI)
	CLOSE (UNIT=OUTUNI)

C End of main loop for cmd line args
	I = I + 1
	END DO

D	WRITE (*,*) ' End of c2e.'

	GO TO 999


C WRITE statements for general use
990	WRITE (0,*) ' c2e: Can''t open file.'
	GO TO 999
991	WRITE (0,*) ' c2e: Array overflow (IDATA)'
	GO TO 999
992	WRITE (0,*) ' c2e: Array overflow (RDATA)'
	GO TO 999


999	CONTINUE

C Fix for -fnonstd flag in -fast compiler option
C (This is for Suns.)
	CALL STANDARD_ARITHMETIC()

	STOP

	END










































