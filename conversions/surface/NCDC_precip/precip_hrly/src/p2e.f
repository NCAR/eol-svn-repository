C $Log$
C

C
C $Id: p2e.f,v 1.1 1993/02/16 22:44:36 john Exp $
C

	PROGRAM P2E
C
C P2E CONVERTS A PQCF FILE TO AN EBUFR FILE.
C
C Synopsis: p2e [<pqcf_data_file>]
C
C Written by: John J. Allison / john@lightning.ofps.ucar.edu
C             NCAR/OFPS
C             19 Oct 1992
C
C Modification history:
C  06 Nov 1992 by John J. Allison
C    the PQCF has changed to include network and station strings and
C    to exclude the time offset from GMT
C
C  18 Oct 1993 by Linda E. Cully
C     Modified s/w to now read fest_sites_sorted file. This way all
C     precip s/w is using same sites info. This file holds more sites
C     than orig. coop.lst file, so several array sizes had to be 
C     increased form 2800 to 3821. Also changed the location s/w
C     accesses the JSBUFF for the longitude from 65:73 to 67:75.
C     Changed s/w that assumed that input file name had only one
C     period to now match whole suffix. This allows complete pathname
C     to be used when specifying the input file. S/w now assume
C     input files end in '.pqcf' suffix.
C
C Description:
C  This program converts a PQCF (Precip QCF) file into an EBUFR file.
C It was modified from Q2E, which converts QCF to EBUFR. P2E uses the
C Generic Encoder Routines (GER) to write an EBUFR file. The basic
C structure of this program is to call the GER setup routines RDCONF
C and WRTHDR, then for each PQCF record, place the values in the
C [CIR]DATA arrays, then call ENCOBS to encode the record into EBUFR.
C See the file "p2e.doc" for more information. Also, the GER documentation
C and the Q2E/H2E documentation.
C 
C Input:
C  PQCF data file (<pqcf_data_file>).
C
C Output:
C  EBUFR file.
C  When compiled with "-xld" option, also prints out progress messages.
C  These are marked in the code with a 'D' in the 1st column.
C
C Files:
C  PQCF data file - the file to be converted
C    argv[1] taken to be PQCF data file
C  EBUFR output file - contains the converted data
C    named same as PQCF file, with ".ebufr" extension
C  Control file - tells GER what type of data is coming
C    assumed to be named "control_pqcf.txt"
C  Class description file - contains list of class descriptions
C    assumed to be named "class_file"
C  Parameter description file - contains param. descriptions
C    assumed to be named "desc_file"
C  Code/Flag table files - one for each code or flag table
C    must be named according to EMPRESS and GEXT conventions
C  P2E opens and closes all files. Code/Flag table files are
C    opened/closed by GER via calls to GTFXYU and CLSUNI. GER writes
C    to EBUFR output file, and reads from the rest (execpt PQCF file).
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
C my function for searching station list
	INTEGER JSCK

C Constant Definitions
	REAL QRMISS
	INTEGER QIMISS
	PARAMETER (QRMISS=-999.99,QIMISS=-99)
	INTEGER ISIZ,RSIZ
	PARAMETER (ISIZ=300,RSIZ=96)

C Internal Variables
C IDATA,RDATA,CDATA,DFLAG,CLIBYT,LATLON,TIME get sent to GER ENCOBS
C *FIL and *UNI are the filenames and unit numbers
C QCTAB is a lookup table for QC flags
C everything else is just temporary stuff
	INTEGER IDATA(ISIZ),TIME(6),IMISS,QCTAB(256)
	INTEGER CLIBYT,ARGCT,I,RECCT,LOC
	INTEGER ENCCT,MISCT
	INTEGER QCFUNI,OUTUNI,CTLUNI,CLAUNI,PRMUNI
	LOGICAL DFLAG,DATSEN,HRLY
	REAL RDATA(RSIZ),LATLON(2),RMISS
	CHARACTER*10 NETSTR,STNSTR
	CHARACTER*20 CDATA
	CHARACTER*256 QCFFIL,OUTFIL,CTLFIL,CLAFIL,PRMFIL,ARGV
	REAL PRECIP(96)
	INTEGER QUAL(96)
	CHARACTER QCF(96)
	INTEGER MAXNUM,K,CT,J,QCFLAG,IOS

C vars to hold station list
	INTEGER JSLAT(3821),JSLON(3821),JSUNIT,JSCT
	CHARACTER*10 JSSTN(3821)
	CHARACTER*256 JSFILE,JSBUFF
	LOGICAL JSFLAG
C	LOGICAL JSHRLY(3821)
	REAL TMPF
C	REAL TMPG
	INTEGER TMPI,TMPJ

	COMMON /JS1/ JSLAT,JSLON,JSCT,JSSTN

C Must get RMISS set to all 1s
	EQUIVALENCE (IMISS,RMISS)

C read in station list

	JSUNIT = 16
	JSCT = 0
	JSFLAG = .FALSE.
C Was:	JSFILE = '/home/john/work/coop.lst'
	JSFILE = 'fest_sites_sorted'
	OPEN (FILE=JSFILE, UNIT=JSUNIT, STATUS='OLD', ERR=9990)
10	CONTINUE
	READ (JSUNIT,'(A)', ERR=9991, END=20) JSBUFF
	JSCT = JSCT + 1
	READ (JSBUFF(12:21),'(A10)') JSSTN(JSCT)
C	IF (JSBUFF(7:10) .EQ. 'HRLY') THEN
C	  JSHRLY(JSCT) = .TRUE.
C	ELSE
C	  JSHRLY(JSCT) = .FALSE.
C	END IF
	READ (JSBUFF(58:63),'(F)') TMPF
	TMPF = TMPF * 100.0
	JSLAT(JSCT) = NINT(TMPF)
	READ (JSBUFF(67:75),'(F)') TMPF 
	TMPF = TMPF * 100.0
	JSLON(JSCT) = NINT(TMPF)
	JSFLAG = .TRUE.
	GO TO 10
20	CONTINUE

C print station list
D	IF (JSFLAG) THEN
D	  DO 21, J=1,JSCT
D	    WRITE (0,'(I,X,I,X,A10)') JSLAT(J),JSLON(J),JSSTN(J)
D21	  CONTINUE
D	ELSE
D	  WRITE (0,*) 'JSFLAG == .FALSE.'
D	END IF

C Initialize Variables
C Missing value for integer == all 1s for 32 bits
	IMISS = 2147483647
	DFLAG = .FALSE.
	TIME(6) = 0
	I = 1
C Remember: GTFXYU uses unit 10.
	QCFUNI = 11
	OUTUNI = 12
	CTLUNI = 13
	CLAUNI = 14
	PRMUNI = 15

	CTLFIL = 'control_pqcf.txt'
	CLAFIL = 'class_file'
	PRMFIL = 'desc_file'

C Lookup table for quality control code values
	QCTAB(85) = 0
	QCTAB(71) = 1
	QCTAB(66) = 2
	QCTAB(68) = 3
	QCTAB(77) = 15
	QCTAB(78) = 4
	QCTAB(88) = 5
	QCTAB(69) = 6

C ***** next line TEMPORARY for testing ONLY
	QCTAB(74) = 9


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

	QCFFIL = ARGV
C QCF filename is in form of file.pqcf or file
C EBUFR output file is in form of file.ebufr
	LOC = INDEX(ARGV,'.pqcf')
	IF (LOC .EQ. 0) THEN
	  LOC = INDEX(ARGV,' ')
	  LOC = LOC - 1
	  OUTFIL = ARGV(1:LOC) // '.ebufr'
	ELSE
	  OUTFIL = ARGV(1:LOC) // 'ebufr'
	END IF

C A 'D' in the 1st column means only compile this statement
C  when the -xld flag is given to f77. For debugging purposes.
D	WRITE (0,*) 'PQCF filename: ',QCFFIL
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


C Read first record to get TIME to send to WRTHDR

	CALL RDPQF(QCFUNI,TIME,NETSTR,STNSTR,LATLON,CLIBYT,
&	  PRECIP,QUAL,QCF,HRLY,IOS)
	IF (IOS .NE. 0) THEN
	  GO TO 198
	END IF
D	WRITE (*,'(A,6(X,I2))') 'TIME = ',TIME
D	WRITE (*,'(A,A10,A,A10)') 'NET = ',NETSTR,' STN = ',STNSTR
D	WRITE (*,'(A,X,F10.5,X,F11.5,X,I3)') 'LATLONCLI = ',
D&	  LATLON,CLIBYT
D	WRITE (*,*) 'HRLY=',HRLY
D	STOP

C Call GER WRTHDR to write EBUFR header data

	CALL WRTHDR (OUTUNI,CLAUNI,PRMUNI,GTFXYU,CLSUNI,TIME,1)
	CLOSE (UNIT=CLAUNI)
	CLOSE (UNIT=PRMUNI)

D	WRITE (0,*) 'GER WRTHDR returned.'

C Skip the read the first time (just read above)
C I know this is messy but reading the first record was an afterthought
	GO TO 102


C Data processing loop:
C  Read data
C  "Compress" data into time interval "replications"
C  Call GER ENCOBS to write EBUFR data


100	CONTINUE

C Read data

	CALL RDPQF(QCFUNI,TIME,NETSTR,STNSTR,LATLON,CLIBYT,
&	  PRECIP,QUAL,QCF,HRLY,IOS)
	IF (IOS .NE. 0) THEN
	  GO TO 198
	END IF

102	CONTINUE

	IF (LATLON(1) .EQ. -999.99) THEN
	  LATLON(1) = -360.0
	END IF
	IF (LATLON(2) .EQ. -999.99) THEN
	  LATLON(2) = -180.0
	END IF

	DATSEN = .FALSE.
	RECCT = RECCT + 1
CD	WRITE (0,'(A,I,A)') 'PQCF record ',RECCT,' read.'

C latlon,clibyt already correct
C time (includes date)
	TIME(1) = TIME(1) + 1900

	IF (NETSTR(1:4) .EQ. 'NCDC') THEN
	  CDATA(1:10) = 'COOP      '
	  IF (JSFLAG) THEN
	    TMPI = NINT(LATLON(1)*100.0)
	    TMPJ = NINT(LATLON(2)*100.0)
	    CDATA(11:20) = '          '
	    J = JSCK(TMPI,TMPJ,CDATA)
D	    IF (J .GT. 0) THEN
D	      WRITE
D&		(0,'(I4,X,A10,X,I8,X,I9,X,F10.5,X,F11.5)')
D&		J,JSSTN(J),JSLAT(J),JSLON(J),
D&	        LATLON(1),LATLON(2)
D	    ELSE
D             WRITE (0,'(A,F10.5,X,F11.5,X,I8,X,I9)')
D&	      'Not in fest_sites files: ',
D&	      LATLON(1),LATLON(2),TMPI,TMPJ
D	    END IF
	  END IF

C---------------------------------------------------------------
C	  TMPF = LATLON(1)*100.0
C	  TMPG = LATLON(2)*100.0
C	  TMPI = INT(TMPF)
C	  TMPJ = INT(TMPG)
C	  DO 103, J=1,JSCT
C we know that there are NO duplicates in 'fest_sites'?
C	    IF (HRLY .NE. JSHRLY(J)) THEN
C	      GO TO 103
C	    END IF
C	    IF ((TMPI .EQ. JSLAT(J)) .AND.
C&		(TMPJ .EQ. JSLON(J))) THEN
C		CDATA(11:20) = JSSTN(J)
C		WRITE
C&		(0,'(I3,X,L1,X,L1,X,A10,X,I8,X,I9,X,F10.5,X,F11.5)')
C&		J,HRLY,JSHRLY(J),JSSTN(J),JSLAT(J),JSLON(J),
C&		(0,'(I3,X,A10,X,I8,X,I9,X,F10.5,X,F11.5)')
C&		J,JSSTN(J),JSLAT(J),JSLON(J),
C&	        LATLON(1),LATLON(2)
C		GO TO 104
C	    END IF
C103	  CONTINUE
C---------------------------------------------------------------

	ELSE
	  CDATA(1:10) = NETSTR
	  CDATA(11:20) = STNSTR
	END IF

104	CONTINUE
	IF (HRLY) THEN
	  MAXNUM = 24
	ELSE
	  MAXNUM = 96
	ENDIF
	K = 1
	CT = 1
	IF (QUAL(1) .EQ. 3) THEN
	  RDATA(1) = RMISS
	  IDATA(3) = 3
	  IDATA(4) = 15
	ELSE IF (QCF(1) .EQ. 'N') THEN
	  RDATA(1) = RMISS
	  IDATA(3) = 7
	  IDATA(4) = 4
	ELSE IF ((PRECIP(1) .EQ. QRMISS) .OR. (QUAL(1) .EQ. 7) .OR.
&	    (QCF(1) .EQ. 'M')) THEN
	  RDATA(1) = RMISS
	  IDATA(3) = 7
	  IDATA(4) = 15
	ELSE
	  RDATA(1) = PRECIP(1)
	  IDATA(3) = QUAL(1)
	  IDATA(4) = QCTAB(QCF(1))
	  DATSEN = .TRUE.
	END IF

	DO 110, J=2,MAXNUM,1
	  QCFLAG = QCTAB(QCF(J))
C---------------------------------------------------------------
C	  IF ( ( (PRECIP(J) .EQ. RDATA(K)) .OR.
C&	         ( (PRECIP(J) .EQ. QRMISS) .AND.
C&	           (RDATA(K) .EQ. RMISS) ) ) .AND.
C---------------------------------------------------------------
	  IF ((PRECIP(J) .EQ. PRECIP(J-1)) .AND.
&	      (QUAL(J) .EQ. IDATA(3+(K-1)*3)) .AND.
&	      (QCFLAG .EQ. IDATA(4+(K-1)*3))) THEN
	    CT = CT + 1
	  ELSE
	    IF (QUAL(J) .EQ. 3) THEN
	      RDATA(K+1) = RMISS
	      IDATA(3+K*3) = 3
	      IDATA(4+K*3) = 15
	    ELSE IF (QCF(J) .EQ. 'N') THEN
	      RDATA(K+1) = RMISS
	      IDATA(3+K*3) = 7
	      IDATA(4+K*3) = 4
	    ELSE IF ((PRECIP(J) .EQ. QRMISS) .OR. (QUAL(J) .EQ. 7) .OR.
&	        (QCF(J) .EQ. 'M')) THEN
	      RDATA(K+1) = RMISS
	      IDATA(3+K*3) = 7
	      IDATA(4+K*3) = 15
	    ELSE
	      RDATA(K+1) = PRECIP(J)
	      IDATA(3+K*3) = QUAL(J)
	      IDATA(4+K*3) = QCFLAG
	      DATSEN = .TRUE.
	    END IF
	    IDATA(2+(K-1)*3) = CT
	    CT = 1
	    K = K + 1
	  END IF
110	CONTINUE
	IDATA(1) = K
	IDATA(2+(K-1)*3) = CT

CD	WRITE (0,*) 'PQCF data converted.'

C If we've seen some data (i.e. they're not all Missing), then
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

	WRITE (0,'(A50,A)') QCFFIL,': reached EOF'
	WRITE (0,'(I,A)') RECCT,' PQCF records seen.'
	WRITE (0,'(I,A)') ENCCT,' records encoded.'
	WRITE (0,'(I,A)') MISCT,' records with all missing.'

C Close files that are still open
C  unless they're stdin,stdout
	IF (QCFUNI .NE. 5) THEN
	  CLOSE (QCFUNI)
	  CLOSE (OUTUNI)
	END IF

C End of main DO loop for processing cmd line args
	I = I + 1
	END DO

C Quit

D	WRITE (0,*) 'End of p2e.'
	GO TO 9999

C Error messages for general use, written to stderr

9990	WRITE (0,*) 'Error opening a file.'
	GO TO 9999
9991	WRITE (0,*) 'Error reading fest_sites_sorted'
	GO TO 9999

9999	CONTINUE

C Fix for -fnonstd flag in -fast option
	CALL STANDARD_ARITHMETIC()

	STOP

	END

C Here's the write/format statements used by ep2pq.f:
C	WRITE (OUTUNI,211)
C&	  CLYEAR(3:4),CLMON,CLDAY,
C&	  CLHOUR,CLMIN,CLSEC,
C&	  NETSTR,STNSTR,
C&	  CLLAT,CLLON,CLIBYT,
C&	  (PRECIP(K), QUAL(K), QCFLG(K), K=1,MAXTIM)
C211	FORMAT (A2,'/',A2,'/',A2,X,A2,':',A2,':',A2,
C&	    X,A10,X,A10,
C&	    X,F10.5,X,F11.5,X,A3,X,I3,96(: X,F7.2,X,I1,X,A1))


	SUBROUTINE RDPQF(IUNIT,TIME,NETSTR,STNSTR,LATLON,CLIBYT,
&	  PRECIP,QUAL,QCF,HRLY,IOS)
	INTEGER IUNIT
	INTEGER TIME(6)
	CHARACTER*10 NETSTR,STNSTR
	REAL LATLON(2)
	INTEGER CLIBYT
	REAL PRECIP(*)
	INTEGER QUAL(*)
	CHARACTER QCF(*)
	LOGICAL HRLY
	INTEGER IOS

	CHARACTER*1225 BUFFER
	INTEGER K,MAXTIM

	READ (IUNIT,'(A)',IOSTAT=IOS) BUFFER
	IF (IOS .NE. 0) THEN
	  RETURN
	END IF
	DO 1000, K=1225,1,-1
	  IF (BUFFER(K:K) .NE. ' ') THEN
	    GO TO 1001
	  END IF
1000	CONTINUE
1001	CONTINUE
	IF (K .GT. 365) THEN
	  HRLY = .FALSE.
	  MAXTIM = 96
	ELSE
	  HRLY = .TRUE.
	  MAXTIM = 24
	END IF
	READ (BUFFER,1002) TIME,NETSTR,STNSTR,LATLON,CLIBYT,
&	  (PRECIP(K), QUAL(K), QCF(K), K=1,MAXTIM)
1002	FORMAT (I2,'/',I2,'/',I2,X,I2,':',I2,':',I2,
&	    X,A10,X,A10,
&	    X,F10.5,X,F11.5,X,I3,96(: X,F7.2,X,I1,X,A1))

	RETURN
	END


	INTEGER FUNCTION JSCK (LATI,LONI,CDATA)
	INTEGER LATI,LONI
	CHARACTER*20 CDATA
C
C JSCK - check if latlon LATI,LONI is in the station list
C      - implements a binary search (stn list is assumed sorted)
C
C 18 Oct 93 lec
C   Note that a binary search of this data may not locate
C   every lat/lon pair even if it is in the stn list. This is 
C   a fn of the binary search procedure and the fact that it
C   is neither the lat nor lon which makes the search unique,
C   but the pair lat/lon.  If pairs in the stn list are not
C   located, this procedure should be reevaluated.
C
	INTEGER J,MIN,MAX

	INTEGER JSLAT(3821),JSLON(3821),JSCT
	CHARACTER*10 JSSTN(3821)
	COMMON /JS1/ JSLAT,JSLON,JSCT,JSSTN

C initialize
	J = JSCT / 2
	MIN = 1
	MAX = JSCT
2001	CONTINUE
C found
	IF ((JSLAT(J) .EQ. LATI) .AND. (JSLON(J) .EQ. LONI)) THEN
	  CDATA(11:20) = JSSTN(J)
	  JSCK = J
	  RETURN
	END IF
C not found
	IF (MIN .GE. MAX) THEN
	  JSCK = 0
	  RETURN
	END IF
C move indices
	IF (JSLAT(J) .GT. LATI) THEN
	  MAX = J - 1
	ELSE IF (JSLAT(J) .LT. LATI) THEN
	  MIN = J + 1
	ELSE IF (JSLON(J) .GT. LONI) THEN
	  MAX = J - 1
	ELSE
	  MIN = J + 1
	END IF
	J = MIN + (MAX - MIN) / 2
C loop
	GO TO 2001
	END
