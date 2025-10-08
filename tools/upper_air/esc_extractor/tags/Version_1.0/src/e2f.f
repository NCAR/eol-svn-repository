C
C $Id: e2f.f,v 1.1 1992/11/02 21:08:32 john Exp $
C
	PROGRAM E2F
C
C E2F converts a SCF file with std. dev. QC flags into a file
C     with Good/Bad/etc QC flags.
C
C Synopsis: e2f [<class_data_file>]
C
C Written by: John J. Allison
C             UCAR/OFPS
C             15 Jun 1992
C
C Modification history:
C  <none>
C
C Description:
C  This is a quick hack copied from S2M.
C  It is intended to do a quick and dirty quality control on SCF files,
C in order to compare the results of S2M (seconds to millibars conversion)
C with the results of Claude Morel's HP-BASIC sec to mb program.
C  If the std dev = 77, then set the flag to Good.
C  If the std dev < some limit, then set the flag to Good.
C  Else, set the flag to Bad.
C
C Input:
C  CLASS data file (<class_data_file>).
C
C Output:
C
C Files:
C
C Subroutines and Functions:
C (SUN) FORTRAN IARGC,GETARG
C FORTRAN ABS,SIGN,INT,REAL,LOG,EXP,SQRT
C
C23456789012345678901234567890123456789012345678901234567890123456789012

C External Subroutines and Functions
	EXTERNAL GTNXIN
	REAL CDEWPT,CWNDIR
	EXTERNAL CDEWPT,CWNDIR
C FORTRAN function for cmd line args
	INTEGER IARGC

C Constant Definitions
C Maximum # of input records (array size)
	INTEGER MAXREC
	PARAMETER (MAXREC=2000)
C A,B time ranges (in seconds) for Pressure,Temperature,Humidity,Wind
C   specify the time range to look for interpolation data
C   (want 2 values <= A apart before <= B apart before > B apart)
	REAL PAR,PBR,TAR,TBR,HAR,HBR,WAR,WBR
	PARAMETER (PAR=100.0,PBR=200.0)
	PARAMETER (TAR=50.0,TBR=100.0)
	PARAMETER (HAR=50.0,HBR=100.0)
	PARAMETER (WAR=50.0,WBR=100.0)
C MINPRS - Minimum Pressure to output (i.e. stopping point)
C PRSINT - Interval on which to interpolate
	REAL MINPRS,PRSINT
	PARAMETER (MINPRS=100.0,PRSINT=10.0)

C Internal Variables
C SDATA - SCF (input) Data (all of it)
	REAL SDATA(21,MAXREC)
C CTR - index of last record in SDATA
	INTEGER CTR
C File units,names
	INTEGER SCFUNI,OUTUNI,LOGUNI
	CHARACTER*256 SCFFIL,OUTFIL,LOGFIL,ARGV
C Temporary, loop control variables
	INTEGER I,J,K,ARGCT,LOC,IOS
	CHARACTER*130 HDRSTR


C Initialize Variables
	I = 1
C Remember: GTFXYU uses unit 10.
	SCFUNI = 11
	OUTUNI = 12
	LOGUNI = 13

C Get number of cmd line args
	ARGCT = IARGC()
C If no arguments, error: can't use stdin/stdout
	IF (ARGCT .EQ. 0) THEN
	  SCFUNI = 5
	  SCFFIL = 'stdin'
	  OUTUNI = 6
	  OUTFIL = 'stdout'
	  LOGFIL = 'e2f.log'
	  GO TO 50
	END IF

C Loop to run program once on each file named
	DO WHILE (I .LE. ARGCT)

C Get SCF filename from command line
	CALL GETARG(I,ARGV)
	SCFFIL = ARGV
C SCF filename is in form of file.class or file
C mb output file is in form of file.??mb, where ?? = Pressure Interval,
C  or 'XX' if PRSINT > 99
C *** assumes that the first period in the filename is also the last!!!
	LOC = INDEX(ARGV,'.')
	IF (LOC .EQ. 0) THEN
	  LOC = INDEX(ARGV,' ')
	  ARGV(LOC:LOC) = '.'
	END IF
	OUTFIL(1:LOC+4) = ARGV(1:LOC) // 'jcls'
	LOGFIL(1:LOC+3) = ARGV(1:LOC) // 'log'

C Open Files

	OPEN (FILE=SCFFIL, UNIT=SCFUNI, STATUS='OLD', ERR=990)
	OPEN (FILE=OUTFIL, UNIT=OUTUNI, STATUS='NEW', ERR=990)
50	CONTINUE
	OPEN (FILE=LOGFIL, UNIT=LOGUNI, STATUS='UNK', ERR=990)

D	WRITE (LOGUNI,*) 'Files open.'

C A 'D' in the 1st column means only compile this statement
C  when the -xld flag is given to f77. For debugging purposes.
D	WRITE (LOGUNI,'(A20)') 'SCF filename: ',SCFFIL
D	WRITE (LOGUNI,'(A20)') 'Output filename: ',OUTFIL
D	WRITE (LOGUNI,'(A20)') 'Log filename: ',LOGFIL

C Initialize variables for each new input file

C Read,write SCF header lines
	DO 20, J = 1,15
	  READ (SCFUNI,'(A130)') HDRSTR
	  DO 15, K = 130,1,-1
	    IF (HDRSTR(K:K) .NE. ' ') THEN
	      GO TO 16
	    END IF
15	  CONTINUE
16	  CONTINUE
	  WRITE (OUTUNI,'(A)') HDRSTR(1:K)
20	CONTINUE

C Write first record as is
	CALL RDSCF (SCFUNI,SDATA,1,IOS)
	CTR = 1
	CALL WRTSCF (OUTUNI,SDATA,1,IOS)
C Read all of the SCF data
25	CONTINUE
C	READ (SCFUNI,*,END=30,ERR=991) SDATA(1:21,1)
	CALL RDSCF (SCFUNI,SDATA,1,IOS)
	CTR = CTR + 1
	IF (IOS .LT. 0) THEN
C EOF
	  GO TO 30
	ELSE IF (IOS .GT. 0) THEN
C ERR
	  GO TO 991
	END IF
C	SDATA(16,1) = 1.0
C	SDATA(17,1) = 1.0
C	SDATA(18,1) = 1.0
C	SDATA(19,1) = 1.0
C	SDATA(20,1) = 1.0
C	SDATA(21,1) = 1.0
C	GO TO 26
	IF ((SDATA(16,1) .EQ. 77) .OR.
&	    (SDATA(16,1) .LE. 1.0)) THEN
	  SDATA(16,1) = 1.0
	ELSE IF (SDATA(16,1) .EQ. 99.0) THEN
	  SDATA(16,1) = 9.0
	ELSE
	  SDATA(16,1) = 3.0
	END IF
	IF ((SDATA(17,1) .EQ. 77) .OR.
&	    (SDATA(17,1) .LE. 1.0)) THEN
	  SDATA(17,1) = 1.0
	ELSE IF (SDATA(17,1) .EQ. 99.0) THEN
	  SDATA(17,1) = 9.0
	ELSE
	  SDATA(17,1) = 3.0
	END IF
	IF ((SDATA(18,1) .EQ. 77) .OR.
&	    (SDATA(18,1) .LE. 3.0)) THEN
	  SDATA(18,1) = 1.0
	ELSE IF (SDATA(18,1) .EQ. 99.0) THEN
	  SDATA(18,1) = 9.0
	ELSE
	  SDATA(18,1) = 3.0
	END IF
	IF (SDATA(19,1) .GT. SDATA(20,1)) THEN
	  SDATA(19,1) = SDATA(20,1)
	END IF
	IF ((SDATA(19,1) .EQ. 77) .OR.
&	    (SDATA(19,1) .LE. 2.0)) THEN
	  SDATA(19,1) = 1.0
	  SDATA(20,1) = 1.0
	ELSE IF (SDATA(16,1) .EQ. 99.0) THEN
	  SDATA(19,1) = 9.0
	  SDATA(20,1) = 9.0
	ELSE
	  SDATA(19,1) = 3.0
	  SDATA(20,1) = 3.0
	END IF
26	CONTINUE
	CALL WRTSCF (OUTUNI,SDATA,1,IOS)
	GO TO 25

C Loop exit place on EOF
30	CONTINUE

C Close in/out files, unless they're stdin,stdout
	IF (SCFUNI .NE. 5) THEN
	  CLOSE (UNIT=SCFUNI)
	  CLOSE (UNIT=OUTUNI)
	END IF

C May want to 'D' following lines
	WRITE (LOGUNI,'(A20,A2,I,A)') SCFFIL,': ',CTR,' SCF records read.'

C End of main loop for cmd line args
	I = I + 1
	END DO

D	WRITE (LOGUNI,*) 'End of e2f.'
	CLOSE (UNIT=LOGUNI)

	GO TO 999


C FORMAT statements for general use
C800	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
C&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)


C WRITE statements for general use
990	WRITE (0,*) 'e2f: Can''t open file.'
	GO TO 999

991	WRITE (0,*) 'e2f: Error reading SCF file.'
	GO TO 999

999	CONTINUE

C Fix for -fnonstd flag in -fast compiler option
	CALL STANDARD_ARITHMETIC()

	STOP
	END



	SUBROUTINE RDSCF (FILENO,SDATA,INDEX,IOS)
	INTEGER FILENO,INDEX,IOS
	REAL SDATA(21,*)
C
C Read one SCF record from FILENO into SDATA(*,INDEX)
C Returns IOS from READ
C
	READ (FILENO,1616,IOSTAT=IOS)
&	  SDATA(1,INDEX),SDATA(2,INDEX),SDATA(3,INDEX),SDATA(4,INDEX),
&	  SDATA(5,INDEX),SDATA(6,INDEX),SDATA(7,INDEX),SDATA(8,INDEX),
&	  SDATA(9,INDEX),SDATA(10,INDEX),SDATA(11,INDEX),
&	  SDATA(12,INDEX),SDATA(13,INDEX),SDATA(14,INDEX),
&	  SDATA(15,INDEX),SDATA(16,INDEX),SDATA(17,INDEX),
&	  SDATA(18,INDEX),SDATA(19,INDEX),SDATA(20,INDEX),
&	  SDATA(21,INDEX)
	RETURN

1616	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)

	END



	SUBROUTINE WRTSCF (FILENO,SDATA,INDEX,IOS)
	INTEGER FILENO,INDEX,IOS
	REAL SDATA(21,*)
C
C Write SDATA(*,INDEX) onto FILENO
C Returns IOS from WRITE
C
	WRITE (FILENO,1717,IOSTAT=IOS)
&	  SDATA(1,INDEX),SDATA(2,INDEX),SDATA(3,INDEX),SDATA(4,INDEX),
&	  SDATA(5,INDEX),SDATA(6,INDEX),SDATA(7,INDEX),SDATA(8,INDEX),
&	  SDATA(9,INDEX),SDATA(10,INDEX),SDATA(11,INDEX),
&	  SDATA(12,INDEX),SDATA(13,INDEX),SDATA(14,INDEX),
&	  SDATA(15,INDEX),SDATA(16,INDEX),SDATA(17,INDEX),
&	  SDATA(18,INDEX),SDATA(19,INDEX),SDATA(20,INDEX),
&	  SDATA(21,INDEX)
	RETURN

1717	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)

	END

