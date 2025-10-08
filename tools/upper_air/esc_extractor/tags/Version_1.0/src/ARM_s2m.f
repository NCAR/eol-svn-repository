C
C $Id: s2m.f,v 1.3 1992/12/10 18:44:24 john Exp $
C
	PROGRAM S2M
C
C S2M CONVERTS A CLASS FILE IN x-SECOND FORM TO INTERPOLATED y-MB INTERVALS.
C
C Synopsis: s2m [<class_data_file>]
C
C Written by: John J. Allison / john@lightning.ofps.ucar.edu
C             NCAR/STORM Project Office
C             10 Jun 1992
C
C Modification history:
C 03 Sept 1996 by Darren R. Gallant
C  adapted for Arm Cart soundings which have undergone auto qc but not visual
C  qc. Change 99.0 QC values to 1.0 for interpolation in RDSCF then change 1.0
C  QC values back to 99.0 in WRTSCF 
C 06 Feb 1996 by Darren R. Gallant
C  added logic preventing interpolation of missing latitudes and longitudes
C 19 Jan 1996 by Scot M. Loehrer
C  ensured that if first data point in file has missing pressure, it
C  is not used in interpolation (problem with GTS data).
C 17 Aug 1992 by John J. Allison
C  if rel hum is 0 then humidity gets missing (previously got -NaN)
C  added CKWID()
C 05 Aug 1992 by John J. Allison
C  changed RDSCF() to list-directed input (not using FORMAT) in order to
C  handle some bad data that overran its field width for missing values
C 20 Jul 1992 by John J. Allison
C  added a bunch of stuff to compensate for undocumented versions of
C  missing values (most data that I'm getting uses 999.0 for any missing
C  value, even though my SCF documentation had varying missing values
C  depending on field width)
C
C Description:
C  y is specified as PARAMETER constants PRSINT (pressure interval).
C x does not need to be specified.
C
C Input:
C  CLASS data file (<class_data_file>).
C
C Output:
C
C Files:
C
C Subroutines and Functions:
C FORTRAN ABS SIGN INT REAL LOG EXP SQRT
C SUN IARGC GETARG
C GNI GTNXIN CDEWPT CWNDIR
C S2M RDSCF WRTSCF SCFCP CKQC CKWID
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
	PARAMETER (MAXREC=4000)
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
	PARAMETER (MINPRS=50.0,PRSINT=5.0)

C Internal Variables
C SDATA - SCF (input) Data (all of it)
C IDATA - Interpolated (output) Data (one record's worth)
C RMB - current pressure level to be interpolated
	REAL SDATA(21,MAXREC),IDATA(21),RMB
C CTR - index of last record in SDATA
	INTEGER CTR
C Indices for hi/lo interpolation points for each field
	INTEGER IPHI,IPLO,ITHI,ITLO,IRHI,IRLO,IWHI,IWLO
C QC Flags for each field
	REAL QCPRS,QCTMP,QCHUM,QCWND
C File units,names
	INTEGER SCFUNI,OUTUNI,LOGUNI
	CHARACTER*256 SCFFIL,OUTFIL,LOGFIL,ARGV
C Number of records output, interpolated
	INTEGER OUTCT,INTCT
C Temporary, loop control variables
	INTEGER I,J,K,ARGCT,LOC,IOS
	REAL TIME,DIFF,HICO,LOCO
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
	  LOGFIL = 's2m.log'
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
	OUTFIL(1:LOC) = ARGV(1:LOC)
	LOGFIL(1:LOC+3) = ARGV(1:LOC) // 'log'
	IF (PRSINT .LT. 100.0) THEN
	  WRITE (OUTFIL(LOC+1:LOC+4),'(I2.2,A2)') INT(PRSINT),'mb'
	ELSE
	  OUTFIL = OUTFIL // 'XXmb'
	END IF

C Open Files

	OPEN (FILE=SCFFIL, UNIT=SCFUNI, STATUS='OLD', ERR=990)
	OPEN (FILE=OUTFIL, UNIT=OUTUNI, STATUS='NEW', ERR=990)
50	CONTINUE
	OPEN (FILE=LOGFIL, UNIT=LOGUNI, STATUS='NEW', ERR=990)

D	WRITE (LOGUNI,*) 'Files open.'

C A 'D' in the 1st column means only compile this statement
C  when the -xld flag is given to f77. For debugging purposes.
D	WRITE (LOGUNI,'(A20)') 'SCF filename: ',SCFFIL
D	WRITE (LOGUNI,'(A20)') 'Output filename: ',OUTFIL
D	WRITE (LOGUNI,'(A20)') 'Log filename: ',LOGFIL

C Initialize variables for each new input file
	IPHI = 1
	IPLO = 1
	ITHI = 1
	ITLO = 1
	IRHI = 1
	IRLO = 1
	IWHI = 1
	IWLO = 1
	OUTCT = 0
	INTCT = 0

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

	J = 1
C Read all of the SCF data
25	CONTINUE
C	READ (SCFUNI,*,END=30,ERR=991) SDATA(1:21,J)
	CALL RDSCF (SCFUNI,SDATA,J,IOS)
        
	IF (IOS .LT. 0) THEN
C EOF
	  GO TO 30
	ELSE IF (IOS .GT. 0) THEN
C ERR
	  GO TO 991
	END IF
        if(sdata(2,j) .eq. 9999.0) then
          go to 25
        end if 
C Next line fixes lapse in QC program
	CALL CKQC (SDATA,J)
C Next 2 lines make sure that fields 13,14 don't ever have data
C (if a desired pressure appears in input, then the input is copied
C  to the output with no change; this ensures that 13,14 won't have
C  the original data in them and cause an inconsistency with the rest
C  of the interpolated data, which sets them to missing)
	SDATA(13,J) = 999.0
	SDATA(14,J) = 999.0
	J = J + 1
	IF (J .LT. MAXREC) THEN
	  GO TO 25
	ELSE
	  WRITE (LOGUNI,'(A,I5,A)') 'Read in ',J-1,' records without EOF.'
	  WRITE (LOGUNI,'(A)') 'Change MAXREC and recompile.'
	END IF
C Loop exit place on EOF
30	CONTINUE

	IF (J .EQ. 1) THEN
	  WRITE (LOGUNI,'(A,A20)')
&	    's2m: No data records found in ',SCFFIL
	  GO TO 300
	END IF

	SDATA(1,J) = 99999.9
	DO 35, K=2,21
	  SDATA(K,J) = 0.0
35	CONTINUE
C Number of records seen
	CTR = J - 1
D	WRITE (LOGUNI,*) 'CTR = ',CTR

C Write first record as is
C	WRITE (OUTUNI,800) SDATA(1:21,1)
	CALL WRTSCF (OUTUNI,SDATA,1,IOS)
	OUTCT = OUTCT + 1
D	WRITE (LOGUNI,*) 'First record output:'
D	WRITE (LOGUNI,'(A,F6.1)') ' Pressure ',SDATA(2,1)
D	WRITE (LOGUNI,'(A,F6.1)') ' time ',SDATA(1,1)

C Start at next level of PRSINT mb
C Example: Start pressure is 997.3, Pressure Interval is 10.0 mb
C   Then we want to interpolate at 990.0,980.0,970.0,...
C   MAXPRS = INT(997.3/10.0)*10.0 = (99)*10.0 = 990.0
	RMB = INT(SDATA(2,1)/PRSINT)*PRSINT
        if (rmb .eq. sdata(2,1)) then
          rmb = rmb - 5.0
        endif

C Interpolation loop
	DO WHILE (RMB .GE. MINPRS)

	CALL GTNXIN (RMB,16,IPHI,IPLO,QCPRS,PAR,PBR,SDATA,CTR)
D	WRITE (LOGUNI,'(A,F6.1,A)') 'RMB = ',RMB,', using (Pressure):'
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' HI = ',IPHI,', time ',SDATA(1,IPHI)
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' LO = ',IPLO,', time ',SDATA(1,IPLO)
C If we can't find a pressure to use, then quit (go to 300)
	IF (IPHI .GT. CTR) THEN
	  GO TO 300
	END IF

C If the data contains the desired pressure, then write it --- no
C  interpolation is needed
	IF (RMB .EQ. SDATA(2,IPHI)) THEN
C	  IDATA(1:21) = SDATA(1:21,IPHI)
	  CALL SCFCP (SDATA,IPHI,IDATA)
	  GO TO 200
	END IF
	IF (RMB .EQ. SDATA(2,IPLO)) THEN
C	  IDATA(1:21) = SDATA(1:21,IPLO)
	  CALL SCFCP (SDATA,IPLO,IDATA)
	  GO TO 200
	END IF

C Initialize values not interpolated or calculated from interp'd values
	IDATA(13) = 999.0
	IDATA(14) = 999.0

C Set pressure, QC flag
	IDATA(2) = RMB
	IDATA(16) = QCPRS

C If a time value is missing, then skip this level
	IF ((SDATA(1,IPHI) .GE. 9999.0) .OR.
&	    (SDATA(1,IPLO) .GE. 9999.0)) THEN
	  WRITE (0,'(A,F6.1)')
&	    'TIME VALUE MISSING FOR RMB = ',RMB
	  WRITE (LOGUNI,'(A,F6.1)')
&	    'TIME VALUE MISSING FOR RMB = ',RMB
	  GO TO 250
	END IF

C Interpolate time
	DIFF = SDATA(1,IPHI) - SDATA(1,IPLO)
	TIME = SDATA(1,IPLO) + DIFF * LOG(RMB/SDATA(2,IPLO)) /
&	       LOG(SDATA(2,IPHI)/SDATA(2,IPLO))
	IDATA(1) = TIME

C If an altitude is missing, then set it and Asc.rate to missing
C   else interp alt and comp asc rate
	IF ((SDATA(15,IPHI) .GE. 99999.0) .OR.
&	    (SDATA(15,IPLO) .GE. 99999.0)) THEN
	  IDATA(15) = 99999.0
	  IDATA(10) = 999.0
	  IDATA(21) = 9.0
	ELSE
C Interpolate altitude
	  HICO = (TIME - SDATA(1,IPLO)) / DIFF
	  LOCO = (SDATA(1,IPHI) - TIME) / DIFF
	  IDATA(15) = SDATA(15,IPHI)*HICO + SDATA(15,IPLO)*LOCO
C Compute Ascension rate
	  IDATA(10) = (SDATA(15,IPHI) - SDATA(15,IPLO)) / DIFF
          
           
C Set QC flag to Unchecked
	  IDATA(21) = 99.0
C	  IDATA(21) = QCWRST(SDATA(21,IPHI),SDATA(21,IPLO))
          IF((IDATA(10).LT.-99.9).OR.(IDATA(10).GT.999.9))THEN
             IDATA(10) = 999.0
             IDATA(21) = 9.0
          END IF
	END IF

C  if the time of interpolation is not w/in Temp. interp. pts., then
C   then get some new Temp. interp. pts.
	IF (TIME .GE. SDATA(1,ITHI)) THEN
	  CALL GTNXIN (RMB,17,ITHI,ITLO,QCTMP,TAR,TBR,SDATA,CTR)
c	IF (SDATA(1,ITHI) .LT. TIME) THEN
c	  WRITE (LOGUNI,*) 'Warning: ITHI.time < TIME.'
c	  WRITE (LOGUNI,*) ' TIME = ',TIME,', RMB = ',RMB
c	  RMB = 0.0
c	  GO TO 250
c	END IF
D	WRITE (LOGUNI,'(A,F6.1,A)') 'RMB = ',RMB,', using (Temperature):'
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' HI = ',ITHI,', time ',SDATA(1,ITHI)
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' LO = ',ITLO,', time ',SDATA(1,ITLO)
	END IF
C If we can't find a temperature to use, then set temp to missing
	  IF (ITHI .GT. CTR) THEN
	    IDATA(3) = 999.0
	    IDATA(17) = 9.0
	    GO TO 101
	  END IF

C If both hi,lo pts are the same then just use that value
	IF (ITHI .EQ. ITLO) THEN
	  IDATA(3) = SDATA(3,ITHI)
	  IDATA(17) = QCTMP
	  GO TO 101
	END IF

C Interpolate temperature
	DIFF = SDATA(1,ITHI) - SDATA(1,ITLO)
	HICO = (TIME - SDATA(1,ITLO)) / DIFF
	LOCO = (SDATA(1,ITHI) - TIME) / DIFF
	IDATA(3) = SDATA(3,ITHI)*HICO + SDATA(3,ITLO)*LOCO
	IDATA(17) = QCTMP

101	CONTINUE
C  if the time of interpolation is not w/in Hum. interp. pts., then
C   then get some new Hum. interp. pts.
	IF (TIME .GE. SDATA(1,IRHI)) THEN
D          WRITE(16,*)'TIME =',TIME,' SDATA TIME =',SDATA(1,IRHI)
	  CALL GTNXIN (RMB,18,IRHI,IRLO,QCHUM,HAR,HBR,SDATA,CTR)
D          WRITE(16,*)'IRHI =',IRHI
 
c	IF (SDATA(1,IRHI) .LT. TIME) THEN
c	  WRITE (LOGUNI,*) 'Warning: IRHI.time < TIME.'
c	  WRITE (LOGUNI,*) ' TIME = ',TIME,', RMB = ',RMB
c	  RMB = 0.0
c	  GO TO 250
c	END IF
D	WRITE (LOGUNI,'(A,F6.1,A)') 'RMB = ',RMB,', using (Humidity):'
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' HI = ',IRHI,', time ',SDATA(1,IRHI)
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' LO = ',IRLO,', time ',SDATA(1,IRLO)
	END IF
C If we can't find a rel hum to use, then set hum to missing
	  IF (IRHI .GT. CTR) THEN
	    IDATA(5) = 999.0
	    IDATA(18) = 9.0
	    GO TO 102
	  END IF

C If both hi,lo pts are the same then just use that value
	IF (IRHI .EQ. IRLO) THEN
	  IDATA(5) = SDATA(5,IRHI)
	  IDATA(18) = QCHUM
	  GO TO 102
	END IF

C Interpolate relative humidity
	DIFF = SDATA(1,IRHI) - SDATA(1,IRLO)
	HICO = (TIME - SDATA(1,IRLO)) / DIFF
	LOCO = (SDATA(1,IRHI) - TIME) / DIFF
	IDATA(5) = SDATA(5,IRHI)*HICO + SDATA(5,IRLO)*LOCO
 
	IDATA(18) = QCHUM

102	CONTINUE
	IF (IRHI .EQ. IRLO) THEN
	  IDATA(4) = SDATA(4,IRHI)
C Calculate dew point from temp,humidity
	ELSE IF ((IDATA(3) .EQ. 999.0) .OR. (IDATA(5) .EQ. 999.0) .OR.
&	    (IDATA(5) .EQ. 0.0)) THEN
	  IDATA(4) = 999.0
C	  IDATA(18) = 9.0
	ELSE
	  IDATA(4) = CDEWPT(IDATA(3),IDATA(5))
          IF (IDATA(4) .LE. -99.9) THEN
             IDATA(4) = -99.9
          END IF
	END IF

C  if the time of interpolation is not w/in Wind interp. pts., then
C   then get some new Wind interp. pts.
	IF (TIME .GE. SDATA(1,IWHI)) THEN
C QC flags for u,v components are always supposed to be the same
	  CALL GTNXIN (RMB,19,IWHI,IWLO,QCWND,WAR,WBR,SDATA,CTR)
c	IF (SDATA(1,IWHI) .LT. TIME) THEN
c	  WRITE (LOGUNI,*) 'Warning: IWHI.time < TIME.'
c	  WRITE (LOGUNI,*) ' TIME = ',TIME,', RMB = ',RMB
c	  RMB = 0.0
c	  GO TO 250
c	END IF
D	WRITE (LOGUNI,'(A,F6.1,A)') 'RMB = ',RMB,', using (Wind):'
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' HI = ',IWHI,', time ',SDATA(1,IWHI)
D	WRITE (LOGUNI,'(A,I,A,F6.1)') ' LO = ',IWLO,', time ',SDATA(1,IWLO)
	END IF
C If we can't find a wind to use, then set all winds and latlon to missing
	  IF (IWHI .GT. CTR) THEN
	    IDATA(6) = 9999.0
	    IDATA(7) = 9999.0
	    IDATA(8) = 999.0
	    IDATA(9) = 999.0
	    IDATA(11) = 9999.0
	    IDATA(12) = 999.0
	    IDATA(19) = 9.0
	    IDATA(20) = 9.0
	    GO TO 103
	  END IF

C If both hi,lo pts are the same then just use that value
	IF (IWHI .EQ. IWLO) THEN
	  IDATA(6) = SDATA(6,IWHI)
	  IDATA(7) = SDATA(7,IWHI)
	  IDATA(8) = SDATA(8,IWHI)
	  IDATA(9) = SDATA(9,IWHI)
	  IDATA(11) = SDATA(11,IWHI)
	  IDATA(12) = SDATA(12,IWHI)
	  IDATA(19) = QCWND
	  IDATA(20) = QCWND
	  GO TO 103
	END IF

C Interpolate wind u,v
C  then calc wind speed,dir from u,v
C  then interpolate latlon
C23456789012345678901234567890123456789012345678901234567890123456789012

        if((sdata(6,iwhi).ne.9999.0).and.(sdata(6,iwhi).ne.9999.0)
     &  .and. (sdata(7,iwlo).ne. 9999.0).and.(sdata(7,iwlo) .ne.9999.0))
     &  then
	   DIFF = SDATA(1,IWHI) - SDATA(1,IWLO)
	   HICO = (TIME - SDATA(1,IWLO)) / DIFF
	   LOCO = (SDATA(1,IWHI) - TIME) / DIFF
	   IDATA(6) = SDATA(6,IWHI)*HICO + SDATA(6,IWLO)*LOCO
	   IDATA(7) = SDATA(7,IWHI)*HICO + SDATA(7,IWLO)*LOCO
	   IDATA(8) = SQRT(IDATA(6)*IDATA(6)+IDATA(7)*IDATA(7))
           IF (IDATA(8) .GE. 1000.) THEN
              IDATA(6) = 9999.0
              IDATA(7) = 9999.0
              IDATA(8) = 999.0
              IDATA(9) = 999.0
              IDATA(19) = 9.0
              IDATA(20) = 9.0
           ELSE
	      IDATA(9) = CWNDIR(IDATA(6),IDATA(7))
	      IDATA(19) = QCWND
	      IDATA(20) = QCWND
           END IF
           if((sdata(11,iwhi).ne.9999.0).and.
     &     (sdata(11,iwhi).ne.9999.0).and.
     &     (sdata(12,iwlo).ne.999.0).and.
     &     (sdata(12,iwlo).ne.999.0))then 
              
	      IDATA(11)=SDATA(11,IWHI)*HICO+SDATA(11,IWLO)*LOCO
	      IDATA(12)=SDATA(12,IWHI)*HICO+SDATA(12,IWLO)*LOCO
           else
              idata(11) = 9999.0
              idata(12) = 999.0
           end if
        else
	    IDATA(6) = 9999.0
	    IDATA(7) = 9999.0
	    IDATA(8) = 999.0
	    IDATA(9) = 999.0
	    IDATA(11) = 9999.0
	    IDATA(12) = 999.0
	    IDATA(19) = 9.0
	    IDATA(20) = 9.0
	    GO TO 103
        end if

103	CONTINUE
	INTCT = INTCT + 1
C End of interpolation
200	CONTINUE

C Check if any of the interpolated values overflowed their field widths
	CALL CKWID (SDATA,J)
C Write data
	CALL WRTSCF (OUTUNI,IDATA,1,IOS)
C	WRITE (OUTUNI,800) IDATA
	OUTCT = OUTCT + 1
D	WRITE (LOGUNI,'(A,F6.1,A)') 'Level ',RMB,' mb interpolated.'
D	WRITE (LOGUNI,'(A,I,A)') 'Record ',OUTCT,' output.'

250	CONTINUE
C End of interpolation loop
	RMB = RMB - PRSINT
	END DO

300	CONTINUE

C Close in/out files, unless they're stdin,stdout
	IF (SCFUNI .NE. 5) THEN
	  CLOSE (UNIT=SCFUNI)
	  CLOSE (UNIT=OUTUNI)
	END IF

C May want to 'D' following lines
	WRITE (LOGUNI,'(A20,A2,I,A)')
&	  SCFFIL,': ',CTR,' SCF records read.'
	WRITE (LOGUNI,'(X,I,A)') OUTCT,' records written.'
	WRITE (LOGUNI,'(X,I,A)') INTCT,' output records interpolated.'

C End of main loop for cmd line args
	I = I + 1
	END DO

D	WRITE (LOGUNI,*) 'End of s2m.'

	GO TO 999


C FORMAT statements for general use
C800	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
C&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)


C WRITE statements for general use
990	WRITE (0,*) 's2m: Can''t open file.'
	WRITE (LOGUNI,*) 's2m: Can''t open file.'
	GO TO 999

991	WRITE (0,*) 's2m: Error reading SCF file.'
	WRITE (LOGUNI,*) 's2m: Error reading SCF file.'
	GO TO 999

999	CONTINUE

C Fix for -fnonstd flag in -fast compiler option
	CALL STANDARD_ARITHMETIC()

	STOP
	END



	SUBROUTINE RDSCF (FILENO,SDATA,INDEX,IOS)
	INTEGER FILENO,INDEX,IOS,I
	REAL SDATA(21,*)
C
C Read one SCF record from FILENO into SDATA(*,INDEX)
C Returns IOS from READ
C
C Note how the FORMAT statement is not being used. This is because
C  some data that I'm getting turned out to have missing values that
C  overran the field width and screwed things up. This list-directed
C  input should read things okay.
C
	READ (FILENO,*,IOSTAT=IOS)
&	  SDATA(1,INDEX),SDATA(2,INDEX),SDATA(3,INDEX),SDATA(4,INDEX),
&	  SDATA(5,INDEX),SDATA(6,INDEX),SDATA(7,INDEX),SDATA(8,INDEX),
&	  SDATA(9,INDEX),SDATA(10,INDEX),SDATA(11,INDEX),
&	  SDATA(12,INDEX),SDATA(13,INDEX),SDATA(14,INDEX),
&	  SDATA(15,INDEX),SDATA(16,INDEX),SDATA(17,INDEX),
&	  SDATA(18,INDEX),SDATA(19,INDEX),SDATA(20,INDEX),
&	  SDATA(21,INDEX)
C Adaptation for unchecked ARM-CART soundings
        DO I = 16,20
          IF(SDATA(I,INDEX) .EQ. 99.0) THEN
            SDATA(I,INDEX) = 1.0
          END IF
        END DO
	RETURN

1616	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)

	END



	SUBROUTINE CKQC (SDATA,INDEX)
	REAL SDATA(21,*)
	INTEGER INDEX
C
C CKQC - ChecK QC flags
C Does what the QC program should do but doesn't.
C If the value is Missing, Then set the QC flag to missing
C   Else If the QC flag is Missing, Then set the value to missing
C ***** NOTE *******
C Note that the "class.fmt" document written by Mark Bradford
C  specifies to bascially fill the field with 9's to indicate missing.
C  For winds, this means 9999.0. However, much of the data that
C  s2m will see will use 999.0 exclusively, in any field, to indicate
C  missing. We must check for both. For the moment, we will output
C  999.0 for missing for winds and use the "class.fmt" values for
C  the other fields.
C Also, some data I get may have 99999.0 as a missing in the dry-bulb
C  value. This throws the format off terribly (see RDSCF()), and we
C  also have to check for it here.
C All this combines to say: if val >= 999.0, then it's missing
C  (except for Time, Pressure, Altitude which could have legal values
C   of 999.0)
C
        if (sdata(16,index) .eq. 99.0) then
          sdata(16,index) = 2.0
        endif
        if (sdata(17,index) .eq. 99.0) then
          sdata(17,index) = 2.0
        endif       
        if (sdata(18,index) .eq. 99.0) then
          sdata(18,index) = 2.0
        endif
        if (sdata(19,index) .eq. 99.0) then
          sdata(19,index) = 2.0
        endif
        if (sdata(20,index) .eq. 99.0) then
          sdata(20,index) = 2.0
        endif
	IF (SDATA(2,INDEX) .GE. 9999.0) THEN
	  SDATA(16,INDEX) = 9.0
	ELSE IF (SDATA(16,INDEX) .EQ. 9.0) THEN
	  SDATA(2,INDEX) = 9999.0
	END IF
	IF (SDATA(3,INDEX) .GE. 999.0) THEN
	  SDATA(17,INDEX) = 9.0
	ELSE IF (SDATA(17,INDEX) .EQ. 9.0) THEN
	  SDATA(3,INDEX) = 999.0
	END IF
	IF (SDATA(5,INDEX) .GE. 999.0) THEN
	  SDATA(18,INDEX) = 9.0
	ELSE IF (SDATA(18,INDEX) .EQ. 9.0) THEN
	  SDATA(5,INDEX) = 999.0
	END IF
C If one wind is missing, then both winds should be missing
	IF (SDATA(6,INDEX) .GE. 999.0) THEN
	  SDATA(19,INDEX) = 9.0
	  SDATA(6,INDEX) = 9999.0
	  SDATA(7,INDEX) = 9999.0
	  SDATA(20,INDEX) = 9.0
	ELSE IF (SDATA(19,INDEX) .EQ. 9.0) THEN
	  SDATA(6,INDEX) = 9999.0
	  SDATA(7,INDEX) = 9999.0
	  SDATA(20,INDEX) = 9.0
	END IF
	IF (SDATA(7,INDEX) .GE. 999.0) THEN
	  SDATA(20,INDEX) = 9.0
	  SDATA(6,INDEX) = 9999.0
	  SDATA(7,INDEX) = 9999.0
	  SDATA(19,INDEX) = 9.0
	ELSE IF (SDATA(20,INDEX) .EQ. 9.0) THEN
	  SDATA(7,INDEX) = 9999.0
	  SDATA(6,INDEX) = 9999.0
	  SDATA(19,INDEX) = 9.0
	END IF
	IF (SDATA(10,INDEX) .GE. 999.0) THEN
	  SDATA(21,INDEX) = 9.0
	ELSE IF (SDATA(21,INDEX) .EQ. 9.0) THEN
	  SDATA(10,INDEX) = 999.0
	END IF

	RETURN
	END



	SUBROUTINE WRTSCF (FILENO,SDATA,INDEX,IOS)
	INTEGER FILENO,INDEX,IOS,I
	REAL SDATA(21,*)
C
C Write SDATA(*,INDEX) onto FILENO
C Returns IOS from WRITE
C
        DO I = 16,20
          IF(SDATA(I,INDEX) .EQ. 1.0) THEN
            SDATA(I,INDEX) = 99.0
          END IF
        END DO
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



	SUBROUTINE SCFCP (SDATA,INDEX,IDATA)
	REAL SDATA(21,*),IDATA(21)
	INTEGER INDEX
C
C Copies SDATA(*,INDEX) into IDATA(*)
C
	INTEGER I

	DO 1818, I=1,21
	  IDATA(I) = SDATA(I,INDEX)
1818	CONTINUE
	RETURN
	END



	SUBROUTINE CKWID (SDATA,INDEX)
	REAL SDATA(21,*)
	INTEGER INDEX
C
C CKWID - check if any of the values are bigger than their field width
C
	IF ((SDATA(1,INDEX) .GT. 9999.9) .OR.
&	    (SDATA(1,INDEX) .LT. -999.9)) THEN
	  SDATA(1,INDEX) = 9999.0
	END IF
	IF ((SDATA(2,INDEX) .GT. 9999.9) .OR.
&	    (SDATA(2,INDEX) .LT. -999.9)) THEN
	  SDATA(2,INDEX) = 9999.0
	  SDATA(16,INDEX) = 9.0
	END IF
	IF ((SDATA(3,INDEX) .GT. 999.9) .OR.
&	    (SDATA(3,INDEX) .LT. -99.9)) THEN
	  SDATA(3,INDEX) = 999.0
	  SDATA(17,INDEX) = 9.0
	END IF
	IF ((SDATA(4,INDEX) .GT. 999.9) .OR.
&	    (SDATA(4,INDEX) .LT. -99.9)) THEN
	  SDATA(4,INDEX) = 999.0
	END IF
	IF ((SDATA(5,INDEX) .GT. 999.9) .OR.
&	    (SDATA(5,INDEX) .LT. -99.9)) THEN
	  SDATA(5,INDEX) = 999.0
	  SDATA(18,INDEX) = 9.0
	END IF
	IF ((SDATA(6,INDEX) .GT. 9999.9) .OR.
&	    (SDATA(6,INDEX) .LT. -999.9)) THEN
	  SDATA(6,INDEX) = 9999.0
	  SDATA(19,INDEX) = 9.0
	END IF
	IF ((SDATA(7,INDEX) .GT. 9999.9) .OR.
&	    (SDATA(7,INDEX) .LT. -999.9)) THEN
	  SDATA(7,INDEX) = 9999.0
	  SDATA(20,INDEX) = 9.0
	END IF
	IF ((SDATA(8,INDEX) .GT. 999.9) .OR.
&	    (SDATA(8,INDEX) .LT. -99.9)) THEN
	  SDATA(8,INDEX) = 999.0
	END IF
	IF ((SDATA(9,INDEX) .GT. 999.9) .OR.
&	    (SDATA(9,INDEX) .LT. -99.9)) THEN
	  SDATA(9,INDEX) = 999.0
	END IF
	IF ((SDATA(10,INDEX) .GT. 999.9) .OR.
&	    (SDATA(10,INDEX) .LT. -99.9)) THEN
	  SDATA(10,INDEX) = 999.0
	  SDATA(21,INDEX) = 9.0
	END IF
	IF ((SDATA(11,INDEX) .GT. 9999.999) .OR.
&	    (SDATA(11,INDEX) .LT. -999.999)) THEN
	  SDATA(11,INDEX) = 9999.0
	END IF
	IF ((SDATA(12,INDEX) .GT. 999.999) .OR.
&	    (SDATA(12,INDEX) .LT. -99.999)) THEN
	  SDATA(12,INDEX) = 999.0
	END IF
	IF ((SDATA(13,INDEX) .GT. 999.9) .OR.
&	    (SDATA(13,INDEX) .LT. -99.9)) THEN
	  SDATA(13,INDEX) = 999.0
	END IF
	IF ((SDATA(14,INDEX) .GT. 999.9) .OR.
&	    (SDATA(14,INDEX) .LT. -99.9)) THEN
	  SDATA(14,INDEX) = 999.0
	END IF
	IF ((SDATA(15,INDEX) .GT. 99999.9) .OR.
&	    (SDATA(15,INDEX) .LT. -9999.9)) THEN
	  SDATA(15,INDEX) = 99999.0
	END IF
	RETURN
	END









