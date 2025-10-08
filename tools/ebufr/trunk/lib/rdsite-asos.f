      SUBROUTINE READSITE(SITEID,NAME,ST,LAT,LON,ELEV,
     *LOGNUM,SITENO,SITNAM,INUM,RC)
C
C      RDSITE-ASOS.F   - READS SURFACE SITE INFORMATION FOR ASOS, ETC.
C

c     27apr92 - commented out most messages except errors
c
c     09may95 - modified for vortex project - Lia Pennington
c             - removed NETWRK variable.



C	 1	   2	     3	       4	 5	   6	     7
C    +	 0    +    0	+    0	  +    0    +	 0    +    0	+    012
C*****!*****************************************************************

      CHARACTER*3  SITEID(750)
      CHARACTER*26 NAME(750)
      CHARACTER*2  ST(750)
      CHARACTER*67 BUFFER
      CHARACTER*50 SITNAM  !NAME OF SURFACE SITES LIST
      CHARACTER*7  STATUS  !STATUS FOR FILE OPEN


      REAL  LAT(750)
      REAL  LON(750)


      INTEGER ELEV(750) !ELEVATION IN METERS FOR SITE LOCATION
      INTEGER SITENO    !UNIT NUMBER FOR SURFACE SITES LIST
      INTEGER LOGNUM    !UNIT NUMBER FOR LOG FILE
      INTEGER IOS       !RETURN STATUS CODE FOR FILE OPEN/CLOSE
      INTEGER RC        !RETURN CODE TO PASS BACK TO CALLING ROUTINE
      INTEGER INUM      !NUMBER OF ELEMENTS PASSED BACK IN ARRAYS
      INTEGER DASH      !TRACK THE NUMBER OF DASHED LINES READ FROM SITES FILE
C                        SECOND DASHED LINE SIGNALS END OF PROCESSING OF FILE



C*****!*****************************************************************
C
C     INITIALIZATIONS
C
C*****!*****************************************************************


      INUM = 0
      RC = 0
      DASH = 0

C*****!*****************************************************************

c     WRITE ( 6, * ) ' '
c     WRITE ( 6, * ) 'STARTING RDSITE SUBROUTINE'
c     WRITE ( LOGNUM, * ) ' '
c     WRITE ( LOGNUM, * ) 'STARTING RDSITE SUBROUTINE'





C*****!*****************************************************************
C     OPEN FILES FOR PROGRAM
C*****!*****************************************************************


      STATUS = 'UNK'
      CALL OPNFIL (SITENO, SITNAM, STATUS, IOS, LOGNUM )
      IF ( IOS .NE. 0 ) THEN
         WRITE (      6, * ) ' '
         WRITE (      6, '(''ERROR OPENING FILE '',A50)') SITNAM
         WRITE (      6, '(''FOR LIST OF SURFACE SITE LOCATIONS'')' )
         WRITE (      6, * ) ' '
         WRITE ( LOGNUM, * ) ' '
         WRITE ( LOGNUM, '(''ERROR OPENING FILE '',A50)') SITNAM
         WRITE ( LOGNUM, '(''FOR LIST OF SURFACE SITE LOCATIONS'')' )
         WRITE ( LOGNUM, * ) ' '
         CALL ANALYZ ( IOS, LOGNUM )
         RC = 1
         GOTO 9999
      END IF


C*****!*****************************************************************
1000  CONTINUE
      READ ( SITENO, '(A67)', END=3000 ) BUFFER

C     CHECK FOR DASHED LINES
      IF ( BUFFER(1:4) .EQ. '----' ) THEN
         DASH = DASH + 1
         IF ( DASH .GE. 2 ) THEN
C           THIS IS THE SECOND DASHED LINE (OCCURS NEAR END OF FILE)
C           NO MORE SITE LOCATION ENTRIES IN THIS FILE
C           FINISHED READING SITE FILE NOW
c           WRITE(LOGNUM,'('' FINISHED READING SITE FILE'')' )
            GOTO 2000
         ELSE
         END IF
      ELSE
      END IF

      IF ( BUFFER(1:4) .EQ. '    ' .OR. 
     *BUFFER(5:6) .NE. '  ' ) GOTO 1000 !SKIP BLANK LINES
      IF ( DASH .EQ. 0 ) GOTO 1000  !SKIP ALL RECORDS UNTIL AFTER FIRST DASHED LINE
      INUM = INUM + 1
      SITEID(INUM) = BUFFER(1:3)
      NAME(INUM)   = BUFFER(7:32)
      ST(INUM)     = BUFFER(34:35)
      READ ( BUFFER(40:46), '(F7.4)' ) LAT(INUM)
      READ ( BUFFER(49:56), '(F8.4)' ) LON(INUM)
      READ ( BUFFER(58:61), '(I4)' ) ELEV(INUM)
      WRITE ( LOGNUM, '(''BUFFER = '',A67)' ) BUFFER
      GOTO 1000
C*****!*****************************************************************



C*****!*****************************************************************
2000  CONTINUE
C     ALL OF THE ASOS SITE INFORMATION IS NOW CAPTURED

      DO 2500 I = 1, INUM
         WRITE ( LOGNUM, * ) ' '
         WRITE ( LOGNUM, '(''SITEID('',I3,'') = '',A3)' ) I, SITEID( I )
         WRITE ( LOGNUM, '(''NAME('',I3,'')   = '',A26)' ) I, NAME( I )
         WRITE ( LOGNUM, '(''ST('',I3,'')     = '',A2)' ) I, ST( I )
         WRITE ( LOGNUM, '(''LAT('',I3,'')    = '',F8.4)' ) I, LAT( I )
         WRITE ( LOGNUM, '(''LON('',I3,'')    = '',F8.4)' ) I, LON( I )
         WRITE ( LOGNUM, '(''ELEV('',I3,'')   = '',I4)' ) I, ELEV( I )
         WRITE ( LOGNUM, * ) ' '
2500  CONTINUE


3000  CONTINUE
c     WRITE ( LOGNUM, * ) ' '
c     WRITE ( LOGNUM, '(''REACHED END OF FILE ON UNIT '',I2)' ) SITENO
c     WRITE ( LOGNUM, '(''FOR FILE NAME '',A50)' ) SITNAM
      GOTO 9999


9999  CONTINUE
      CALL CLSFIL ( SITENO, SITNAM, IOS, LOGNUM )
c     WRITE ( 6, * ) 'LEAVING RDSITE SUBROUTINE'
c     WRITE ( LOGNUM, * ) 'LEAVING RDSITE SUBROUTINE'
      RETURN
      END
