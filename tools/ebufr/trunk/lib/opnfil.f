      SUBROUTINE OPNFIL(FILENO, FILNAM, STATE, IOS, LISNUM )
******!****************************************************************!
C     SUBROUTINE  OPNFIL.F
C
C     SUBROUTINE TO OPEN FILES
C
C
C     WRITTEN BY:  RONALD A. MURDOCK
C                  MMM/NCAR
C                  DECEMBER 23, 1991
C
C     DATE LAST MODIFIED:  TODAY
C
C     HISTORY OF CHANGES:
C
C
C*****!****************************************************************!
C
C     PARAMETERS PASSED INTO THIS SUBROUTINE

C     FILENO - INTEGER - FILE UNIT NUMBER
C
C     LISNUM - INTEGER - FILE UNIT FOR MESSAGES FROM THIS ROUTINE
C
C     FILNAM - CHARACTER - FILE NAME
C
C*****!****************************************************************!
C
C     PARAMETERS RETURNED TO CALLING PROGRAM
C
C     IOS - INTEGER - RETURN CODE  (0 = OK)
C
C
C
C   +    1    +    2    +    3    +    4    +    5    +    6    +    7
C23456789012345678901234567890123456789012345678901234567890123456789012
******!****************************************************************!


C     TYPE STATEMENTS
C
      INTEGER   FILENO
      INTEGER   LISNUM
      INTEGER   IOS
C

      CHARACTER*(*)  FILNAM
      CHARACTER*(*)  STATE


C  ***!****************************************************************!
C
C     OPEN THE FILE
C
C  ***!****************************************************************!

      OPEN( UNIT=FILENO, FILE=FILNAM, IOSTAT=IOS, STATUS=STATE,
     * ERR=1300 )

C     Successfully opened.
      REWIND ( FILENO )
      GOTO 9999


1300  WRITE( LISNUM,1400) IOS, FILENO, FILNAM
      WRITE(      6,1400) IOS, FILENO, FILNAM
1400  FORMAT(/,'OPNFIL:ERROR ',I4, ' OPENING UNIT ',I2, ' FOR FILE ',
     *A)
      GOTO 9999


9999  RETURN
      END
