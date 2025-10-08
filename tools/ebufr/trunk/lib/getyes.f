      SUBROUTINE GETYES ( YY, MM, DD , LOGNUM )
C        1         2         3         4         5         6         7
C        0         0         0         0         0         0         0
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C*****!*****************************************************************
C
C      SUBROUTINE GETYES.F
C
C      USES SYSTEM CALL TO FDATE TO BUILD YESTERDAY'S DATE
C
C
      CHARACTER*2  YY
      CHARACTER*2  MM
      CHARACTER*2  DD
      CHARACTER*2  BUFFER
      CHARACTER*3  MON
      CHARACTER*24 STRING

      INTEGER TODAY
      INTEGER LOGNUM  !UNIT NUMBER OF LOG FILE

      CALL FDATE ( STRING )
C     WRITE ( *, * ) STRING
      WRITE ( 6, 
     *'(''CURRENT DATE & TIME IS '', A24 )' ) STRING
      WRITE ( LOGNUM, 
     *'(''CURRENT DATE & TIME IS '', A24 )' ) STRING

C     RESET DATE TO CHECK OTHER POSSIBILITIES
C     STRING = 'Fri Jan 10 07:42:35 1992'
C     STRING = 'Sat Feb 01 07:42:35 1992'
C     STRING = 'Sun Mar 01 07:42:35 1992'
C     WRITE ( *, '(''RESETTING DATE - NEW DATE IS '',A24)' ) STRING
      READ ( STRING(23:24), '(A2)' ) YY

C     SET TODAY'S MONTH NUMBER
      READ ( STRING(5:7), '(A3)' ) MON
      IF ( MON .EQ. 'Jan' ) THEN
         MM = '01'
      ELSE
         IF ( MON .EQ. 'Feb' ) THEN
            MM = '02'
         ELSE
            IF ( MON .EQ. 'Mar' ) THEN
               MM = '03'
            ELSE
               WRITE (*,*) 'ERROR GETTING SYSTEM DATE'
               WRITE ( LOGNUM, '(''ERROR GETTING SYSTEM DATE'')' )
               GOTO 99
            END IF
         END IF
      END IF

C     MM IS NOW SET TO TODAYS MONTH NUMBER

C     NOW GET TODAY'S DATE NUMBER
      READ ( STRING(9:10), '(I2)' ) TODAY
      
C     CHECK FOR END OF MONTH PRIOR TO FINAL SETTINGS
      IF ( MM .EQ. '01' ) THEN
         TODAY = TODAY - 1
         WRITE ( BUFFER, '(I2)' ) TODAY
         READ  ( BUFFER, '(A2)' ) DD
         IF ( TODAY .LT. 10 ) DD(1:1) = '0'
      ELSE
      END IF
      IF ( MM .EQ. '02' ) THEN
         IF ( TODAY .EQ. 1 ) THEN
C           RESET TO LAST DAY OF PREVIOUS MONTH
            TODAY = 31
            WRITE ( BUFFER, '(I2)' ) TODAY
            READ  ( BUFFER, '(A2)' ) DD
            MM = '01'

         ELSE
            TODAY = TODAY - 1
            WRITE ( BUFFER, '(I2)' ) TODAY
            READ  ( BUFFER, '(A2)' ) DD
            IF ( TODAY .LT. 10 ) DD(1:1) = '0'
         END IF
      ELSE
      END IF

      IF ( MM .EQ. '03' ) THEN
         IF ( TODAY .EQ. 1 ) THEN
C           RESET TO LAST DAY OF PREVIOUS MONTH
            TODAY = 29
            WRITE ( BUFFER, '(I2)' ) TODAY
            READ  ( BUFFER, '(A2)' ) DD
            MM = '02'

         ELSE
            TODAY = TODAY - 1
            WRITE ( BUFFER, '(I2)' ) TODAY
            READ  ( BUFFER, '(A2)' ) DD
            IF ( TODAY .LT. 10 ) DD(1:1) = '0'
         END IF
      ELSE
      END IF

      WRITE ( 6, '(''YESTERDAYS DATE IS '',A2,
     *''/'',A2,''/'', A2 )' ) YY, MM, DD
      WRITE ( LOGNUM, '(''YESTERDAYS DATE IS '',A2,
     *''/'',A2,''/'', A2 )' ) YY, MM, DD


99    RETURN
      END

