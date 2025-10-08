C
C $Id: read-observation-descriptions.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-observation-descriptions.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C     Routine Long Name: read-observation-descriptions
C     6 Character Name: RDOBSD
C     Purpose: To read the descriptions of all observations, 
C              together with associated parameter and code 
C     information.
C     Import Parameters: (none)
C     Export Parameters: (none)
C     Prerequisites: The control file must have been opened for reading
C                    and positioned correctly.
C     Commons directly
C     accessed: CTLFIL


      SUBROUTINE RDOBSD()
C     IMPLICIT NONE
      INCLUDE '../../Commons/ctl_file.f'

      EXTERNAL FDFXY
      INTEGER FDFXY
      
C     ====================================================================
C     Local variables
      INTEGER OCODE, PSTART, PEND
      INTEGER F,X,Y
      INTEGER WHERE, WIDTH, TYPE
      INTEGER PLAIN, CODE
      INTEGER C1, C2
      INTEGER IFXY
      CHARACTER*20 FORMT
      INTEGER IDXCOM
C     ====================================================================
      
C     while looking at a new observation
 10   IF (LINCLS .EQ. OBSCL) THEN
         
         READ(UNIT=LINE(12:LEN(LINE)),FMT='(BN,I20)') OCODE
         
         CALL RDCLIN
         CALL STPRMS
         
C     while looking at a new parameter
 20      IF (LINCLS .EQ. OTHECL) THEN
            READ(UNIT=LINE,FMT=9997) F,X,Y
            READ(UNIT=LINE,FMT=9996) WHERE, FORMT, WIDTH, TYPE
            CALL RDCLIN
            CALL STCDPR
C     while looking at code pairs
 30         IF (LINCLS .EQ. CODECL) THEN
               IDXCOM=INDEX(LINE,',')
               READ(UNIT=LINE(6:IDXCOM-1),FMT='(BN,I6)') PLAIN
               READ(UNIT=LINE(IDXCOM+1:LEN(LINE)),FMT='(BN,I6)')  CODE
               CALL STNCPR(PLAIN, CODE)
               CALL RDCLIN
               GO TO 30
            ENDIF 
            CALL ENCDPR(C1,C2)
            IFXY = FDFXY(F,X,Y)
            CALL STNPAR(IFXY, WHERE, FORMT, WIDTH, TYPE, C1, C2)
            GO TO 20
         ENDIF
         
         IF (LINCLS .NE. EOBSCL) THEN
            CALL ENCERR(39)
         ENDIF
         CALL RDCLIN
         CALL ENPRMS(PSTART, PEND)
         CALL STNOBS(OCODE, PSTART, PEND)
         GO TO 10
      ENDIF 
      
      IF (LINCLS .NE. EOFCL) THEN
         CALL ENCERR(40)
      ENDIF
      RETURN 
         
C     Format for the observation code.
C9998    format *
C     Format for F,                  X,      Y values
 9997    FORMAT (T1, BN, 1X, I1, 1X, I2, 1X, I3)
C     Format for WHERE,       FORMT,   WIDTH,  TYPE
 9996    FORMAT (T11, I1, 1X, A20, 1X, I2, 1X, I1)
C     Format for code pairs.
C9995    FORMAT *
         END
