C
C $Id: store-new-observation-type.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: store-new-observation-type.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: store-new-observation-type
C  6 Character Name: STNOBS
C           Purpose: To create an entry in the observation table for a
C                    new observation.
C Import Parameters:
C    OCODE   --  Code labelling the observation.
C    PSTART  --  Index (in ptable) of the first parameter in this
C                observation.
C    PEND    --  Index (in ptable) of the last  parameter in this
C                observation.
C Export Parameters: none
C     Prerequisites: The observation tables should be cleared before
C                    the first call to this routine.
C Commons directly
C          accessed: OBSTBL
      SUBROUTINE STNOBS(OCODE, PSTART, PEND)
      INTEGER OCODE, PSTART, PEND
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/observation_table.f'
C ====================================================================
C   Local variables
      INTEGER INDEX, I
C ====================================================================
      INDEX = -1
      DO 10 I = 1,OBSTOP,1
         IF (OBSTBL(1,I) .EQ. OCODE) THEN
            INDEX = I
            GOTO 11
         ENDIF
 10   CONTINUE
C     Exit label
 11   CONTINUE

      IF (INDEX .NE. -1) THEN
         CALL ENCERR(8)
      ELSE
         IF (OBSTOP .GE. MXNOBS) THEN
            CALL ENCERR(14)
         ELSE
            OBSTOP = OBSTOP + 1
            INDEX = OBSTOP
         ENDIF 
         OBSTBL(1, INDEX) = OCODE
         OBSTBL(2, INDEX) = PSTART
         OBSTBL(3, INDEX) = PEND 
      ENDIF

      RETURN
      END
