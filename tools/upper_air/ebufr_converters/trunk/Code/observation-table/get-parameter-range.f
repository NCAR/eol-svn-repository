C
C $Id: get-parameter-range.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-parameter-range.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-parameter-range
C  6 Character Name: GPRNGE
C           Purpose: To return indices to the parameters associated
C                    with observation labelled OCODE.
C Import Parameters:
C    OCODE   --  Code labelling the observation.
C Export Parameters:
C    PSTART  --  Index (in ptable) of the first parameter in this
C                observation.
C    PEND    --  Index (in ptable) of the last  parameter in this
C                observation.
C     Prerequisites:  The appropriate information must have been stored
C                     in the OBSTBL common.
C Commons directly
C          accessed: OBSTBL
      SUBROUTINE GPRNGE(OCODE, PSTART, PEND)
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

      IF (INDEX .EQ. -1) THEN
         CALL ENCERR(7)
      ENDIF

      PSTART = OBSTBL(2, INDEX)
      PEND   = OBSTBL(3, INDEX)
      RETURN
      END
