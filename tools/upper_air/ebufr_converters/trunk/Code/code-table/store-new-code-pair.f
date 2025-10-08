C
C $Id: store-new-code-pair.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: store-new-code-pair.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: store-new-code-pair
C  6 Character Name: STNCPR
C           Purpose: To store a code pair for the current parameter.
C Import Parameters:
C    PLAIN   --  Unencoded value (to be supplied by the user).
C    CODE    --  Encoded value (to be stored in EBUFR record).
C Export Parameters: none
C     Prerequisites: The start-code-pairs routine should be called 
C                    before the first call to this routine for a 
C                    particular parameter.
C Commons directly
C          accessed: CDTABL
      SUBROUTINE STNCPR(PLAIN, CODE)
      INTEGER PLAIN, CODE
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/code_table.f'

      IF (CDTTOP .GE. MXCDPR) THEN
         CALL ENCERR(15)
      ELSE
         CDTTOP = CDTTOP + 1
         CODTBL(1, CDTTOP) = PLAIN
         CODTBL(2, CDTTOP) = CODE
      ENDIF

      RETURN
      END
