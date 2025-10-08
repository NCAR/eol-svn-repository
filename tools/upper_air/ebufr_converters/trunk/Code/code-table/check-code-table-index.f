C
C $Id: check-code-table-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: check-code-table-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: check-code-table-index
C  6 Character Name: CHKCTI
C           Purpose: To check that an integer is a legitimate code 
C                    table index, and halt with an error if not.
C Import Parameters: 
C    INDEX   --  purported index to check.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: CDTABL
      SUBROUTINE CHKCTI(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/code_table.f'

      IF ((INDEX .LT. 1) .OR. (INDEX .GT. CDTTOP)) THEN
         CALL ENCERR(13)
      ENDIF
      RETURN
      END
