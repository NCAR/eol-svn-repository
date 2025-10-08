C
C $Id: check-parameter-table-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: check-parameter-table-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: check-parameter-table-index
C  6 Character Name: CHKPTI
C           Purpose: To check that an integer is a legitimate parameter
C                    table index, and halt with an error if not.
C Import Parameters: 
C    INDEX   --  purported index to check.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: PTAB1
      SUBROUTINE CHKPTI(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      IF ((INDEX .LT. 1) .OR. (INDEX .GT. PTOP)) THEN
         CALL ENCERR(16)
      ENDIF
      RETURN
      END
