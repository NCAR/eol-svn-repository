C
C $Id: check-file-table-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: check-file-table-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: check-file-table-index
C  6 Character Name: CHKFTI
C           Purpose: To check that an integer is a legitimate file 
C                    table index, and halt with an error if not.
C Import Parameters: 
C    INDEX   --  purported index to check.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: FINFO
      SUBROUTINE CHKFTI(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/file_info.f'

      IF ((INDEX .LT. 1) .OR. (INDEX .GT. FILTOP)) THEN
         CALL ENCERR(9)
      ENDIF
      RETURN
      END
