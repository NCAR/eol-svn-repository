C
C $Id: check-FXY-table-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: check-FXY-table-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: check-FXY-table-index
C  6 Character Name: CHKFXI
C           Purpose: To check that an integer is a legitimate FXY
C                    table index, and halt with an error if not.
C Import Parameters: 
C    INDEX   --  purported index to check.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB
      SUBROUTINE CHKFXI(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      IF ((INDEX .LT. 1) .OR. (INDEX .GT. FXYTOP)) THEN
         CALL ENCERR(30)
      ENDIF
      RETURN
      END
