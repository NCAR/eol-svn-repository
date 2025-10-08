C
C $Id: clear-FXY-tables.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: clear-FXY-tables.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Clear-FXY-tables
C  6 Character Name: CLRFXY
C           Purpose: To reset the FXY tables to an initial no
C                    data state.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB
      SUBROUTINE CLRFXY()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      FXYTOP = 0
      RETURN
      END
