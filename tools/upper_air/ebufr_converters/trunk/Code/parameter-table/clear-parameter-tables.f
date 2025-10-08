C
C $Id: clear-parameter-tables.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: clear-parameter-tables.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: clear-parameter-tables
C  6 Character Name: CLRPAR
C           Purpose: To reset the parameter tables to an initial no
C                    data state.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: PTAB1
      SUBROUTINE CLRPAR()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      PTOP = 0
      RETURN
      END
