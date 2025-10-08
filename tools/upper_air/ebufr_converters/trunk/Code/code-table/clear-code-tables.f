C
C $Id: clear-code-tables.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: clear-code-tables.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: clear-code-tables
C  6 Character Name: CLRCOD
C           Purpose: To reset the code table to an initial no
C                    data state.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: CDTABL
      SUBROUTINE CLRCOD()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/code_table.f'

      CDTTOP = 0
      RETURN
      END
