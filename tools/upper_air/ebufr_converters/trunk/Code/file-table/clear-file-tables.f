C
C $Id: clear-file-tables.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: clear-file-tables.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: clear-file-tables
C  6 Character Name: CLRFIL
C           Purpose: To reset the file table to an initial no
C                    data state.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: FINFO1, FINFO2
      SUBROUTINE CLRFIL()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/file_info.f'

      FILTOP = 0
      RETURN
      END
