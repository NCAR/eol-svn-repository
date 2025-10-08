C
C $Id: mark-FXY-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: mark-FXY-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: mark-FXY-index
C  6 Character Name: MKFXY
C           Purpose: To set a mark for a given FXY table entry.
C Import Parameters: 
C    INDEX   --  Index into to FXY table to unmark.
C Export Parameters: (none)
C Commons directly
C          accessed: FXYTAB
      SUBROUTINE MKFXY(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)
      MARKD(INDEX) = .TRUE.
      RETURN
      END
