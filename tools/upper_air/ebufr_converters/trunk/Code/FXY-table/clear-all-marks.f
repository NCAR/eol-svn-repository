C
C $Id: clear-all-marks.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: clear-all-marks.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Clear-all-marks
C  6 Character Name: CLRMKS
C           Purpose: To clear the marks associated with current FXY
C                    entries.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB
      SUBROUTINE CLRMKS()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'
C ====================================================================
C   Local variables
      INTEGER I
C ====================================================================

      DO 10 I = 1,FXYTOP,1
         MARKD(I) = .FALSE.
 10   CONTINUE 
      RETURN
      END
