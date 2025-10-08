C
C $Id: marked.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: marked.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: marked
C  6 Character Name: MARKED
C           Purpose: To test whether a given FXY table entry is marked.
C Import Parameters: 
C    INDEX   --  Index into to FXY table to test.
C Export Parameters: 
C    --  Returns true if the corresponding mark is set.
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB
      LOGICAL FUNCTION MARKED(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)
      MARKED = MARKD(INDEX)
      RETURN
      END
