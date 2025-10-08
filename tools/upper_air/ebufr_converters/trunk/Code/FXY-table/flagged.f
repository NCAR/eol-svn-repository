C
C $Id: flagged.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: flagged.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: flagged
C  6 Character Name: FLAGGD
C           Purpose: To test whether a given FXY-tuple is a flag.
C Import Parameters: 
C    INDEX   --  Index into to FXY table to test.
C Export Parameters: 
C    --  Returns true if the corresponding (F,X,Y)-tuple has a flag 
C        table value.
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB
      LOGICAL FUNCTION FLAGGD(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)
      FLAGGD = FTABLE(INDEX)
      RETURN
      END
