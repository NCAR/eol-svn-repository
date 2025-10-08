C
C $Id: coded.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: coded.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: coded
C  6 Character Name: CODED
C           Purpose: To test whether a given FXY table entry is coded.
C Import Parameters: 
C    INDEX   --  Index into to FXY table to test.
C Export Parameters: 
C    --  Returns true if the corresponding (F,X,Y)-tuple has a code 
C        table value.
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB
      LOGICAL FUNCTION CODED(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)
      CODED = CTABLE(INDEX)
      RETURN
      END
