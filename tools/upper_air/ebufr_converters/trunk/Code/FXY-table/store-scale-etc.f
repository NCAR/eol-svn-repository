C
C $Id: store-scale-etc.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: store-scale-etc.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Store-scale-etc
C  6 Character Name: STRSCL
C           Purpose: To store the scale factor, reference value, and
C                    bit width associated with a given FXY table index.
C Import Parameters:
C    INDEX   --  Index for values to be found.
C    SCALE1  --  Scale factor for this FXY entry.
C    REFVL1  --  Reference value for this FXY entry.
C    BTWDT1  --  Bit width to use for BUFR entry for this FXY entry.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: FXYTAB

      SUBROUTINE STRSCL(INDEX, SCALE1, REFVL1, BTWDT1)
      INTEGER INDEX, SCALE1, REFVL1, BTWDT1
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)

      SCALE (INDEX) = SCALE1
      REFVAL(INDEX) = REFVL1
      BTWDTH(INDEX) = BTWDT1

      RETURN
      END
