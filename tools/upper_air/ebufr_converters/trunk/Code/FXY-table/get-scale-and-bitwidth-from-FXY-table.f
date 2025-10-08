C
C $Id: get-scale-and-bitwidth-from-FXY-table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-scale-and-bitwidth-from-FXY-table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Get-scale-and-bitwidth-from-FXY-table
C  6 Character Name: GT1SCL
C              (N.B. GETSCL is parameter table routine. (q.v.))
C           Purpose: To find the scale factor, reference value, and
C                    bit width associated with a given FXY table index.
C Import Parameters:
C    INDEX   --  Index for values to be found.
C Export Parameters:
C    SCALE1  --  Scale factor for this FXY entry.
C    REFVL1  --  Reference value for this FXY entry.
C    BTWDT1  --  Bit width to use for BUFR entry for this FXY entry.
C     Prerequisites: Triple must have been previously stored.
C Commons directly
C          accessed: FXYTAB

      SUBROUTINE GT1SCL(INDEX, SCALE1, REFVL1, BTWDT1)
      INTEGER INDEX, SCALE1, REFVL1, BTWDT1

      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)

      SCALE1 = SCALE (INDEX)
      REFVL1 = REFVAL(INDEX)
      BTWDT1 = BTWDTH(INDEX)

      RETURN
      END
