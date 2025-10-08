C
C $Id: get-scale-from-parameter-table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-scale-from-parameter-table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-scale-from-parameter-table-index
C  6 Character Name: GETSCL
C           Purpose: To find the scale factor and reference value for
C                    a parameter.
C Import Parameters:
C    INDEX   --  Index into the parameter table.
C Export Parameters:
C    SCALE   --  Scale factor for this parameter (signed integer).
C    REFVAL  --  Reference value for this parameter (after scaling).
C     Prerequisites:  The data must have been stored first.
C Commons directly
C          accessed: PTAB1
      SUBROUTINE GETSCL(INDEX, SCALE, REFVAL)
      INTEGER INDEX, SCALE, REFVAL
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'
C ====================================================================
C   Local variables
      INTEGER DUMMY
C ====================================================================

      CALL CHKPTI(INDEX)
      CALL GT1SCL(PTABLE(1,INDEX), SCALE, REFVAL, DUMMY)
      RETURN
      END
