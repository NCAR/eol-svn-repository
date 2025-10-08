C
C $Id: get-bit-width-from-parameter-table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-bit-width-from-parameter-table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-bit-width-from-parameter-table
C  6 Character Name: GETBTW
C           Purpose: To find the bit width of a parameter.
C Import Parameters:
C    INDEX   --  Index into the parameter table.
C Export Parameters:
C    --  returns bit width for this parameter.
C     Prerequisites:  The data must have been stored first.
C Commons directly
C          accessed: PTAB1
      INTEGER FUNCTION GETBTW(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'
C ====================================================================
C   Local variables
      INTEGER DUMMY1, DUMMY2
      INTEGER BTWDTH
C ====================================================================

      CALL CHKPTI(INDEX)
      CALL GT1SCL(PTABLE(1,INDEX), DUMMY1, DUMMY2, BTWDTH)
      GETBTW = BTWDTH
      RETURN
      END
