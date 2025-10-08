C
C $Id: store-code-and-flag-table-info.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: store-code-and-flag-table-info.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: store-code-and-flag-table-info
C  6 Character Name: STRCFL
C           Purpose: To set the code/flag table bits for a given FXY
C                    tuple as appropriate.
C Import Parameters:
C    INDEX   --  Index of the appropriate tuple in the FXY tables.
C    ISCODE  --  True if this FXY value has a code table value.
C    ISFLAG  --  True if this FXY value has a flag table value.
C Export Parameters: (none)
C     Prerequisites:
C Commons directly
C          accessed: FXYTAB
      SUBROUTINE STRCFL(INDEX, ISCODE, ISFLAG)
      INTEGER INDEX
      LOGICAL ISCODE, ISFLAG
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)

      CTABLE(INDEX) = ISCODE
      FTABLE(INDEX) = ISFLAG
      RETURN
      END
