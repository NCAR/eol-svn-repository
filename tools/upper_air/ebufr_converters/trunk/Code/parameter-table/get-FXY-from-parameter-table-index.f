C
C $Id: get-FXY-from-parameter-table-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-FXY-from-parameter-table-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-FXY-from-parameter-table-index
C  6 Character Name: GETFXY
C           Purpose: To find the FXY values correspoding to a parameter
C                    table index.
C Import Parameters:
C    INDEX   --  Index into the parameter table.
C Export Parameters:
C    F       --  }
C    X       --  }  Data requested.
C    Y       --  }
C     Prerequisites:  The data must have been stored first.
C Commons directly
C          accessed: PTAB1
      SUBROUTINE GETFXY(INDEX, F,X,Y)
      INTEGER INDEX, F,X,Y
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      CALL GT1FXY(PTABLE(1,INDEX), F,X,Y)
      RETURN
      END
