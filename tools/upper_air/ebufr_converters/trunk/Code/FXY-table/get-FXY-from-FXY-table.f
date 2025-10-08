C
C $Id: get-FXY-from-FXY-table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-FXY-from-FXY-table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Get-FXY-from-FXY-table
C  6 Character Name: GT1FXY
C              (N.B. GETFXY is parameter table routine. (q.v.))
C           Purpose: To find the FXY values associated with a given
C                    FXY table index.
C Import Parameters:
C    INDEX   --  Set to index for (FN,XN,YN) if found.  If not set to
C                next available index.
C Import Parameters:
C    INDEX   --  Index for (FN,XN,YN).
C Export Parameters:
C    FN      --  }
C    XN      --  }  Triple of FXY values from FXY table.
C    YN      --  }
C     Prerequisites: Triple must have been previously stored.
C Commons directly
C          accessed: FXYTAB

      SUBROUTINE GT1FXY(INDEX, FN, XN, YN)
      INTEGER INDEX, FN, XN, YN

      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'

      CALL CHKFXI(INDEX)

      FN = F(INDEX)
      XN = X(INDEX)
      YN = Y(INDEX)

      RETURN
      END
