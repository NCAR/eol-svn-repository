C
C $Id: find-fxy-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: find-fxy-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Find-FXY-index
C  6 Character Name: FDFXYI
C           Purpose: To search for a given FXY triple in the FXY tables.
C Import Parameters:
C    FN      --  }
C    XN      --  }  Triple to search for.
C    YN      --  }
C Export Parameters:
C    INDEX   --  Set to index for (FN,XN,YN) if found.  If not set to
C                next available index.
C    -- Returns true if the triple is found.
C     Prerequisites:
C Commons directly
C          accessed: FXYTAB
      LOGICAL FUNCTION FDFXYI(FN, XN, YN, INDEX)
      INTEGER FN, XN, YN, INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'
C ====================================================================
C   Local variables
      LOGICAL FOUND, NEW
      INTEGER A,B,C, I

      FOUND(I,A,B,C) = (F(I).EQ. A) .AND. (X(I).EQ.B) .AND. (Y(I).EQ.C)
C ====================================================================

      NEW = .TRUE.
      DO 10 I = 1,FXYTOP,1
         IF (FOUND(I,FN,XN,YN)) THEN
            NEW = .FALSE.
            GO TO 11
         ENDIF
 10   CONTINUE
C     Loop exit
 11   CONTINUE 

      IF (NEW) THEN
         INDEX = FXYTOP + 1
      ELSE
         INDEX = I
      ENDIF
      FDFXYI = .NOT. NEW
      RETURN
      END
