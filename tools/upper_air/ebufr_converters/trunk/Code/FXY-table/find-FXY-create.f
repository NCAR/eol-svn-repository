C
C $Id: find-FXY-create.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: find-FXY-create.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Find-FXY-create
C  6 Character Name: FDFXY
C           Purpose: To search for a given FXY triple in the FXY 
C                    tables, and create a new such triple if none 
C                    exists.
C Import Parameters:
C    FN      --  }
C    XN      --  }  Triple to search for.
C    YN      --  }
C Export Parameters:
C    --  Returns an index for (FN,XN,YN).  This is either the index of
C        of a previously create entry for this triple if it existed, or
C        that of a newly created triple.
C     Prerequisites:
C Commons directly
C          accessed: FXYTAB
      INTEGER FUNCTION FDFXY(FN, XN, YN)
      INTEGER FN, XN, YN
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/FXY_table.f'
      
      EXTERNAL FDFXYI
      LOGICAL FDFXYI

C ====================================================================
C   Local variables
      INTEGER I
C ====================================================================
      IF (.NOT. FDFXYI(FN,XN,YN,I)) THEN
         IF (I .GT. MAXFXY) THEN
            CALL ENCERR(29)
         ENDIF
         FXYTOP = I
         F(I) = FN
         X(I) = XN
         Y(I) = YN
         MARKD(I) = .FALSE.
         CTABLE(I) = .FALSE.
         FTABLE(I) = .FALSE.
      ENDIF

      FDFXY = I
      RETURN
      END
