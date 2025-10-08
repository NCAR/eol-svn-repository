C
C $Id: gni.f,v 1.1 1992/11/02 21:08:32 john Exp $
C
	SUBROUTINE GTNXHL (RMB,FIELD,QCFLG,INHI,INLO,SDATA,CTR)
	REAL RMB,QCFLG,SDATA(21,*)
	INTEGER FIELD,INHI,INLO,CTR
C
C GTNXHL - GeT NeXt Hi Lo
C  returns the next hi,lo indices surrounding pressure RMB
C   with SDATA(FIELD,*) == QCFLG
C  INHI,INLO - INdex HI,LO - current positions of hi,lo indices
C QCFLG is considered to be the minimum acceptable value. If there
C  is a better flag closer to RMB, then the better flag is returned.
C
	INTEGER QCCMP
	INTEGER TMPLO

	IF (RMB .GE. SDATA(2,INHI)) THEN
	  RETURN
	END IF
10	CONTINUE
	INHI = INHI + 1
        IF(FIELD .EQ. 18) THEN
D          WRITE(16,*)'RMB = ',RMB
D          WRITE(16,*)'INHI = ',INHI
        END IF         
	IF (INHI .GT. CTR) THEN
	  RETURN
	END IF
C If (pressure not reached) or (INHI.qcflg is worse than QCFLG),
C  then keep looking
	IF ( (SDATA(2,INHI) .GT. RMB) .OR.
&	     (QCCMP(SDATA(FIELD,INHI),QCFLG) .LT. 0) ) THEN
C&	    (SDATA(FIELD,INHI) .NE. QCFLG)) THEN
	  GO TO 10
	END IF
	TMPLO = INLO
20	CONTINUE
	TMPLO = TMPLO + 1
	IF (SDATA(2,TMPLO) .LT. RMB) THEN
	  RETURN
	END IF
	IF (SDATA(2,TMPLO) .EQ. RMB) THEN
	  INLO = TMPLO
	  RETURN
	END IF
	IF (TMPLO .GT. CTR) THEN
	  RETURN
	END IF
C If (a) (TMPLO.qcflg is better than or same as QCFLG), or
C    (b) (TMPLO.qcflg is better than or same as INLO.qcflg),
C then move INLO
C (a) moves INLO if there is a qcflg closer to RMB that meets the QCFLG minimum
C (b) moves INLO if there is a better qcflg between INLO and RMB
	IF ((QCCMP(SDATA(FIELD,TMPLO),QCFLG) .GE. 0) .OR.
&	    (QCCMP(SDATA(FIELD,TMPLO),SDATA(FIELD,INLO)) .GE. 0)) THEN
	  INLO = TMPLO
	END IF
	GO TO 20
	RETURN
	END



	SUBROUTINE GTNXIN
&	  (RMB,FIELD,HINDX,LONDX,QCFLG,ARANGE,BRANGE,SDATA,CTR)
	REAL RMB,QCFLG,ARANGE,BRANGE,SDATA(21,*)
	INTEGER FIELD,HINDX,LONDX,CTR
C
C GTNXIN - GeT NeXt INdices
C Input: RMB - desired pressure around which to interpolate
C        FIELD - SDATA(FIELD,*) to check against QCFLG
C        HINDX,LONDX - current Hi,Lo Indices
C Output: HINDX,LONDX - new Hi,Lo Indices around RMB
C         QCFLG - QC value derived from both SDATA(FIELD,HINDX) and
C                 SDATA(FIELD,LONDX)
C         HINDX,LONDX > CTR means there are no more values to be found
C                 for FIELD
C
	REAL QCWRST
	REAL QCDEG

C Local vars,initialization
	INTEGER GDHI,GDLO,ESHI,ESLO,DUHI,DULO,BAHI,BALO
	GDHI = HINDX
	ESHI = HINDX
	DUHI = HINDX
        BAHI = HINDX
	GDLO = LONDX
	ESLO = LONDX
	DULO = LONDX
        BALO = LONDX
C Algorithm:
C 1.try and get Good values <= A time apart (QC flag is Good)
C 2.try and get Estimated values <= A time apart (QC flag is Estimated)
C 3.try and get Good values <= B time apart (QC flag is Dubious)
C 4.try and get Estimated values <= B time apart (QC flag is Dubious)
C 5.try and get Dubious values <= B time apart (QC flag is D or Bad)
C 6.try and get Good values > B time apart (QC flag is Bad)
C 7.try and get Estimated values > B time apart (QC flag is Bad)
C 8.try and get Dubious values > B time apart (QC flag is Bad)
C 9.try and get Bad values (QC flag is Bad)
C
C Note that because of the QCCMP statements in GTNXHL, pairs of
C  QC flags like (Good,Estimated) or (Estimated,Dubious) are considered
C  with the code for the worse flag. For example, (Good,Estimated) <= A
C  time apart are considered in part 2. (G,E)<=B in 4;
C  (G,D)<=B and (E,D)<=B in 5; etc.

	CALL GTNXHL (RMB,FIELD,1.0,GDHI,GDLO,SDATA,CTR)
	IF (GDHI .LE. CTR) THEN
	  IF ((SDATA(1,GDHI) - SDATA(1,GDLO)) .LE. ARANGE) THEN
	    HINDX = GDHI
	    LONDX = GDLO
	    QCFLG = QCWRST(SDATA(FIELD,GDHI),SDATA(FIELD,GDLO))
	    RETURN
	  END IF
	END IF
	CALL GTNXHL (RMB,FIELD,4.0,ESHI,ESLO,SDATA,CTR)
	IF (ESHI .LE. CTR) THEN
	  IF ((SDATA(1,ESHI) - SDATA(1,ESLO)) .LE. ARANGE) THEN
	    HINDX = ESHI
	    LONDX = ESLO
	    QCFLG = QCWRST(SDATA(FIELD,ESHI),SDATA(FIELD,ESLO))
	    RETURN
	  END IF
	END IF
	IF (GDHI .LE. CTR) THEN
	  IF ((SDATA(1,GDHI) - SDATA(1,GDLO)) .LE. BRANGE) THEN
	    QCFLG = QCWRST(SDATA(FIELD,GDHI),SDATA(FIELD,GDLO))
C If the Good Hi/Lo flags are actually worse than good, then
C   skip to Est, (then Dubious if necessary). If Good Hi/Lo are
C   actually Dubious, for example, then GTNXHL guarrantees that
C   the Dubious Hi/Lo won't be any farther away than Good, but
C   they might be closer and thus might give a better flag!
C Don't bother skipping all the way to Bad if even the Dubious
C   Hi/Lo are Bad, since a Bad value is a Bad value and will
C   probably be ignored anyway
	    IF (QCFLG .NE. 1.0) THEN
	      GO TO 2001
	    END IF
	    HINDX = GDHI
	    LONDX = GDLO
	    QCFLG = QCDEG(QCFLG)
	    RETURN
	  END IF
	END IF
2001	CONTINUE
	IF (ESHI .LE. CTR) THEN
	  IF ((SDATA(1,ESHI) - SDATA(1,ESLO)) .LE. BRANGE) THEN
	    QCFLG = QCWRST(SDATA(FIELD,ESHI),SDATA(FIELD,ESLO))
	    IF (QCFLG .NE. 4.0) THEN
	      GO TO 2002
	    END IF
	    HINDX = ESHI
	    LONDX = ESLO
	    QCFLG = QCDEG(QCFLG)
	    RETURN
	  END IF
	END IF
2002	CONTINUE
	CALL GTNXHL (RMB,FIELD,2.0,DUHI,DULO,SDATA,CTR)
	IF (DUHI .LE. CTR) THEN
	  IF ((SDATA(1,DUHI) - SDATA(1,DULO)) .LE. BRANGE) THEN
	    HINDX = DUHI
	    LONDX = DULO
	    QCFLG = QCWRST(SDATA(FIELD,DUHI),SDATA(FIELD,DULO))
	    IF ((SDATA(1,DUHI) - SDATA(1,DULO)) .GT. ARANGE) THEN
	      QCFLG = QCDEG(QCFLG)
	    END IF
	    RETURN
	  END IF
	END IF
	IF (GDHI .LE. CTR) THEN
	  HINDX = GDHI
	  LONDX = GDLO
	  QCFLG = QCWRST(SDATA(FIELD,GDHI),SDATA(FIELD,GDLO))
	  QCFLG = QCDEG(QCFLG)
	  QCFLG = QCDEG(QCFLG)
	  RETURN
	END IF
	IF (ESHI .LE. CTR) THEN
	  HINDX = ESHI
	  LONDX = ESLO
	  QCFLG = QCWRST(SDATA(FIELD,ESHI),SDATA(FIELD,ESLO))
	  QCFLG = QCDEG(QCFLG)
	  QCFLG = QCDEG(QCFLG)
	  RETURN
	END IF
	IF (DUHI .LE. CTR) THEN
	  HINDX = DUHI
	  LONDX = DULO
	  QCFLG = QCWRST(SDATA(FIELD,DUHI),SDATA(FIELD,DULO))
	  QCFLG = QCDEG(QCFLG)
	  QCFLG = QCDEG(QCFLG)
	  RETURN
	END IF
        CALL GTNXHL (RMB,FIELD,3.0,BAHI,BALO,SDATA,CTR)
D        write(16,*)'bahi =',bahi
        IF (BAHI .LE. CTR) THEN
          HINDX = BAHI
          LONDX = BALO    
	  QCFLG = QCWRST(SDATA(FIELD,BAHI),SDATA(FIELD,BALO))
          
          RETURN
       END IF
       hindx = bahi
       londx = balo
       if (sdata(2,BAHI) .eq. 0.0) then
            go to 9999
          endif
       IF ((SDATA(1,HINDX) - SDATA(1,LONDX)) .LE. BRANGE) THEN
	  QCFLG = QCDEG(QCFLG)
       END IF
       IF ((SDATA(1,HINDX) - SDATA(1,LONDX)) .GT. BRANGE) THEN
	  QCFLG = QCDEG(QCFLG)
       END IF
  
9999    continue
	RETURN
        END



	INTEGER FUNCTION QCCMP(X,Y)
	REAL X,Y
C
C QCCMP - QC flag CoMPare
C Returns -1 if X is worse than Y
C          0 if X is same as Y
C         +1 if X is better than Y
C
	IF (X .EQ. Y) THEN
	  QCCMP = 0
	  RETURN
	END IF
	IF ((X .EQ. 99.0) .OR. (Y .EQ. 1.0)) THEN
	  QCCMP = -1
	  RETURN
	END IF
	IF ((X .EQ. 1.0) .OR. (Y .EQ. 99.0)) THEN
	  QCCMP = +1
	  RETURN
	END IF
	IF ((X .EQ. 9.0) .OR. (Y .EQ. 4.0)) THEN
	  QCCMP = -1
	  RETURN
	END IF
	IF ((X .EQ. 4.0) .OR. (Y .EQ. 9.0)) THEN
	  QCCMP = +1
	  RETURN
	END IF
	IF ((X .EQ. 3.0) .OR. (Y .EQ. 2.0)) THEN
	  QCCMP = -1
	  RETURN
	END IF
	QCCMP = +1
	RETURN
	END



	REAL FUNCTION QCWRST(X,Y)
	REAL X,Y
C
C QCWRST - returns the worst QC flag from X,Y
C Descending value of flags: U,M,B,D,E,G
C
	INTEGER QCCMP

	IF (QCCMP(X,Y) .LE. 0) THEN
	  QCWRST = X
	ELSE
	  QCWRST = Y
	END IF
	RETURN
	END



	REAL FUNCTION QCDEG(X)
	REAL X
C
C QCDEG - QC Degrade; degrades X by one QC level
C i.e. if X is Good, then Return Dubious
C      Estimated->Dubious, Dubious->Bad
C Note that you can't degrade any worse than Bad (3.0)
C
	IF (X .EQ. 4.0) THEN
	  QCDEG = 2.0
	ELSE IF (X .LE. 2.0) THEN
	  QCDEG = X + 1.0
        ELSE IF (X .EQ. 3.0) THEN
          QCDEG = X
	END IF
	RETURN
	END

