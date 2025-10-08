C
C USE the C version of this s/w to determine sigma!!!!!!
C
      SUBROUTINE MOMENT(DATA,N,AVE,ADEV,SDEV,VAR,SKEW,CURT)
      DIMENSION DATA(N)
      IF(N.LE.1)WRITE(6,*) 'N must be at least 2'
      S=0.
      DO 11 J=1,N
        S=S+DATA(J)
11    CONTINUE
      AVE=S/N
      ADEV=0.
      VAR=0.
      SKEW=0.
      CURT=0.
      DO 12 J=1,N
        S=DATA(J)-AVE
        ADEV=ADEV+ABS(S)
        P=S*S
        VAR=VAR+P
        P=P*S
        SKEW=SKEW+P
        P=P*S
        CURT=CURT+P
12    CONTINUE
      ADEV=ADEV/N
      VAR=VAR/(N-1)
      SDEV=SQRT(VAR)
      IF(VAR.NE.0.)THEN
        SKEW=SKEW/(N*SDEV**3)
        CURT=CURT/(N*VAR**2)-3.  
      ELSE
       WRITE(6,*)  'no skew or kurtosis when zero variance'
      ENDIF
      RETURN
      END

