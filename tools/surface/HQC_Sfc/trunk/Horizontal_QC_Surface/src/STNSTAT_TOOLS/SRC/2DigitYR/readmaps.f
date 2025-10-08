      PROGRAM READMAPS

C	This program reads in a MAPS surface analyses in packed ASCII
C	format and prints them out.

C	9 variables are stored: 
C	  1=MAPS sea-level pressure (mb), 2=potential temperature (deg K),
C	  3=u component (m/s), 4=v component (m/s) 
C	  5=dew point temperature (deg K), 6=dew point depression (deg C),
C	  7=altimeter change in 3 hours (mb/3h),
C	  8=NWS sea-level pressure (mb), 9=altimeter (mb)


C *** The packed ASCII format is a number between 0 and 9999
C	which is a fraction of the range between the max and min
C	values for the field.

C *** UNPACKING ALGORITHM 
C	value = range * IG / 10000. + min

	 IMPLICIT NONE

	 INTEGER P,X,Y
      PARAMETER (P = 9,		! No. of variables
     +	   X = 81,		! No. of grid points in X direction
     +	   Y = 62)		! No. of grid points in Y direction

	 REAL G(X,Y,P) 		! Gridded field (analysis)

	 INTEGER	I,J,K
      INTEGER IG(X,Y)
      REAL FMIN,RANGE
      CHARACTER*1 YN
      CHARACTER*8 VARNAME(P)
	 CHARACTER*9 ATIME
C	9-digit time is in format yyjjjhh00 where yy is the year,
C	  jj is the Julian date, and hh is the hour UTC.

      DATA VARNAME /'MAPS SLP','Pot Temp','U Wind  ',
     +              'V Wind  ','Dew Pt  ','DP Depr ',
     +              'Alt Chg ','NWS SLP ','Altimetr'/

C******************************************************************

	 TYPE *,' Enter time portion of the input file as YYDDDHHMM?'
	 ACCEPT 100, ATIME
100	 FORMAT(A9)

      OPEN(UNIT=10,NAME=ATIME//'.anx',
     +FORM='FORMATTED',STATUS='OLD')

      READ (10,1005) ATIME
      WRITE (6,1005) ATIME

      DO K=1,P
        READ(10,1003) VARNAME(K),FMIN,RANGE
        WRITE(6,1004) VARNAME(K),FMIN,RANGE
        DO J=1,Y
           READ(10,1001) (IG(I,J),I= 1,32)
           READ(10,1001) (IG(I,J),I=33,64)
           READ(10,1002) (IG(I,J),I=65,81)
        END DO
        DO J=1,Y
           DO I=1,X
              G(I,J,K) = RANGE*IG(I,J)/10000. + FMIN
           END DO
        END DO
      END DO

	 TYPE *,' Grid is loaded.  Display it?'
      ACCEPT 101, YN
101   FORMAT(A1)
      IF(YN .EQ. 'N' .OR. YN .EQ. 'n') GOTO 999

      WRITE(6,1007) (VARNAME(I),I=1,P)
      DO J=1,Y
         DO I=1,X
            WRITE(6,1008) I,J,(G(I,J,K),K=1,P)
         END DO
	    TYPE *,' Continue?'
         ACCEPT 101, YN
         IF(YN .EQ. 'N' .OR. YN .EQ. 'n') GOTO 999
      END DO

999   CLOSE (10)
	 STOP

1001  FORMAT(32I4.4)
1002  FORMAT(17I4.4)
1003  FORMAT(' ******************     ',A8,
     +       '     ****************** MIN VAL = ',F10.3,
     +       ' RANGE = ',F10.3)
1004  FORMAT(' Reading in Parameter=',A8,
     +       '   Min Val=',F10.3,'   Range=',F10.3)
1005  FORMAT(' ** MAPS SURFACE ANALYSIS AT TIME = ',A9,
     +       '     ******************')
1007  FORMAT(' ',//,'  X  Y   ',9(A8,3X),/)
1008  FORMAT(' ',2(I2,X),9(F10.3,X))

	 END
