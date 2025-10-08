      PROGRAM llxy

C     This program tests routines ll2xy and xy2ll

      IMPLICIT NONE

      REAL lat,lon,rlat,rlon,x,y
      character*2 t

C******************************************************************
      rlat=22.83731
      rlon=-120.49050

      TYPE *,'Enter ll for ll2xy test or xy for xy2ll test:'
      ACCEPT 100, t
100   FORMAT(A2)

      if(t .eq. 'll') then
         type *, 'Enter latitude [xx.xxxxx]:'
         accept 101, lat
101      format(F9.5)
         type *, 'Enter longitude [(-)xxx.xxxxx]:'
         accept 102, lon
102      format(F10.5)
         call ll2xy(lat,lon,x,y,rlat,rlon)
         write(6,1001) lat,rlat,x,lon,rlon,y
      else
         type *, 'Enter X distance in km [xxxx.xx]:'
         accept 103, x
103      format(F8.2)
         type *, 'Enter Y distance in km [xxxx.xx]:'
         accept 103, y
         call xy2ll(lat,lon,x,y,rlat,rlon)
         write(6,1001) lat,rlat,x,lon,rlon,y
      end if

      stop
1001  FORMAT(' lat=',F9.5,'   reference lat=',F9.5,'   X (km)=',F8.2,/,
     +       ' lon=',F10.5,'  reference lon=',F10.5,'  Y (km)=',F8.2)

      END
C
C $Id: ll2xy.f,v 1.1 1991/12/23 16:35:01 mark Exp $
C $Log: ll2xy.f,v $
c Revision 1.1  1991/12/23  16:35:01  mark
c Initial revision
c
C

      SUBROUTINE  LL2XY (DEGLAT, DEGLON, X, Y, SWLAT, SWLON)
C
C  TO CONVERT LAT.,LONG. TO X,Y IN KM WITH RESPECT TO SWLAT,SWLON
C  PASCAL BY P. JOHNSON, 17-FEB-81.  FORTRAN TRANS R. VAUGHAN 9/81.
C  FINAL REPAIR, M. BRADFORD, 4/88
C  WARNING!  WORKS ONLY IN NORTHERN/WESTERN HEMISPHERES!
C
      PARAMETER (PI=3.141592654)
      PARAMETER (R=6380.12)
      PARAMETER (DEGRAD=0.01745329)
C
      X=0.0
      Y=0.0
      ALAT = SWLAT * DEGRAD
      CALAT = COS(ALAT)
      SALAT = SIN(ALAT)
      ALONG = ABS(SWLON * DEGRAD)
      BLAT = DEGLAT*DEGRAD
      BLONG = ABS(DEGLON)*DEGRAD
      CBLAT = COS(BLAT)
      SBLAT = SIN(BLAT)
      DLON = ALONG-BLONG
      DLAT=ABS(DEGLAT-SWLAT)*DEGRAD
      IF(DLAT.LT.0.0001.AND.ABS(DLON).LT.0.0001) GO TO 90
      CDLON = COS(DLON)
      AZA = ATAN(SIN( DLON)/(CALAT*SBLAT/CBLAT-SALAT*CDLON))
      AZB = ATAN(SIN(-DLON)/(CBLAT*SALAT/CALAT-SBLAT*CDLON))
C
C  GET BEARING
C
      IF(BLAT .LT. ALAT) AZA = AZA+PI
      IF(ALAT .LT. BLAT) AZB = AZB+PI
      IF(AZA .LT. 0) AZA = AZA + 2.*PI
      IF(AZB .LT. 0) AZB = AZB + 2.*PI
C
      SINN =  DLAT
      IF(DLON.NE.0.0)
     .       SINN = SIN(DLON) * SIN(PI/2.0-BLAT) / SIN(AZA)
C
      COSN = SALAT*SBLAT + CALAT*CBLAT*CDLON
      S = R * ATAN(SINN/COSN)
      X = S * SIN(AZA)
      Y = S * COS(AZA)
C
   90 CONTINUE
      RETURN
C
      END
C
C $Id: xy2ll.f,v 1.1 1991/12/23 16:37:59 mark Exp $
C $Log: xy2ll.f,v $
c Revision 1.1  1991/12/23  16:37:59  mark
c Initial revision
c
C

      SUBROUTINE XY2LL (DEGLAT,DEGLON,X,Y,SWLAT,SWLON)
C
C-----COMPUTES LATITUDE AND LONGITUDE FROM X,Y COORDINATES RELATIVE TO
C     LOCATION SPECIFIED BY SWLAT,SWLON.
C
C     DEGLAT- OUTPUT PARAMETER IN DEGREES OF LATITUDE
C     DEGLON- OUTPUT PARAMETER IN DEGREES OF LONGITUDE
C     X,Y-    INPUT COORDINATES OF LOCATION TO BE CONVERTED, IN KM
C     SWLAT-  INPUT PARAMETER IN DEG OF LATITUDE OF REFERENCE LOCATION
C     SWLON-  INPUT PARAMETER IN DEG OF LONG. OF REFERERENCE LOCATION
C
      REAL LAMDA1,LAMDA2
      DATA DEGARC/111.354/
C
      DEGLAT = SWLAT
      DEGLON = SWLON
C
      R = SQRT(X**2 + Y**2)
      IF (R .LT. 0.01) GO TO 10
C
      DTR = ATAN(1.)/45.
      RTD = 1./DTR
      THETA = ATAN2(X,Y)
      R = (R/DEGARC) * DTR
C
      PHI1 = DTR * SWLAT
      LAMDA1 = DTR * SWLON
C
      SANG = COS(THETA)*COS(PHI1)*SIN(R)+SIN(PHI1)*COS(R)
      IF(ABS(SANG).GT.1.0) SANG=SIGN(1.0,SANG)
      PHI2=ASIN(SANG)
C
      CANG = (COS(R)-SIN(PHI1)*SIN(PHI2))/(COS(PHI1)*COS(PHI2))
      IF(ABS(CANG).GT.1.0) CANG=SIGN(1.0,CANG)
      ACZ=ACOS(CANG)
C
      IF (X .LT. 0.0) THEN
          LAMDA2 = LAMDA1 + ACZ
      ELSE
          LAMDA2 = LAMDA1 - ACZ
      END IF
C
      DEGLAT = RTD * PHI2
      DEGLON = RTD * LAMDA2
C
 10   CONTINUE
C
      RETURN
      END
