      PROGRAM gridswapt

C     This program tests gridswap routines 

      IMPLICIT NONE

      REAL lat,lon,rlat,rlon,x,y
      REAL x_grid,y_grid,lat_xy,lon_xy
      character*2 t

C******************************************************************
C orig      rlat=22.83730717
C orig      rlon=-120.49050192

      rlat=22.8756
      rlon=-120.4911

      TYPE *,'Enter ll for ll2xy test or xy for xy2ll test:'
      ACCEPT 100, t
100   FORMAT(A2)

      if(t .eq. 'll') then
         type *, 'Enter latitude [xx.xxxxxxxx]:'
         accept 101, lat
101      format(F12.8)
         type *, 'Enter longitude [(-)xxx.xxxxxxxx]:'
         accept 102, lon
102      format(F13.8)
         x = x_grid(lat,lon)
         y = y_grid(lat,lon)
         write(6,1001) lat,rlat,x,lon,rlon,y
      else
         type *, 'Enter X distance in m [xxxxxxx.x]:'
         accept 103, x
103      format(F10.1)
         type *, 'Enter Y distance in m [xxxxxxx.x]:'
         accept 103, y
         lat = lat_xy(x,y)
         lon = lon_xy(x,y)
         write(6,1001) lat,rlat,x,lon,rlon,y
      end if

      stop
1001  FORMAT(' lat=',F12.8,'  reference lat=',F12.8,'  X (m)=',F10.1,/,
     +       ' lon=',F13.8,'  reference lon=',F13.8,'  Y (m)=',F10.1)

      END
C
C * * * * * * * * * * EARTH LENGTH CONVERSIONS* * * * * * * * * * * * * * *
C
C
C  Transformation functions from (LAT,LON) to (X,Y) in meters in the MAPS grid,
C  and the inverse functions.  (taken from the MAPS Developer Guide - 
C  Chapter 6). Added on October 29, 1984, from Stan Benjamin macros.
C
C	X_GRID and Y_GRID give (X,Y) as a function of (LAT,LON)
C
      Real FUNCTION GRIDSWAP()
C
C GRIDSWAP IS A DUMMY FUNCTION WHICH CONTAINS THE GRID TRANSFORMATION
C FUNCTIONS AS ENTRY POINTS.
C WARNING: NON-STANDARD FORTRAN FUNCTIONS ARE UTILIZED
C          SIND,COSD,ATAND,ATAN2D
C
C History:
c
c	K. Brewster 8-85
c	B. Jewett   7-29-87	altered for use with different grid domains
C
C
      Implicit None
      REAL LAT,LON,X,Y
      REAL LAT_XY,LON_XY,X_GRID,Y_GRID
      REAL SIND,COSD,ATAN2D,ATAND
      REAL LAT_TRUE_P,LON_XX_P,X_POLE_P,Y_POLE_P,EARTH_RAD_P
      parameter (EARTH_RAD_P = 6371230.)	! Mean earth radius (m)
      parameter (LAT_TRUE_P = 40.0)
      parameter (LON_XX_P = -15.0)
      parameter (X_POLE_P = 1854642.0)
      parameter (Y_POLE_P = 6691869.)
C
      ENTRY X_GRID(LAT,LON)
      X_GRID=(X_POLE_P+((1.+SIND(LAT_TRUE_P))/
     +(1.+SIND(LAT)))*EARTH_RAD_P*COSD(LAT)*COSD(LON-LON_XX_P))
      RETURN
C
      ENTRY Y_GRID(LAT,LON)
      Y_GRID=(Y_POLE_P+((1.+SIND(LAT_TRUE_P))/
     +(1.+SIND(LAT)))*EARTH_RAD_P*COSD(LAT)*SIND(LON-LON_XX_P))
      RETURN
C
C	LAT_XY and LON_XY give (LAT,LON) as a function of (X,Y)
C
      ENTRY LAT_XY(X,Y)
      LAT_XY=90.-2.*ATAND(SQRT((X-X_POLE_P)*(X-X_POLE_P)
     + +(Y-Y_POLE_P)*(Y-Y_POLE_P))/(EARTH_RAD_P*(1.+SIND(LAT_TRUE_P))))
      RETURN
C
      ENTRY LON_XY(X,Y)
      LON_XY=LON_XX_P+ATAN2D((Y-Y_POLE_P),(X-X_POLE_P))
      RETURN
      END
