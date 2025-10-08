#include <stdlib.h>
#include <math.h>
#include "distance.h"

/* since adding in math.h - link iwht -lm library */

/*
 * Use to determine deg2dist between two points. This s/w takes
 * 2 points; determines distance from a to b then b to a and 
 * forms and returns the average distance for the final distance.
 */

/* ---------------------------------------------------------------------*
* Here is the package that we use for converting from latitude-longitude to xy
* distances from a given origin in latitude-longitude.  I wrote the main driver 
* for testing purposes and Bill Anderson wrote the driver routines to access
* conversion routines developed back in the mid-seventies, but which did not
* handle the transprintfion except in the US (west longitude,  north latitude).
* The original code was also ill-behaved under some circumstance like when the 
* longitude was nearly the same as the longitude of the origin, hence double 
* precision arithmetic is required.  The code also allows for the azimuth
* angle of the +x direction (eastward) to be specified for purposes of
* computing positions in a rotated Cartesian coordinate system.
* 
* Jay Miller (ljmill@ncar.ucar.edu)
* ---------------------------------------------------------------------*/

main()  /* was called program ll_xy */
   {
   double x,y,r;
   double olat, olon, plat, plon;

   /*
    *     Given: The latitude and longitude of the origin
    *     Calculate: The (x,y) location of a point in (lat,lon)
    *     Input this (x,y) back into routine to calculate a (lat,lon)
    *
    *           (lat,lon) ---> (x,y)
    *           (x,y)     ---> (lat,lon)
    *
    *     All latitudes and longitudes are in degrees, (x,y) in km.
    */

   /*   input origin (lat/lon), point (lat,lon) */

     printf("Input  latitude (deg) of origin: \n");
     scanf("%lf",&olat);
     printf("Input longitude (deg) of origin: \n");
     scanf("%lf",&olon);

     printf("Input   latitude (deg) of point: \n");
     scanf("%lf",&plat);
     printf("Input  longitude (deg) of point: \n");
     scanf("%lf",&plon);

     printf("olat/lon, plat/lon,x,y,r=%10.4lf %10.4lf %10.4lf %10.4lf\n",
             olat,olon,plat,plon);

   /*   convert the lat/lon of point to (x,y) from (olat,olon) */

     ll2xydrv(plat,plon,&x,&y,olat,olon);
     r=sqrt(x*x+y*y);

     printf("olat/lon, plat/lon,x,y,r=%10.4lf %10.4lf %10.4lf %10.4lf %10.4lf %10.4lf %10.4lf\n",
             olat,olon,plat,plon,x,y,r);

   /*   interchange roles of origin and point and calculate (x,y) */

     ll2xydrv(olat,olon,&x,&y,plat,plon);
     r=sqrt(x*x+y*y);

     printf("plat/lon, olat/lon,x,y,r==%10.4lf %10.4lf %10.4lf %10.4lf %10.4lf %10.4lf %10.4lf\n",
             plat,plon,olat,olon,x,y,r);
   }

/* --------------------------------------------------------------------- 
 *    DRIVER ROUTINE FOR CONVERTING THE SEPARATION OF TWO POINTS
 *    SPECifIED IN LAT, LON TO SEPARATION IN X, Y IN KM. 
 *    THIS ROUTINE (AND LL2XY) WILL WORK FOR POINTS IN ANY PART OF
 *    THE GLOBE WITH SOME RESTRICTIONS (SEE BELOW). THE ANGLE CONVENTIONS ARE:
 *    0   < LAT < 90   ==>  NORTHERN HEMISPHERE
 *    -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
 *    
 *    0    < LON < 180 ==>  WESTERN HEMISPHERE
 *    -180 < LON < 0   ==>  EASTERN HEMISPHERE
 *    
 *    PLAT  - LAT. OF POINT FOR WHICH X,Y IS DESIRED
 *    PLON  - LON. OF POINT FOR WHICH X,Y IS DESIRED
 *    X     - OUTPUT X VALUE RELATIVE TO ORLAT, ORLON IN KM
 *    Y     - OUTPUT Y VALUE RELATIVE TO ORLAT, ORLON IN KM
 *    ORLAT - LAT. OF ORIGIN
 *    ORLON - LON. OR ORIGIN
 *    ANGXAX- ANGLE OF X-AXIS REL. TO TRUE NORTH (USUALLY 90.0)
 *
 *    KNOWN RESTRICTIONS AND LIMITATIONS:
 *
 *    1) ||PLAT| - |ORLAT|| <= 90.0
 *    2) ||PLON| - |ORLON|| <= 90.0
 *    3) NO INPUT LAT OR LON VALUE SHOULD BE==AL TO EXACTLY ZERO
 *    4) THE CODE IS NOT SETUP TO HANDLE CROSsinG OVER BOTH HEMISPHERE 
 *    BOUNDARIES AT ONCE; IT CAN HANDLE CROSsinG EITHER THE NORTH/SOUTH 
 *    HEMISPHERE BOUNDARY OR THE WEST/EAST BOUNDARY, BUT NOT BOTH AT ONCE. 
 *    FOR EXAMPLE, YOU CAN'T HAVE AN ORIGIN AT (1.0 deg, 1.0 deg) AND TRY 
 *    TO FIND THE X,Y OF A POINT AT (-1.0 deg, -1.0 deg). YOU COULD FIND 
 *    THE X,Y OF A POINT AT (-1.0 deg, 1.0 deg), HOWEVER.
 *    5) CODE WON'T WORK ifYOU TRY TO CROSS A POLE
 *
 * ---------------------------------------------------------------------*/
void ll2xydrv(/*in*/ double PLAT, 
              /*in*/ double PLON, 
              /*out*/ double *X,   
              /*out*/ double *Y, 
              /*in*/  double ORLAT, 
              /*in*/  double ORLON )
  {

  int     ICROSS=0;
  int     INHEM, IWHEM;

  double  EPS     = 0.0001;
  double  DEGRAD  = 0.01745329;
  double  ANGXAX  = 90.0;

  double   X1, Y1, X2, Y2, XT, YT;
  double   DEGLAT, DEGLON;
  double   SWLAT, SWLON;
  double   THETA;

  /* 
   *    DETERMINE if A HEMISPHERE BOUNDARY HAS BEEN CROSSED
   */
  if ((ORLAT > 0.0) && (PLAT < 0.0)) ICROSS=1;
  if ((ORLAT < 0.0) && (PLAT > 0.0)) ICROSS=2;

  if ((ORLON > 0.0) && (PLON < 0.0))
     {
     if(ICROSS!=0) 
        {
        printf("+++ CANNOT HANDLE DUAL HEMISPHERE CROSSOVER \n");
        exit(1); 
        }
     else
        ICROSS=3;
     }
   else if((ORLON < 0.0) && (PLON > 0.0)) 
     {
     if(ICROSS!=0)
        {
        printf("+++ CANNOT HANDLE DUAL HEMISPHERE CROSSOVER \n");
        exit(1);
        }
     else
       ICROSS=4;
     }
         
  if(ORLAT>0.0)
     INHEM=1;
  else
     INHEM=0;

  if(ORLON>0.0)
     IWHEM=1;
  else
     IWHEM=0;

  switch (ICROSS)   {

     case 0:
       /*
        *  NO HEMISPHERE CROSSOVER; JUST CALL LL2XY
        */
      
       /* 
        *  MAKE SIGNED VALUES POSITIVE 
        */
       DEGLAT=fabs(PLAT);
       DEGLON=fabs(PLON);
       SWLAT =fabs(ORLAT);
       SWLON =fabs(ORLON);        
         
       LL2XY(DEGLAT,DEGLON,X,Y,SWLAT,SWLON);

       /*
        * SWITCH SIGNS if NOT IN NORTHERN OR WESTERN HEMISPHERES
        */
       if(INHEM==0)
          {
          *Y=(*Y)*(-1.0);
          }

       if(IWHEM==0) 
          {
          *X=(*X)*(-1.0);
          }

       break;

     case 1:
       /*
        *    +LAT -> -LAT
        */
       DEGLAT=EPS;
       DEGLON=fabs(PLON);
       SWLAT =fabs(ORLAT);
       SWLON =fabs(ORLON);

       LL2XY(DEGLAT,DEGLON,&X1,&Y1,SWLAT,SWLON);
         
       if(IWHEM==0) X1=X1*(-1.0);

       DEGLAT=fabs(PLAT);
       DEGLON=fabs(PLON);
       SWLAT =EPS;
       SWLON =fabs(PLON);

       LL2XY(DEGLAT,DEGLON,&X2,&Y2,SWLAT,SWLON);
         
       *Y= (fabs(Y1)+fabs(Y2))*(-1.0);
       *X=X1;

       break;

     case 2:
 
       /*
        *    -LAT -> +LAT
        */
       DEGLAT=EPS;
       DEGLON=fabs(PLON);
       SWLAT =fabs(ORLAT);
       SWLON =fabs(ORLON);

       LL2XY(DEGLAT,DEGLON,&X1,&Y1,SWLAT,SWLON);
         
       if(IWHEM==0) X1=X1*(-1.0);

       DEGLAT=fabs(PLAT);
       DEGLON=fabs(PLON);
       SWLAT =EPS;
       SWLON =fabs(PLON);

       LL2XY(DEGLAT,DEGLON,&X2,&Y2,SWLAT,SWLON);
         
       *Y=fabs(Y1)+fabs(Y2);
       *X=X1;

       break;
     
    case 3:
       /* 
        *    +LON -> -LON
        */
       DEGLAT=fabs(PLAT);

       if(fabs(PLON) <= 45.0)
          DEGLON = EPS;
       else
          {
          if(fabs(PLON)>=135.0)
             DEGLON=180.0-EPS;
          }

       SWLAT =fabs(ORLAT);
       SWLON =fabs(ORLON);

       LL2XY(DEGLAT,DEGLON,&X1,&Y1,SWLAT,SWLON);

       if(INHEM==0) Y1=Y1*(-1.0);

       DEGLAT=fabs(PLAT);
       DEGLON=fabs(PLON);
       SWLAT =fabs(PLAT);

       if(fabs(PLON)<=45.0)
          SWLON=EPS;
       else
          {
          if(fabs(PLON)>=135.0)
             SWLON=180.0-EPS;
          }

       LL2XY(DEGLAT,DEGLON,&X2,&Y2,SWLAT,SWLON);
         
       *Y=Y1;
       *X=fabs(X1)+fabs(X2);

       break;

     case 4:
       /*
        *    -LON -> +LON
        */
       DEGLAT=fabs(PLAT);

       if(fabs(PLON)<=45.0)
          DEGLON=EPS;
       else 
          {
          if(fabs(PLON)>=135.0)
             DEGLON=180.0-EPS;
          }

       SWLAT =fabs(ORLAT);
       SWLON =fabs(ORLON);

       LL2XY(DEGLAT,DEGLON,&X1,&Y1,SWLAT,SWLON);

       if(INHEM==0) Y1=Y1*(-1.0);

       DEGLAT=fabs(PLAT);
       DEGLON=fabs(PLON);
       SWLAT =fabs(PLAT);

       if(fabs(PLON)<=45.0)
          SWLON=EPS;
       else
          {
          if(fabs(PLON)>=135.0)
             SWLON=180.0-EPS;
          }

       LL2XY(DEGLAT,DEGLON,&X2,&Y2,SWLAT,SWLON);
         
       *Y=Y1;
       *X=(fabs(X1)+abs(X2))*(-1.0);
         
       break;

     default:
        printf ("What is ICROSS??\n");

     } /* switch ICROSS */

   /*
    *    ROTATE, if NECESSARY
    */
   if(ANGXAX!=90.0) 
      {
      THETA=(ANGXAX-90.0)*DEGRAD;
      XT=*X;
      YT=*Y;
      *X=XT*cos(THETA) - (YT*sin(THETA));
      *Y=XT*sin(THETA) + YT*cos(THETA);
      }

   } /* ll2xydrv() */
      
/*--------------------------------------------------------------------
 *
 *     LL2XY (DEGLAT, DEGLON, X, Y, SWLAT, SWLON)
 *
 * TO CONVERT LAT.,LONG. TO X,Y IN KM WITH RESPECT TO SWLAT,SWLON
 * PASCAL BY P. JOHNSON, 17-FEB-81.  FORTRAN TRANS R. VAUGHAN 9/81.
 * FINAL REPAIR, M. BRADFORD, 4/88
 * WARNING!  WORKS ONLY IN NORTHERN/WESTERN HEMISPHERES!
 *
 * --------------------------------------------------------------------*/
void LL2XY (/*in*/  double DEGLAT,   /*internally defined as double */
            /*in*/  double DEGLON, 
            /*out*/ double *X,
            /*out*/ double *Y, 
            /*in*/  double  SWLAT, 
            /*in*/  double  SWLON)
   {
   double PI = 3.141592654;
   double DEGRAD = 0.01745329;

   double ALAT, BLAT, CLAT, SALAT, ALONG,
          CALAT,CBLAT,SBLAT,DLON,DLAT, CDLON,
          BLONG,AZA,AZB,SINN, R, S, COSN;

   double  smallnum     = 0.0001;

   R=6380.12;

      *X=0.0;
      *Y=0.0;

      ALAT = SWLAT * DEGRAD;
      CALAT = cos(ALAT);
      SALAT = sin(ALAT);
      ALONG = fabs(SWLON * DEGRAD);
      BLAT = DEGLAT*DEGRAD;
      BLONG = fabs(DEGLON)*DEGRAD;
      CBLAT = cos(BLAT);
      SBLAT = sin(BLAT);
      DLON = ALONG-BLONG;
      DLAT=fabs(DEGLAT-SWLAT)*DEGRAD;

      if( (DLAT      < smallnum) && 
          (fabs(DLON)< smallnum) ) return;

      CDLON = cos(DLON);
      AZA = atan(sin( DLON)/(CALAT*SBLAT/CBLAT-SALAT*CDLON));
      AZB = atan(sin(-1.0*DLON)/(CBLAT*SALAT/CALAT-SBLAT*CDLON));
/*
 *  GET BEARING
 */
      if(BLAT < ALAT) AZA = AZA+PI;
      if(ALAT < BLAT) AZB = AZB+PI;
      if(AZA < 0) AZA = AZA + 2.*PI;
      if(AZB < 0) AZB = AZB + 2.*PI;

      SINN =  DLAT;
      if(DLON!=0.0)
         SINN = sin(DLON) * sin(PI/2.0-BLAT) / sin(AZA);

      COSN = SALAT*SBLAT + CALAT*CBLAT*CDLON;
      S = R * atan(SINN/COSN);

      *X = S * sin(AZA);
      *Y = S * cos(AZA);
   }/* LL2XY() */
