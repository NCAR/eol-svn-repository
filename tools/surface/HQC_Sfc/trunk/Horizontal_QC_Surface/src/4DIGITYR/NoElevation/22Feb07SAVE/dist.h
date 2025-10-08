/*----------------------------------------------------------
 * distance.h - Header file for distance.c
 *
 *---------------------------------------------------------*/
#ifndef DISTANCE_H
#define DISTANCE_H

#ifdef __STDC__

   extern void ll2xydrv(/*in*/ double PLAT,  /*in*/  double PLON,
                        /*out*/ double *X,   /*out*/ double *Y,
                        /*in*/ double ORLAT, /*in*/  double ORLON);

   extern void LL2XY (/*in*/  double DEGLAT,   /*internally defined as double */
                      /*in*/  double DEGLON,
                      /*out*/ double *X,
                      /*out*/ double *Y,
                      /*in*/  double  SWLAT,
                      /*in*/  double  SWLON);

#else /*!__STDC__*/

   extern void ll2xydrv();

   extern void LL2XY ();

#endif /*__STDC__*/

#endif /* DISTANCE_H */
