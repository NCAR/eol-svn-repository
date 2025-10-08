/*----------------------------------------------------------
 * gQC.h - Header file for gQC.c
 *
 *---------------------------------------------------------*/
#ifndef GQC_H
#define GQC_H

#include "local.h"

#ifdef __STDC__

   extern void weight(/*in/out*/ float A[MAXNUMSTNS][4],
                      /*in*/     float b[2],
                      /*in*/     int   n,
                      /*in*/     int   i);

   extern void determine_qcflag(/*in*/  float A [MAXNUMSTNS][4],  /*nx4 */
                                /*in*/  float theta_o[MAXNUMSTNS],
                                /*in*/  int   n,                  /* num stns */
                                /*in*/  int   pmethod,            /* weighting method */
                                /*in*/  float b[2],               /* ref stn */
                                /*in*/  float theta_obs,
                                /*in*/  float sigma,              /* valid for set time */
                                /*out*/ char  *qcflag);

   extern void determine_sigma(/*in*/  float data[MAXNUMSTNS],
                               /*in*/  int   n,
                               /*out*/ float *ave,
                               /*out*/ float *svar,
                               /*out*/ float *adev,
                               /*out*/ float *sdev);

#else /*!__STDC__*/

   extern void weight();
   extern void determine_qcflag();
   extern void determine_sigma();

#endif /*__STDC__*/

#endif /* GQC_H */
