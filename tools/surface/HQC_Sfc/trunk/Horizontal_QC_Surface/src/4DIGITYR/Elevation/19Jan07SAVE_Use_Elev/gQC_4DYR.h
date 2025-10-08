/*----------------------------------------------------------
 * gQC_4DYR.h - Header file for gQC_4DYR.c.
 *
 * 19 August 2002 lec
 *   Update for 4 digit year.
 *
 * Jan 2007 lec
 *   Updated A[][] to now include elevation as 3rd parameter
 *   so that elev can be used in HQC. Parm stn_no also now
 *   included for determine_qcflag(). parm max_delta_elev
 *   also included so that HQC now uses elev when determining
 *   HQC flags.
 *---------------------------------------------------------*/
#ifndef GQC_H
#define GQC_H

/*
 * SIGMA_PERIOD - Maximum number of days over which variances
 *                will be computed.
 * MINIMUM_NUM_VALS - Minimum number of days over which
 *                variances will be computed.
 */
#define SIGMA_PERIOD 30  
#define MINIMUM_NUM_VALS 15  

#include "local.h"
#include "process_qcfrec_4DYR.h"

#ifdef __STDC__

   extern void weight(/*in/out*/ float A[MAXNUMSTNS][5],
                      /*in*/     int   n,
                      /*in*/     int   i);


   extern void determine_qcflag(/*in*/  float A [MAXNUMSTNS][5],       /* nx5 */
                                /*in*/  float theta_o[MAXNUMSTNS],     /* Current observations for parm data_type */
                                /*in*/  int   n,                       /* num stns */
                                /*in*/  float sigma_sq,                /* variance */
                                /*in*/  float min_weight,              /* min weight - determines AOI. */
                                /*in*/  float max_delta_elev,          /* max_delta_elev - determines vertical AOI. */
                                /*in*/  float alpha_sq[NUMQCPARMS][2], /* squared alpha values */
                                /*in*/  float theta_obs,               /* Observation being QC'd */
                                /*in*/  int   stn_no,                  /* Current station being QC'd. Assigned by main QC */
                                /*in*/  int   data_type,               /* indicates type of data being QC'd */
                                /*in/out*/ char *qcflag);              /* Set QC flag */


   extern void determine_sigma(/*in*/  float data[SIGMA_PERIOD],
                               /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                               /*out*/ float *ave,  /* DEBUG */
                               /*out*/ float *svar,
                               /*out*/ float *adev, /* DEBUG */
                               /*out*/ float *sdev);/* DEBUG */

#else /*!__STDC__*/

   extern void weight();
   extern void determine_qcflag();
   extern void determine_sigma();

#endif /*__STDC__*/

#endif /* GQC_H */
