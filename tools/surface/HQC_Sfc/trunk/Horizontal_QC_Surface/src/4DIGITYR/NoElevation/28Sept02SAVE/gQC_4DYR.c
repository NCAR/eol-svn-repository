/*-------------------------------------------------------
 * gQC_4DYR - This module contains the functions to perform
 *   horizontal Quality Control.
 *
 * Var defs:
 *   n          = number of total stns at time t.
 *   A[n,0]     = lat of station
 *   A[n,1]     = lon of station
 *   A[n,2]     = weight for station
 *   A[n,3]     = distance from ref station
 *   theta_o[n] = Observations at stns in A[,].
 *
 *   b[]       = reference station (lat/lon)
 *   theta_obs = Observation at stn b.
 *   pmethod   = flag indicating weight fn to be used.
 *   qcflag    = Quality Control flag returned.
 *
 * 07 Jun 95 lec
 *    Cleanup.
 * 11 Sept 95 lec
 *    Placed DEBUG checks around some computations in
 *    determine_sigma() fn, so that unless DEBUG is
 *    requested, unnecessary computations are not done.
 * 08 Nov 95 lec
 *    Updated determine_sigma() fn to properly handle
 *    new set of data points from compute_sigma().
 * 19 August 2002 lec
 *    Update to handle 4 digit year.
 *-------------------------------------------------------*/
#include <stdlib.h>
#include <math.h>   /* for stats code */
#include <errno.h>

#include "gQC_4DYR.h"
#include "process_qcfrec_4DYR.h"
#include "dist.h"
#include "local.h"

/*
 * Set DEBUG to 1 for debug printouts. 
 * Set DEBUG to 0 to prevent debug printouts. 
 */
#define DEBUG  0
#define DEBUG1 0

/*-------------------------------------------------------
 * weight -  Determines weight of each station according
 *  to method selected by value of i.
 *
 *   A[,0]= lat of station
 *   A[,1]= lon of station
 *   A[,2]= weight for station
 *   A[,3]= distance from ref station
 *   i = function selector
 *
 *   Note that obs from ref station should NOT
 *   be included in estimated theta.
 *-------------------------------------------------------*/
void weight(/*in/out*/float A[MAXNUMSTNS][4],
            /*in*/    int   n,
            /*in*/    int   i)
   {   
   int j;
  
   /*-------------------------------------------
    *	Now select functions based on value of i
    *------------------------------------------*/
   switch (i)  {
      case 1:
        /*---------------------------------
         *   This is i=1 => r^-1 case (p=1)
         *---------------------------------*/
#if DEBUG
        printf ("p=1\n");
#endif
         for (j=0;j<=n-1;j++)
            {
            if (A[j][3] != 0)
                A[j][2]=1.0/(A[j][3]);
            else
                A[j][2]=0.0;
#if DEBUG1
            printf ("A[%4d][weight] : %f\n", j,  A[j][2]);
#endif
            }
         break;

      case 2:
         /*--------------------------------
          * This is i=2 => r^-2 case (p=2) 
          *-------------------------------*/
#if DEBUG
         printf ("p=2\n");
#endif
         for (j=0;j<=n-1;j++)
            if (A[j][3] != 0)
               A[j][2]=1.0/(A[j][3]*A[j][3]);
            else 
                A[j][2]=0.0;
         break;

      case 3:
         /*--------------------------------
          * This is i=3 => r^-3 case (p=3)
          *--------------------------------*/
#if DEBUG
         printf ("p=3\n");
#endif
         for (j=0;j<=n-1;j++)
            if (A[j][3] != 0)
               A[j][2]=1.0/(A[j][3]*A[j][3]*A[j][3]);
            else 
                A[j][2]=0.0;
         break;

      case 0:
         /*-------------------------------- 
          * This is i=0 => r^-0 case (p=4)
          *--------------------------------*/
#if DEBUG
         printf ("p=0\n");
#endif
         for (j=0;j<=n-1;j++)
            A[j][2]=1.0;

         break;

      case 9:
         /*-----------------------
          * Add p=9 => gaussian?
          *-----------------------*/
#if DEBUG
         printf ("p=9\n");
#endif
         break;

      default:
         printf ("Error: Unknown pmethod in weight fn!\n");
         break;

      } /* switch */

   } /* weight() */

/*------------------------------------------------------------ 
 * determine_qcflag -  compute and compare thetas
 *                     and return quality control flag.
 *
 * Input:
 *   A[,]        - Array containing lats,lons, weights, and distances.
 *   theta_o[]   - Array of observations.
 *   n           - number of stations.
 *   sigma_sq    - Sigma squared (variance) computed for this
 *                 station and this parameter at current time.
 *   min_weight  - Minimum weight allowed to influence current
 *                 station. Determines Area of Influence around
 *                 each station.
 *   alpha_sq[,] - Squared input alpha values that determine
 *                 Good, Dubious, and Unlikely for each parameter.
 *   data_type   - Type of data begin processed. (0=Stn Press,
 *                 1=Sea Lvl Press, 2=Calc Sea Lvl Press, 3=Temp,
 *                 4=Dew Point, 5=Wind Speed, 6=Wind Direction.)
 *   theta_obs   - Observation to be QC'd.
 *
 * Output:
 *   qcflag - One of Good (G), Dubious (D), or Unlikely (B).
 *
 *------------------------------------------------------------*/
void determine_qcflag(/*in*/  float A [MAXNUMSTNS][4],       /*nx4*/
                      /*in*/  float theta_o[MAXNUMSTNS],
                      /*in*/  int   n,                       /* num stns */
                      /*in*/  float sigma_sq,                /* variance */
                      /*in*/  float min_weight,              /* min weight - determines AOI. */
                      /*in*/  float alpha_sq[NUMQCPARMS][2], /* squared alpha values */ 
                      /*in*/  float theta_obs,               /* Observation begin QC'd */
                      /*in*/  int   data_type,               /* indicates type of data being QC'd */
                      /*in/out*/ char *qcflag)               /* Set QC flag */

   {
   int   i,j;
   long  wts_added = 0;

   float s = 0.0;
   float p;

   float theta_eb = 0.0; /* bottom */
   float theta_et = 0.0; /* top    */

   float theta_diff   = 0.0;
   float theta_diffsq = 0.0;

   float ave = 0.0;

#if DEBUG
   printf ("Enter determine_qcflag\n");
   printf ("Determine_qcflag: n, theta_obs, sigma_sq, *qcflag, min_weight: %d %7.2f %f %c %f\n",
            n, theta_obs, sigma_sq, *qcflag, min_weight);
#endif

   /*-------------------------------------------------
    * Leave Missing (M), Not Measured (N), Glitch (X),
    * Exceeds (C), Insufficient (I) flags unchanged.
    * Also if value is -999.99, set flag to missing.
    * This check is already performed by the calling
    * fn. It can be removed. Just a safety check.
    *------------------------------------------------*/
   if ( *qcflag == 'M' || *qcflag == 'N' || *qcflag == 'X' ||
        *qcflag == 'C' || *qcflag == 'I'  )
       {
#if DEBUG
       printf ("Determine_qc: qcflag is M,N,X,C or I. RETURN\n");
#endif
       return;
       }

   /*--------------------------------------------------------
    * Compute theta_e the expected (estimated) observation
    * value for point b. Then compute the difference between
    * the expected and observed parameter value at b.
    *--------------------------------------------------------*/ 
   theta_et = theta_eb = theta_diffsq = 0.0;

   for (i=0;i<=n-1;i++)
      {
      if (A[i][2] >= min_weight && theta_o[i] >-999.00) /* Don't add in missing. */
         {
         theta_et =theta_et + theta_o[i]*A[i][2];  /* top theta */
         theta_eb =theta_eb + A[i][2];             /* bottom theta */

         wts_added++;

#if DEBUG
         printf ("Add theta_o[%d], A[%d][weight] :%7.2f %f\n", i, i, theta_o[i], A[i][2]);
#endif
         }
#if DEBUG1
      else
          printf ("Weight too small or Missing. Not added! theta_o[%d], A[%d][2]: %7.2f %f\n", 
                   i, i, theta_o[i], A[i][2]);
#endif
      } 

   if (wts_added > 0)
      {
      /*----------------------------------------------------------------- 
       * Compute the square of the difference between the estimated and
       * observed values, since we are comparing to the variance (std sq
       * or sigma squared).
       *----------------------------------------------------------------*/
      theta_diff = ((theta_et/theta_eb) -theta_obs);
   
      /*-------------------------------------------------------------------
       * Correct wind direction differences that are more than 180 degrees.
       *------------------------------------------------------------------*/
      if (data_type == 6)
         {
         if (theta_diff > 180.0)
            theta_diff = theta_diff - 360.0;
         else if (theta_diff < -180.0)
            theta_diff = theta_diff + 360.0;
         }
      
      theta_diffsq = theta_diff*theta_diff;

#if DEBUG
      printf ("\ntheta_et, theta_eb, theta_e:\n%f %f %f\n",
              theta_et, theta_eb, theta_et/theta_eb); 
      printf ("\ntheta_obs, theta_e, sq(theta_e-theta_obs)=theta_diffsq:\n%7.2f %7.2f %f\n",
              theta_obs, theta_et/theta_eb, theta_diffsq);
#endif

      /*--------------------------------------------------
       * Set Quality Control flag and return! Note that
       * both sides of following checks are squared...even
       * the constants (i.e., Alpha). Treat wind direction
       * (when wind speed < 10m/s) as special case. That is,
       * when wind speed >= 10m/s use input B,D,G as usual,
       * but when wind speed < 10m/s (light and var winds)
       * never set qcflag to B for Unlikely. Only allow
       * Good (G) or Dubious (D). This is currently 
       * handled as another logical check in the calling fn.
       *-------------------------------------------------*/
      if (theta_diffsq >= alpha_sq[data_type][0]*sigma_sq)
         *qcflag='B';                   /* Unlikely or Bad */

      else if (theta_diffsq >= alpha_sq[data_type][1]*sigma_sq)
         *qcflag='D';                   /* Dubious  */

      else
         *qcflag='G';                   /* Good */

#if DEBUG
       printf ("(Data_type = %d; Alpha_sq: B, D: %4.2f   %4.2f)\n\n",
              data_type, alpha_sq[data_type][0], alpha_sq[data_type][1]);

       printf ("Theta_diffsq   (Alpha[B]*VAR)**2  (Alpha[D]*VAR)**2  Variance QCflag\n");
       printf ("%f        %f          %f         %f   %c\n\n",
              theta_diffsq, alpha_sq[data_type][0]*sigma_sq, alpha_sq[data_type][1]*sigma_sq,
              sigma_sq, *qcflag);
#endif
      } /* wts_added */

   } /* determine_qcflag() */


/*-------------------------------------------------------
 * determine_sigma - Computes average, variance and
 *   standard deviation, etc. for input data.
 *
 *  (Actually only need to compute ave and var for this
 *   HQC processing.)
 *-------------------------------------------------------*/
void determine_sigma(/*in*/  float data[SIGMA_PERIOD],
                     /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                     /*out*/ float *ave,  /* DEBUG */
                     /*out*/ float *svar,
                     /*out*/ float *adev, /* DEBUG */
                     /*out*/ float *sdev) /* DEBUG */
   {
   int   j = 0;
   int   count = 0;
   float s = 0.0;
   float p = 0.0;
   
   *ave = 0.0;
   *svar = 0.0;

   *adev=(*sdev)=0.0;

#if DEBUG
   printf ("Enter det_sigma: n=%ld\n", n);
   for (j=0;j<n;j++)
      printf ("data[%ld]: %7.2f\n", j, data[j]);
#endif

   for (j=0;j<n;j++)
      if (data[j] > -990.00)
         {
         s += data[j];  /* count and add only non-missing values. */
         count++;
         }

#if DEBUG
   printf ("n, count: %d %d\n", n, count);
#endif

   /*---------------------------------------------
    * Min Num pts to compute variances is parameter
    * defined in gQC.h. count is actual count. Be
    * aware that missing values may be mixed with
    * non-missing values in the input array.
    *---------------------------------------------*/
   if (n < MINIMUM_NUM_VALS || count < MINIMUM_NUM_VALS)
      return;

   *ave=s/count;

   for (j=0;j<=n-1;j++)
      {
      if (data[j] > -990.00)
         {
#if DEBUG
         *adev += fabs(s=data[j]-(*ave));
#endif
         s=data[j]-(*ave);
         *svar += (p=s*s);
         }
      } /* for */

   *svar /= (count-1);

#if DEBUG
   *adev /= count;
   *sdev=sqrt(*svar);
   printf ("ave, svar, adev, sdev: %f %f %f %f\n", *ave, *svar, *adev, *sdev);
#endif
   } /* determine_sigma() */
