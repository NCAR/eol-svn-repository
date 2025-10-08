/*-------------------------------------------------------
 * gQC - This module contains the functions to perform
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
 *-------------------------------------------------------*/
#include <stdlib.h>
#include <math.h>   /* for stats code */
#include <errno.h>

#include "gQC.h"
#include "dist.h"
#include "local.h"

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
            /*in*/    float b[2],
            /*in*/    int   n,
            /*in*/    int   i)
   {   
   int j;
  
   /*
    *	Now select functions based on value of i
    *
    */
   switch (i)  {
      case 1:
        /*
         *   This is i=1 => r^-1 case (p=1)
         */
        printf ("p=1\n");

         for (j=0;j<=n-1;j++)
            {
            if (A[j][3] != 0)
                A[j][2]=1.0/(A[j][3]);
            printf ("A[%4d][weight] : %f\n", j,  A[j][2]);
            }
         break;

      case 2:
         /* 
          * This is i=2 => r^-2 case (p=2) 
          */
         printf ("p=2\n");
         for (j=0;j<=n-1;j++)
            if (A[j][3] != 0)
               A[j][2]=1.0/(A[j][3]*A[j][3]);
         break;

      case 3:
         /* 
          * This is i=3 => r^-3 case (p=3)
          */
        printf ("p=3\n");
         for (j=0;j<=n-1;j++)
            if (A[j][3] != 0)
               A[j][2]=1.0/(A[j][3]*A[j][3]*A[j][3]);

         break;

      case 0:
         /* 
          * This is i=0 => r^-0 case (p=4)
          */
        printf ("p=0\n");
         for (j=0;j<=n-1;j++)
            A[j][2]=1.0;

         break;

      case 9:
         /*
          * Add p=9 => gaussian?
          */
        printf ("p=9\n");
         break;

      default:
         break;

      } /* switch */

   printf ("Exit weight\n");
   } /* weight() */


/*------------------------------------------------------- 
 * determine_qcflag -  compute and compare thetas
 *   and return quality control flag.
 *
 *-------------------------------------------------------*/
void determine_qcflag(/*in*/  float A [MAXNUMSTNS][4],  /*nx4*/
                      /*in*/  float theta_o[MAXNUMSTNS],
                      /*in*/  int   n,                  /* num stns */
                      /*in*/  int   pmethod,            /* weighting method */
                      /*in*/  float b[2],               /* ref stn */
                      /*in*/  float theta_obs,   
                      /*in*/  float sigma_sq,           /* variance */
                      /*out*/ char  *qcflag)
   {
   int   i,ii,j;
   float s = 0.0;
   float p;

   float theta_eb, theta_et=0.0;
   float theta_diff = 0.0;
   float theta_e = 0.0;

   float ave = 0.0; /* debug only */

   double x,y;

   printf ("Enter determine_qcflag.\n");

   /* 
    *  Compute distance from ref station and put in A[,4]
    */
   for (ii=0;ii<=n-1;ii++) 
      { 
      ll2xydrv( b[0], b[1], &x, &y, A[ii][0], A[ii][1]);
      A[ii][3]=sqrt(x*x+y*y); /* distance */
      printf ("A[%4d][dist] : %f\n", ii,  A[ii][3]);
      } 

   /*
    * Determine "weights" (influence) of other stations.
    */
   weight(A, b, n, pmethod);

   /*
    * Compute theta_e the expected (estimated) observation
    * value for point b. Then compute then difference between
    * the expected and observed parameter value at b.
    */ 
   theta_et = theta_eb = theta_diff = 0.0;

   for (i=0;i<=n-1;i++)
      {
      theta_et =theta_et + theta_o[i]*A[i][2];  /* top theta */
      theta_eb =theta_eb + A[i][2];             /* bottom theta */
      printf ("theta_o[%d], A[%d][weight] :%f %f\n", i, i, theta_o[i], A[i][2]);
      }

   /* 
    * Compute the square of the difference between the estimated and
    * observed values, since we are comparing to the variance (std or
    * sigma squared).
    */
   theta_diff = ((theta_et/theta_eb) -theta_obs) * ((theta_et/theta_eb) -theta_obs);

   printf ("(theta_et, theta_eb), theta_e, theta_obs, theta_diff:\n%f %f %f %f %f\n",
             theta_et, theta_eb, theta_e, theta_obs, theta_diff);

   /*
    * Set Quality Control flag and return!
    */
   if (theta_diff > 1.5*sigma_sq)
      *qcflag='F';                   /*Failed test */

   else if (theta_diff < 0.5*sigma_sq)
      *qcflag='L';                   /*okay passed low */

   else if (theta_diff < 1.0*sigma_sq)
      *qcflag='M';                   /*okay passed mod*/

   else if (theta_diff < 1.5*sigma_sq)
      *qcflag='H';                   /*okay passed high */

   } /* determine_qcflag() */


/*-------------------------------------------------------
 * determine_sigma - Computes average and variance for
 *   input data.
 * 
 *-------------------------------------------------------*/
void determine_sigma(/*in*/  float data[MAXNUMSTNS],
                     /*in*/  int   n,
                     /*out*/ float *ave,  /* debug */
                     /*out*/ float *svar,
                     /*out*/ float *adev, /* debug */
                     /*out*/ float *sdev) /* debug */
   {
   int j;
   float s,p = 0.0;
 
   if (n <= 1) perror("n must be at least 2 in MOMENT");

   for (j=0;j<=n-1;j++) s += data[j];   /* was from 1 to n!!!!!*/

   *ave=s/n;
   *adev=(*svar)=(*sdev)=0.0;

   for (j=0;j<=n-1;j++) {               /* was from 1 to n!!!! */
      *adev += fabs(s=data[j]-(*ave));
      *svar += (p=s*s);
   }
   *adev /= n;
   *svar /= (n-1);
   *sdev=sqrt(*svar);

   } /* determin_sigma() */
