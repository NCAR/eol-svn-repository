#include <stdio.h>
#include <stdlib.h>

#include "local.h"
#include "process_qcfrec.h"
#include "qcfrec.h"
#include "gQC.h"
#include "date.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * Set DEBUG2 to 1 for even more debug type output to
 * screen.
 *-------------------------------------------------------*/
#define   DEBUG  1

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
   extern void old_determine_sigma(/*in*/  double data[SIGMA_PERIOD],
                               /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                               /*out*/ double *ave,  /* DEBUG */
                               /*out*/ double *svar,
                               /*out*/ double *adev, /* DEBUG */
                               /*out*/ double *sdev);/* DEBUG */

   extern void new_determine_sigma(/*in*/  double data[SIGMA_PERIOD],
                               /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                               /*out*/ double *ave,  /* DEBUG */
                               /*out*/ double *svar,
                               /*out*/ double *adev, /* DEBUG */
                               /*out*/ double *sdev);/* DEBUG */

#else /*!__STDC__*/
   int main ();

   void old_determine_sigma();
   void new_determine_sigma();
#endif /*__STDC__*/

void old_determine_sigma(/*in*/  double data[SIGMA_PERIOD],
                        /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                        /*out*/ double *ave,  /* DEBUG */
                        /*out*/ double *svar,
                        /*out*/ double *adev, /* DEBUG */
                        /*out*/ double *sdev) /* DEBUG */
   {
   int   j = 0;
   double s = 0.0;
   double p = 0.0;
  
   *ave = 0.0;
   *svar = 0.0;
 
#if 1
   *adev=(*sdev)=0.0;
     
   printf ("Enter OLD det_sigma: n=%ld\n", n);
   printf ("OLD:  *adev, *svar: %lf %lf\n",
           *adev,*svar);

   for (j=0;j<n;j++)
      printf ("OLD data[%ld]: %7.2lf\n", j, data[j]);
#endif

   if (n < MINIMUM_NUM_VALS) /* Min Num pts to compute variances. See gQC.h. */
      return;
 
   for (j=0;j<n;j++)
      s += data[j];
 
   *ave=s/n;
 
   for (j=0;j<=n-1;j++)
      {
#if 1
      *adev += fabs(s=data[j]-(*ave));
#endif
      s=data[j]-(*ave);
      *svar += (p=s*s);
#if 0
      printf ("OLD: j, data[j], *ave:  %d %lf %lf\n",
               j, data[j], *ave);
      printf ("OLD: s, *adev, p, *svar:  %lf %lf %lf %lf %lf\n",
               s, *adev, p, *svar);
#endif

      printf ("OLD: j, data[j], *ave, s, *svar:  %d %lf %lf %lf %lf\n",
               j, data[j], *ave, s, *svar);
      }
 
   *svar /= (n-1);

#if 1
   *adev /= n;
   *sdev=sqrt(*svar);
   printf ("OLD: ave, svar, adev, sdev: %lf %lf %lf %lf\n", *ave, *svar, *adev, *sdev);
#endif
   } /* old_determine_sigma() */

/*-------------------------------------------------------
 * new_determine_sigma - Computes average, variance and
 *   standard deviation, etc. for input data.
 *
 *  (Actually only need to compute ave and var for this
 *   HQC processing.)
 *-------------------------------------------------------*/
void new_determine_sigma(/*in*/  double data[SIGMA_PERIOD],
                     /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                     /*out*/ double *ave,  /* DEBUG */
                     /*out*/ double *svar,
                     /*out*/ double *adev, /* DEBUG */
                     /*out*/ double *sdev) /* DEBUG */
   {
   int   j = 0;
   int   count = 0;
   double s = 0.0;
   double p = 0.0;
  
   *ave = 0.0;
   *svar = 0.0;
 
   *adev=(*sdev)=0.0;
 
#if 1
 
   printf ("NEW: Enter det_sigma: n=%ld\n", n);
   for (j=0;j<n;j++)
      printf ("data[%ld]: %7.2f\n", j, data[j]);
#endif

   if (n < MINIMUM_NUM_VALS) /* Min Num pts to compute variances. See gQC.h. */
      return;
 
   for (j=0;j<n;j++)
      if (data[j] > -990.00)
         {
         s += data[j];  /* count and add only non-missing values. */
         count++;
         }
#if 1
   printf ("NEW: n, count: %d %d\n", n, count);
#endif
 
   if (count < MINIMUM_NUM_VALS) /* Check actual count */
      return;
 
   *ave=s/count;
 
   for (j=0;j<=n-1;j++)
      {
      if (data[j] > -990.00)
         {
#if 1
         *adev += fabs(s=data[j]-(*ave));
#endif
         s=data[j]-(*ave);
         *svar += (p=s*s);
#if 0
         printf ("NEW: j, data[j]:  %d %f\n",
                 j, data[j]);
         printf ("NEW: *ave, s, *adev, p, *svar: %f %f %f %f %f\n",
                 *ave, s, *adev, p, *svar);
#endif
         printf ("NEW: j, data[j], *ave, s, *svar:  %d %lf %lf %lf %lf\n", 
               j, data[j], *ave, s, *svar);
         }
      } /* for */
         
   *svar /= (count-1);
 
#if 1
   *adev /= count;
   *sdev=sqrt(*svar);
   printf ("NEW: ave, svar, adev, sdev: %lf %lf %lf %lf\n", *ave, *svar, *adev, *sdev);
#endif
   } /* new_determine_sigma() */


/*---------------------------------------------------------
 * main() -  Controls variance (sigma squared) computations.
 *
 * 09 Dec 94 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   int           i,ii,j,jj,k,kk = 0;
   
   double         ave = 0.0;
static   double         adev = 0.0;
static   double         sdev = 0.0;
   double         svar = 0.0;


   /*
    * stn_data    - data from which sigmas are computed.
    */
   double stn_data[700][7][30];


i=13;
j=6;
stn_data[13][6][0]=   20.00;
stn_data[13][6][1]=   30.00;
stn_data[13][6][2]=    0.00;
stn_data[13][6][3]=  160.00;
stn_data[13][6][4]=  140.00;
stn_data[13][6][5]=   20.00;
stn_data[13][6][6]=  360.00;
stn_data[13][6][7]=  360.00;
stn_data[13][6][8]=  160.00;
stn_data[13][6][9]=  190.00;
stn_data[13][6][10]=  190.00;
stn_data[13][6][11]=  180.00;
stn_data[13][6][12]=  170.00;
stn_data[13][6][13]=   20.00;
stn_data[13][6][14]=   70.00;
stn_data[13][6][15]=   80.00;
stn_data[13][6][16]=  350.00;
stn_data[13][6][17]=    0.00;
stn_data[13][6][18]=  340.00;
stn_data[13][6][19]=  190.00;
stn_data[13][6][20]=  300.00;
stn_data[13][6][21]=  190.00;
stn_data[13][6][22]=  330.00;

                  old_determine_sigma(stn_data[i][j], 23, &ave, &svar, &adev, &sdev);

                  printf ("OLD: stn (%d) parm (%d), ave, svar, adev, sdev: %12.5lf %12.5lf %12.5lf %12.5lf\n\n",
                          i, j, ave, svar, adev, sdev);
#if 1
stn_data[13][6][0] =  330.00; /* It's the order in which they are read!!!!!! */
stn_data[13][6][1] = -999.99;
stn_data[13][6][2] = -999.99;
stn_data[13][6][3] = -999.99;
stn_data[13][6][4] = -999.99;
stn_data[13][6][5] = -999.99;
stn_data[13][6][6] = -999.99;
stn_data[13][6][7] = -999.99;
stn_data[13][6][8] =   20.00;
stn_data[13][6][9] =   30.00;
stn_data[13][6][10] =    0.00;
stn_data[13][6][11] =  160.00;
stn_data[13][6][12] =  140.00;
stn_data[13][6][13] =   20.00;
stn_data[13][6][14] =  360.00;
stn_data[13][6][15] =  360.00;
stn_data[13][6][16] =  160.00;
stn_data[13][6][17] =  190.00;
stn_data[13][6][18] =  190.00;
stn_data[13][6][19] =  180.00;
stn_data[13][6][20] =  170.00;
stn_data[13][6][21] =   20.00;
stn_data[13][6][22] =   70.00;
stn_data[13][6][23] =   80.00;
stn_data[13][6][24] =  350.00;
stn_data[13][6][25] =    0.00;
stn_data[13][6][26] =  340.00;
stn_data[13][6][27] =  190.00;
stn_data[13][6][28] =  300.00;
stn_data[13][6][29] =  190.00;
#endif
#if 0
stn_data[13][6][0]=  20.00;
stn_data[13][6][1]=   30.00;
stn_data[13][6][2]=    0.00;
stn_data[13][6][3]=  160.00;
stn_data[13][6][4]=  140.00;
stn_data[13][6][5]=   20.00;
stn_data[13][6][6]=  360.00;
stn_data[13][6][7]=  360.00;
stn_data[13][6][8]=  160.00;
stn_data[13][6][9]=  190.00;
stn_data[13][6][10]=  190.00;
stn_data[13][6][11]=  180.00;
stn_data[13][6][12]=  170.00;
stn_data[13][6][13]=   20.00;
stn_data[13][6][14]=   70.00;
stn_data[13][6][15]=   80.00;
stn_data[13][6][16]=  350.00;
stn_data[13][6][17]=    0.00;
stn_data[13][6][18]=  340.00;
stn_data[13][6][19]=  190.00;
stn_data[13][6][20]=  300.00;
stn_data[13][6][21]=  190.00;
stn_data[13][6][22]=  330.00;
stn_data[13][6][23]=  -999.99;
#endif
                  new_determine_sigma(stn_data[i][j], 30, &ave, &svar, &adev, &sdev);
 
                  printf ("NEW: stn (%d) parm (%d), ave, svar, adev, sdev: %12.5lf %12.5lf %12.5lf %12.5lf\n\n",
                          i, j, ave, svar, adev, sdev);
 
   }  /* test_gQC() */
