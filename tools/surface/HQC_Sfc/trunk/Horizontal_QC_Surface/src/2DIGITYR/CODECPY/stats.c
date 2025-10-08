#include <stdio.h>
#include <stdlib.h>

#include "local.h"
#include "process_qcfrec.h"
#include "qcfrec.h"
#include "gQC.h"
#include "date.h"
#include "math.h"

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

   extern void new_determine_sigma(/*in*/  double data[SIGMA_PERIOD],
                               /*in*/  int   n,     /* MINIMUM_NUM_VALS=<n=< SIGMA_PERIOD */
                               /*out*/ double *ave,  /* DEBUG */
                               /*out*/ double *svar,
                               /*out*/ double *adev, /* DEBUG */
                               /*out*/ double *sdev);/* DEBUG */

#else /*!__STDC__*/
   int main ();

   void new_determine_sigma();
#endif /*__STDC__*/

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
 
   *adev =0.0;
   *sdev =0.0;
 
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
#if 1
         printf ("NEW: j, data[j]:  %d %f\n",
                 j, data[j]);
#endif
#if 1
         printf ("NEW: *ave, s, *adev, p, *svar: %lf %lf %lf %lf %lf\n",
                 *ave, s, *adev, p, *svar);
         printf ("NEW: j, data[j], *ave, s, *svar:  %d %lf %lf %lf %lf\n", 
               j, data[j], *ave, s, *svar);
#endif
         }
      } /* for */
         
   *svar /= (count-1);
 
#if 1
   *adev /= count;
   *sdev=sqrt(*svar);
   printf ("NEW: ave, svar: %lf %lf\n", *ave, *svar);
   printf ("NEW: adev, sdev: %lf %lf\n", *adev, *sdev);
#endif
   } /* new_determine_sigma() */


/*---------------------------------------------------------
 * main() -  Controls statistical computations.
 *
 * 09 Dec 94 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   int      i,ii,j,jj,k,kk = 0;
   int      fix_i = 0;
   int      fix_j = 0; 
   double           ave = 0.0;
   static   double  adev = 0.0;
   static   double  sdev = 0.0;
   double           svar = 0.0;

   int      num_pts = 30;
   int      num_in_1std = 0;
   int      num_in_2std = 0;
   int      num_in_3std = 0;

   double   hi_end_limits[3];
   double   low_end_limits[3];


   /*
    * stn_data    - data from which sigmas are computed.
    */
   double stn_data[5][7][200];
   double norm_stn_data[5][7][200];


#if 0
printf ("Temps for AWOSQ20 SUE at time 02:20 for days 950401 through 950430.\n");

fix_i=13;
fix_j=4;
stn_data[13][4][0]= 4.44;
stn_data[13][4][1]= 2.22;
stn_data[13][4][2]= 5.56;
stn_data[13][4][3]= 17.22;
stn_data[13][4][4]= 0.00;
stn_data[13][4][5]= 2.22;
stn_data[13][4][6]= 13.33;
stn_data[13][4][7]= 12.22;
stn_data[13][4][8]= 14.44;
stn_data[13][4][9]= 5.00;
stn_data[13][4][10]=8.33;
stn_data[13][4][11]=20.00;
stn_data[13][4][12]=11.67;
stn_data[13][4][13]=7.78;
stn_data[13][4][14]=4.44;
stn_data[13][4][15]=10.00;
stn_data[13][4][16]=13.33;
stn_data[13][4][17]=5.00;
stn_data[13][4][18]=5.56;
stn_data[13][4][19]=2.78;
stn_data[13][4][20]=3.89;
stn_data[13][4][21]=3.89;
stn_data[13][4][22]=3.33;
stn_data[13][4][23]=2.78;
stn_data[13][4][24]=1.67;
stn_data[13][4][25]=5.56;
stn_data[13][4][26]=3.89;
stn_data[13][4][27]=3.89;
stn_data[13][4][28]=7.22;
stn_data[13][4][29]=6.11;
#endif

#if 0
printf ("Dew Pt Temps for AWOSQ20 SUE at time 02:20 for days 950401 through 950430.\n");
 
fix_i=13;
fix_j=5;
stn_data[13][5][0]= -6.11;
stn_data[13][5][1]= -5.56;
stn_data[13][5][2]= -3.98;
stn_data[13][5][3]= 0.56;
stn_data[13][5][4]= -11.11;
stn_data[13][5][5]= -12.78;
stn_data[13][5][6]= 7.78;
stn_data[13][5][7]= -2.22;
stn_data[13][5][8]= 9.44;
stn_data[13][5][9]= 0.00;
stn_data[13][5][10]=3.89;
stn_data[13][5][11]=7.78;
stn_data[13][5][12]=3.33;
stn_data[13][5][13]=-1.67;
stn_data[13][5][14]=-3.89;
stn_data[13][5][15]=-2.78;
stn_data[13][5][16]=1.11;
stn_data[13][5][17]=-2.78;
stn_data[13][5][18]=4.44;
stn_data[13][5][19]=-1.67;
stn_data[13][5][20]=-0.56;
stn_data[13][5][21]=0.56;
stn_data[13][5][22]=-2.22;
stn_data[13][5][23]=-2.78;
stn_data[13][5][24]=0.00;
stn_data[13][5][25]=-2.22;
stn_data[13][5][26]=1.11;
stn_data[13][5][27]=-1.67;
stn_data[13][5][28]=-2.78;
stn_data[13][5][29]=-0.56;
#endif

#if 1
printf ("Temps for OKMESO5 BURN at time 00:05 for days 950401 through 950430.\n");

fix_i=14;
fix_j=4;
stn_data[14][4][0]= 15.40;
stn_data[14][4][1]= 21.30;
stn_data[14][4][2]= 21.30;
stn_data[14][4][3]= 14.50;
stn_data[14][4][4]= 18.10;
stn_data[14][4][5]= 14.80;
stn_data[14][4][6]= 20.30;
stn_data[14][4][7]= 23.20;
stn_data[14][4][8]= 26.70;
stn_data[14][4][9]= 25.40;
stn_data[14][4][10]= 8.80;
stn_data[14][4][11]=13.20;
stn_data[14][4][12]=20.20;
stn_data[14][4][13]=22.40;
stn_data[14][4][14]=23.80;
stn_data[14][4][15]=24.50;
stn_data[14][4][16]=23.60;
stn_data[14][4][17]=23.90;
stn_data[14][4][18]=21.80;
stn_data[14][4][19]=14.10;
stn_data[14][4][20]=23.30;
stn_data[14][4][21]=24.80;
stn_data[14][4][22]=8.70;
stn_data[14][4][23]=13.90;
stn_data[14][4][24]=18.40;
stn_data[14][4][25]=20.20;
stn_data[14][4][26]=19.70;
stn_data[14][4][27]=17.00;
stn_data[14][4][28]=22.20;
stn_data[14][4][29]=23.70;
#endif

#if 0
printf ("Dew pts for OKMESO5 BURN at time 00:05 for days 950401 through 950430.\n");
 
fix_i=14;
fix_j=5;
stn_data[14][5][0]= 2.42;
stn_data[14][5][1]= 2.65;
stn_data[14][5][2]= 3.41;
stn_data[14][5][3]= 14.36;
stn_data[14][5][4]= 13.25;
stn_data[14][5][5]= 11.89;
stn_data[14][5][6]= 13.25;
stn_data[14][5][7]= 15.42;
stn_data[14][5][8]= 17.71;
stn_data[14][5][9]= 18.00;
stn_data[14][5][10]= 7.10;
stn_data[14][5][11]=-0.77;
stn_data[14][5][12]=3.51;
stn_data[14][5][13]=6.77;
stn_data[14][5][14]=9.40;
stn_data[14][5][15]=18.92;
stn_data[14][5][16]=20.93;
stn_data[14][5][17]=21.11;
stn_data[14][5][18]=4.74;
stn_data[14][5][19]=13.76;
stn_data[14][5][20]=7.28;
stn_data[14][5][21]=11.00;
stn_data[14][5][22]=7.94;
stn_data[14][5][23]=5.68;
stn_data[14][5][24]=3.34;
stn_data[14][5][25]=7.96;
stn_data[14][5][26]=14.69;
stn_data[14][5][27]=4.70;
stn_data[14][5][28]=17.54;
stn_data[14][5][29]=17.70;
#endif

#if 0
stn_data[13][6][0]= ;
stn_data[13][6][1]= ;
stn_data[13][6][2]= ;
stn_data[13][6][3]= ;
stn_data[13][6][4]= ;
stn_data[13][6][5]= ;
stn_data[13][6][6]= ;
stn_data[13][6][7]= ;
stn_data[13][6][8]= ;
stn_data[13][6][9]= ;
stn_data[13][6][10]= ;
stn_data[13][6][11]=;
stn_data[13][6][12]=;
stn_data[13][6][13]=;
stn_data[13][6][14]=;
stn_data[13][6][15]=;
stn_data[13][6][16]=;
stn_data[13][6][17]=;
stn_data[13][6][18]=;
stn_data[13][6][19]=;
stn_data[13][6][20]=;
stn_data[13][6][21]=;
stn_data[13][6][22]=;
stn_data[13][6][23]=;
stn_data[13][6][24]=;
stn_data[13][6][25]=;
stn_data[13][6][26]=;
stn_data[13][6][27]=;
stn_data[13][6][28]=;
stn_data[13][6][29]=;
#endif 

   /*------------------------------------------- 
    * Determine average, std, variance, etc. 
    *-------------------------------------------*/ 
   new_determine_sigma(stn_data[fix_i][fix_j], num_pts, &ave, &svar, &adev, &sdev);

   printf ("Stats: stn (%d) parm (%d), ave, svar, sdev: %12.5lf %12.5lf %12.5lf\n\n",
           fix_i, fix_j, ave, svar, sdev);

   /*-------------------------------------------
    * Determine limit ends for normality tests.
    *-------------------------------------------*/
   hi_end_limits[0]  = ave + svar;
   low_end_limits[0] = ave - svar;
   hi_end_limits[1]  = ave + 2.0*svar; 
   low_end_limits[1] = ave - 2.0*svar; 
   hi_end_limits[2]  = ave + 3.0*svar; 
   low_end_limits[2] = ave - 3.0*svar; 

   printf ("Standardized ((X-u)/std) values:\n");

   for (j=0;j<=29;j++)
      {  
      norm_stn_data[fix_i][fix_j][j] = ((stn_data[fix_i][fix_j][j]-ave)/sdev);
#if 1
      printf ("NEW: j, norm_stn_data[fix_i][fix_j][j]: %d %lf\n",
                j, norm_stn_data[fix_i][fix_j][j]);
#endif 
      if (stn_data[fix_i][fix_j][j]<hi_end_limits[0] && 
          stn_data[fix_i][fix_j][j]>low_end_limits[0] ) num_in_1std++;

      if (stn_data[fix_i][fix_j][j]<hi_end_limits[1] && 
          stn_data[fix_i][fix_j][j]>low_end_limits[1] ) num_in_2std++;

      if (stn_data[fix_i][fix_j][j]<hi_end_limits[2] && 
          stn_data[fix_i][fix_j][j]>low_end_limits[2] ) num_in_3std++;
      } /* for */


   num_in_2std = num_in_2std + num_in_1std;
   num_in_3std = num_in_3std + num_in_2std;
 
   printf ("num_in_1std: %d\nnum_in_2std: %d\nnum_in_3std: %d\n",
           num_in_1std, num_in_2std, num_in_3std);

   printf ("percent pts in 1std: %5.4f\npercent pts in 2std: %5.4f\npercent pts in 3std: %5.4f\n",
           1.0-(num_in_1std/num_pts), 1.0-(num_in_2std/num_pts), 1.0-(num_in_3std/num_pts));


   /* Verify that data follows normal distribution */

   if ((num_in_1std-.68*num_pts) >= 1.41*sqrt(num_pts) &&
       (num_in_1std-.95*num_pts) >= .654*sqrt(num_pts))
      printf ("Data fails 1st check of normality\n");
   else
      printf ("Data passed 1st check of normality\n");

   if ((num_in_1std-.997*num_pts) >= .164*sqrt(num_pts))
      printf ("Data fails 2rd check of normality\n"); 
   else 
      printf ("Data passed 2rd check of normality\n");

 
   }  /* test_gQC() */
