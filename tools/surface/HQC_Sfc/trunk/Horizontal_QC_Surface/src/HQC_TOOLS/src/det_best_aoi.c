/*--------------------------------------------------------
 * det_best_aoi.c - Helps user determine "best" size for
 *   Area Of Interest (AOI) parameter used in the
 *   Horizontal Quality Control (HQC) process.
 *
 *   Usage: det_best_aoi <stnlatlon_input_file>
 *
 * WARNINGS:
 *   The information produced by this program  (such as
 *   the minimum number of influential stns per different
 *   AOI) is theoretical, only. That is, it is possible
 *   that any particular time, that all or part of
 *   the influential stns are missing. This s/w considers
 *   only the case where all stns are present at all
 *   times.
 *   
 * Assumptions:
 *   - Input file has certain format.
 *   - Distance between stn1 and stn2 is equal to the
 *     distance from stn2 to stn1.
 *
 * Input:
 *    User must indicate name of input file contains station
 *    ID, lat, lon when executing this program.
 *    User may also modify the aoi_limits array to contain
 *    desired radius to check. If the aoi_limits array is
 *    modified, ensure that the NUM_AOI_LIMITS matches
 *    exact number of elements in the aoi_limits array.
 *
 * Output:
 *    Info to help user determine "best" AOI for set of
 *    input stations. As a rule of thumb, consider "best"
 *    AOI to be that radius where the stn with the 
 *    minimum number of influential stns is at least 3.
 *
 *    det_aoi.out - main output file.
 *
 *    See output file named "det_aoi.out". This file
 *    contains general statistics and information about
 *    set of input stations.
 *
 *    Following out files are handy for plotting.
 *
 *    max_aoi.out - each line of this file contains
 *           an AOI km radius number followed by 
 *           the maximum count of stns found within
 *           this radius. There is at least one stn
 *           in the input dataset that has this max
 *           number of stations within the indicated AOI.
 *
 *    min_aoi.out - each line of this file contains 
 *           an AOI km radius number followed by  
 *           the minimum count of stns found within
 *           this radius. 
 *   
 *    ave_aoi.out - each line of this file contains  
 *           an AOI km radius number followed by  
 *           the average number of stns found within
 *           this radius. 
 *
 *    All output AOI values are in kilometers (km).
 *
 * 13 Oct 95 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "local.h"
#include "dist.h"
#include "process_qcfrec.h"
#include "qcfrec.h"


/*
 * Number of AOI limits in aoi_limits[] array.
 */
#define NUM_AOI_LIMITS  23


/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * Don't turn DEBUG on unless you are working with a
 * very small set of data.
 *-------------------------------------------------------*/
#define  DEBUG  0

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() - controls processing flow.
 *
 * 13 Oct 95 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char         input_file_name[NAMELEN_MAX] = "\0";
   char         output_file_name[NAMELEN_MAX] = "det_aoi.out\0";

   char         max_output_file_name[NAMELEN_MAX] = "max_aoi.out\0";
   char         min_output_file_name[NAMELEN_MAX] = "min_aoi.out\0";
   char         ave_output_file_name[NAMELEN_MAX] = "ave_aoi.out\0";

   FILE         *input_stream;
   FILE         *output_stream;

   FILE         *max_output_stream;
   FILE         *min_output_stream;
   FILE         *ave_output_stream;

   char         stnlatlon[15]= "\0";
   long int     ii,xx,yy = 0;
   int          jj = 0;

   long         stn_no      = -1;
   long         numstns     = 0;
   long         below_min   = 0;
   long         index       = 0;
   float        min_weight; /* input value - determines area of influence around each stn */

   long int     current_stnno = 0;
   float        current_lat = 0.0;
   float        current_lon = 0.0;

   double       x,y = 0.0;

   /*------------------------------------------------------
    * dist - distance matrix. Stns are aligned 1 to n along
    *        each matrix side (row/coln). If we assume that
    *        the dist between stn1 and stn2 equal the dist
    *        between stn2 to stn1, then only really need to
    *        compute and save one set of distances. So
    *        this matrix is symmetric with zeroes on the 
    *        diagonal.
    *-----------------------------------------------------*/
   float        dist[MAXNUMSTNS][MAXNUMSTNS];
   STRING27     stn_list[MAXNUMSTNS]; /* stns in same order as stored in dist[][] */

   long int     in_AOI_ct[MAXNUMSTNS][NUM_AOI_LIMITS];
   float        A [MAXNUMSTNS][4];    /* lat, lon, dist, weight in same stn order */

   float        aoi_limits[NUM_AOI_LIMITS] = {  50.0, 100.0, 150.0, 200.0, 250.0, 300.0, 350.0,
                                               400.0, 450.0, 500.0, 550.0, 600.0, 650.0, 700.0,
                                               750.0, 800.0, 850.0, 900.0, 950.0, 1000.0, 1200.0,
                                               1500.0, 2000.0};

   long int     max_stns_in_aoi = 0;
   long int     min_stns_in_aoi = 9999;
   float        ave_stns_in_aoi = 0.0;

   long int     no_stns_in_AOI = 0;
   long int     one_stn_in_AOI = 0;
   long int     two_stns_in_AOI = 0;
   long int     three_stns_in_AOI = 0;
   long int     four_stns_in_AOI = 0;
   long int     five_stns_in_AOI = 0;
   long int     other_stns_nums_in_AOI = 0;


   /*--------------------------------------------
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 2)
      {  
      printf ("Usage: det_best_aoi <stnlatlon_input_file>\n");
      exit(1);
      }  

   strcpy (input_file_name, argv[1]);

   printf ("\n---- determine best AOI processing ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("Station ID/lat/lon file name (e.g., stnlatlon.inp): %-s\n", input_file_name);
   
   open_file (input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);
   open_file (output_file_name, "w", FILE_NOT_COMPRESSED, &output_stream);

   open_file (max_output_file_name, "w", FILE_NOT_COMPRESSED, &max_output_stream);
   open_file (min_output_file_name, "w", FILE_NOT_COMPRESSED, &min_output_stream);
   open_file (ave_output_file_name, "w", FILE_NOT_COMPRESSED, &ave_output_stream);

   fprintf (max_output_stream, "AOI     Max # stns in AOI\n");
   fprintf (min_output_stream, "AOI     Min # stns in AOI\n");
   fprintf (ave_output_stream, "AOI       Ave # stns in AOI\n");

   for (xx=0;xx<MAXNUMSTNS;xx++)
      { 
      stn_list[xx][15] = '\0';
      A[xx][0] = -999.99; /*latitude*/
      A[xx][1] = -999.99; /*longitude*/
      A[xx][2] = 0.0;     /*weight - generally a function of distance.*/
      A[xx][3] = 0.0;     /*distance*/

      for (yy=0;yy<MAXNUMSTNS;yy++)
         {
         dist[xx][yy] = 0.0;
         in_AOI_ct[xx][yy] = 0;
         }
      } 

   printf ("Reading stn ID's lats lons \n");

   /*-----------------------------
    * Read data from input file.
    *----------------------------*/ 
   numstns = -1;
   while (!feof(input_stream))
     {
     numstns++;

     fgets(stn_list[numstns], 14, input_stream);
     if (feof(input_stream)) break;

     /* read lat/lon */

     fscanf (input_stream, "%f %f", &A[numstns][0], &A[numstns][1]);  STRIPLINE(input_stream);
     if (feof(input_stream)) break;

#if DEBUG
     printf ("numstns, Stn ID, lat, lon: %d, xxx%-sxxx, %10.5f %11.5f\n",
              numstns, stn_list[numstns], A[numstns][0], A[numstns][1]);
#endif
     }

#if DEBUG
   printf ("Total number stations read: %d\n", numstns);
#endif

   /*-----------------------
    * Compute distances.
    *----------------------*/
   for (current_stnno=0; current_stnno<numstns; current_stnno++)
      {
      current_lat = A[current_stnno][0];
      current_lon = A[current_stnno][1];
#if DEBUG
      printf ("Current stn: %ld\n", current_stnno);
#endif 
      for (ii=0;ii<numstns;ii++)
         {
         if (ii > current_stnno) /* only do computations for half symmetric matrix */
            {
            ll2xydrv( current_lat, 
                      current_lon, 
                      &x, &y, 
                      A[ii][0], A[ii][1]);

            dist[current_stnno][ii] = sqrt(x*x+y*y);             /* distance */
            dist[ii][current_stnno] = dist[current_stnno][ii];
#if DEBUG 
            printf ("x, y, dist[%ld][%ld]: %lf %lf %f\n", current_stnno, ii, x, y, dist[current_stnno][ii]); 
#endif
            } /* if (ii > current_stnno) */

         } /* for ii */

      } /*for current_stnno */


#if DEBUG
   /*-----------------------------------------
    * Write out distances, etc to output file.
    *-----------------------------------------*/
   for (yy=0;yy<numstns;yy++)
      fprintf (output_stream, "%15s ", stn_list[yy]); 
   fprintf (output_stream, "\n"); 

   for (xx=0;xx<numstns;xx++)
      {  
      for (yy=0;yy<numstns;yy++)
         fprintf (output_stream, "%15.3f", dist[xx][yy]);
      fprintf (output_stream, "\n");
      }  
  fprintf (output_stream, "\n\n");
#endif

   /*------------------------------------------
    * Check every element in the matrix and 
    * determine (from the given AOI_LIMIT)
    * how many stns are within that AOI_LIMIT.
    *-----------------------------------------*/
   for (current_stnno=0; current_stnno<numstns; current_stnno++)
      {  
#if DEBUG
      printf ("Current stn: %ld\n", current_stnno);
#endif
      for (ii=0;ii<numstns;ii++)
         {
         for (jj=0;jj< NUM_AOI_LIMITS;jj++)
            {
            if (ii != current_stnno && dist[current_stnno][ii] <aoi_limits[jj]) 
               {
               in_AOI_ct[current_stnno][jj]++;
               } /* if (ii > current_stnno) */
            }
         } /* for ii */

      } /*for current_stnno */

#if DEBUG
   fprintf (output_stream, "\t\t\tAOI CTs per AOI_LIMIT for each stn\n\n");
   fprintf (output_stream, "                ");

   for (jj=0;jj<NUM_AOI_LIMITS;jj++)
      fprintf (output_stream, "%5.1f ", aoi_limits[jj]);
   fprintf (output_stream, "\n");

   for (xx=0;xx<numstns;xx++)
      {  
      fprintf (output_stream, "%-15s ", stn_list[xx]);
      for (yy=0;yy<NUM_AOI_LIMITS;yy++)
         fprintf (output_stream, "%-5ld ", in_AOI_ct[xx][yy]);
      fprintf (output_stream, "\n");
      }  
  fprintf (output_stream, "\n\n");

#endif

   /*--------------------------------------------
    * Now determine the Max, Min, ave, mean, etc.
    * number of cts for this project....process
    * all stns in the array. Want to know these
    * numbers for a complete project. Also good
    * to know how many stns had the minimum and
    * how many stns at each of the other cts.
    *--------------------------------------------*/
   for (jj=0;jj< NUM_AOI_LIMITS;jj++) 
      { 
      max_stns_in_aoi = 0;
      min_stns_in_aoi = 9999;
      ave_stns_in_aoi = 0.0;

      no_stns_in_AOI = 0;
      one_stn_in_AOI = 0;
      two_stns_in_AOI = 0;
      three_stns_in_AOI = 0;
      four_stns_in_AOI = 0;
      five_stns_in_AOI = 0;
      other_stns_nums_in_AOI = 0;

      for (current_stnno=0; current_stnno<numstns; current_stnno++)
         {  
         if (in_AOI_ct[current_stnno][jj] > max_stns_in_aoi) 
            max_stns_in_aoi = in_AOI_ct[current_stnno][jj];

         if (in_AOI_ct[current_stnno][jj] < min_stns_in_aoi) 
            min_stns_in_aoi = in_AOI_ct[current_stnno][jj];

         ave_stns_in_aoi = ave_stns_in_aoi + 
             (float)(in_AOI_ct[current_stnno][jj])/(float)numstns;

         /*---------------------------------------
          * The following switch counts only exact
          * matches.
          *---------------------------------------*/
         switch (in_AOI_ct[current_stnno][jj])  {
            case 0:
               no_stns_in_AOI++;
               break;
            case 1: 
               one_stn_in_AOI++;
               break; 
            case 2: 
               two_stns_in_AOI++;
               break; 
            case 3: 
               three_stns_in_AOI++;
               break; 
            case 4: 
               four_stns_in_AOI++;
               break; 
            case 5: 
               five_stns_in_AOI++;
               break; 
            default:
               other_stns_nums_in_AOI++;
               break;

            }  /*switch*/

         } /*for current_stnno */

      /*-----------------------------------------------
       * Print final output for each Area Of Interest.
       *----------------------------------------------*/
      fprintf (max_output_stream, "%7.2f %ld\n", aoi_limits[jj], max_stns_in_aoi);
      fprintf (min_output_stream, "%7.2f %ld\n", aoi_limits[jj], min_stns_in_aoi);
      fprintf (ave_output_stream, "%7.2f %7.2f\n", aoi_limits[jj], ave_stns_in_aoi);

      fprintf (output_stream, "AOI radius: %7.2f\n", aoi_limits[jj]);
      fprintf (output_stream, "   MIN number of stns in AOI: %5ld\n", min_stns_in_aoi);
      fprintf (output_stream, "   AVERAGE number of stns in AOI: %7.2f\n", ave_stns_in_aoi);
      fprintf (output_stream, "   MAX number of stns in AOI: %5ld\n\n", max_stns_in_aoi);

      fprintf (output_stream, "   Number of stns with 0 stns within %7.2f: %ld\n",
               aoi_limits[jj],no_stns_in_AOI);
      fprintf (output_stream, "   Number of stns with 1 stn  within %7.2f: %ld\n",
               aoi_limits[jj], one_stn_in_AOI);
      fprintf (output_stream, "   Number of stns with 2 stns within %7.2f: %ld\n",
               aoi_limits[jj], two_stns_in_AOI);
      fprintf (output_stream, "   Number of stns with 3 stns within %7.2f: %ld\n",
               aoi_limits[jj], three_stns_in_AOI);
      fprintf (output_stream, "   Number of stns with 4 stns within %7.2f: %ld\n",
               aoi_limits[jj], four_stns_in_AOI);
      fprintf (output_stream, "   Number of stns with 5 stns within %7.2f: %ld\n",
               aoi_limits[jj], five_stns_in_AOI);
      fprintf (output_stream, "   Number of stns with >5 stns within %7.2f: %ld\n",
               aoi_limits[jj], other_stns_nums_in_AOI);

      below_min = 0;
      if (aoi_limits[jj] < 400.0)
         {
         for (current_stnno=0; current_stnno<numstns; current_stnno++)
            {
            if (in_AOI_ct[current_stnno][jj] <= min_stns_in_aoi)
               {
               fprintf (output_stream, "Following stn has only %5ld stns within %7.2f: %-s\n",
                    in_AOI_ct[current_stnno][jj], aoi_limits[jj], stn_list[current_stnno]);
               below_min++;
               }
            }

         fprintf (output_stream, "Number of stns equal to min: %d\n", below_min);
         fprintf (output_stream, "\n");

         } /* just more info */

      } /*for jj  - each aoi */

   close_file (&input_stream, FILE_NOT_COMPRESSED);
   close_file (&output_stream, FILE_NOT_COMPRESSED);

   printf ("Processing completed on %-s %-s\n", __DATE__, __TIME__);

   }  /* main() */
