/*--------------------------------------------------------
 * det_closest_stn.c - Helps user determine Closest Stn.
 *
 *   Usage: det_closest_stn <stnlatlon_input_file>
 *
 * Assumptions:
 *   - Input file has certain format.
 *   - Distance between stn1 and stn2 is equal to the
 *     distance from stn2 to stn1.
 *
 * Input:
 *    User must indicate name of input file contains station
 *    ID, lat, lon when executing this program.
 *
 * Output:
 *    Info to help user determine Closest Stn for set of
 *    input stations. 
 *
 *    det_closest_stn.out - main output file.
 *
 *    See output file named "det_closest_stn.out". This file
 *    contains distance information about set of input stations.
 *
 *    All output distance values are in kilometers (km).
 *
 * 27 April 2004 lec
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



/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * Don't turn DEBUG on unless you are working with a
 * very small set of data.
 *-------------------------------------------------------*/
#define  DEBUG  1
#define  MAXDISTSTNS 1440

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() - controls processing flow.
 *
 * 27 April 2004 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char         input_file_name[NAMELEN_MAX] = "\0";
   char         output_file_name[NAMELEN_MAX] = "det_closest_stn.out\0";
   char         output_file_closest[NAMELEN_MAX] = "Closest_stns.out\0";

   FILE         *input_stream;
   FILE         *output_stream;
   FILE         *output_stream_closest;

   long int     ii,xx,yy = 0;
   int          jj = 0;

   int          junk = 0;

   long         stn_no      = -1;
   long         numstns     = 0;
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
   float        dist[MAXDISTSTNS][MAXDISTSTNS];
   STRING27     stn_list[MAXDISTSTNS]; /* stns in same order as stored in dist[][] */

   float        A [MAXDISTSTNS][4];    /* lat, lon, dist, weight in same stn order */
   float        AOI_limit;            /* Area of Interest Limit in KM */
   int          num_found;            /* Number of stations found with AOI_limit KM of current stn.*/


   /*------------------------------------------------------------------------
    * Expect input file name and AOI Limit in Kilometers on the command line.
    *-----------------------------------------------------------------------*/
   if (argc != 3)
      {  
      printf ("Usage: det_closest_stn <stnlatlon_input_file> <AOI_Limit_in_KM>\n");
      exit(1);
      }  

   strcpy (input_file_name, argv[1]);
   AOI_limit = atof(argv[2]); /* Convert to float */

   printf("AOI_limit = %f (%s)\n", AOI_limit, argv[2]);

   printf ("\n---- Determine Closest Station Processing ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("Station ID/lat/lon file name (e.g., stnlatlon.inp): %-s\n", input_file_name);
   
   open_file (input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);
   open_file (output_file_name, "w", FILE_NOT_COMPRESSED, &output_stream);
   open_file (output_file_closest, "w", FILE_NOT_COMPRESSED, &output_stream_closest);

   for (xx=0;xx<MAXDISTSTNS;xx++)
      { 
      stn_list[xx][15] = '\0'; /*was 15*/
      A[xx][0] = -999.99; /*latitude*/
      A[xx][1] = -999.99; /*longitude*/
      A[xx][2] = 0.0;     /*weight - generally a function of distance.*/
      A[xx][3] = 0.0;     /*distance*/

      for (yy=0;yy<MAXDISTSTNS;yy++)
         {
         dist[xx][yy] = 0.0;
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

     fgets(stn_list[numstns], 14, input_stream); /*was 14*/
     if (feof(input_stream)) break;

     /* read lat/lon */

/*     fscanf (input_stream, "%f %f", &A[numstns][0], &A[numstns][1]);  STRIPLINE(input_stream); */
     fscanf (input_stream, "%d %f %f", &junk, &A[numstns][0], &A[numstns][1]);  STRIPLINE(input_stream); 
     if (feof(input_stream)) break;

#if DEBUG
     printf ("numstns, Stn ID, type, lat, lon: %d, xxx%-sxxx, %5d %10.5f %11.5f\n",
              numstns, stn_list[numstns], junk, A[numstns][0], A[numstns][1]);
#endif
     }

   printf ("Total number stations read: %d\n", numstns);

   /*-----------------------
    * Compute distances.
    *----------------------*/
   for (current_stnno=0; current_stnno<numstns; current_stnno++)
      {
      current_lat = A[current_stnno][0];
      current_lon = A[current_stnno][1];
#if DEBUG
      printf ("stn_list[], Current stn (lat, lon): %-s  %ld %10.5f %11.5f\n", 
               stn_list[current_stnno], current_stnno, current_lat, current_lon);
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
            printf ("stn_ID, stn_no, lat, lon; x, y, dist[%ld][%ld]: %-s %ld %10.5f %11.5f %lf %lf %f\n",  current_stnno, ii, stn_list[ii], ii, A[ii][0], A[ii][1], x, y, dist[current_stnno][ii]); 
#endif
            } /* if (ii > current_stnno) */

         } /* for ii */

      } /*for current_stnno */


   /*-----------------------------------------
    * Write out distances, etc to output file.
    *-----------------------------------------*/
   fprintf (output_stream, "                                   "); 

   for (yy=0;yy<numstns;yy++)
      fprintf (output_stream, "%15s ", stn_list[yy]);  /*was 15*/
   fprintf (output_stream, "\n"); 

   for (xx=0;xx<numstns;xx++)
      {  
      fprintf (output_stream, "%15s ", stn_list[xx]); /*was 15*/
       
      for (yy=0;yy<numstns;yy++)
         fprintf (output_stream, "%15.3f ", dist[xx][yy]); /*was 15.3*/
      fprintf (output_stream, "\n");
      }  
  fprintf (output_stream, "\n\n");


   /*------------------------------------------
    * Check every element in the matrix and 
    * determine (from the given AOI_LIMIT)
    * how many stns are within that AOI_LIMIT.
    * Print all stns that fall within the limit.
    *-----------------------------------------*/
   fprintf (output_stream_closest, "All Stations within a %7.2f AOI for each stn.\n", AOI_limit);
   fprintf (output_stream_closest, "----------------------------------------------------\n");

   for (current_stnno=0; current_stnno<numstns; current_stnno++)
      {  
#if DEBUG
      printf ("Current stn: %ld\n", current_stnno);
#endif
      num_found = 0;

      fprintf (output_stream_closest, "\n%15s (%10.5f %11.5f):: ", stn_list[current_stnno], A[current_stnno][0], A[current_stnno][1]); /* output stnID, lat, lon */

      for (ii=0;ii<numstns;ii++)
         {
         if (ii != current_stnno && dist[current_stnno][ii] <AOI_limit) 
            {
            num_found++;

            /* Within limit, print to output */
            fprintf (output_stream_closest, " %15s (%7.2f), ", 
                     stn_list[ii], dist[current_stnno][ii]);

            } /* if (ii > current_stnno) */

         } /* for ii */
      fprintf (output_stream_closest, "[Found %5d stations within %7.2f KM.]\n", num_found, AOI_limit);

      } /*for current_stnno */

   close_file (&input_stream, FILE_NOT_COMPRESSED);
   close_file (&output_stream, FILE_NOT_COMPRESSED);
   close_file (&output_stream_closest, FILE_NOT_COMPRESSED);

   printf ("Processing completed on %-s %-s\n", __DATE__, __TIME__);

   }  /* main() */
