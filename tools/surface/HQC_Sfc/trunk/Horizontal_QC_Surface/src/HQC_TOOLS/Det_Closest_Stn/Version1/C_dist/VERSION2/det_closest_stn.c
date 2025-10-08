/*--------------------------------------------------------
 * det_closest_stn.c - Helps user determine Closest Stn.
 *
 *   Usage: det_closest_stn <stnlatlon_input_file> <AOI_limit>
 *
 * Assumptions:
 *   - Input file has certain format (i.e. stationCD.out).
 *   - Distance between stn1 and stn2 is equal to the
 *     distance from stn2 to stn1.
 *
 * Input:
 *    User must indicate name of input file contains station
 *    ID, lat, lon when executing this program. This can be
 *    a file of the stationCD.out form. User must provide the
 *    AOI_limit which is the distance in KM. Any station 
 *    within AOI_limit distance of another station will be
 *    listed in the output file Closest_stn_LIST.out for that
 *    other station.
 *
 * Output:
 *    Info to help user determine Closest Stn for set of
 *    input stations. 
 *
 *    Closest_stn_MATRIX.out - Symetric Matrix containing
 *       the distances between every station in the input 
 *       station list. Note the zeroes on the diagonal.
 *
 *    Closest_stn_LIST.out - Lists every station and all
 *       associated stations within the area of interest
 *       limit (AOI_limit).
 *
 * NOTE: All output distance values are in kilometers (km).
 *
 * Possible Improvments: Add in network.
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
/*#include "qcfrec.h" */



/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * Don't turn DEBUG on unless you are working with a
 * very small set of data.
 *-------------------------------------------------------*/
#define  DEBUG 0
#define  DEBUG2 0
/* Was 2000 */
#define  MAXDISTSTNS 7000
/* Process matrix in MAXPROCESS hunks. was 100*/
#define  MAXPROCESS 50

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
   char         output_file_MATRIX[NAMELEN_MAX] = "Closest_stns_MATRIX.out\0";
   char         output_file_LIST[NAMELEN_MAX] = "Closest_stns_LIST.out\0";

   FILE         *input_stream;
   FILE         *output_stream_MATRIX;
   FILE         *output_stream_LIST;

   long int     ii,xx,yy = 0;
   int          jj = 0;

   int          platformID = 0;

   long         limit       = 0;
   long         stn_no      = -1;
   long         stn_count   = 0;
   long         whole_matrix_stnno = 0;
   long         numstns     = 0;
   long         total_number_stations_processed = 0;

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
    *  Original definition:
    *  float        dist[MAXDISTSTNS][MAXDISTSTNS];
    *
    *  Had to limit the size of the matrix due to computer
    *  memory limitations. This way we compute strips of
    *  of the matrix and use less memory.
    *-----------------------------------------------------*/
   float        dist[MAXPROCESS][MAXDISTSTNS];
   STRING27     stn_list[MAXDISTSTNS]; /* stns in same order as stored in dist[][] */

   float        A [MAXDISTSTNS][4];    /* lat, lon, dist, weight in same stn order */
   float        AOI_limit;             /* Area of Interest Limit in KM */
   int          num_found;             /* Number of stations found with AOI_limit KM of current stn.*/


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
   open_file (output_file_MATRIX, "w", FILE_NOT_COMPRESSED, &output_stream_MATRIX);
   open_file (output_file_LIST, "w", FILE_NOT_COMPRESSED, &output_stream_LIST);


   /* Initialize the station information and distance arrays. */
   for (xx=0;xx<MAXDISTSTNS;xx++)
      { 
      stn_list[xx][15] = '\0'; /*was 15*/
      A[xx][0] = -999.99; /*latitude*/
      A[xx][1] = -999.99; /*longitude*/
      A[xx][2] = 0.0;     /*weight - generally a function of distance.*/
      A[xx][3] = 0.0;     /*distance*/

      for (yy=0;yy<MAXPROCESS;yy++)
         {
         dist[yy][xx] = 0.0;
         }
      } 

   printf ("Reading stn ID's lats lons \n");

   /*---------------------------------
    * Read station information 
    * from input file (stationCD.out).
    *---------------------------------*/ 
   numstns = -1;
   while (!feof(input_stream))
     {
     numstns++;

     fgets(stn_list[numstns], 14, input_stream); /*was 14*/
     if (feof(input_stream)) break;

     /* read lat/lon */
     /*was: fscanf (input_stream, "%f %f", &A[numstns][0], &A[numstns][1]);  STRIPLINE(input_stream); */

     /* stationCD.out form */
     fscanf (input_stream, "%d %f %f", &platformID, &A[numstns][0], &A[numstns][1]);  STRIPLINE(input_stream); 
     if (feof(input_stream)) break;

#if DEBUG
     printf ("numstns, Stn ID, platformID, lat, lon: %d, xxx%-sxxx, %5d %10.5f %11.5f\n",
              numstns, stn_list[numstns], platformID, A[numstns][0], A[numstns][1]);
#endif
     }

   printf ("Total number stations read: %d\n", numstns);


   fprintf (output_stream_LIST, "All Stations within a %7.2f AOI for each stn.\n", AOI_limit);
   fprintf (output_stream_LIST, "----------------------------------------------------\n");


   /*-------------------------------------------------------
    * Compute distances in sets of MAXPROCESS at a time.
    *  numstns = actual number of stations read from input.
    *  MAXPROCESS = size of strip of distance matrix being
    *    processed at one time.
    *------------------------------------------------------*/
   for (stn_count=0; stn_count<numstns; stn_count+MAXPROCESS) /* increment by 100 */
      {
#if DEBUG
      printf ("Processing stns %ld  thru %ld\n", stn_count, stn_count+MAXPROCESS);
#endif

      if (total_number_stations_processed+MAXPROCESS < numstns)
          limit = MAXPROCESS;
      else
          limit = numstns-stn_count;

#if DEBUG
      printf ("******Inner loop limit = %ld; total_number_stations_processed = %ld\n", limit, total_number_stations_processed);
#endif

      for (current_stnno=0; current_stnno<limit; current_stnno++)
         {

         whole_matrix_stnno = stn_count+current_stnno;

         current_lat = A[whole_matrix_stnno][0];
         current_lon = A[whole_matrix_stnno][1];

         if (current_lat >-999.00 && current_lon > -999.00)
            { 
#if DEBUG
            printf ("Calc Distances for stn_list[%ld] (lat, lon): %-s  at lat/lon: %10.5f %11.5f\n", 
                     whole_matrix_stnno, stn_list[whole_matrix_stnno], 
                     current_lat, current_lon);
#endif    
            for (ii=0;ii<numstns;ii++)
               {
               ll2xydrv( current_lat, 
                         current_lon, 
                         &x, &y, 
                         A[ii][0], A[ii][1]);

               dist[current_stnno][ii] = sqrt(x*x+y*y);             /* distance */

#if DEBUG2
               printf ("stn_ID, stn_no, lat, lon; x, y, dist[%ld][%ld]: %-s %ld %10.5f %11.5f %lf %lf %f\n",  
                       current_stnno, ii, stn_list[ii], ii, A[ii][0], A[ii][1], x, y, dist[current_stnno][ii]); 
#endif
               } /* for ii */
            } /* Not missing lat/lon */

         } /*for current_stnno */
#if DEBUG
      printf ("******Inner loop calc'd distances for next %ld stations.\n", current_stnno);
#endif

      /*-----------------------------------------
       * Write out distances, etc to output file
       * containing the large Distance matrix.
       * Write out the top header line only once.
       *-----------------------------------------*/
      if (stn_count==0)
         {
#if DEBUG
         printf ("Write Distances to Output Files.\n");
#endif
         fprintf (output_stream_MATRIX, "                     "); 
         for (yy=0;yy<numstns;yy++)
            fprintf (output_stream_MATRIX, "%15s ", stn_list[yy]);
         fprintf (output_stream_MATRIX, "\n"); 

         } /* Write header only once.*/

     /* ---------------------------------------------------------------
      * Write out the next MAXPROCESS (or less) station/distance lines.
      * ---------------------------------------------------------------*/
      for (xx=0;xx<limit;xx++)
         {  
         total_number_stations_processed++; /* count stations processed and written to output. */

         fprintf (output_stream_MATRIX, "%15s ", stn_list[stn_count+xx]);
          
         for (yy=0;yy<numstns;yy++)
            fprintf (output_stream_MATRIX, "%15.3f ", dist[xx][yy]);
         fprintf (output_stream_MATRIX, "\n");
         }  

      /*---------------------------------------------
       * Check every element in the matrix and 
       * determine (from the given AOI_LIMIT)
       * how many stns are within that AOI_LIMIT.
       * Print ONLY stns that fall within the limit.
       *
       * For loop limit is MAXPROCESS or less.
       *--------------------------------------------*/
      for (current_stnno=0; current_stnno<limit; current_stnno++)
         {  
         num_found = 0;
         whole_matrix_stnno = stn_count+current_stnno;

#if DEBUG
         printf ("Check Matrix and Determine if in AOI_LIMIT for Current stn: (%ld)  %-s at (%10.5f %11.5f)\n", 
                  current_stnno, stn_list[whole_matrix_stnno], A[whole_matrix_stnno][0], A[whole_matrix_stnno][1]);
#endif
         fprintf (output_stream_LIST, "\n%15s (%10.5f %11.5f):: ", stn_list[whole_matrix_stnno], 
                  A[whole_matrix_stnno][0], A[whole_matrix_stnno][1]); /* output stnID, lat, lon */

         for (ii=0;ii<numstns;ii++)
            {
            /* -------------------------------------------------
             * The station will have zero distance from itself,
             * so don't output it's own name.
             * -------------------------------------------------*/
            if (dist[current_stnno][ii] < AOI_limit && (whole_matrix_stnno != ii))
               {
               num_found++;

               /* ----------------------------
                * Within limit, print to output 
                * ----------------------------*/
               fprintf (output_stream_LIST, " %15s (%7.2f), ", 
                        stn_list[ii], dist[current_stnno][ii]);

#if DEBUG
               printf ("%-s (%10.5f %11.5f) is within AOI_LIMIT Print to Output. Dist = %7.2f\n", 
                       stn_list[ii], A[ii][0], A[ii][1], dist[current_stnno][ii]);
#endif
               } /* if (ii > current_stnno) */

            } /* for ii */

         fprintf (output_stream_LIST, "[Found %5d stations within %7.2f KM.]\n", num_found, AOI_limit);
         } /*for current_stnno */

      stn_count = stn_count+MAXPROCESS;

#if DEBUG
      printf ("Done Writing. Compute Next Batch of Distances. Incremented stn_count to: %ld\n", stn_count);
#endif
      } /* while stn_count */

   close_file (&input_stream, FILE_NOT_COMPRESSED);
   close_file (&output_stream_MATRIX, FILE_NOT_COMPRESSED);
   close_file (&output_stream_LIST, FILE_NOT_COMPRESSED);

   printf ("Processing completed on %-s %-s\n", __DATE__, __TIME__);

   }  /* main() */
