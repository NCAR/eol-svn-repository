/*--------------------------------------------------------
 * VERSION 2.0
 * compute_var_cts.c -  Reads sigmasq files computed by
 *   compute_sigmasq() then counts and bins the data
 *   so that a histogram of the data can be produced. Any
 *   of the following parameter can be chosen:
 *    (0)         Station Pressure,
 *    (1)         Sea Level Pressure,
 *    (2)         Calculated Sea Level Pressure,
 *    (3)         Temperature,
 *    (4)         Dew Point Temperature,
 *    (5)         Wind speed,
 *    (6)         Wind Direction.
 *
 *   This can be used to determine the range of values
 *   that will include say 95% of all sigmasq values.
 *   In the past we ran comp_ct_percent.pl to do this summing.
 *   This range, in turn, can be used to compute the
 *   values put in Table 3-2 "Ranges of HQC flag Limit
 *   Values for 'project-name' 'composite-name'. Based
 *   upon hardcoded alpha values, this s/w will also 
 *   attempt to compute the values used to fill Table 3-2
 *   in the related surface composite document.
 *
 *   This s/w locates the peak value (i.e., the 
 *   bin with the max number of counts) in the limit
 *   bins. Starting at this location, the s/w adds in
 *   bin counts working it's way in both directions from
 *   the peak towards the first and last bins. Note that
 *   the user determine the bin slots (See variable "limits".)
 *
 *   At each pass, the bins to the right of the peak
 *   and the bin to the left of the peak are compared.
 *   The bin with the largest number of counts is always added.
 *   When a bin count is added, the s/w prepares for the
 *   next pass/comparison by moving to the next bin in that
 *   direction. This means that it is possible that the s/w will
 *   only add bin counts from one side of the peak. But generally,
 *   the s/w will alternate from side to side of the peak
 *   adding the highest value. This would be true of a "bell-shaped"
 *   curve. This method should also handle curves with 
 *   multiple peaks and other odd shapes. But the user should
 *   always verify the results. Note that each time a 
 *   bin's counts are dded to the total counts,
 *   the s/w checks the number of counts to determine if
 *   we have summed to 95% (or near to 95%) of the total counts.
 *   If we have hit near 95% or we've considered
 *   all the possible bins, then the s/w ends. 
 *
 *   The user should always verify the results for Table 3-2.
 *
 *   Usage: compute_var_cts
 *
 * INPUT : List of files to be processed in a file named :
 *         'file_list.txt'. This should be a list of files
 *         with the sigmasq file format.
 *
 *  This s/w assumes that the input *.sig files are compressed.
 * 
 *  Variance files must have file names of the form:
 *                   yyyyjjjhhmm.sig
 *
 *    where yyyy is year, jjj is julian date, hh is hour, mm is
 *    minute for which the contained sigmasq (variance) values
 *    are valid. And will be located in the directory specified
 *    as the output directory in the QC input control file. 
 *    The form of all output variance files is:
 *
 *          Definition                        (var) (type)
 *  -----------------------------------------------------------------
 *  Line 1: Number of stations at this time   (numstns) (int)
 *  Line 2->xx: Internal ID Network:Stn ID    (i, stn_list[i][27]) (int, string)
 *  Line xx+1: Comment line                   ("stn_no parm_no  sigma value")
 *  Line xx+2: InternalID  Parm# Variance     (i, j, sigma_sq[i][j]) (int, int, float)
 *
 * (Line xx+2 is repeated for each variance for parameter (0-6 (j)) for each stn.)
 *
 *  Example of variance file:
 *   1
 *     0 E02
 *   stn_no parm_no  sigma value
 *       0     0 33.714584
 *       0     1 33.271523
 *       0     2 67.128426
 *       0     3 29.454615
 *       0     4 38.043491
 *       0     5 3.950475
 *       0     6 13018.718750
 *  (end example)
 *
 * Output:
 *   Bin (histogram) counts for each parameter into file named
 *   final_var_ct.out.
 *
 * 08 Apr 95 lec
 *   Created.
 * 22 May 98 lec
 *   Added documentation. Fixed output header format.
 * 27 May 98 lec
 *   Updated and increased number of bins. Added s/w to 
 *   attempt to determine the range of sigmasq values
 *   that contains about 95% of the values.
 * 4-12 June 98 lec
 *   Updated s/w to try and better the method used to
 *   determine the range of bins that contains 95% of
 *   the values. New method should handle odd shaped curves.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "local.h"
#include "process_qcfrec.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * NOTE: Some #if statements have been turned on
 *       permanently by setting #if 1.
 *
 *-------------------------------------------------------*/
#define  DEBUG   0
#define  DEBUG1  0
#define  ORIG    0

/* Following was only 30 , then 40 bins. */
#define  MAX_NUM_BINS 110 

#define  LOW    0
#define  HIGH   1

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * add_in_low_value() - adds requested value to current
 *   total.
 *
 * 28 May 98 lec
 *   Created.
 *--------------------------------------------------------*/
void add_in_low_value( /*in/out*/ long int  *low_in,
                       /*in/out*/ int       *low_done_in,
                       /*in*/     long int  value_to_add,
                       /*in*/     long int  var_totals,
                       /*in/out*/ long int  *counts_in_range,
                       /*in/out*/ long int  *range_low_end)
   {
   float temp_total = 0.0;
   float percent = 0.0;

   long int low;
   long int low_done;
   long int range_low;

   low = *low_in;
   low_done = *low_done_in;

   range_low = *range_low_end;


#if DEBUG
   printf ("       Add low(top): low=%ld, low_done=%ld, valToAdd=%ld,\n        var_totals=%ld, ctsInRng=%ld, range[low]: %ld\n",
   *low_in, *low_done_in, value_to_add, var_totals, *counts_in_range, *range_low_end);
#endif

   /*--------------------------
    * Try and add in the value.
    *--------------------------*/
   if (low > -1 && low_done!=1 && (value_to_add > 0))
      {
      temp_total = *counts_in_range + value_to_add;
      percent = temp_total/var_totals;

#if DEBUG
      printf ("       (LOW)  (.95-(temp_total/var_totals) & fabs: %7.4f %7.4f\n",
              (.95 - (percent)), fabs(.95 - (percent)) );

      printf ("       (int)(100*(percent)) = %d\n", 
               (int)(100.0*(percent)) );
#endif

      /*---------------------------------------
       * Don't add it in if it pushes val > 95%
       * If we are under 95% always add it in.
       * Only let the value go slightly over
       * 95% say to 97%.
       *--------------------------------------*/
/*was:      if ( fabs(.95 - percent) >= 0.004  ||
           (int)(100.0*(percent))== 95 ) */

      if ( ((.95 - percent) >= 0.0) ||
           ( ((.95 - percent) < 0.0 ) &&
             ((.95 - percent) >=-0.02) ) )
         {
         *counts_in_range = *counts_in_range + value_to_add;
         range_low = low;

         low--;              /* move down one slot only if value added */

#if DEBUG
         printf ("       Add LOW value. low=%ld, low_done=%ld\n", low, low_done);
#endif
         }
      else
         {
#if DEBUG
         printf ("       Set low_done = 1.\n");
#endif
         low_done=1;
         }
      } 
   else
      {
#if DEBUG
      printf ("Don't add LOW value.\n");
#endif
      }

   *low_in = low;
   *low_done_in = low_done;
   *range_low_end = range_low;

#if DEBUG
   printf ("       Add low(end): low=%ld, low_done=%ld, valToAdd=%ld, \n        var_totals=%ld, ctsInRng=%ld, range[low]: %ld\n",
   *low_in, *low_done_in, value_to_add, var_totals, 
   *counts_in_range, *range_low_end);
#endif

   return;

   } /* add_in_low_value()*/

/*---------------------------------------------------------
 * add_in_high_value() - adds requested value to current
 *   total.
 *
 * 28 May 98 lec
 *   Created.
 *--------------------------------------------------------*/
void add_in_high_value( /*in/out*/ long int  *high_in,
                        /*in/out*/ int       *high_done_in,
                        /*in*/     long int  value_to_add,
                        /*in*/     long int  var_totals,
                        /*in/out*/ long int  *counts_in_range,
                        /*in/out*/ long int  *range_high_end)
   {
   float temp_total = 0.0;
   float percent = 0.0;
   long int high;
   long int high_done;
   long int range_high;

   high = *high_in;
   high_done = *high_done_in;
   range_high = *range_high_end;

#if DEBUG
   printf ("       Add high(top): high=%ld, high_done=%ld, valToAdd=%ld, \n        var_totals=%ld, ctsInRng=%ld, range[high]: %ld\n",
   *high_in, *high_done_in, value_to_add, var_totals, *counts_in_range, *range_high_end);
#endif

   /*--------------------------------------------
    * Try to add in the high end value. Don't add
    * the peak value twice (mm!=0)...dont' think
    * this can occur anymore.
    *-------------------------------------------*/
   if ((high <= MAX_NUM_BINS-1) && (high_done!=1) && (value_to_add > 0))
      {
      temp_total = *counts_in_range + value_to_add;
      percent = temp_total/var_totals;

#if DEBUG 
      printf ("       (HIGH) (.95-percent & fabs: %7.4f %7.4f\n",
              .95 - (percent), fabs(.95 - percent) );

      printf ("       (int)(100*(percent)) = %d\n",
               (int)(100.0*(percent)) );
#endif
 
      /*---------------------------------------
       * Don't add it in if it pushes val > 95%
       *---------------------------------------*/
/*was:      if ( fabs(.95 - percent) >= 0.004 ||
           (int)(100.0*percent)== 95 )  */

      if ( ((.95 - percent) >= 0.0) ||
           ( ((.95 - percent) < 0.0 ) &&
             ((.95 - percent) >=-0.02) ) )
         {
         *counts_in_range = *counts_in_range + value_to_add;
         range_high = high;
         high++;     /* move to next slot only if added in value. */
#if DEBUG
         printf ("       Add HIGH value.\n");
#endif
         }
      else
         {
#if DEBUG
         printf ("       Set high_done = 1.\n");
#endif
         high_done=1;
         }
      }
   else
      {
#if DEBUG
      printf ("Don't add HIGH value.\n"); 
#endif
      }

   *high_in = high;
   *high_done_in = high_done;
   *range_high_end = range_high;

#if DEBUG
   printf ("       Add high(end): high=%ld, high_done=%ld, valToAdd=%ld, \n        var_totals=%ld, ctsInRng=%ld, range[high]: %ld\n",
   *high_in, *high_done_in, value_to_add, var_totals, *counts_in_range, *range_high_end); 
#endif

    return;

   } /* add_in_high_value() */


/*---------------------------------------------------------
 * main() - controls processing flow.
 *
 * 08 Apr 95 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char      sigma_input_file_name[NAMELEN_MAX] = "\0";
   char      output_file_name[NAMELEN_MAX] = "var_ct.out\0";
   char      filelist_file_name[NAMELEN_MAX] = "file_list.txt\0";

   FILE      *output_stream;
   FILE      *sigma_input_stream;
   FILE      *file_list_stream;

   long int  ii = 0;
   int       jj = 0;
   long int  kk = 0;
   long int  mm = 0;
   int       low_done = 0;
   int       high_done = 0;

   long int  numstns_in_file = 0;

   float     sigma_sq;

   long int  var_ct[NUMQCPARMS][MAX_NUM_BINS]; /* Number cts in each bin */

   float     var_totals[NUMQCPARMS]= {0.0,0.0,0.0,0.0,0.0,0.0,0.0}; /* Total counts for each parm. */
   float     temp_total = 0.0;

   /* position 0=location of max cts; 1= max cts */
   float     var_max[NUMQCPARMS][2]= {{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},
                                      {0.0,0.0},{0.0,0.0},{0.0,0.0}};
   float     peak_pos;

   float     percent = 0.0;
   int       int_percent = 0;

   /* range position 0=bin number of low range end;
      range position 1=bin number of high range end; */
   long int    range[NUMQCPARMS][2]= {{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}};

   long int  low = 0;
   long int  high = 0;

   long int  counts_in_range[NUMQCPARMS] = {0,0,0,0,0,0,0}; /* Total counts in range */

   char      compression_cmd[NAMELEN_MAX] = "\0";
   long int  offset2, offset3 = 0;

   int       firstfile = 1;

   int       num_limit[NUMQCPARMS] = {30,30,30,30,30,30};

   float     good = 0.0;
   float     questionable_low = 0.0;
   float     questionable_hi  = 0.0;
   float     unlikely = 0.0;
   float     alpha_G[NUMQCPARMS] = {0.2, 0.2, 0.4, 0.5, 0.5,
                                    2.25, 1.22};

   float     alpha_B[NUMQCPARMS] = {0.5, 0.5, 1.0, 1.0, 1.0,
                                    4.0, 2.2};

#if ORIG
   /* 30 bins */
   long int  limit[NUMQCPARMS][MAX_NUM_BINS] = 
   {{5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
     90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* stn press */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
     90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* SLP  */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
     90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* CSLP */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
     90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* temp */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
     90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* dewpt */
    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     21,22,23,24,25,26,27,28,29,1000000}, /* Wind Speed */
    {5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,
     10500,11000,11500,12000,12500,13000,13500,14000,14500,
     15000,15500,16000,16500,17000,17500,18000,18500,19000,1000000}}; /*Wind dir*/
#endif

/* -------------------------------------------------------------------*
 * stn press=0, SLP=1, CSLP=2, temp=3, dewpt=4, WindSpeed=5, WindDir=6
 * -------------------------------------------------------------------*/
#if Version1
   float  limit[NUMQCPARMS][MAX_NUM_BINS] =
   {{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,
     50,55,60,65,70,75,80,85,90,95,100,250,500,1000,1000000},
    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,
     50,55,60,65,70,75,80,85,90,95,100,250,500,1000,1000000},
    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,
     50,55,60,65,70,75,80,85,90,95,100,250,500,1000,1000000},

    {0,.05,.1,.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,
     50,55,60,65,70,80,90,100,1000,2000,1000000},
    {0,.05,.1,.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,
     50,55,60,65,70,80,90,100,1000,2000,1000000},

    {0,.05,.1,.25,.5,.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.5,5,
     5.5,6,6.5,7,7.5,8,8.5,9,10,11,12,13,14,15,16,20,100,1000,1000000}, 
    {5,10,15,20,25,30,35,40,45,50,100,250,500,1000,2000,3000,4000,5000,6000,7000,
     8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,
     30000,40000,50000,100000,250000,500000,1000000}}; 
#endif

/* -------------------------------------------------------------------*
 * stn press=0, SLP=1, CSLP=2, temp=3, dewpt=4, WindSpeed=5, WindDir=6
 * -------------------------------------------------------------------*/
   float  limit[NUMQCPARMS][MAX_NUM_BINS] =
   {{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,
     60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,
     100,250,500,1000,2500,5000,10000,50000,100000,500000,1000000},

    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,
     60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,
     100,250,500,1000,2500,5000,10000,50000,100000,500000,1000000},

    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,
     60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,
     80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,
     100,250,500,1000,2500,5000,10000,50000,100000,500000,1000000},


    {0,.01,.02,.03,.04,.05,.06,.07,.08,.09,.1,.2,.3,.4,.5,.6,.7,.8,.9,
     1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
     30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,
     50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
     70,71,72,73,74,75,76,77,78,79,80,90,100,500,1000,1200,1400,1600,1800,2000,
     5000,1000000},

    {0,.01,.02,.03,.04,.05,.06,.07,.08,.09,.1,.2,.3,.4,.5,.6,.7,.8,.9,
     1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
     30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,
     50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
     70,71,72,73,74,75,76,77,78,79,80,90,100,500,1000,1200,1400,1600,1800,2000,
     5000,1000000},

    {0,.01,.02,.03,.04,.05,.06,.07,.08,.09,.1,.2,.3,.4,.5,.6,.7,.8,.9, 
     1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,
     5,5.25,5.5,5.75,6.,6.25,6.5,6.75,7.,7.25,7.5,7.75,8.,8.25,8.5,8.75,
     9.,9.25,9.5,9.75,10.,10.25,10.75,11.,11.25,11.5,11.75,12.,12.5,12.75,
     13,13.5,14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,30,40,50,60,70,80,90,
     100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,
     7000,8000,9000,10000,25000,50000,100000,100000.0},

    {0,1,2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,
     100,250,500,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250,3500,3750,
     4000,4250,4500,4750,5000,5250,5500,5750,6000,6250,6500,6750,7000,7250,
     7500,7750,8000,8250,8500,8750,9000,9250,9500,9750,10000,10250,10500,10750,
     11000,11250,11500,11750,12000,12250,12500,12750,13000,13250,13500,13750,
     14000,14250,14500,14750,15000,15250,15500,15750,16000,16250,16500,16750,
     17000,17250,17500,17750,18000,18250,18500,18750,19000,19500,20000,25000,
     30000,40000,50000,100000,250000,500000,1000000}}; 


#if DEBUG
   printf ("\n---- Count Sigmas ----\n");
#endif
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("Output file is named: var_ct.out\n");

   if (( file_list_stream = fopen("./file_list.txt", "r")) == NULL)
      perror ("Error: Can't open file_list.txt for reading");
 
   fscanf (file_list_stream, "%s", sigma_input_file_name);

  /*-----------------------------
   * Open output file and 
   * Initialize variance counts. 
   *----------------------------*/
  if (( output_stream = fopen(output_file_name, "w")) == NULL)
     perror ("Error: Can't open output_file_name for writing");

#if DEBUG1
     printf ("Open output file: %-s\n", output_file_name);
#endif

  for (jj = 0; jj< NUMQCPARMS; jj++)
     {   
     for (kk = 0; kk< MAX_NUM_BINS; kk++)
        var_ct[jj][kk] = 0.0;
     }   
 
   while (!feof(file_list_stream))
      {  
      if ( feof(file_list_stream))
         {
         if (fclose (sigma_input_stream) == EOF)
            perror ("Can't close input stream.");
 
         if (fclose (output_stream) == EOF)
            perror ("Can't close output_stream.");
         break;
         }

      printf ("Processing first file: %-s.\n", sigma_input_file_name);

      strncpy(output_file_name, "var_ct.out\0", 11);

     /*------------------------------------------
      * At this point, sigmasq file needs to be
      * uncompressed. 
      *------------------------------------------*/
     sprintf (compression_cmd, "gunzip %-s.gz\0",
              sigma_input_file_name);
#if DEBUG1
     printf ("executing compression_cmd: %-sxxx\n", compression_cmd);
#endif
     system (compression_cmd);
 
     /*----------------------------------------------
      * Open input file.
      * Read how many stns are currently listed in
      * this sigma file. 
      *---------------------------------------------*/
     if (( sigma_input_stream = fopen(sigma_input_file_name, "r")) == NULL)
        perror ("Error: Can't open sigma_input_file_name for reading");

     fscanf (sigma_input_stream, "%ld\n", &numstns_in_file);

#if DEBUG1
     printf ("Open output file: %-s\n", output_file_name);
#endif
 
    /*------------------------------------------------
     * Determine lengths of records and skip 1st sect.
     *-----------------------------------------------*/
    STRIPLINE(sigma_input_stream);
    offset2 = ftell (sigma_input_stream); /* 2 lines down from top */
    STRIPLINE(sigma_input_stream);
    offset3 = ftell (sigma_input_stream); /* 3 lines down from top */
 
#if DEBUG1
    printf ("offset3, offset2, (offset3-offset2): %ld %ld %ld\n",
             offset3, offset2, (offset3-offset2));
#endif
  
    if (fseek(sigma_input_stream, (numstns_in_file-2)*(offset3-offset2), SEEK_CUR) !=0) /*skip 1st section*/
       {
       printf ("Error(1): Problems calling fseek\n");
       exit(1);
       }

    STRIPLINE (sigma_input_stream); /* strip comment line */

    /*----------------------------------------------
     * C requires that an fseek (or ftell, etc)
     * call be made between switching I/O operations.
     *----------------------------------------------*/
#if DEBUG1
    printf ("call fseek\n");
#endif  
    if (fseek(sigma_input_stream, 0, SEEK_CUR) !=0) /*go nowhere!*/
       {
       printf ("Error(2): Problems calling fseek in rewrite_sigmasq()\n");
       exit(1);
       }

    ii = 0; /* station number */
    kk = 0; /* parameter number */

    while (!feof(sigma_input_stream))
      {
      fscanf (sigma_input_stream, "%ld%d", &ii, &kk);
      if (feof(sigma_input_stream)) break;
 
      fscanf (sigma_input_stream, "%f", &sigma_sq);
      if (feof(sigma_input_stream)) break;

#if DEBUG1
      printf ("(read)sigma_sq[%ld][%d] = %f\n", ii, kk, sigma_sq);
#endif

      for (mm =0; mm< MAX_NUM_BINS; mm++)
        {
        if ((sigma_sq > -888.88) && (sigma_sq < limit[kk][mm]))
           {
           var_ct[kk][mm]++;
           break;
           }
        } /* for mm */

      } /* while !feof(sigma_input_stream) */

    if (fclose (sigma_input_stream) == EOF)
      perror ("Can't close input stream.");

    sprintf (compression_cmd, "gzip %-s\0", sigma_input_file_name);
    system (compression_cmd);

    fscanf (file_list_stream, "%s", sigma_input_file_name);
 
    } /* while feof(file_list_stream) */

  if (fclose (file_list_stream) == EOF)   
     perror ("Can't close file_list_stream.");


  /*-----------------------------------------------
   * Print out the data. Each parameter in a row.
   *----------------------------------------------*/
  fprintf (output_stream, "     Parm   Bin   Upper_limit  Count\n");

  for (jj=0; jj<NUMQCPARMS; jj++) 
    { 
    for (mm=0; mm<MAX_NUM_BINS; mm++)
/*orig: fprintf (output_stream, "%7d %7ld %7ld %8ld\n", jj, mm, limit[jj][mm], var_ct[jj][mm]); */
       fprintf (output_stream, "%7d %7ld %7.2f %8ld\n", jj, mm, limit[jj][mm], var_ct[jj][mm]); 
    }

   /*******************************
    *  End section to fill bins.  *
    *******************************/


  /*---------------------------------------------
   * Try to determine a range that includes 95%
   * of the sigmasq values. This could be done
   * in the loop above, but this seperates the
   * logic. Calc the total number of values in
   * all bins and locate the peak of the curve
   * for each parameter.
   ***********************************************
   *  Locate the Peak of the curve               *
   ***********************************************/
  for (jj=0; jj<NUMQCPARMS; jj++)
    {

    for (mm=0; mm<MAX_NUM_BINS; mm++)
       {
       var_totals[jj] = var_totals[jj] + var_ct[jj][mm];

       /*
        * Locate the bin with the max number of counts.
        */
       if (var_ct[jj][mm] > var_max[jj][1])
          {
          var_max[jj][1] = var_ct[jj][mm]; /* counts */
          var_max[jj][0] = mm;             /* bin number */
          }

       } /*check all Max Num bins*/
#if DEBUG
     printf ("Max for %7ld is %7.2f in bin %7d\n", jj, var_max[jj][1], (int)(var_max[jj][0]));
#endif

    } /* for all parameters (jj) */


  /*------------------------------------
   * Do a quick check to verify that bin
   * value selections are satisfactory.
   * If the max counts occurred in the
   * first or last bins, then the bin
   * slot values need to be modified/
   * shifted to see more detail in those
   * areas.
   *------------------------------------*/
  for (jj=0; jj<NUMQCPARMS; jj++)
    {
    if (var_max[jj][0] ==0 || var_max[jj][0] ==(MAX_NUM_BINS-1) )
       {
       fprintf (output_stream, "MAX num cts occurred in bin# %ld. Redo bin values!\n", var_max[jj][0]);

#if DEBUG
       printf ("MAX num cts occurred in bin# %ld. Redo bin values!\n", var_max[jj][0]);       
#endif

       if (fclose (output_stream) == EOF)
           perror ("Can't close output stream.");
 
       printf ("\nEarly Completion on %-s %-s\n", __DATE__, __TIME__);
       exit(0);
       }
    } /* for all parameters */


  /*--------------------------------------------*
   * Now that we have the bin with the max 
   * number of counts, try and determine the
   * range about that peak that contains 95%
   * of the counts. This may not work well
   * when there are double peaks in the count
   * bins that are not close together. The
   * user must verify the results by looking
   * at the printout of the list of counts
   * per bin (printed above). Also we know
   * how many bins there are (i.e., MAX_NUM_BINS).
   * The "best" possible case would be if the
   * peak were exactly in the middle of the
   * number of bins. Remember that C starts at
   * zero not one. The worst case would be if
   * the max occurred in the first or last bin.
   *
   * Do for all parameters. (jj)
   *---------------------------------------------*/
  for (jj=0; jj<NUMQCPARMS; jj++)
    {
    low_done = 0;
    high_done = 0;

    /*--------------------------
     * Initialize to peak point.
     *-------------------------*/
    peak_pos = var_max[jj][0];

    low = peak_pos;   
    high = peak_pos + 1;

    range[jj][LOW] = peak_pos;
    range[jj][HIGH] = peak_pos;


    for (mm=0; mm<MAX_NUM_BINS; mm++)
       { 
       percent = counts_in_range[jj]/var_totals[jj]; 
       int_percent = (int)(100.0*percent);

       /***********************************************************************
        * Don't divide by zero. Stop if current percent equals or is close to
        * 95%. Stop if no more values can be added in on either low or high end
        * without forcing percent over 95%. Stop if beyond bins on both ends.
        ***********************************************************************/
/*was: if ( ((var_totals[jj]!=0) &&
             (fabs(.95 - (counts_in_range[jj]/var_totals[jj])) < 0.005)) ||
            (low_done==1 && high_done==1) ||
            (low<0 && high>MAX_NUM_BINS-1))
*/

       if ( (int_percent == 95) ||
            (low_done==1 && high_done==1)  ||
            (low<0 && high>MAX_NUM_BINS-1) ||
            ((var_totals[jj]!=0) && (fabs(.95-percent) < 0.005)) )
          {
#if DEBUG
          printf ("-----Hit ~95 prcnt for %2d, ctInRng= %7d, totCtInRng= %7.0f, ",
                   jj, counts_in_range[jj], var_totals[jj]);
          printf ("Percent= %7.4f (RANGE low,hi: %ld %ld)\n\n",
                  (counts_in_range[jj]/var_totals[jj]), range[jj][0],range[jj][1]);
#endif
          break;     /* don't add any more if at 95% or higher */
          }
       else
          {
          /*******************************************************
           * Keep adding bin counts until very close to or at 95%.
           *******************************************************/
#if DEBUG
          printf ("\nMAKE NEXT COMPARISON.(low_ct=%ld, high_ct=%ld) (mm= %ld)\n", 
		          var_ct[jj][low], var_ct[jj][high], mm);
          printf ("\nBefore add val for %2d, ctInRng= %7d, totCtInRng= %7.0f,\n",
                  jj, counts_in_range[jj], var_totals[jj] );
          printf ("Percent= %7.4f (low,high: %ld, %ld)\n", 
                  (counts_in_range[jj]/var_totals[jj]), low, high);
          printf ("low,  low_done,  var_ct[%d][low]:  %d, %d, %7d\n",
                  jj, low, low_done, var_ct[jj][low]);
#endif
          /*-------------------------------------------------
           * Start at the peak. Add in the peak number of bins.
           * Compare the bins on each side of the peak. Add in
           * the bin with the larger number of counts. Move
           * to the next bin on the side where the last "added-in"
           * bin was from. Compare that bin to the bin from
           * the side that was not added in. Add in the bin
           * with the largest number of counts and move
           * to the next bin only on the "added-in" side. Repeat
           * this process until the total sum is as close as 
           * possible to 95%.  This method of adding the bins
           * may result in a switching from side to side
           * of the peak, only adding in values from one side,
           * or a combination of these adding processes. 
           * Draw some sample curves with multiple peaks,
           * odd shapes, etc. and starting from the peak
           * add in the "highest" point comparing values
           * from both sides of the peak. This can help
           * understand what this processing should be doing.
           *------------------------------------------------*/
          if ( (var_ct[jj][low] >= var_ct[jj][high])  || (mm==0))
             {
#if DEBUG
             printf ("       Add LOW value. Move to next LOW value (1).\n");
#endif
             if (var_ct[jj][low] !=0 && low_done!=1 && low>=0)
                {             
                add_in_low_value( &low, &low_done, var_ct[jj][low], 
                                  var_totals[jj], &counts_in_range[jj],
                                  &range[jj][LOW]);
  	        }
             else
                {
                low--; /* skip zeroes */
                if (low <0) low_done = 1;
 
                if (var_ct[jj][high] == 0) /* catch if both equal zero. */
                   {
                   high++; /* skip zeroes */
                   if (high > MAX_NUM_BINS-1) high_done=1;
                   }
                }

#if DEBUG 
             printf ("After add LOW val for %2d, ctInRng= %7d, totCtInRng= %7.0f,\n",
                     jj, counts_in_range[jj], var_totals[jj]);
             printf ("Percent= %7.4f (low,high: %ld, %ld)\n",
                    (counts_in_range[jj]/var_totals[jj]), low, high);
             printf ("(End of Loop) low_done, high_done: %d %d. Range loc (low, high): %ld %ld\n",
                     low_done, high_done, range[jj][0], range[jj][1]);
#endif
             } /* if low >= hi */
          else
             {
#if DEBUG
             printf ("       Add HIGH value. Move to next HIGH value (2).\n");
#endif
             if (var_ct[jj][high]!=0 && high_done!=1 && high<=MAX_NUM_BINS-1)
	        {
                add_in_high_value( &high, &high_done, var_ct[jj][high], 
                                   var_totals[jj], &counts_in_range[jj],
                                   &range[jj][HIGH]);     
	        }					    
             else
                {
                high++; /* skip zeroes */
                if (high > MAX_NUM_BINS-1) high_done=1;
                }

#if DEBUG
             printf ("After add HIGH val for %2d, ctInRng= %7d, totCtInRng= %7.0f,\n",
                     jj, counts_in_range[jj], var_totals[jj]);
             printf ("Percent= %7.4f (low,high: %ld, %ld)\n", 
                    (counts_in_range[jj]/var_totals[jj]), low, high);
             printf ("(End of Loop) low_done, high_done: %d %d. Range loc (low, high): %ld %ld\n", 
                     low_done, high_done, range[jj][0], range[jj][1]);
#endif

             } /* if low >= high */ 
						    
          } /*add another value - total < 95% */

       } /*for all Max Num bins*/

    } /* for all parameters */



  /**************************************************
   * Print out the results of best guess ranges.
   * The user may still need to verify these ranges.
   **************************************************/
  fprintf (output_stream, "\n\nParmNum  RangeLowEnd(limit) RangeHighEnd(limit) CtsInRange TotCts  PercentInRange\n");

#if DEBUG
  printf ("\n\nParmNum  RangeLowEnd(limit) RangeHighEnd(limit) CtsInRange TotCts  PercentInRange\n");
#endif

  for (jj=0; jj<NUMQCPARMS; jj++)
    {
    fprintf (output_stream, 
             "%7d %7ld (%6.2f)  %7ld (%6.2f)  %7ld     %7.0f     %7.3f\n",
             jj, range[jj][0], limit[jj][range[jj][0]], 
             range[jj][1], limit[jj][range[jj][1]], counts_in_range[jj], 
             var_totals[jj],  (counts_in_range[jj]/var_totals[jj]) );

#if DEBUG
    printf ("%7d %7ld (%6.2f)  %7ld (%6.2f)  %7ld      %7.0f     %7.3f\n",
            jj, range[jj][0], limit[jj][range[jj][0]],
 	    range[jj][1], limit[jj][range[jj][1]], counts_in_range[jj], 
            var_totals[jj], (counts_in_range[jj]/var_totals[jj]) );
#endif

    } /* for jj - all parms */


  fprintf (output_stream, "\n\nWARNING: Following values based on hardcoded Normalization Factors. (See the code for alpha.)\n");
  fprintf (output_stream, "\nTable 3-2 Ranges of HQC flag limit values for Current project.\n");
  fprintf (output_stream, "\nParmNum  Good   Q_low  Q_high  Unlikely\n");

#if DEBUG
  printf ("\nParmNum  Good    Q_low   Q_high  Unlikely\n");
#endif 

  for (jj=0; jj<NUMQCPARMS; jj++)
    {
    good = alpha_G[jj]* sqrt(limit[jj][range[jj][1]]); /*Highest Good value*/
   
    questionable_low = alpha_G[jj]*sqrt(limit[jj][range[jj][0]]);
    questionable_hi  = alpha_B[jj]*sqrt(limit[jj][range[jj][1]]);

    unlikely = alpha_B[jj]* sqrt(limit[jj][range[jj][0]]); /*Lowest Bad value*/


    fprintf (output_stream,"%5d  %7.2f  %7.2f  %7.2f  %7.2f\n",
       jj, good, questionable_low, questionable_hi, unlikely);

#if DEBUG
    printf ("%5d %7.2f %7.2f %7.2f %7.2f\n", 
       jj, good, questionable_low, questionable_hi, unlikely);
#endif
    } /* for jj - all parms */

  fprintf (output_stream, "\nIf a wind direction value is > 180 degrees, reset the value to 180.0 degrees.\n");

  fprintf (output_stream, "\nWARNING: The user should verify that the percentage above are acceptable.\nIf not, the user should recalc the upper and lower limit ranges and recal Table 3-2. \nSee the code for the eqns to calc Table 3-2.\n");

#if DEBUG
  printf ("\nIf a wind direction value is > 180 degrees, reset the value to 180.0 degrees.\n");
#endif

  if (fclose (output_stream) == EOF)
     perror ("Can't close output stream.");

  printf ("\nProcessing completed on %-s %-s\n", __DATE__, __TIME__);

  }  /* main() */
