/*--------------------------------------------------------
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
 *   Run comp_ct_percent.pl to do this summing.
 *   This range, in turn, can be used to compute the
 *   values put in Table 3-2 "Ranges of HQC flag Limit
 *   Values for 'project-name' 'composite-name'.
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
 *   Updated and increased number of bins.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
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
#define  ORIG    0

/* Following was only 30 bins. */
#define  MAX_NUM_BINS  40

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


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

   long int  numstns_in_file = 0;

   float     sigma_sq;
   long int  var_ct[NUMQCPARMS][MAX_NUM_BINS];
 
   char      compression_cmd[NAMELEN_MAX] = "\0";
   long int  offset2, offset3 = 0;

   int       firstfile = 1;

   int       num_limit[NUMQCPARMS] = {30,30,30,30,30,30};

#if ORIG
   /* 30 bins */
   long int  limit[NUMQCPARMS][MAX_NUM_BINS] = 
   {{5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* stn press */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* SLP  */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* CSLP */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* temp */
    {5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,1000000}, /* dewpt */
    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,1000000}, /* Wind Speed */
    {5000,5500,6000,6500,7000,7500,8000,8500,9000,9500,10000,10500,11000,11500,12000,12500,13000,13500,14000,14500,15000,15500,16000,16500,17000,17500,18000,18500,19000,1000000}}; /*Wind dir*/
#endif

/* -------------------------------------------------------------------*
 * stn press=0, SLP=1, CSLP=2, temp=3, dewpt=4, WindSpeed=5, WindDir=6
 * -------------------------------------------------------------------*/
   float  limit[NUMQCPARMS][MAX_NUM_BINS] =
   {{1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,250,500,1000,1000000},
    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,250,500,1000,1000000},
    {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,250,500,1000,1000000},

    {0,.05,.1,.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,55,60,65,70,80,90,100,1000,2000,1000000},
    {0,.05,.1,.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,55,60,65,70,80,90,100,1000,2000,1000000},

    {0,.05,.1,.25,.5,.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,10,11,12,13,14,15,16,20,100,1000,1000000}, 
    {5,10,15,20,25,30,35,40,45,50,100,250,500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,30000,40000,50000,100000,250000,500000,1000000}}; 


   printf ("\n---- Count Sigmas ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);

   if (( file_list_stream = fopen("./file_list.txt", "r")) == NULL)
      perror ("Error: Can't open file_list.txt for reading");
 
   fscanf (file_list_stream, "%s", sigma_input_file_name);

  /*-----------------------------
   * Open output file and 
   * Initialize variance counts. 
   *----------------------------*/
  if (( output_stream = fopen(output_file_name, "w")) == NULL)
     perror ("Error: Can't open output_file_name for writing");

#if DEBUG
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
#if DEBUG
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

#if DEBUG
     printf ("Open output file: %-s\n", output_file_name);
#endif
 
    /*------------------------------------------------
     * Determine lengths of records and skip 1st sect.
     *-----------------------------------------------*/
    STRIPLINE(sigma_input_stream);
    offset2 = ftell (sigma_input_stream); /* 2 lines down from top */
    STRIPLINE(sigma_input_stream);
    offset3 = ftell (sigma_input_stream); /* 3 lines down from top */
 
#if DEBUG
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
#if DEBUG 
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

#if DEBUG
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

  if (fclose (output_stream) == EOF)
     perror ("Can't close output stream.");

  printf ("\nProcessing completed on %-s %-s\n", __DATE__, __TIME__);

  }  /* main() */
