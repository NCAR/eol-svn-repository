/*--------------------------------------------------------
 * compute_ave_var.c -  Reads sigmasq files computed by
 *   compute_sigmasq() and computes ave variances and 
 *   std for each parameter (i.e.,
 *                Calculated Sea Level Pressure,
 *                Sea Level Pressure,
 *                Temperature,
 *                Dew Point Temperature,
 *                Wind speed,
 *                Wind Direction.
 *
 *   Usage: compute_ave_var <sigmasq_input_file> <firstfile/nextfile>
 *
 * Input:
 *   This program expects 2 command line arguments after the
 *   program executable name. The first arg is the name of the input
 *   file. The second arg tells the program whether the data being processed 
 *   is the first or some other in a series of data files being processed.
 *   If it the first file, then the word "firstfile" should be in arg 2.
 *   If it the file being processed is the second or other in a series
 *   of files being processed, then the second arg should be the word
 *   "nextfile". These are the only two acceptable words for the second
 *   argument. The second argument allows this program to concatenate
 *   the output from several runs of this program into a single output file.
 *
 *   Some example command lines are:
 *
 *    compute_ave_var infile1.sig firstfile
 *    compute_ave_var infile2.sig nextfile
 *    compute_ave_var infile3.sig nextfile
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
 *   Time and average std for each parameter into file named
 *   final_ave_var.out.
 *
 * 13 Dec 95 lec
 *   Created.
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

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() - controls processing flow.
 *
 * 13 Dec 95 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char      sigma_output_file_name[NAMELEN_MAX] = "\0";
   char      output_file_name[NAMELEN_MAX] = "ave_var.out\0";

   FILE      *output_stream;
   FILE      *sigma_output_stream;

   long int  ii = 0;
   int       jj, kk = 0;

   long int  numstns_in_file = 0;

   float     sigma_sq;
   int       count[NUMQCPARMS];
   float     ave_var[NUMQCPARMS];
   float     std_var[NUMQCPARMS];
 
   char      compression_cmd[NAMELEN_MAX] = "\0";
   long int  offset2, offset3 = 0;

   int       firstfile = 1;

   char      HHMM_str[5] = "\0\0\0\0\0"; /* time sigsq values are valid. */
   int       HHMM = 0;


   printf ("\n---- Compute Average Sigmas ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);

   /*--------------------------------------------
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 3)
      {  
      printf ("Usage: compute_ave_var <sigmasq_input_file> <firstfile/nextfile>\n");
      exit(1);
      }  

   strcpy (sigma_output_file_name, argv[1]);

   /*---------------------------------------------------------
    * Pick out the time the siqsq values are valid
    * from the sigma file name (i.e, yyyyjjjhhmm.sig
    * where yyyy is year, jjj is julian date, hh is hour, mm is
    * minute for which the contained sigmasq (variance) values
    * are valid.
    *---------------------------------------------------------*/
   strncpy ( HHMM_str, &sigma_output_file_name[7],4);
   HHMM_str[4] = '\0';
   HHMM = atoi (HHMM_str);

#if DEBUG
   printf ("HHMM: %d\n", HHMM);
   printf ("output_file_name: xxx%-sxxx\n", output_file_name);
#endif

   if (!strncmp(argv[2], "firstfile",9))
      {  
      firstfile = 1;
      printf ("Processing first file: %-s.\n", sigma_output_file_name);
      }  
   else
      {  
      firstfile =0;
      printf ("Processing next file in a series: %-s.\n", sigma_output_file_name);
      }

   strncpy(output_file_name, "ave_var.out\0", 11);

   /*------------------------------------------
    * At this point, sigmasq file needs to be
    * uncompressed. 
    *------------------------------------------*/
   sprintf (compression_cmd, "gunzip %-s.gz\0",
            sigma_output_file_name);
#if DEBUG
   printf ("executing compression_cmd: %-sxxx\n", compression_cmd);
#endif
   system (compression_cmd);
 
   /*----------------------------------------------
    * Open input and output files.
    * Read how many stns are currently listed in
    * this sigma file. 
    *---------------------------------------------*/
   open_file (sigma_output_file_name, "r", FILE_NOT_COMPRESSED, &sigma_output_stream);
   open_file (output_file_name, "w", FILE_NOT_COMPRESSED, &output_stream);

   fscanf (sigma_output_stream, "%ld\n", &numstns_in_file);

#if DEBUG
   printf ("Open input file: %-s\n", sigma_output_file_name);
   printf ("Open output file: %-s\n", output_file_name);
   printf ("numstns_in_file: %ld\n", numstns_in_file);
#endif
 
  /*------------------------------------------------
   * Determine lengths of records and skip 1st sect.
   *-----------------------------------------------*/
  STRIPLINE(sigma_output_stream);
  offset2 = ftell (sigma_output_stream); /* 2 lines down from top */
  STRIPLINE(sigma_output_stream);
  offset3 = ftell (sigma_output_stream); /* 3 lines down from top */
 
#if DEBUG
  printf ("offset3, offset2, (offset3-offset2): %ld %ld %ld\n",
           offset3, offset2, (offset3-offset2));
#endif
 
  if (fseek(sigma_output_stream, (numstns_in_file-2)*(offset3-offset2), SEEK_CUR) !=0) /*skip 1st section*/
     {
     printf ("Error(1): Problems calling fseek in rewrite_sigmasq()\n");
     exit(1);
     }

  STRIPLINE (sigma_output_stream); /* strip comment line */

  /*----------------------------------------------
   * C requires that an fseek (or ftell, etc)
   * call be made between switching I/O operations.
   *----------------------------------------------*/
#if DEBUG 
  printf ("call fseek\n");
#endif  
  if (fseek(sigma_output_stream, 0, SEEK_CUR) !=0) /*go nowhere!*/
     {
     printf ("Error(2): Problems calling fseek in rewrite_sigmasq()\n");
     exit(1);
     }

  /*----------------------------------------------
   * Read sigmasq values for second half of file
   * and compute ave variances and std. Write to
   * output file.
   *----------------------------------------------*/
  for (jj = 0; jj< NUMQCPARMS; jj++)
     {
     ave_var[jj] = 0.0;
     count[jj] = 0;
     std_var[jj] = 0.0;
     }

  ii = 0;
  kk = 0;
  while (!feof(sigma_output_stream))
    {
    fscanf (sigma_output_stream, "%ld%d", &ii, &kk);
    if (feof(sigma_output_stream)) break;
 
    fscanf (sigma_output_stream, "%f", &sigma_sq);
    if (feof(sigma_output_stream)) break;

#if DEBUG
    printf ("(read)sigma_sq[%ld][%d] = %f\n", ii, kk, sigma_sq);
#endif

    ave_var[kk] += sigma_sq;
    ++count[kk];

    } /* data in file */

  for (jj=0; jj<NUMQCPARMS; jj++) 
     { 
     if (count[jj] > 0)
        {
        ave_var[jj] = ave_var[jj]/count[jj]; 
        std_var[jj] = sqrt( ave_var[jj] ); 

        fprintf (output_stream, "%5d %5d %8.3f\n", HHMM, jj, std_var[jj]);
        }
     }

  close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
  close_file (&output_stream, FILE_NOT_COMPRESSED);

   /*-------------------------------------------------------
    * Copy or cat the precip stats to the final output file.
    *-------------------------------------------------------*/
   if (firstfile)
      {
#if DEBUG
      printf ("mv ave_var.out final_ave_var.out\n");
#endif   
      system ("mv ave_var.out final_ave_var.out\0");
      }
   else
      {
#if DEBUG
      printf ("cat ave_var.out >> final_ave_var.out \n");
#endif   
      system ("/bin/cat ave_var.out >> final_ave_var.out");
      system ("/bin/rm ave_var.out");
      }

  sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
  system (compression_cmd);
 
  printf ("\nProcessing completed on %-s %-s\n", __DATE__, __TIME__);
  }  /* main() */
