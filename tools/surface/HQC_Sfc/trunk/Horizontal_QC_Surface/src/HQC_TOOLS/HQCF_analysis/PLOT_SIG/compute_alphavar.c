/*--------------------------------------------------------
 * compute_alphavar.c -  Reads sigmasq files computed by
 *   compute_sigmasq() and computes alpha sigma for a 
 *   particular network and parameter. Note that this
 *   program can generate a huge output file.
 *   Possible parameters to select (i.e.,
 *                Calculated Sea Level Pressure,
 *                Sea Level Pressure,
 *                Temperature,
 *                Dew Point Temperature,
 *                Wind speed,
 *                Wind Direction.
 *
 *   Usage: compute_alphavar <sigmasq_input_file> <firstfile/nextfile>
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
 *    compute_alphavar infile1.sig firstfile
 *    compute_alphavar infile2.sig nextfile
 *    compute_alphavar infile3.sig nextfile
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
 *   Time and alpha_sq*var for the specified parameter and
 *   network. Output file is named final_alpha_sigma.out
 *
 * 03 Apr 96 lec
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
#define  DEBUG   1

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() - controls processing flow.
 *
 * 03 Apr 96 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char      sigma_output_file_name[NAMELEN_MAX] = "\0";
   char      output_file_name[NAMELEN_MAX] = "alpha_sig.out\0";

   FILE      *output_stream;
   FILE      *sigma_output_stream;

   long int  ii = 0;
   int       jj, kk = 0;

   float     alpha = 0;
   char      network[11] = "\0\0\0\0\0\0\0\0\0\0\0";
   char      current_network[11] = "\0\0\0\0\0\0\0\0\0\0\0";

   /*-----------------------------------------
    * parameter may be any of the following:
    *  paramter[12]       parameter_num
    *  ------------       -------------
    *  "stnpress\0"        0 
    *  "SLP\0"             1
    *  "calcSLP\0"         2
    *  "temperature\0"     3
    *  "dewpoint\0"        4
    *  "windspeed\0"       5
    *  "winddir\0"         6
    *----------------------------------------*/
   char      parameter[12] = "\0\0\0\0\0\0\0\0\0\0\0\0";
   int       parameter_num = -1;

   /*-------------------------------------------
    * sigma_sq    - variances for each stn and 
    *        each parameter at a specified time.
    *-------------------------------------------*/
   float sigma_sq[MAXNUMSTNS][NUMQCPARMS]    = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}};

   STRING27  stn_list[MAXNUMSTNS];

   long int  numstns_in_file = 0;
   char      junk_char;
   char      compression_cmd[NAMELEN_MAX] = "\0";

   int       firstfile = 1;

   char      HHMM_str[5] = "\0\0\0\0\0"; /* time sigsq values are valid. */
   int       HHMM = 0;


   printf ("\n---- Compute Alpha_sq*Variance ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);

   /*--------------------------------------------
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 6)
      {  
      printf (
     "Usage: compute_alphavar <sigmasq_input_file> <alpha> <network> <parameter> <firstfile/nextfile>\n");
      exit(1);
      }  

   strcpy (sigma_output_file_name, argv[1]);
   alpha = atof (argv[2]);
   strcpy (network, argv[3]);
   strcpy (parameter, argv[4]);

   if (!strcmp (parameter, "stnpress\0"))
      parameter_num = 0;
   else if (!strcmp (parameter, "SLP\0"))   
      parameter_num = 1; 
   else if (!strcmp (parameter, "calcSLP\0"))   
      parameter_num = 2; 
   else if (!strcmp (parameter, "temperature\0"))    
      parameter_num = 3; 
   else if (!strcmp (parameter, "dewpoint\0"))    
      parameter_num = 4; 
   else if (!strcmp (parameter, "windspeed\0"))    
      parameter_num = 5; 
   else if (!strcmp (parameter, "winddir\0"))    
      parameter_num = 6;

#if DEBUG
   printf ("alpha, network: %f, %-s\n", alpha, network);
   printf ("parameter, parameter_num: %-s, %d\n", parameter, parameter_num);
#endif

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

   if (!strncmp(argv[5], "firstfile",9))
      {  
      firstfile = 1;
      printf ("Processing first file: %-s.\n", sigma_output_file_name);
      }  
   else
      {  
      firstfile =0;
      printf ("Processing next file in a series: %-s.\n", sigma_output_file_name);
      }

   strncpy(output_file_name, "alpha_sig.out\0", 11);

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
 
   /*-------------------------------------------------
    * Read variance data from sigma sq file.
    *-------------------------------------------------*/
   for (ii=0;ii<numstns_in_file;ii++)
      { 
      fscanf (sigma_output_stream, "%d%1c", &jj, &junk_char);
      if (feof(sigma_output_stream))
        {
        printf ("WARNING(1): hit premature End Of File on %-s.\n",
                sigma_output_file_name);
        printf ("WARNING(1): Some additional searching will be required for sigmas.\n");
        }
 
      fgets (stn_list[jj], 27, sigma_output_stream);
#if DEBUG
      printf ("stn_list[%d] = xxx%-sxxx\n", jj, stn_list[jj]);
#endif
      } 

      /*--------------------------------------------------
       * Initialize all sigma_sq values to missing -999.99)
       * even beyond what will be read.
       *--------------------------------------------------*/
      for (ii=0;ii<MAXNUMSTNS;ii++)
      for (kk=0;kk<=NUMQCPARMS;kk++) sigma_sq[ii][kk] = -999.990000;
 
      STRIPLINE (sigma_output_stream); /* Skip the comment line. */
      STRIPLINE (sigma_output_stream); /* Skip the comment line. */
 
      ii = 0;
      kk = 0;
      while (!feof(sigma_output_stream))
         { 
         fscanf (sigma_output_stream, "%ld%d", &ii, &kk);
         if (feof(sigma_output_stream))
            {
#if DEBUG
            if (ii < numstns_in_file-1)
               {
               printf ("WARNING(2): hit premature End Of File on %-s.\n",
                       sigma_output_file_name);
               printf ("WARNING(2): Some additional searching will be required for sigmas.\n");
               }
            else
#endif
               break;
            }
 
         fscanf (sigma_output_stream, "%f", &sigma_sq[ii][kk]);
         if (feof(sigma_output_stream)) break;
#if DEBUG
         printf ("(read)sigma_sq[%ld][%d] = %f\n", ii, kk, sigma_sq[ii][kk]);
#endif
         } /* data in file */


   /*------------------------------------------------------
    * Only write to output file the specifed parameter for
    * the specified Network. Maybe can combine this loop
    * with loop above.
    *------------------------------------------------------*/
   for (ii=0;ii<numstns_in_file;ii++)
      {  
      strncpy (current_network, stn_list[ii], 10);
      current_network[10] = '\0';

#if DEBUG
      printf ("network, current_network: xxx%-sxxx xxx%-sxxx\n",
               network, current_network);
#endif

      if (!strncmp(network, current_network, strlen(network)))
         {
         /*--------------------------------
          * This is the type network we
          * are looking for. Write specified
          * sigma_sq to output file.
          *-------------------------------*/
         printf ("HHMM, sigma_sq, a**2*var, a*v: %d %f %f %f \n",
                 HHMM,
                 sigma_sq[ii][parameter_num],
                 alpha*alpha*sigma_sq[ii][parameter_num],
                 alpha*sqrt(sigma_sq[ii][parameter_num]));

         fprintf (output_stream, "%d %f %f %f\n",
                 HHMM, 
                 sigma_sq[ii][parameter_num],
                 alpha*alpha*sigma_sq[ii][parameter_num],
                 alpha*sqrt(sigma_sq[ii][parameter_num]));
         }
      }

  printf ("Close the files\n");

  close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
  close_file (&output_stream, FILE_NOT_COMPRESSED);

   /*-------------------------------------------------------
    * Copy or cat the precip stats to the final output file.
    *-------------------------------------------------------*/
   if (firstfile)
      {
#if DEBUG
      printf ("mv alpha_sig.out final_ave_var.out\n");
#endif   
      system ("mv alpha_sig.out final_ave_var.out\0");
      }
   else
      {
#if DEBUG
      printf ("cat alpha_sig.out >> final_ave_var.out \n");
#endif   
      system ("/bin/cat alpha_sig.out >> final_ave_var.out");
      system ("/bin/rm alpha_sig.out");
      }

  sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
  system (compression_cmd);
 
  printf ("\nProcessing completed on %-s %-s\n", __DATE__, __TIME__);
  }  /* main() */
