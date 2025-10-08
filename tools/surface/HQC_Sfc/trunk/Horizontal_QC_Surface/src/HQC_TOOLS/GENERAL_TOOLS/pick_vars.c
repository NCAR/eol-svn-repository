/*--------------------------------------------------------
 * pick_vars.c -  Picks out variances for particular
 *  hardcoded station(s) from requested input files.
 *
 *   Usage: pick_vars <input_file>
 *
 * Input: This s/w operates on sigma_sq input files.
 * These sigma_sq or variance files have file names of the form:
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
 *   Variance for hardcoded station(s).
 *
 * 25 Apr 96 lec
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
 *
 *-------------------------------------------------------*/
#define  DEBUG     0
#define  DEBUG2    0

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() - controls Horizontal Quality Control processing flow.
 *
 * 25 Apr 96 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char         sigma_input_file_name[NAMELEN_MAX] = "\0";
   char         output_file_name[NAMELEN_MAX] = "\0";

   FILE         *sigma_input_stream;
   FILE         *output_stream;

   char         compression_cmd [NAMELEN_MAX+10]= "\0";
   char         junk_char = '\0';

/*--------------------------------------------------------------
   char         input_stn[27]= "ASOS5     :TOP            \0";
   char         input_stn[27]= "ASOS5     :TRL            \0";
   char         input_stn[27]= "OKMESO5   :BBOW           \0";
   char         input_stn[27]= "AWOS1     :Carroll        \0";
   char         input_stn[27]= "AWOSA05   :OUN            \0";
   char         input_stn[27]= "ARMSFC1   :Meeker         \0";
   ------------------------------------------------------------*/
   char         input_stn[27]= "ARMSFC1   :Meeker         \0";

   int          lesser_stn_length = 0;

   int          ii,jj,kk = 0;
   STRING27     stn_list[MAXNUMSTNS];
   long         stn_no      = -1;
   long         numstns_inp = 0;

   /*
    * sigma_sq    - variances for each stn and each parameter at a specified time.
    */
   float sigma_sq[MAXNUMSTNS][NUMQCPARMS]    = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}};

   /*--------------------------------------------
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 3)
      {  
      printf ("Usage: pick_vars <sigma_input_file> <output_file>\n");
      exit(1);
      }  

   strcpy (sigma_input_file_name, argv[1]);
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);

   strcpy (output_file_name, argv[2]);
   open_file (output_file_name, "w", FILE_NOT_COMPRESSED, &output_stream);

   printf ("Processing file: %-s\n", sigma_input_file_name);
   printf ("Output file: %-s\n", output_file_name);

   /*-------------------------------------------
    * Assume the input sigma file is compressed.
    *------------------------------------------*/ 
#if UNIX_ENV
   open_file (sigma_input_file_name, "r", FILE_COMPRESSED,
              &sigma_input_stream);
#else
   sprintf (compression_cmd, "gunzip %-s.gz", sigma_input_file_name);
   system (compression_cmd);
   open_file (sigma_input_file_name, "r", FILE_NOT_COMPRESSED, 
              &sigma_input_stream);
#endif

   fscanf (sigma_input_stream, "%ld\n", &numstns_inp);

#if DEBUG
   printf ("open_file: %-sxxx\n", sigma_input_file_name);
   printf ("excuting cmd:%-sxxx\n", compression_cmd);
   printf ("numstns_inp: %ld\n", numstns_inp);
#endif

   for (ii=0;ii<numstns_inp;ii++)
      {
      fscanf (sigma_input_stream, "%d%1c", &jj, &junk_char);
      if (feof(sigma_input_stream))
         {
         printf ("WARNING(1): hit premature End Of File on %-s.\n",
                 sigma_input_file_name);
         printf ("WARNING(1): Some additional searching will be required for sigmas.\n");
         }

      fgets (stn_list[jj], 27, sigma_input_stream);
#if DEBUG2
      printf ("stn_list[%d] = xxx%-sxxx\n", jj, stn_list[jj]);
#endif

      if (strlen(stn_list[ii]) <= strlen(input_stn))
        lesser_stn_length = strlen(stn_list[ii]);
      else
        lesser_stn_length = strlen(input_stn);

#if DEBUG
        printf ("Comparing stn_list[%ld], input_stn: xxx%-sxxx xxx%-sxxx\n",
                ii, stn_list[ii], input_stn);
#endif
        if (!strncmp(stn_list[ii], input_stn, lesser_stn_length))
          {  
          stn_no = ii;
#if DEBUG
          printf ("   STN FOUND MATCH stn_no, stn_list[%d]:%d xxx%sxxx. Now locate variance.\n",
                 stn_no, stn_no, stn_list[stn_no]);
#endif
          }   /* if match */

        } /* for */

     /*--------------------------------------------------
      * Initialize all sigma_sq values to missing -999.99.
      *--------------------------------------------------*/
     for (ii=0;ii<MAXNUMSTNS;ii++)
        for (kk=0;kk<=NUMQCPARMS;kk++) sigma_sq[ii][kk] = -999.990000;


     STRIPLINE (sigma_input_stream); /* Skip the comment line. */
     STRIPLINE (sigma_input_stream); /* Skip the comment line. */

     ii = 0;
     kk = 0;
     while (!feof(sigma_input_stream))
        {
        fscanf (sigma_input_stream, "%ld%d", &ii, &kk);
        if (feof(sigma_input_stream))
           {
#if DEBUG
           if (ii < numstns_inp-1)
              {
              printf ("WARNING(2): hit premature End Of File on %-s.\n", 
                      sigma_input_file_name);
              printf ("WARNING(2): Some additional searching will be required for sigmas.\n");
              }
           else
#endif
              break;
           }

        fscanf (sigma_input_stream, "%f", &sigma_sq[ii][kk]);
        if (feof(sigma_input_stream)) break;
#if DEBUG2
        printf ("(read)sigma_sq[%ld][%d] = %f\n", ii, kk, sigma_sq[ii][kk]);
#endif
        if (ii == stn_no)
           fprintf (output_stream, "%-s %d %d %10.2f %10.2f\n", input_stn, ii, kk, 
                    sigma_sq[ii][kk], sqrt(sigma_sq[ii][kk]));

        } /* data in file */


     /*---------------------------------------
      * In UNIX environment, have used a
      * pipe open to access compressed
      * file without using time to uncompress.
      * Close_file() will then close pipe.
      * If file is not compressed, close_file()
      * close normally with fclose command.
      *----------------------------------------*/
#if UNIX_ENV 
     close_file (&sigma_input_stream, FILE_COMPRESSED);
#else
     close_file (&sigma_input_stream, FILE_NOT_COMPRESSED);
     sprintf (compression_cmd, "gzip %-s\0", sigma_input_file_name);
     system (compression_cmd);
#endif

   close_file (&output_stream, FILE_NOT_COMPRESSED);

   printf ("\nProcessing completed on %-s %-s\n", __DATE__, __TIME__);
   }  /* main() */
