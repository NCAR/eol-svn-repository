/*--------------------------------------------------------
 * remove_from_sigsqfiles - Removes the specified station
 *     and its data from the input sigma sq (variance) file
 *     and writes remaining sig sq data to output file.
 *
 *  Usage: remove_from_sigsqfiles network stnID infile outfile
 *
 *  WARNING: If the format of the sigma sq files changes,
 *           this s/w must be updated! Assumes outfile
 *           does not already exist.
 *  WARNING: This s/w assumes that the sigma file(s) being
 *           processed are COMPRESSED (i.e., gzipped).
 *
 *  Input:
 *     infile - file in sigma sq file format.
 *
 *  Output:
 *     outfile - file in sigma sq file format with requested
 *               station removed.
 *
 *    Sigma sq (variance) files are produced by the
 *    compute_sigmasq program and these files generally
 *    have names of the following form:
 *                   yyyyjjjhhmm.sig 
 * 
 *    where yyyy is year, jjj is julian date, hh is hour, mm is
 *    minute for which the contained sigmasq (variance) values
 *    are valid. 
 *
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
 *     0 ASOSH     :E02
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
 * 13 May 96 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "local.h"
#include "process_qcfrec.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 *-------------------------------------------------------*/
#define   DEBUG 0
#define   DEBUG1 0

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() -  Controls main processing.
 *
 * 13 May 96 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char		input_file_name[NAMELEN_MAX]="\0";
   char         sigma_output_file_name[NAMELEN_MAX] = "\0";

   FILE		*input_stream;
   FILE         *sigma_output_stream;

   long int     ii, jjj = 0;
   long int     stn_to_remove = -1;
   int          kk,jj = 0;
   int          stn_no = -999;

   STRING27     stn_list[MAXNUMSTNS];

   float sigma_sq[MAXNUMSTNS][NUMQCPARMS]    = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}};

   long         numstns_inp     = 0;
   long         final_numstns_inp  = 0;
   long         final_ct  = 0;
   char         junk_char; 
   char         current_stn[27] = "\0\0\0\0\0\0\0\0";

   char         input_network[11] = "\0\0\0\0\0\0\0\0\0\0\0";
   char         input_stnID[16] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
   static char  network_stnID[27] = "\0";
   static char  command[100] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
 

   /*---------------------------------------------
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 5)
      {  
      printf ("Usage: remove_from_sigsqfiles <network> <stnID> <infile> <outfile>\n");
      exit(1);
      }  

   strcpy (input_network, argv[1]);
   strcpy (input_stnID, argv[2]);
   strcpy (input_file_name, argv[3]);
   strcpy (sigma_output_file_name, argv[4]);

   /*-----------------------------------------------
    * Open input/output files. Assume that sig files
    * are gzipped.
    *----------------------------------------------*/
   open_file (input_file_name, "r", FILE_COMPRESSED, &input_stream);
   open_file (sigma_output_file_name, "w", FILE_NOT_COMPRESSED, &sigma_output_stream);

   printf ("\nProcessing file %-s\n", input_file_name);

   /*-----------------------------------------------
    * Form the network/stnID pair that can be used
    * for searching.
    *----------------------------------------------*/
   sprintf (network_stnID, "%-10s:%-15s\0", input_network, input_stnID);

#if DEBUG1
   printf ("network_stnID= xxx%-sxxx\n", network_stnID);
#endif

   stn_to_remove = -999;

   /*-----------------------------------------------
    * Read the input files and search for specified
    * station to be removed. Decrement ct only if
    * locate requested ID.
    *----------------------------------------------*/
   fscanf (input_stream, "%ld", &numstns_inp); STRIPLINE(input_stream);

   final_numstns_inp = numstns_inp; /* Start off with this count */

#if DEBUG
   printf ("numstns_inp: %ld\n", numstns_inp);
/* printf ("(orig) final_numstns_inp: %ld\n", final_numstns_inp); */
#endif

   /*--------------------------------------------------
    * Initialize all sigma_sq values to missing -999.99
    * even beyond what will be read.
    *--------------------------------------------------*/
   for (ii=0;ii<MAXNUMSTNS;ii++)
      for (kk=0;kk<=NUMQCPARMS;kk++) 
         {
         sigma_sq[ii][kk] = -999.990000000;
         }

   /*------------------------------------------
    * Read in the first input sigma file's values.
    * Let the values in the second input file
    * override this input file's values, if
    * duplicates occur.
    *-----------------------------------------*/
   for (ii=0;ii<numstns_inp;ii++)
      {
      fscanf (input_stream, "%d%1c", &jjj, &junk_char);
      if (feof(input_stream))
         {
         printf ("WARNING(1): hit premature End Of File on %-s.\n", input_stream);
         printf ("WARNING(1): Some additional searching will be required for sigmas.\n");
         }

      fgets (stn_list[jjj], 27, input_stream);

#if DEBUG
      printf ("Comparing stations: xxx%-sxxx AND stn_list[%ld]=xxx%-sxxx\n",
              network_stnID, jjj, stn_list[jjj]);
#endif 

      if (!strncmp (stn_list[jjj], network_stnID, 27))
         {
#if DEBUG
         printf ("Match found with xxx%-sxxx AND stn_list[%ld]=xxx%-sxxx\n", 
                 network_stnID, jjj, stn_list[jjj]);
#endif
         stn_to_remove = jjj;
         final_numstns_inp--;
         }

#if DEBUG1
      printf ("(input_file)stn_list[%ld] = %-27sxxx\n", jjj, stn_list[jjj]);
#endif
      } 

   STRIPLINE (input_stream); /* Skip the comment line. */
   STRIPLINE (input_stream); /* Skip the comment line. */

   while (!feof(input_stream))
     {
     fscanf (input_stream, "%ld%d", &ii, &jjj);
     if (feof(input_stream)) break;
 
     fscanf (input_stream, "%f", &sigma_sq[ii][jjj]);
     if (feof(input_stream)) break;

#if DEBUG11
     printf ("(read - input_file)sigma_sq[%ld][%ld] = %f\n", ii, jjj, sigma_sq[ii][jjj]);
#endif
     } /* data in file */

   /*------------------------------------------------
    * Write out data to new sigma sq file. Only
    * if match found!
    *-----------------------------------------------*/
   if (stn_to_remove > -1)
      {
#if DEBUG
      printf ("Match found - write NEW file\n");
#endif
      fprintf (sigma_output_stream, "%5ld\n", final_numstns_inp);

      final_ct = -1;
      for (ii=0;ii<numstns_inp;ii++) 
         {
         if (strncmp (stn_list[ii], network_stnID,27)) /* print all except stn to be removed */
            {
            final_ct ++;
            fprintf (sigma_output_stream, "%5ld %-27s\n", final_ct, stn_list[ii]);
            }
         }


      fprintf (sigma_output_stream, "stn_no parm_no  sigma value\n");

      final_ct = -1;
      for (ii=0;ii<numstns_inp;ii++)
         {  
         if (strncmp (stn_list[ii], network_stnID,27)) /* print all except stn to be removed */
            {
            final_ct++;
            for (kk=0;kk<NUMQCPARMS;kk++)
               if (sigma_sq[ii][kk] > -990.00)
                  fprintf (sigma_output_stream, "%5ld %5d %f\n", final_ct, kk, sigma_sq[ii][kk]);
            }  
         }

      close_file (&input_stream, FILE_COMPRESSED);
      close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);

      /*-----------------------------
       * Compress the output stream.
       *----------------------------*/
      sprintf (command, "gzip %-s", sigma_output_file_name);
      system (command);

      } /* stn_to_remove > 0 */
    else
      {
#if DEBUG
      printf ("Match NOT found! Just copy orig file to new file!\n");
#endif
      close_file (&input_stream, FILE_COMPRESSED);
      printf ("/bin/rm %-s\n", sigma_output_file_name );
      sprintf (command, "/bin/rm %-s\n", sigma_output_file_name );
      system (command);

      printf ("cp %-s %-s.gz\n", input_file_name, sigma_output_file_name );
      sprintf (command, "cp %-s %-s.gz", input_file_name, sigma_output_file_name );
      system (command);
      }

   }  /* remove_from_sigsqfiles() */
