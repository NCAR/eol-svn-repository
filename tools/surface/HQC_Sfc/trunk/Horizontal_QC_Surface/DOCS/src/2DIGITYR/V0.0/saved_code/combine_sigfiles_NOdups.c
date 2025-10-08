/*--------------------------------------------------------
 * combine_sigsqfiles - Combines two input sigma sq (variance)
 *     files into one output sigma sq file.
 *
 *  Usage: combine_sigsqfiles infile1 infile2 outfile
 *
 *  This program (using knowledge about the format of sigma
 *  square files) combines two sigma square files and puts
 *  the output into the specified output file.
 *
 *  WARNING: If the format of the sigma sq files changes,
 *           this s/w must be updated!
 *
 *  WARNING: Assumes that there are NO duplicates between
 *           the two input files.
 *
 *  Input:
 *     infile1 - file in sigma sq file format.
 *     infile2 - file in sigma sq file format.
 *
 *  Output:
 *     outfile - file in sigma sq file format.
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
 * 10 Oct 95 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>

#include "local.h"
#include "process_qcfrec.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 *-------------------------------------------------------*/
#define   DEBUG 1

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() -  Controls variance (sigma squared) computations.
 *
 * 09 Dec 94 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char		input_file1_name[NAMELEN_MAX]="\0";
   char		input_file2_name[NAMELEN_MAX]="\0";
   char         sigma_output_file_name[NAMELEN_MAX] = "\0";

   FILE		*input_stream1;
   FILE		*input_stream2;
   FILE         *sigma_output_stream;

   int           ii,kk = 0;
   int           index = 0;
   STRING27      stn_name;
   long          numstns_inp1     = 0;
   long          numstns_inp2     = 0;
   long          total_numstns_inp  = 0;
   char          junk_char; 
   char          current_stn[27] = "\0";
   char          NULL27[27] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
 
   float sigma_sq = -999.9900000;


   /*
    * Expect input file name on the command line.
    */
   if (argc != 4)
      {  
      printf ("Usage: combine_sigsqfiles <infile1> <infile2> <outfile>\n");
      exit(1);
      }  

   strcpy (input_file1_name, argv[1]);
   strcpy (input_file2_name, argv[2]);
   strcpy (sigma_output_file_name, argv[3]);


   /*-----------------------------------------------
    * Open input/output files.
    *----------------------------------------------*/
   open_file (input_file1_name, "r", &input_stream1);
   open_file (input_file2_name, "r", &input_stream2);
   open_file (sigma_output_file_name, "w", &sigma_output_stream);


   /*-----------------------------------------------
    * Read the input files and write to output.
    *----------------------------------------------*/
   fscanf (input_stream1, "%ld", &numstns_inp1); STRIPLINE(input_stream1);
   fscanf (input_stream2, "%ld", &numstns_inp2); STRIPLINE(input_stream2);
   total_numstns_inp = numstns_inp1 + numstns_inp2;

   fprintf (sigma_output_stream, "%5ld\n", total_numstns_inp);

#if DEBUG       
   printf ("numstns_inp1: %ld\n", numstns_inp1);
   printf ("numstns_inp2: %ld\n", numstns_inp2);
   printf ("total_numstns_inp: %ld\n", total_numstns_inp);
#endif
 
   for (ii=0;ii<numstns_inp1;ii++)
      {
      fscanf (input_stream1, "%d%1c", &kk, &junk_char);
      if (feof(input_stream1))
         {
         printf ("ERROR(1): hit premature End Of File on %-s.\n", input_stream1);
         printf ("WARNING(1): Some additional searching will be required for sigmas.\n");
         }

      fgets (stn_name, 27, input_stream1);
      fprintf (sigma_output_stream, "%5ld %-27s\n", kk, stn_name);
      } 
 
   /*-------------------------------------------------
    * Now read the stn list from the second file. 
    * Begin numbering of stations where the first
    * file left off.
    *------------------------------------------------*/
   for (ii=0;ii<numstns_inp2;ii++)
      {  
      fscanf (input_stream2, "%d%1c", &index, &junk_char);
      if (feof(input_stream2))
         {
         printf ("ERROR(2): hit premature End Of File on %-s.\n", input_stream2);
         printf ("WARNING(2): Some additional searching will be required for sigmas.\n");
         }
 
      fgets (stn_name, 27, input_stream2);

      printf ("Read from stream2: index, junk_char, stn_name - %d %1c %-27s\n", 
               index, junk_char, stn_name); 
      fprintf (sigma_output_stream, "%5ld %-27s\n", numstns_inp1+index, stn_name); 
      }  

   fprintf (sigma_output_stream, "stn_no parm_no  sigma value\n");


   /*--------------------------------------------------
    * Initialize all sigma_sq values to missing -999.99
    * even beyond what will be read.
    *--------------------------------------------------*/
   STRIPLINE (input_stream1); /* Skip the comment line. */
   STRIPLINE (input_stream1); /* Skip the comment line. */
   STRIPLINE (input_stream2); /* Skip the comment line. */              
   STRIPLINE (input_stream2); /* Skip the comment line. */
 
   ii = 0;
   kk = 0;
   sigma_sq = -999.990000000;

   while (!feof(input_stream1))
     { 
     fscanf (input_stream1, "%d%d", &ii, &kk);
     if (feof(input_stream1)) break;
 
     fscanf (input_stream1, "%f", &sigma_sq);
     if (feof(input_stream1)) break;
#if DEBUG
     printf ("(read - input_file1)sigma_sq[%d][%d] = %f\n", ii, kk, sigma_sq);
#endif

     /*------------------------------------------------
      * Write out data from file1 to new sigma sq file.
      *-----------------------------------------------*/
     fprintf (sigma_output_stream, "%5d %5d %f\n", ii, kk, sigma_sq);
     } /* data in file1 */

   kk = numstns_inp1; /* numbers start at 0 */ 

   while (!feof(input_stream2))
     {   
     fscanf (input_stream2, "%d%d", &index, &ii);
     if (feof(input_stream2)) break;
 
     fscanf (input_stream2, "%f", &sigma_sq);
     if (feof(input_stream2)) break;

#if DEBUG
     printf ("(read - input_file2) sigma_sq[%d][%d] = %f\n", index+kk, ii, sigma_sq);
#endif
 
     /*-----------------------------
      * Write out new sigma sq file.
      *----------------------------*/
     fprintf (sigma_output_stream, "%5d %5d %f\n", index+kk, ii, sigma_sq);
     } /* data in file2 */

   close_file (&input_stream1);
   close_file (&input_stream2);
   close_file (&sigma_output_stream);

   }  /* combine_sigsqfiles() */
