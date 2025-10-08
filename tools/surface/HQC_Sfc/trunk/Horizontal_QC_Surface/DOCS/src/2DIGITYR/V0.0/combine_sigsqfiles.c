/*--------------------------------------------------------
 * combine_sigsqfiles - Combines two input sigma sq (variance)
 *     files into one output sigma sq file. This version of
 *     the combine s/w handles duplicates between the two
 *     input files.
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
 *  WARNING: Assumes that the second input file has the "best"
 *           values when duplicates occur between the two input files.
 *           THE VALUES FROM THE SECOND INPUT FILE WILL OVERWRITE
 *           VALUES FROM THE FIRST INPUT FILE when same stns
 *           occur in both input files.
 *
 *  Input:
 *     infile1 - file in sigma sq file format.
 *     infile2 - file in sigma sq file format.
 *
 *  Output:
 *     outfile - file in sigma sq file format. (NO DUPS)
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
#define   DEBUG 0

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() -  Controls main processing.
 *
 * 10 Oct 95 lec
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

   long int      ii, jjj = 0;
   int           kk,jj = 0;
   int           stn_no = -999;

   STRING27      stn_list1[MAXNUMSTNS];
   STRING27      stn_list2[MAXNUMSTNS];

   float sigma_sq1[MAXNUMSTNS][NUMQCPARMS]    = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}};

   float sigma_sq2[MAXNUMSTNS][NUMQCPARMS]    = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}};
   long          numstns_inp1     = 0;
   long          numstns_inp2     = 0;
   long          total_numstns_inp  = 0;
   char          junk_char; 
   char          current_stn[27] = "\0";
 

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
    * Read the input files and then write to output.
    *----------------------------------------------*/
   fscanf (input_stream1, "%ld", &numstns_inp1); STRIPLINE(input_stream1);
   fscanf (input_stream2, "%ld", &numstns_inp2); STRIPLINE(input_stream2);

   total_numstns_inp = numstns_inp1; /* Start off with this count */

#if DEBUG
   printf ("numstns_inp1: %ld\n", numstns_inp1);
   printf ("numstns_inp2: %ld\n", numstns_inp2);
   printf ("(orig) total_numstns_inp: %ld\n", total_numstns_inp);
#endif

   /*--------------------------------------------------
    * Initialize all sigma_sq values to missing -999.99
    * even beyond what will be read.
    *--------------------------------------------------*/
   for (ii=0;ii<MAXNUMSTNS;ii++)
      for (kk=0;kk<=NUMQCPARMS;kk++) 
         {
         sigma_sq1[ii][kk] = -999.990000000;
         sigma_sq2[ii][kk] = -999.990000000;
         }

   /*------------------------------------------
    * Read in the first input sigma file's values.
    * Let the values in the second input file
    * override this input file's values, if
    * duplicates occur.
    *-----------------------------------------*/
   for (ii=0;ii<numstns_inp1;ii++)
      {
      fscanf (input_stream1, "%d%1c", &jjj, &junk_char);
      if (feof(input_stream1))
         {
         printf ("WARNING(1): hit premature End Of File on %-s.\n", input_stream1);
         printf ("WARNING(1): Some additional searching will be required for sigmas.\n");
         }

      fgets (stn_list1[jjj], 27, input_stream1);

#if DEBUG
      printf ("(input_file1)stn_list1[%ld] = %-27sxxx\n", jjj, stn_list1[jjj]);
#endif
      } 

   STRIPLINE (input_stream1); /* Skip the comment line. */
   STRIPLINE (input_stream1); /* Skip the comment line. */

   while (!feof(input_stream1))
     {
     fscanf (input_stream1, "%ld%d", &ii, &jjj);
     if (feof(input_stream1)) break;
 
     fscanf (input_stream1, "%f", &sigma_sq1[ii][jjj]);
     if (feof(input_stream1)) break;

#if DEBUG
     printf ("(read - input_file1)sigma_sq1[%ld][%ld] = %f\n", ii, jjj, sigma_sq1[ii][jjj]);
#endif
     } /* data in file1 */


   /*-------------------------------------------------
    * Now read the stn list from the second file. 
    *------------------------------------------------*/
   for (ii=0;ii<numstns_inp2;ii++)
      {  
      fscanf (input_stream2, "%d%1c", &jjj, &junk_char);
      if (feof(input_stream2))
         {
         printf ("WARNING(2): hit premature End Of File on %-s.\n", input_stream2);
         printf ("WARNING(2): Some additional searching will be required for sigmas.\n");
         }

      fgets (stn_list2[jjj], 27, input_stream2);

#if DEBUG
      printf ("(input_file2)stn_list2[%ld] = %-27sxxx\n", jjj, stn_list2[jjj]);
#endif
      }  

   STRIPLINE (input_stream2); /* Skip the comment line. */
   STRIPLINE (input_stream2); /* Skip the comment line. */

   while (!feof(input_stream2))
     { 
     fscanf (input_stream2, "%d%d", &ii, &kk);
     if (feof(input_stream2)) break;
 
     fscanf (input_stream2, "%f", &sigma_sq2[ii][kk]);
     if (feof(input_stream2)) break;

#if DEBUG
     printf ("(input_file2)stn_list2[%ld] = %-27sxxx\n", ii, stn_list2[ii]);
     printf ("(read - input_file2)sigma_sq2[%ld][%d] = %f\n", ii, kk, sigma_sq2[ii][kk]);
#endif
     } /* data in file2 */


   /*-------------------------------------------------
    * Search for a matching stn already in the stn_list.
    * If found, then update the stn's sigma values.
    * If not found, just add this new entry to bottom
    * of list and increment the final total count.
    *------------------------------------------------*/
   for (ii=0;ii<numstns_inp2;ii++)
      {  
#if DEBUG
      printf ("\n(input_file2)stn_list2[%ld] = %-27sxxx\n", ii, stn_list2[ii]);
#endif
      strncpy (current_stn, stn_list2[ii], 27); 

#if DEBUG
      printf ("current_stn: xxx%-27sxxx.\n", current_stn);
#endif

      stn_no = -999;

      /*--------------------------------------
       * Add to stn_list if not already there.
       *--------------------------------------*/
      stn_no = determine_stn_no(stn_list1, &total_numstns_inp, current_stn);

#if DEBUG
      printf ("stn: %-27s has stn no: %d.\n", current_stn, stn_no);
#endif

      /*------------------------------------
       * Let this second set of sigma values
       * override the first set.
       *------------------------------------*/
      for (jj=0;jj<NUMQCPARMS;jj++)
        {
#if DEBUG
        printf ("sigma_sq1[%d][%d], sigma_sq2[%ld][%d]:: %f %f\n", stn_no, jj,ii,jj,
                 sigma_sq1[stn_no][jj], sigma_sq2[ii][jj]);
#endif
        if (sigma_sq2[ii][jj] > -990.00)
           sigma_sq1[stn_no][jj] =  sigma_sq2[ii][jj];

        } /* for */
   }   /* for ii < numstns_inp2 */

   /*------------------------------------------------
    * Write out data to new sigma sq file.
    *-----------------------------------------------*/
   fprintf (sigma_output_stream, "%5ld\n", total_numstns_inp);

   for (ii=0;ii<total_numstns_inp;ii++) 
      fprintf (sigma_output_stream, "%5ld %-27s\n", ii, stn_list1[ii]);

   fprintf (sigma_output_stream, "stn_no parm_no  sigma value\n");

   for (ii=0;ii<total_numstns_inp;ii++)
      {  
      for (kk=0;kk<NUMQCPARMS;kk++)
         if (sigma_sq1[ii][kk] > -990.00)
            fprintf (sigma_output_stream, "%5ld %5d %f\n", ii, kk, sigma_sq1[ii][kk]);
      }  
 
   close_file (&input_stream1);
   close_file (&input_stream2);
   close_file (&sigma_output_stream);

   }  /* combine_sigsqfiles() */
