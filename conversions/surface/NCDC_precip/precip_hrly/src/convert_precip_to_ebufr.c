#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * convert_precip_to_ebufr - This code automates the conversion of the 
 *   Precipitation data from pqcf format to E-BUFR format. Each of the 
 *   processing steps has a quick description in the s/w comments below.
 *   For a detailed description of each step refer to detailed documentation.
 *   To build this s/w, use the Makeconvert make file. Issue the following
 *   command to execute this s/w:
 *
 *   (nohup convert_precip_to_ebufr <convert_precip_to_ebufr.inp &) >& convert.log
 *
 *         convert_precip_to_ebufr.inp - This file has one line. On this line,
 *            the file contains a 0 if intermediate files are to retained
 *            or a 1 if all intermediate files are to be deleted during
 *            processing. 
 *
 * OUTPUT: Final converted 'daily' files are located in ../out/final/*.ebufr.
 *
 * BEWARE: This s/w and the system commands it executes assume a 
 *         specific directory layout! This s/w must be executed from 
 *         the exe (executable) area. This s/w assumes that the files
 *         to be converted are named: ../out/final/*.pqcf.
 *
 * 000 04 Oct 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS   256

/*----------------------------------------------------------------------
 * main()
 *
 * 000 04 Oct 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *data_stream1;
   FILE         *data_stream2;

   int          items;
   int          delete_files = 0;
   int          j;

   static char	in_file_name[MAX_CHARS];
   static char  name[MAX_CHARS];
   

   printf ("\nWould you like to delete intermediate files? (0=no;1=yes)\n");
   scanf ("%d", &delete_files);

   if (delete_files)
      printf ("Intermediate files will be DELETED!!!\n");
   else
      printf ("Intermediate files will be saved in the out area\n");


   /*
    * Convert files to final Ebufr format.
    * Call program p2e. Program p2e requires many 
    * files to perform its processing. These files are:
    *
    * class_file, control_pqcf.txt, desc_file, fest_sites_sorted,
    * and several code_*_* files (e.g., code_08_021, code_08_255,
    * code_11_255, code_20_003, code_20_011, code_20_254, 
    * code_31_192, code_33_254, code_33,255).
    */
   printf ("Convert *.pqcf day files into final E-BUBF format.\n");
   system ("ls ../out/final/*.pqcf > file_list");   

   /*
    * Open the ascii input file_list for reading and new convert.sh for writing.
    */
   if (( data_stream1 = fopen("file_list", "r")) == NULL)
      perror ("Error: Can't open file_list");
 
   if (( data_stream2 = fopen("convert.sh", "w")) == NULL)
      perror ("Error: Can't open convert.sh");
 
   /*
    * For each line in the file_list, write a line to convert.sh
    * (e.g., p2e  ../out/final/*.pqcf).
    */
   while (!feof(data_stream1))
      {
      for (j=0; j<20; j++) name[j] = ' ';
      name[0] = '\0';
 
      items = fscanf (data_stream1, "%s", name);
      if (items != 1)
         {
         if ( !feof(data_stream1))
            fprintf (stderr,
              "Error: Read %d of first item of data from data_stream1!\n",
              items);
         else
            break;
         }
 
      fprintf ( data_stream2, "p2e %s\n", name);
 
      } /* while */
 
   if (fclose (data_stream1) == EOF)
      perror ("Can't close data_stream1");
 
   if (fclose (data_stream2) == EOF)
      perror ("Can't close data_stream2");
 
   /*
    * Execute the conversion script to produce the
    * to convert final ebufr format files.
    */
   printf ("Execute script to produce ../out/final/*.ebufr files\n");
   system ("chmod +x convert.sh");
   system ("convert.sh");
 
   if (delete_files)
      {
      printf ("\nDeleting file_list, convert.sh, QC.sh, etc.!\n");
      system ("/bin/rm file_list");
      system ("/bin/rm QC.sh");
      system ("/bin/rm convert.sh");
      }

   printf ("\nConversion of NCDC Precip files is complete!\n");

   }  /* main() */
