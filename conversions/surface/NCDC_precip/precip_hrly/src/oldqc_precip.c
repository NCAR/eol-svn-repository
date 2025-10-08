#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * qc_precip.c - This code automates the Quality control processing 
 *   of the NCDC  Precipitation data. Each of the processing steps is
 *   described below.
 *
 *   (nohup qc_precip  qc_precip.inp &) >& qc_precip.log
 *
 * INPUT:  The qc_precip.inp file contains the max and min variances
 *         which are options for the pqc QC program. If the user
 *         wishes to use the default hourly values in the pqc
 *         program, then the .inp file should contain the line `0 0`.
 *         To specify the Bad Max value and the Dubious Min value,
 *         the .inp file should contain a line like the following:
 *         `-b 65.3 -d 15.4`. Typical values for hourly (mm/hr) data are:
 *         `-b 75.0 -d 25.0`. Typical values for 15 min (mm/15m) data are:
 *         `-b 40.0 -d 20.0`.
 *
 * OUTPUT: Final QC daily files are located in ../out/final/*.pqcf.
 *
 * BEWARE: This s/w and the system commands it executes assume a 
 *         specific directory layout! This s/w must be executed from 
 *         the exe (executable) area.
 *
 * 000 05 Oct 93 lec
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
 * 000 05 Oct 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *data_stream1;
   FILE         *data_stream2;

   int          items;
   int          j, i;

   static char	in_file_name[MAX_CHARS];
   static char  name[MAX_CHARS];
   static char  Boption[10];
   static char  Doption[10];

   printf ("\nEnter options for Precip QC program (pqc):\n");
   printf ("\nForm:  nn.nn  nn.nn; Enter 0 0 to use hrly defaults!\n");
   scanf ("%s %s", Boption, Doption);
   printf ("Current Options: -b %s -d %s.", Boption, Doption);

   printf ("\nBegin QC processing of NCDC Precip data!\n");

   printf ("Build control file to QC data into final PQCF format.\n");
   system ("ls ../out/final/*.0pqc > file_list"); 

   /*
    * Open the ascii input file_list for reading and QC.sh for writing.
    */
   if (( data_stream1 = fopen("file_list", "r")) == NULL)
      perror ("Error: Can't open file_list");

   if (( data_stream2 = fopen("QC.sh", "w")) == NULL)
      perror ("Error: Can't open QC.sh");


   /*
    * For each line in the file_list, write a line to convert.sh 
    * (e.g., pqc  ../out/*.0pqc).
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
  
      /*
       * Write command to QC.sh script. 
       */
      if (!strncmp(Boption, "0", 1) || !strncmp(Doption, "0", 1) )
         fprintf ( data_stream2, "pqc %s\n", name); 
      else
         fprintf ( data_stream2, "pqc -b %s -d %s %s\n", Boption, Doption, name); 

      } /* while */

   if (fclose (data_stream1) == EOF)
      perror ("Can't close data_stream1");

   if (fclose (data_stream2) == EOF)
      perror ("Can't close data_stream2");


   /*
    * Execute the conversion script to produce the
    * to convert *.0pqc format files into *.pqcf format files.
    * At this point, all data should exist in all.0pqc file.
    */
   printf ("Execute Quality Control script to produce ../out/all.pqcf files\n");
   system ("chmod +x QC.sh");   
   system ("QC.sh");   
   
   /*
    * If 15min data, check data for hrly stns (jac.c).  
    */

   /*
    * At this point data is still sorted by stn and 
    * then by time.  Resort the data by time and then
    * stn. Creates all.pqcf_sort from all.pqcf
    */
   system ("cosort pqcf_time_sort" );
   system ("/bin/mv  all.pqcf_sort   all.pqcf");


   /*
    * Divide data into day files.  
    */
   system ("nohup ebdayfs.sc hrly < runsplt.in &"); /* correct cmd?? */

   /*
    * Convert files to final Ebufr format.....Do cleanup??
    */


   printf ("\nQuality Control Processing of NCDC Precip is complete!\n");

   }  /* main() */
