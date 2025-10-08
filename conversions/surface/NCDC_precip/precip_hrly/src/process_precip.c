#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * process_precip.c - This code automates the processing of the NCDC 
 *   Precipitation data. Each of the processing steps has a quick 
 *   description in the s/w comments below. For a detailed description 
 *   of each step refer to detailed documentation. Both the data conversion
 *   and the quality control processing for the precip data is automated
 *   by this software.  To build this s/w, use the Makeprecip make file.
 *   Issue the following command to execute this s/w:
 *
 *   (nohup process_precip <process_precip.inp &) >& process_precip.log
 *
 * INPUT : select.sh - User must have previously created the input 
 *            select.sh script. See below for description of script
 *            contents.
 *
 *         process_precip.inp - This file has three lines. On the first line,
 *            this file contains a 0 if intermediate files are to retained
 *            or a 1 if all intermediate files are to be deleted during
 *            processing. The second line indicates the type of data to
 *            be processed. This line should contain the word '15min' if
 *            15 minute precip data is being processed. The second line
 *            should contain the word 'hourly' if hourly data is to be
 *            processed. If junk is on the second line, this program
 *            assumes hourly data is being processed.  The third line
 *            contains the max and min variances which are options for
 *            the pqc Quality Control program. If the user wishes to use
 *            the default hourly values in the pqc program, then the .inp 
 *            file should contain the line '0 0'.  To specify the Bad Max
 *            value and the Dubious Min value, the third line should 
 *            contain a line like the following:
 *            `65.3  15.4`. Typical values for hourly (mm/hr) data are:
 *            `75.0  25.0`. Typical values for 15 min (mm/15m) data are:
 *            `40.0  20.0`, where 40.0 is the bad max (-b 40.0) and 20.0
 *            is the dubious max (-d 20.0).
 *
 *            Sample process_precip.inp:
 *               1
 *               hourly
 *               75.0  25.0
 *
 *            In addition to the process_precip.inp file, process_percip 
 *            spawns several other processes that require specific
 *            input files/data.  Each input is described in comments
 *            near each spawn.
 *
 *
 * OUTPUT: Final QC daily files are located in ../out/final/*.pqcf.
 *         Use program convert_precip_to_ebufr.c to convert these 
 *         files in E-BUFR format: 
 *               ../out/final/*.ebufr.
 *
 * BEWARE: This s/w and the system commands it executes assume a 
 *         specific directory layout! This s/w must be executed from 
 *         the exe (executable) area.
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
   int          j, i;

   static char	in_file_name[MAX_CHARS];
   static char  name[MAX_CHARS];
   static char  name_substring[MAX_CHARS];
   static char  Boption[10];
   static char  Doption[10];
   
   char         data_type[7] = "      \0";       /* hourly or 15min */ 


   printf ("\nWould you like to delete intermediate files? (0=no;1=yes)\n");
   scanf ("%d", &delete_files);

   if (delete_files)
      printf ("Intermediate files will be DELETED!!!\n");
   else
      printf ("Intermediate files will be saved in the out area\n");

   printf ("\nEnter data_type: <hourly or 15min>\n");
   scanf ("%s", data_type);
   printf ("Current data_type is %-s.", data_type);

   printf ("\nEnter options for Precip QC program (pqc):\n");
   printf ("\nForm:  nn.nn  nn.nn; Enter 0 0 to use hrly defaults!\n");
   scanf ("%s %s", Boption, Doption);
   printf ("Current QC Options: -b %s -d %s.", Boption, Doption);
 
   printf ("\nBegin processing of NCDC Precip data!\n");

   /*
    * Each line in select.sh is of the form:
    * (This is an example of hourly.)
    *
    * './select -time "1992-03-16, 00:00:00": "1992-03-16,23:59:59" <
    *  ../inp/19920316_19920430.ncdc.ebufr > ../out/prcH_316.dat'
    */
   printf ("\nExecute select.sh script to select Time/Area Of Interest.\n");
   system ("./select.sh");

   printf ("Build script and control files to convert data to QCF format.\n");
   system ("ls ../out/*.dat > file_list"); 

   /*
    * Open the ascii input file_list for reading and convert.sh for writing.
    */
   if (( data_stream1 = fopen("file_list", "r")) == NULL)
      perror ("Error: Can't open file_list");

   if (( data_stream2 = fopen("convert.sh", "w")) == NULL)
      perror ("Error: Can't open convert.sh");


   /*
    * For each line in the file_list, write a line to convert.sh.
    * (15 Minute data is processed by ep2pq with the -f option.)
    * (e.g., ep2pq  ../out/prcH_316.dat ../out/final/prcH_316.0pqc).
    */
   while (!feof(data_stream1))
      {
      for (j=0; j<20; j++) name[j] = ' '; 
      for (j=0; j<20; j++) name_substring[j] = ' '; 
      name[0] = '\0';
      name_substring[0] = '\0';

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
  
      i =  0; 
      for(;;)
         {
         if (name[i]   == '.' && name[i+1] == 'd' && 
             name[i+2] =='a'  && name[i+3] == 't') break;

         name_substring[i] = name[i];
         i++;
         }
      name_substring[i] = '\0';  

      /*
       * Write command to convert.sh script. Convert *.dat to *.0pqc.
       */
      if (!strncmp(data_type, "15min",5))
         fprintf ( data_stream2, "./ep2pq -f %s %s.0pqc\n", name, name_substring); 
      else
         fprintf ( data_stream2, "./ep2pq  %s %s.0pqc\n", name, name_substring); 

      } /* while */

   if (fclose (data_stream1) == EOF)
      perror ("Can't close data_stream1");

   if (fclose (data_stream2) == EOF)
      perror ("Can't close data_stream2");


   /*
    * Execute the conversion script to produce the
    * *.0pqc files.
    */
   printf ("Execute conversion script to produce ../out/*.0pqc files\n");
   system ("chmod +x convert.sh");   
   system ("./convert.sh");   

   /*
    * Final *.0qc files are located in ../out area.
    */
   if (delete_files)
      {
      printf ("Removing *.dat, file_list, convert.sh files!\n");
      system ("/bin/rm  ../out/*.dat");
      system ("/bin/rm  file_list");
      system ("/bin/rm  convert.sh");
      }

   /*
    * Determine if there are any precip accumulation values
    * whose series begins before or ends after when data was
    * collected. Program jac2 will convert these series to
    * be missing data. Also note that jac2 drops days if they
    * contain only missing data. Jac2 assumes complete time
    * period is in single file and that data is sorted by 
    * lat/lon then time. Jac2 is currently limited on the 
    * size of the time period allowed.  If problems occur,
    * this program should be reevaluated.  
    *
    * The jac2 program requires a seperate input file
    * named jac2.inp. This file contains 4 values
    * on a single line. Those values in order are:
    *
    *     Number of month when data collection began.
    *     Day of first month that data collection began.
    *     Number of days collected in first month.
    *     Total number of datys collected. 
    *
    * Note: Accum data is identified by a code of 1. Typically,
    * the associated data is 0.00 mm. When the accumulation 
    * period is complete, a code of 2 will appear next to the 
    * total accum value.
    */
   printf ("cat ../out/*.0pqc > ../out/all.0pqc\n");   

   system ("cat ../out/*.0pqc > ../out/all.all");
   system ("/bin/rm ../out/*.0pqc");
   system ("/bin/cp ../out/all.all ../out/all.0pqc");

   printf ("Sort data by lat/lon.\n");   
   system ("cosort pqcf_sort"); /* all.0pqc to all.sort */

   system ("/bin/rm ../out/all.0pqc");

   printf ("Call jac2() to fix accum at begin and end.\n");   

   if (!strncmp(data_type, "15min",5))
      system ("./jac2 15min <../out/all.sort > ../out/all.0pqc" );
   else
      system ("./jac2 <../out/all.sort > ../out/all.0pqc" );

   if (delete_files)
      {
      printf ("Removing all.sort files!\n");
      system ("/bin/rm ../out/all.sort");
      }

   /*
    * Place stn info into the file. Note that the precip data
    * appears to be missing the stn identification.  We must
    * match the lat/lon with names in a file.  This program
    * updates the 15 min precip times.  It adds 15mins to each
    * each reading. This program must only be applied to NCDC 
    * Ebufr data because of this time manipulation.
    */
   printf ("Call snm program to place station names in 0pqc file.\n");
   system ("./snm ../out/all.0pqc");          /* all.0pqc to all.snam */

   if (delete_files)
      system ("/bin/rm ../out/all.0pqc");
      

   /*
    * If 15min data, check data for hrly stns (sck.c).
    * (sck infile). Creates all.snew from all.snam. ( was pqcf)
    * Must then rename .snew file back to old name.
    */
   if (!strncmp(data_type, "15min",5))
      {
      printf ("Check 15Min data for hourly stns. Create and execute script.\n");

      system ("/bin/rm  file_list");
      system ("ls ../out/*.snam > file_list");

      /* was:: system ("ls ../out/*.pqcf > file_list"); */

      /*
       * Open the ascii input file_list for reading and QC.sh for writing.
       */
      if (( data_stream1 = fopen("file_list", "r")) == NULL)
         perror ("Error: Can't open file_list");

      if (( data_stream2 = fopen("convert.sh", "w")) == NULL)
         perror ("Error: Can't open convert.sh");

 
      /*
       * For each line in the file_list, write a line to convert.sh
       * (e.g., sck  ../out/*.snam).
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
          * Write command to convert.sh script.
          */
         fprintf ( data_stream2, "./sck %s\n", name);   /* all.snam to all.snew */

         } /* while */

      if (fclose (data_stream1) == EOF)
         perror ("Can't close data_stream1");
      if (fclose (data_stream2) == EOF)
         perror ("Can't close data_stream2");


      system ("chmod +x convert.sh");
      system ("./convert.sh");

      /*
       * Move back to snam file, so next processing
       * step will function properly.
       */
      system ("/bin/rm ../out/all.snam");
      system ("/bin/mv ../out/all.snew ../out/all.snam");

      /* was:: system ("/bin/mv ../out/all.snew ../out/all.pqcf"); */

      }  /* if 15min data */


   printf ("Build control file to QC data into final PQCF format.\n");
   system ("/bin/rm  file_list");
   system ("ls ../out/*.snam > file_list");

   /*
    * Open the ascii input file_list for reading and QC.sh for writing.
    */
   if (( data_stream1 = fopen("file_list", "r")) == NULL)
      perror ("Error: Can't open file_list");
 
   if (( data_stream2 = fopen("QC.sh", "w")) == NULL)
      perror ("Error: Can't open QC.sh");
 
 
   /*
    * For each line in the file_list, write a line to convert.sh
    * (e.g., pqc  ../out/*.snam). The 'bad' and 'dubious' limit
    * values (if supplied in process_precip.inp) will be added to
    * each pqc call.  If no limit values are supplied, the 
    * program (pqc) defaults to hrly values.
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
         fprintf ( data_stream2, "./pqc %s\n", name);
      else
         fprintf ( data_stream2, "./pqc -b %s -d %s %s\n", Boption, Doption, name);
 
      } /* while */
 
   if (fclose (data_stream1) == EOF)
      perror ("Can't close data_stream1");
 
   if (fclose (data_stream2) == EOF)
      perror ("Can't close data_stream2");
 
   /*
    * Execute the conversion script to convert *.0pqc
    * format files into *.pqcf format files. At this point, 
    * all data should exist in all.0pqc file.
    */
   printf ("Execute Quality Control script to produce ../out/*.pqcf files\n");
   system ("chmod +x QC.sh");   /* all.snam to all.pqcf */
   system ("./QC.sh");

   if (delete_files)
      system ("/bin/rm ../out/all.snam");

   /* Create Station File */
 
   if (!strncmp(data_type, "15min",5))
     system( "./create_stationlist f ../out/all.pqcf" );
   else
     system( "./create_stationlist h ../out/all.pqcf" );


   /*
    * At this point data is still sorted by stn and
    * then by time.  Resort the data by time and then
    * stn. Creates all.pqcf_sort from all.pqcf
    */
   printf ("cosort pqcf_time_sort\n" ); 
   system ("cosort pqcf_time_sort" );    /* all.pqcf to all.pqcf_sort */
 

   if (delete_files)
      system ("/bin/rm  ../out/all.pqcf");


   /*
    * Divide data into day files.
    */
   printf ("Divide data into *.pqcf day files.\n");

   if (!strncmp(data_type, "15min",5))
      system ("./dayfs_15min < ../out/all.pqcf_sort"); 
   else
      system ("./dayfs_hourly < ../out/all.pqcf_sort"); 

   if (delete_files)
      {
      printf ("Deleting all.pqcf_sort, file_list, QC.sh\n");
      system ("/bin/rm  ../out/all.pqcf_sort"); 
      system ("/bin/rm   file_list");
      system ("/bin/rm   QC.sh");
      }

   printf ("\nProcessing (includes QC) of NCDC Precip is complete!\n");
   printf ("\nUse program convert_precip_to_ebufr to convert final files.!\n");

   }  /* main() */
