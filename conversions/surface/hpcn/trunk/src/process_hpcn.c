#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * process_hpcn.c - This code automates the processing of the HPCN 
 *   observations. Each of the processing steps has a quick 
 *   description in the s/w comments below. For a detailed description 
 *   of each step refer to detailed documentation. To build this s/w,
 *   use the Makehpcn make file. Issue the following command to 
 *   execute this s/w as a background task:   
 *
 *   (nohup process_hpcn <process_hpcn.inp &) >& process_hpcn.log
 *
 * INPUT : hplainsqcf.inp - input file for hplainsqcf.f s/w. 
 *
 *         process_hpcn.inp - This file contains a 0 if intermediate
 *            files are to be retained or a 1 if all intermediate 
 *            files are to be deleted during processing.
 *
 * OUTPUT: Final daily files are located in ../out/final/*.0qc.
 *
 * BEWARE: This s/w and the system commands it executes assume a 
 *         specific directory layout! This s/w must be executed from 
 *         the exe (executable) area.
 *
 * 000 12 Jan 93 lec
 *    Created.
 * 001 1 Feb 93 lec
 *    Added current_yr variable and set it to 95 for GIST processing.
 *    Now final 0qc file names will contain the year (as they should).
 * 002 11 Mar 96 ds
 *    Added define for header size - the number of lines to skip at
 *       beginning of input file.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>


/*
 * STRIPLINE is macro to move file pointer past next newline.
 */
#define STRIPLINE(file) {int c; while((c=getc(file))!='\n')if(c==EOF)break;}

#define MAX_CHARS  1500
#define HEADERSIZE    3

/*----------------------------------------------------------------------
 *read_record() - This routine reads and returns to the caller one
 *   line from the specified input file.
 *
 * 000 09 Nov 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
void read_record( /*in/out*/ FILE       **data_stream,
                  /*out*/    char       new_line[MAX_CHARS])
   {
   int   j;
   FILE  *input_stream;
   char  c;
 
   input_stream = *data_stream;
 
   for (j=0;j<MAX_CHARS;j++) new_line[j]='\0';
 
   j = -1;
   while((c=getc(input_stream))!='\n' && j< MAX_CHARS)
      {
      if(c==EOF)break;
      new_line[++j] = c;
 
      } /* while */
 
   *data_stream = input_stream;
 
   } /* read_record() */
 
 

/*----------------------------------------------------------------------
 * main()
 *
 * 000 12 Jan 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *data_stream1;
   FILE         *input_stream;
   FILE         *output_stream;
   FILE         *data_stream3;

   int          items;
   int          delete_files = 0;
   int          jj, j, i;

   char		output_file_name[MAX_CHARS];
   char         name[MAX_CHARS];
   char         name_substring[MAX_CHARS];

   char         new_line[MAX_CHARS];
   char         day_str[3];
   char         current_yr[3] = "95\0"; /* gist */


   printf ("\nWould you like to delete intermediate files? (0=no;1=yes)\n");
   scanf ("%d", &delete_files);
 
   if (delete_files)
      printf ("Intermediate files will be DELETED!!!\n");
   else
      printf ("Intermediate files will be saved in the out area\n");


   printf ("\nBegin processing of HPCN data!\n");

   /*
    * Form a list of all the input data files. Input data files
    * will be stripped of their headers and each line will be 
    * prepended with the station number. Then all input files
    * may be concatenated into a single file without loss of 
    * information.
    */
   printf ("\nReformat and concatenate input files.\n");
   system ("ls ../inp/*.dat > file_list");

   /*
    * Open the ascii input file_list for reading and hplains.spatial for writing.
    */
   if (( data_stream1 = fopen("file_list", "r")) == NULL)
      perror ("Error: Can't open file_list");

   if (( output_stream = fopen("../out/hplains.spatial", "w")) == NULL)
      perror ("Error: Can't open ../out/hplains.spatial");

   printf ("files open!\n");

   /*
    * For each file listed in the file_list: open that file;
    * strip header info from file; write each line from file
    * to hplains.spatial file and prepend each line with the
    * station id. This will form a single file with all the
    * input data.
    */
   while (!feof(data_stream1))
      {
      for (j=0; j<20; j++) name[j] = '\0'; 
      for (j=0; j<20; j++) name_substring[j] = '\0'; 

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

         if (i>6)
            {
            name_substring[i-7] = name[i];   /* assumes '../inp/' prefix */
            }
         i++;
         }
      name_substring[i] = '\0';  

      /*
       * Open the input data file and preprocess data file, 
       * and then close the file.
       */
      if (( data_stream3 = fopen(name, "r")) == NULL)
         perror ("Error: Can't open data_stream3\n");


      /*
       * Assume each file has 3 lines of header information.
       */
      for (jj=0;jj<HEADERSIZE;jj++)
        STRIPLINE (data_stream3);

      /*
       * Now write out each line prepended by stn id.
       */
      while (!feof(data_stream3))
         {
         read_record( &data_stream3, new_line);

         if ( feof(data_stream3))
            break;

         fprintf (output_stream, "%-5s%-s\n", 
                  name_substring, new_line);
         }
 
      if (fclose (data_stream3) == EOF)
         perror ("Can't close data_stream3");

      } /* while */

   if (fclose (data_stream1) == EOF)
      perror ("Can't close data_stream1");

   if (fclose (output_stream) == EOF)
      perror ("Can't close output_stream");

   printf ("File reformat complete!\n");


   printf ("Convert any missing values from *** to 99.99.\n");
   system ("fix_missing.sh");    /* hplains.spatial to hplains.clean */
                    
   if (delete_files)
      {
      printf ("Removing file_list, hplains.spatial files!\n");
      system ("/bin/rm  file_list");
      system ("/bin/rm  ../out/hplains.spatial");
      }

   printf ("Run conversions\n");
   system ("hplainsqcf < hplainsqcf.inp");  /* hplains.spatial to hplains.cvt */

   printf ("Spatially sort the data.\n");
   system ("cosort qcf_sort_spec");         /* hplains.time to hplains.sort    */
                                            /* Sort by nominal date/time and then lat/lon */

   if (delete_files)
      {
      printf ("Removing hplains.clean hplains.cvt files!\n");
      system ("/bin/rm  ../out/hplains.clean");
      system ("/bin/rm  ../out/hplains.cvt");
      system ("/bin/rm  ../out/hplains.drop");
      system ("/bin/rm  ../out/hplains.dups");
      }

   printf ("Build day files from hourly data\n");

   if (( input_stream = fopen("../out/hplains.sort", "r")) == NULL)
      perror ("Error: Can't open hplains.sort");
 
   strncpy (day_str, "00",2);
 
   while (!feof(input_stream))
      {
      read_record( &input_stream, new_line);
 
      if ( feof(input_stream) || !strncmp(new_line, "\0",1)
          || !strncmp(new_line, "  ",2) )
         {
         if (fclose (output_stream) == EOF)
            perror ("Can't close output_stream");
         break;
         }
  
      /*
       * Create/open the qcf file and write data into it,
       * and then close the file after all data for that day
       * written out.
       */
      if ( strncmp(day_str, &new_line[6], 2) )
         {
         /*
          * We have a new day. Close the previous day's file.
          */
         if ( strncmp (day_str, "00", 2))
            if (fclose (output_stream) == EOF)
               perror ("Can't close output_stream");
 
         output_file_name[0] = '\0';
 
         sprintf (output_file_name, "../out/final/hplains_%-2s%1c%1c%1c%1c.0qc",
                  current_yr, new_line[3], new_line[4], new_line[6], new_line[7]);
 
 
         if (( output_stream = fopen(output_file_name, "w")) == NULL)
            perror ("Error: Can't open output_stream");
  
         strncpy (day_str, &new_line[6], 2);
         }
  
      fprintf (output_stream, "%-s\n", new_line);
 
      } /* while */
 
   if (fclose (input_stream) == EOF)
      perror ("Can't close input_stream");
 
 
   if (delete_files)
      {
      printf ("Removing hplains.sort file\n");
      system ("/bin/rm ../out/hplains.sort");
      }

   printf ("\nProcessing of HPCN data is complete!\n");

   }  /* main() */
