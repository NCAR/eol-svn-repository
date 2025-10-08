#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * preproc_hrly - This code strips all USGS precip stations from the 
 *   input file. This program is applied to the GIDS1_HrlyPrcpCmp.pqcf
 *   file to remove the hourly USGS precip stns. This prevents duplicates
 *   of USGS hrly and daily records for the same stn. Since the hourly
 *   precip composite is converted to daily records by the hrly2daily
 *   program, the hrly USGS precip data become daily records which already
 *   exist in the daily USGS data. All of the is data is combined to 
 *   form the daily precip composite.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.txt'. This should be a list of files
 *         with the hourly pqcf format (i.e., *.pqcff). 
 *
 * OUTPUT: File with updates. (*.pqcf.out)
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          data in hourly pqcf format. 
 *          Also network to mod is hardcoded in Main. User must name
 *          file that contains list of files that will be processed
 *          to be named 'filelist.txt'.
 * 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 29 Apr lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS   500

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
 * 000 29 Apr 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *data_stream1;
   FILE         *data_stream2;
   FILE         *data_stream3;

   int          items;
   int          j, i;
   int          count = 0;
   long int     rec_count = 0;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";

   char         new_line[MAX_CHARS] = "\0";
   char         updated_line[MAX_CHARS] = "\0";
   char         stn_id[11] = "\0\0\0\0\0\0\0\0\0\0\0";

   /* 
    * Read each record in.
    */
   if (( data_stream3 = fopen("./filelist.txt", "r")) == NULL)
      perror ("Error: Can't open filelist.txt for reading");

   fscanf (data_stream3, "%s", input_file_name);

   while (!feof(data_stream3))
      {
      if ( feof(data_stream3))
         {
         if (fclose (data_stream1) == EOF)
            perror ("Can't close input stream.");
 
         if (fclose (data_stream2) == EOF)
            perror ("Can't close output_stream.");
         break;
         }

      printf ("\nInput name was: %-s\n", input_file_name);

      sprintf (output_file_name, "%-s.out", input_file_name);
      printf ("Output file will be named: %-s\n", output_file_name);
 
      if (( data_stream1 = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( data_stream2 = fopen(output_file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
 

      while (!feof(data_stream1))
         {
         read_record( &data_stream1, new_line);

         if ( feof(data_stream1) || !strncmp(new_line, "\0",1)
             || !strncmp(new_line, "  ",2) )
            {
            if (fclose (data_stream1) == EOF)
               perror ("Can't close input stream.");

            if (fclose (data_stream2) == EOF)
               perror ("Can't close output_stream.");
            break;
            }

         rec_count++;

         /*printf ("\nnew_line: %-sxxx\n", new_line); */

         for (i=0; i<MAX_CHARS; i++)
            updated_line[i] = new_line[i];

         if (!strncmp("USGS\0", &new_line[18], 4) )
            {
            /*
             * Strip out only USGS network records. 
             */
            count++;
            }
         else
            {
            /*
             * This is not a USGS record. Write line to output unchanged.
             */
            fprintf (data_stream2, "%-s\n", new_line);

            } /* Unknown */

         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stream3.");

   printf ("Number of recs processed: %d\n", rec_count);
   printf ("Number of USGS recs stripped: %d\n", count);
   }  /* main() */
