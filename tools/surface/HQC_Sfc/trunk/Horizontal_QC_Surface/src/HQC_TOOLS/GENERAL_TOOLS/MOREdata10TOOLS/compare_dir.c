/*----------------------------------------------------------------------
 * compare_dir.c - This code 
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.txt'. 
 *
 * OUTPUT: 
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          data in specific format. 
 *          Also stn to mod is hardcoded in Main. User must name
 *          file that contains list of files that will be processed
 *          to be named 'filelist.txt'.
 * 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 28 Jun lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS 500

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
 * 000 28 Jun 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *out_stream;
   FILE         *input_data_stream1;
   FILE         *input_data_stream2;

   int          items;
   int          j, i;
   int          count = 0;
   long int     rec_count = 0;

   char		input_file_name[MAX_CHARS] = "\0";

   char         newline[MAX_CHARS] = "\0";
   char         updated_line[MAX_CHARS] = "\0";
   char         stn_id[11] = "\0\0\0\0\0\0\0\0\0\0\0";

   char         junk_str[MAX_CHARS];
   char         junk_str2[MAX_CHARS];



   /* 
    * Read each record in.
    */
   if (( input_data_stream1 = fopen("./filelist.txt", "r")) == NULL)
      perror ("Error: Can't open filelist.txt for reading");

   fscanf (input_data_stream1, "%s", input_file_name);

   while (!feof(input_data_stream1))
      {
      if ( feof(input_data_stream1))
         {
         if (fclose (out_stream) == EOF)
            perror ("Can't close output_stream.");

         break;
         }

      printf ("\nInput name was: %-s\n", input_file_name);

      if (( input_data_stream2 = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( out_stream = fopen("dirfile.out", "w")) == NULL)
         perror ("Error: Can't open output file.");

      printf ("Begin processing data\n");

      while (!feof(input_data_stream2))
         {
         for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';

         read_record( &input_data_stream2, newline);

         if ( feof(input_data_stream2))
            {
            if (fclose (input_data_stream2) == EOF)
               perror ("Can't close input stream.");

            if (fclose (out_stream) == EOF)
               perror ("Can't close output_stream.");

            break;
            }

         rec_count++;
         count++;

/*          printf ("\nnewline: %-sxxx\n", newline); */

         /*
          * Write out station recs.
          */ 
         if ( strncmp (newline, "                         ", 25) &&
              strncmp (newline, "\n",1) &&
              strncmp (newline, "\0",1) &&
              strncmp (newline, "d",1) &&
              strncmp (newline, "/",1) &&
              strncmp (newline, "total",5) )
            { 
            for (j=0;j<MAX_CHARS;j++)junk_str[j] = '\0';
            for (j=0;j<MAX_CHARS;j++)junk_str2[j] = '\0';

            /*
             * Pick out data write to output files.
             */
            strncpy ( junk_str, &newline[19], 12); /* size */
            strncpy ( junk_str2, &newline[45], 13); /*name */

            fprintf( out_stream,
                   "%-20s %-20s\n",
                   junk_str, junk_str2);
   
            }

         } /* while feof(input_data_stream2) */

      fscanf (input_data_stream1, "%s", input_file_name);

      } /* while feof(input_data_stream1) */
 
   if (fclose (input_data_stream1) == EOF)
      perror ("Can't close input_data_stream1.");

   printf ("Number of recs processed: %d\n", rec_count);
   }  /* main() */
