#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * match_lle.c - This code updates the elevation in every
 *   NCDC record. It truncates the elevation to meters so that
 *   the data will match with the FEST elevation values.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.ncdcsao'. This should be a list of files
 *         with the qcf format (i.e., *.0qc). Only the records
 *         with the network shown as "NCDC" will be updated.
 *
 * OUTPUT: File with updates. (*.0qc.out or *.qcf.out)
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          both nominal and obs times. 
 *          Also stn to mod is hardcoded in Main. User must name
 *          file that contains list of files that will be processed
 *          to be named 'filelist.ncdcsao'.
 * 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 16 Mar lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS   256

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
 * 000 22 Feb 94 lec
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

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";

   char         new_line[MAX_CHARS] = "\0";
   char         updated_line[MAX_CHARS] = "\0";
   char         network[11] = "\0\0\0\0\0\0\0\0\0\0\0";

   /* 
    * Read each record in. Only update NCDC. 
    */
   if (( data_stream3 = fopen("./filelist.ncdcsao", "r")) == NULL)
      perror ("Error: Can't open filelist.ncdcsao for reading");

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


         for (i=0; i<MAX_CHARS; i++)
            updated_line[i] = new_line[i];

         /* printf ("\nnew_line: %-sxxx\n", new_line); */

         strncpy (network, &new_line[30], 10);

         /* printf ("Check for NCDC!! network = %-sxxx\n", network);  */
        
         if (!strncmp("NCDC\0", network, 4))
            {
            /* printf ("It's NCDC!!\n");  */

            /*
             * Update only NCDC records. Truncate the
             * elevation to meters to match FEST data.
             */
            strncpy( &updated_line[84], "00", 2);

            /* printf ("upd_line: %-sxxx\n", updated_line); */
            fprintf (data_stream2, "%-s\n", updated_line);
            }
         else
            {
            /*
             * This is not an NCDCSAO record. Write line to output unchanged.
             */
            fprintf (data_stream2, "%-s\n", new_line);
            }

         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stresam3.");

   }  /* main() */
