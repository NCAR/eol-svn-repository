#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * remove_hdr.c - This code removes the QCF hdrs from the input file.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.hdr'. This should be a list of files
 *         with the qcf format (i.e., *.0qc). 
 *
 * OUTPUT: File with hdr removed. (*.0qc.out or *.qcf.out)
 *         File containing stripped hdrs. (*.*.hdr)
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          both nominal and obs times. 
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
   FILE         *hdr_stream;

   int          items;
   int          j, i;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";
   char		hdr_output_file_name[MAX_CHARS] = "\0";

   char         new_line[MAX_CHARS] = "\0";

   /* 
    * Read each record in.
    */
   if (( data_stream3 = fopen("./filelist.hdr", "r")) == NULL)
      perror ("Error: Can't open filelist.hdr for reading");

   fscanf (data_stream3, "%s", input_file_name);

   while (!feof(data_stream3))
      {
      if ( feof(data_stream3))
         {
         if (fclose (data_stream1) == EOF)
            perror ("Can't close input stream.");
 
         if (fclose (data_stream2) == EOF)
            perror ("Can't close output_stream.");

         if (fclose (hdr_stream) == EOF)
            perror ("Can't close hdr output_stream.");

         break;
         }

      printf ("\nInput name was: %-s\n", input_file_name);

      sprintf (hdr_output_file_name, "%-s.hdr", input_file_name);
      printf ("Header Output file will be named: %-s\n", hdr_output_file_name);

      sprintf (output_file_name, "%-s.out", input_file_name);
      printf ("Output file will be named: %-s\n", output_file_name);
 
      if (( data_stream1 = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( data_stream2 = fopen(output_file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
 
      if (( hdr_stream = fopen(hdr_output_file_name, "w")) == NULL)
         perror ("Error: Can't open hdr output file.");

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
 
            if (fclose (hdr_stream) == EOF)
               perror ("Can't close hdr output_stream.");

            break;
            }


         /* printf ("\nnew_line: %-sxxx\n", new_line); */
         /* printf ("Check for Header!! network = %-sxxx\n", network);  */
        
         if (!strncmp("Nominal\0", &new_line[0], 7) ||
             !strncmp("Date\0", &new_line[0], 4)    ||
             !strncmp("*****\0", &new_line[0], 5) )            
            {
            /* printf ("It's a hdr!!\n");  */

            fprintf (hdr_stream, "%-s\n", new_line);
            }
         else
            {
            /*
             * This is not a hdr record. Write line to output unchanged.
             */
            fprintf (data_stream2, "%-s\n", new_line);
            }

         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stream3.");

   if (fclose (hdr_stream) == EOF)
      perror ("Can't close hdr_stream.");

   }  /* main() */
