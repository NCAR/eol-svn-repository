#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * look_at_qcf.c - This code dumps qcf data in the following ways:
 *  1) Dumps specified number of records beginning at specified
 *     record number.
 *
 * INPUT : List of files to be processed in a file named :
 *         'file_list.txt'. This should be a list of files
 *         with the qcf format (i.e., *.qcf).
 *         User is prompted for beginning rec number and number
 *         of recs to be dumped.
 *
 * OUTPUT: File with dumped data (dump.out)
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          both nominal and obs times.  Check location of vars in qcf recs.
 *
 * 000 09 Mar 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS  256  /* was 256 for qcf ; 700 for precip?*/ 

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
 * 000 09 Mar 94 lec
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

   int          beginning_rec = 0;
   int          number_of_recs = 0;
   int          end_rec = 0;
   int          rec_count = 0;



   /* 
    * Read each record in. If rec is within requested recs,
    * then write to output file. Do nothing for dropped
    * if outside area of interest.
    */
   if (( data_stream3 = fopen("./file_list.txt", "r")) == NULL)
      perror ("Error: Can't open file_list.txt for reading");

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

      printf ("\nOriginal input name was: %-s\n", input_file_name);

      sprintf (output_file_name, "dump.out");
      printf ("Output file will be named: dump.out\n");
 
      if (( data_stream1 = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( data_stream2 = fopen(output_file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
 
      rec_count = 0;
      
      printf ("Enter beginning rec number:\n");
      scanf ("%d", &beginning_rec);
      printf ("Enter number of recs to process:\n");
      scanf ("%d", &number_of_recs); 
      end_rec = beginning_rec + number_of_recs;

      printf ("beg_rec, num_recs, end_rec: %d %d %d\n",
               beginning_rec, number_of_recs, end_rec);
 

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

         if ( rec_count > end_rec) break;

         if ( rec_count >= beginning_rec && rec_count < end_rec)
            {
            fprintf (data_stream2, "%-s\n", new_line);
            }
         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stresam3.");

   }  /* main() */
