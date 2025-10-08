#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * postproc_squallflg.c - This changes the squall indicator flag set
 *   improperly from "M" to " ". The only acceptable values for this
 *   flag are blank (for missing), S for Squall and G for Gust. 
 *   This problem was only noticed in the Handar data, but this s/w
 *   will change it for any network. If the M flag is found for a 
 *   network other than Handar and message will be written saying so.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.squal'. This should be a list of files
 *         with the qcf format (i.e., *.0qc or qcf).
 *
 * OUTPUT: File with updates. (*.0qc.out or *.qcf.out)
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          both nominal and obs times.  User must name
 *          file that contains list of files that will be processed
 *          to be named 'filelist.squal'.
 * 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 8 Mar lec
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
 * 000 08 Mar 95 lec
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
   long         rec_ct = 0;
   long         number_M = 0;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";

   char         new_line[MAX_CHARS] = "\0";
   char         updated_line[MAX_CHARS] = "\0";
   char         network[11] = "\0\0\0\0\0\0\0\0\0\0\0";

   /* 
    * Read each record in.
    */
   if (( data_stream3 = fopen("./filelist.squal", "r")) == NULL)
      perror ("Error: Can't open filelist.squal for reading");

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
         rec_ct++;

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

/*          printf ("\nnew_line: %-sxxx\n", new_line); */

         strncpy (network, &new_line[30], 10);

         if ( new_line[172] =='M')
            {
/*             printf ("Found an M at rec %ld!!\n", rec_ct); */

            number_M++;

            updated_line[172] = ' ';

            if ( strncmp("AWOSH\0", &new_line[30], 5) ) 
               printf ("Found M flag on Squall at rec %ld in file %-s in network: %-s\n", 
                       rec_ct, input_file_name, network);
            }

         fprintf (data_stream2, "%-s\n", updated_line);

         } /* while feof(data_stream1) */

      printf ("Located %ld  M's in file %-s\n\n", number_M, input_file_name);
      number_M = 0;

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stream3.");

   }  /* main() */
