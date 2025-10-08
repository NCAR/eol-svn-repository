/*----------------------------------------------------------------------
 * remove_OKMESO() - This s/w removes only OKMESO data from
 *  data in QCF sfc format.
 *
 * Input: File to be stripped of OKMESO data
 *
 * Output: File stripped of all OKMESO data named *.noOK
 *
 * 26 May 95 lec
 *   Created.
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
 * main() - This is the main program which controls the basic
 *     processing and control.
 *
 * 26 May 95 lec
 *   Created.
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
    * Read each record in.
    */
   if (( data_stream3 = fopen("./filelist.noOK", "r")) == NULL)
      perror ("Error: Can't open filelist.noOK for reading");

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

      sprintf (output_file_name, "%-s.noOK", input_file_name);
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


         strncpy (network, &new_line[30], 10);

         /* printf ("\nnew_line: %-sxxx\n", new_line); */
         /* printf ("Check for OKMESO!! network = %-sxxx\n", network);  */
        
         if (strncmp("OKMESO\0", network, 6))
            {
            /* printf ("It's NOT OKMESO! Write it out!\n");  */

            fprintf (data_stream2, "%-s\n", new_line);
            }
         else
            {
            /*
             * Don't write out OKMESO data.
             */
            }

         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stresam3.");

   }  /* main() */
