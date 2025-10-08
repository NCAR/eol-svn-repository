/*----------------------------------------------------------------------
 * changenum.c - adds input number to first number in input file.
 *  Input files expected in particular format. (See output from
 *  grepit121 scripts for gnuplotting HQC variables.)
 *
 * OUTPUT: File with dumped data (dump.out)
 *
 * 000 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS  25  /* was 256 for qcf */ 

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/

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
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   FILE         *data_stream1;
   FILE         *data_stream2;

   int          items;
   int          j, i;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";

   char         new_line[MAX_CHARS] = "\0";

   char         number_to_add_str[7] = "\0\0\0\0\0\0\0";
   int          number_to_add = 0;
   int          end_rec = 0;
   int          rec_count = 0;
   char         rest_of_line[20]="\0";
   float        num = 0.0;
   char         num_str[6]= "\0\0\0\0\0\0";

   /*----------------------------------
    * Expect input on the command line.
    *---------------------------------*/
   if (argc != 3)
      {  
      printf ("Usage: changenum <input_file> <num_to_add (e.g., 4800)>\n");
      exit(1);
      }  

   strcpy (input_file_name, argv[1]);

   strcpy (number_to_add_str, argv[2]);
   number_to_add = atoi(number_to_add_str);


   printf ("\nOriginal input name was: %-s\n", input_file_name);

   sprintf (output_file_name, "dump.out");
   printf ("Output file will be named: dump.out\n");
 
   if (( data_stream1 = fopen(input_file_name, "r")) == NULL)
      perror ("Error: Can't open input file");
 
   if (( data_stream2 = fopen(output_file_name, "w")) == NULL)
      perror ("Error: Can't open output file.");
 
   rec_count = 0;
      

   while (!feof(data_stream1))
      {
      printf ("read_rec\n");
      read_record( &data_stream1, new_line);
      printf ("read_rec: new_line = %-s\n", new_line);

      if ( feof(data_stream1) || !strncmp(new_line, "\0",1))
         {
         if (fclose (data_stream1) == EOF)
            perror ("Can't close input stream.");

         if (fclose (data_stream2) == EOF)
            perror ("Can't close output_stream.");
         break;
         }

      strncpy (num_str, &new_line[0], 5);
      strncpy (rest_of_line, &new_line[5], 15);

      num_str[5] = '\0';
      num = atof(num_str);
      num = (num + number_to_add)/100.00;

      rec_count++;

      printf ("num_str, num: xxx%-sxxx, %5f\n", num_str, num);
      printf ("output: %5.2f%-s\n", num, rest_of_line);
      fprintf (data_stream2, "%5.2f%-s\n", num, rest_of_line);
      } /* while feof(data_stream1) */

   }  /* main() */
