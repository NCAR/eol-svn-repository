#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * remove_Unkwn.c - This code removes all 'Unknown' stn data from the input file.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.Unkwn'. This should be a list of files
 *         with the pqcf format. This prob was in COOP data.
 *
 * OUTPUT: File with Unknwn removed. (*.pqcf.out)
 *         File containing stripped Unknown recs. (*.*.Unkwn)
 *
 * WARNING: Currently, s/w in Main is set to process file of specific format.
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 27 May lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS  1500

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
 * 000 27 May 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *data_stream1;
   FILE         *data_stream2;
   FILE         *data_stream3;
   FILE         *Unkwn_stream;

   int          items;
   int          j, i;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";
   char		Unkwn_output_file_name[MAX_CHARS] = "\0";

   char         new_line[MAX_CHARS] = "\0";
   char         junk_str1[MAX_CHARS] = "\0";
   char         junk_str2[MAX_CHARS] = "\0";

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

         if (fclose (Unkwn_stream) == EOF)
            perror ("Can't close Unkwn output_stream.");

         break;
         }

      printf ("\nInput name was: %-s\n", input_file_name);

      sprintf (Unkwn_output_file_name, "%-s.Unkwn", input_file_name);
      printf ("Header Output file will be named: %-s\n", Unkwn_output_file_name);

      sprintf (output_file_name, "%-s.out", input_file_name);
      printf ("Output file will be named: %-s\n", output_file_name);
 
      if (( data_stream1 = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( data_stream2 = fopen(output_file_name, "w")) == NULL)
         perror ("Error: Can't open output file.");
 
      if (( Unkwn_stream = fopen(Unkwn_output_file_name, "w")) == NULL)
         perror ("Error: Can't open Unkwn output file.");

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
 
            if (fclose (Unkwn_stream) == EOF)
               perror ("Can't close Unkwn output_stream.");

            break;
            }
   
         for (j=0;j<MAX_CHARS;j++) junk_str1[j]='\0';
         for (j=0;j<MAX_CHARS;j++) junk_str2[j]='\0';


         /* printf ("\nnew_line: %-sxxx\n", new_line); */
       
/*         if (!strncmp("Unknown\0", &new_line[28], 7)  )   /* use for dqcf - sum of day format */
       if (!strncmp("Unknown\0", &new_line[19], 7)  )  /* use for pqcf - ncdc coop pqcf format */
            {
            /* printf ("It's a Unknown!!\n"); */

            fprintf (Unkwn_stream, "%-s\n", new_line);
            }
         else
            {
            /*
             * This is not a Unkwn record. Write line to output unchanged.
             */
/*Add a space: strncpy ( junk_str1, new_line, 18);
               strncpy ( junk_str2, &new_line[18], 1000); */

            fprintf (data_stream2, "%-s\n", new_line);

/*Add a space: fprintf (data_stream2, "%-s %-s\n", junk_str1, junk_str2);  */
            }

         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stream3.");

   }  /* main() */
