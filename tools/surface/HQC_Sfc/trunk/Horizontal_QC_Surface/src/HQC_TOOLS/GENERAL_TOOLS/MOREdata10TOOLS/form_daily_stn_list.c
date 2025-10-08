#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * form_stn_list.c - This code extracts the stn info (lat, lon, elev)
 *   directly from the input file. This version works on Daily
 *   Precip QC format.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.stn_list'. This should be a list of files
 *         with the pqcf format (i.e., *.pqcf). 
 *
 * OUTPUT: File with stn info.
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          both nominal and obs times. 
 * 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 04 May 94 lec
 *    Created from s/w that processed Hourly precip format.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS   1000

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
 * 000 24 Mar 94 lec
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
   char         network[11] = "\0\0\0\0\0\0\0\0\0\0\0";
   char         lat[11] = "\0\0\0\0\0\0\0\0\0\0\0";
   char         lon[12] = "\0\0\0\0\0\0\0\0\0\0\0\0";
   char         elev[8] = "\0\0\0\0\0\0\0";
   char         stn_name[16] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";

   /* 
    * Read each record in. 
    */
   if (( data_stream3 = fopen("./filelist.stn_list", "r")) == NULL)
      perror ("Error: Can't open filelist.stn_list for reading");

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

      sprintf (output_file_name, "%-s.stn_list", input_file_name);
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


         strncpy (network, &new_line[8], 10);

         /* printf ("Check for specific network = %-sxxx\n", network);  */
        
         strncpy( stn_name, &new_line[18], 15);
         strncpy( lat, &new_line[34], 10);
         strncpy( lon, &new_line[45], 11);

         fprintf (data_stream2, "%-10s %-15s %-10s %-11s\n",network, stn_name, lat, lon);

         } /* while feof(data_stream1) */

      fscanf (data_stream3, "%s", input_file_name);

      } /* while feof(data_stream3) */
 
   if (fclose (data_stream3) == EOF)
      perror ("Can't close data_stresam3.");

   }  /* main() */
