#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*---------------------------------------------------------------------
 * divide_data.c - This code divides to input file according to the
 *   input specifications.
 *
 * INPUT : 
 *
 * OUTPUT: 
 *
 * BEWARE: This s/w has hardcoded values according to what is begin
 *   processed.
 *
 * 000  01 Jun 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>


/*
 * STRIPLINE is macro to move file pointer past next newline.
 */
#define STRIPLINE(file) {int c; while((c=getc(file))!='\n')if(c==EOF)break;}

#define MAX_CHARS 1500


/*----------------------------------------------------------------------
 *read_record() 
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
 * 000 01 Jun 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *input_stream; 
   FILE         *output_stream;

   char         input_file_name[MAX_CHARS] = "\0";
   char         output_file_name[MAX_CHARS] = "\0";

   int          j, i, k, ii;

   char         tmpstr[10]= "\0\0\0\0\0\0\0\0\0\0";
   char         new_line[MAX_CHARS];
   char         yr_str[3] = "\0\0\0";
   char         mo_str[3] = "\0\0\0";
   char         day_str[3] = "\0\0\0";


   printf ("\nInput name of input file:\n");
   printf ("\n(All data is expected in single input file)\n");
   scanf ("%s", input_file_name);


   /*
    * Open the ascii input/output files for reading. 
    * Convert data to qcf format.
    */
   if (( input_stream = fopen(input_file_name, "r")) == NULL) 
      perror ("Error: Can't open input_file");


   printf ("\nBegin processing data.\n");

   /*---------------------------------------------------
    * Sort data by date and time. 
    * Prepare to Divide data into seperate files
    * Data is now sorted by date and time and located in 
    * single file. Create daily output files with names
    * of the form: yymmdd.xxx.
    *--------------------------------------------------*/
   printf ("Divide data into seperate files!\n");

   strncpy (yr_str, "00",2);
   strncpy (mo_str, "00",2);
   strncpy (day_str, "00",2);

   while (!feof(input_stream))
      {
      read_record( &input_stream, new_line);

      if ( feof(input_stream) || !strncmp(new_line, "\0",1) 
          || !strncmp(new_line, "  ",2) ) 
         {
         if (fclose (output_stream) == EOF)
            perror ("Can't close output_stream");
         break;
         }

      /*
       * Create/open the output file and write data into it,
       * and then close the file after all data for that day/month
       * written out.
       */
#if 0
      if ( strncmp(day_str, &new_line[6], 2) ||
           strncmp(mo_str,  &new_line[3], 2) )   /*  -- set for day division hly precip & sao qcf*/

      if ( strncmp(day_str, &new_line[8], 2) ||
           strncmp(mo_str,  &new_line[5], 2) )   /*  -- set for day division dly COOP sum */
#endif

      if ( strncmp(mo_str, &new_line[5], 2) )    /* -- set for month division dly precip pqcf */
         {
         /*
          * We have a new day/mo. Close the previous file.
          */
#if 0
         if ( strncmp (day_str, "00", 2))      /* -- set for day division hly precip & sao qcf */
#endif

         if ( strncmp (mo_str, "00", 2))       /* -- set for mo division dly pqcf*/
            if (fclose (output_stream) == EOF)
               perror ("Can't close output_stream");

         output_file_name[0] = '\0';

#if 0
         strncpy (yr_str, &new_line[2],2);  /* -- set for day division of dly COOP */
         strncpy (mo_str, &new_line[5],2);
         strncpy (day_str, &new_line[8], 2); 
         sprintf (output_file_name, "./%-2s%-2s%-2s.dqc", yr_str, mo_str, day_str);  /* dly COOP */

         strncpy (yr_str, &new_line[0],2);  /* -- set for day division hly precip pqcf & sao qcf */
         strncpy (mo_str, &new_line[3],2);
         strncpy (day_str, &new_line[6], 2);
         sprintf (output_file_name, "./%-2s%-2s%-2s.qcf", yr_str, mo_str, day_str);  /* QCF format */

         sprintf (output_file_name, "./%-2s%-2s%-2s.pqc", yr_str, mo_str, day_str);  /* PQC format */
#endif

         strncpy (yr_str, &new_line[2],2); /* set for dly precip files */
         strncpy (mo_str, &new_line[5],2);  
         sprintf (output_file_name, "./%-2s%-2s.pqc", yr_str, mo_str); 

         if (( output_stream = fopen(output_file_name, "w")) == NULL)
            fprintf (stderr,"Divide into Months/Days Error: Can't open output_stream:: %-sxxx\n",
                     output_file_name); 
         } 
 
      fprintf (output_stream, "%-s\n", new_line);

      } /* while */

   if (fclose (input_stream) == EOF)
      perror ("Can't close input_stream");
 
   printf ("\nProcessing is complete!\n");
   }  /* main() */
