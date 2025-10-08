#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*---------------------------------------------------------------------
 * statprecip.c - This code summarizes the number of each quality
 *   code type in the input data.
 *
 *   Issue the following command to execute this s/w:
 *   statprecip
 *
 * INPUT : 
 *         Name of input file (daily precip format).
 *
 * OUTPUT: Final output files are located where ever program executed.
 *         File is named 'precip.stats'.
 *
 * 000  19 Apr 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS 1000


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
 * 000 19 Apr 94 lec
 *    Created.
 * 000 04 Apr 94 lec
 *    Updated to now handle Trace flags seen in NCDC COOP data.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *input_stream; 
   FILE         *output_stream;

   char         input_file_name[256] = "\0";
   char         output_file_name[256] = "\0";

   int          j, i, k, ii;
   long int     rec_count = 0;

   static char  new_line[MAX_CHARS];
   char         qcflag = '\0';

   long         ct_all = 0;
   long         ct_excess = 0;
   long         ct_miss = 0;
   long         ct_bad = 0;
   long         ct_dub = 0;
   long         ct_good = 0;
   long         ct_unchecked = 0;
   long         ct_notmeas = 0;
   long         ct_glitch = 0;
   long         ct_estimated = 0;
   long         ct_trace = 0;

   printf ("\nInput name of input file:\n");
   printf ("\n(All data is expected in single input file)\n");
   scanf ("%s", input_file_name);

   strcpy(output_file_name, "precip.stats");
 
   /*
    * Open the ascii input/output files for reading. 
    */
   if (( input_stream = fopen(input_file_name, "r")) == NULL) 
      perror ("Error: Can't open input_file");

   if (( output_stream = fopen(output_file_name, "w")) == NULL)
      perror ("Error: Can't open output_file");


   /*
    * Process all data. 
    */
   while (!feof(input_stream))
      {
      for (k=0;k<MAX_CHARS;k++) new_line[k] = '\0';

      read_record( &input_stream, new_line);

      if (feof(input_stream)) break;
      ++rec_count;

      /*
       * This is a fixed format record length.
       * We know exactly where the QC flags, etc
       * are located! Every record has 31 days
       * regardless of actual month.
       */
/*       printf ("new_line: %-sxxx\n", new_line); */

      for (j=0;j<=30;j++)
          {
          qcflag = new_line[72+j*15];
/*           printf ("j, qcflag: %d %1c\n", j, qcflag); */

          ct_all++;

          switch (qcflag)
             {
             case 'G' :
               ++ct_good;
               break;
             case 'D' :
               ++ct_dub;  
               break;
             case 'B' :
               ++ct_bad; 
               break;
             case 'U' :
               ++ct_unchecked;
               break;
             case 'N' :
               ++ct_notmeas;
               break;
             case 'X' :
               ++ct_glitch;
               break;
             case 'E' :
               ++ct_estimated;
               break;
             case 'M' :
               ++ct_miss;
               break;
             case 'C' :
               ++ct_excess;
               ++ct_miss;
               break;
             case 'T' :
               ++ct_trace; /* These vals are good, but less than .01 inches  */
               break;
             default:
               printf ("Error: Bad qcflag! qcflag is %1c\n", qcflag);
               break;
             } /* switch */
          } /* for */
      } /* while */


   /*
    * Write final stats to output file
    * and close all files.
    */
   fprintf (output_stream, "\n\nProcessing data from input file: %-s", input_file_name);
   fprintf (output_stream, "\n\nOutput file name: %-s", output_file_name);
   fprintf (output_stream, "\n\nTotal number of input recs processed: %ld", rec_count);

   fprintf (output_stream, "\n\nTotal Bad values: %ld\n", ct_bad);
   fprintf (output_stream, "Total dubious values: %ld\n", ct_dub);
   fprintf (output_stream, "Total good values: %ld\n", ct_good);
   fprintf (output_stream, "Total trace values: %ld\n", ct_trace);
   fprintf (output_stream, "Total good plus trace values: %ld\n", ct_good+ct_trace);

   fprintf (output_stream, "\nTotal excessive values (>9999.99mm OR <0): %ld\n", ct_excess);
   fprintf (output_stream, "\nTotal missing values(inclds excessives): %ld\n", ct_miss);

   fprintf (output_stream, "Total Unchecked values: %ld\n", ct_unchecked);
   fprintf (output_stream, "Total Not Measured values: %ld\n", ct_notmeas);
   fprintf (output_stream, "Total Glitch values: %ld\n", ct_glitch);
   fprintf (output_stream, "Total Estimated values: %ld\n", ct_estimated);

   fprintf (output_stream, "\n\nTotal number values processed: %ld\n", ct_all);
   
   fprintf (output_stream, "\n\n\nPercentages INCLUDING Missing values::\n");

   fprintf (output_stream, "\nPercent Good values:    %f\n", 100.0*((float )ct_good/(float )ct_all));
   fprintf (output_stream, "Percent Trace values:    %f\n", 100.0*((float )ct_trace/(float )ct_all));
   fprintf (output_stream, "Percent Good plus Trace values:    %f\n", 
            100.0*((float )(ct_good+ct_trace)/(float )ct_all));

   fprintf (output_stream, "\nPercent Bad values:    %f\n", 100.0*((float )ct_bad/(float )ct_all));
   fprintf (output_stream, "Percent Dubious values:    %f\n", 100.0*((float )ct_dub/(float )ct_all));
   fprintf (output_stream, "Percent Missing(inclds excessives) values:    %f\n", 
            100.0*((float )ct_miss/(float )ct_all));
   fprintf (output_stream, "Percent Excessive (>9999.99mm OR <0) values:    %f\n", 
            100.0*((float )ct_excess/(float )ct_all));

   fprintf (output_stream, "\nPercent Unchecked values:    %f\n", 
            100.0*((float )ct_unchecked/(float )ct_all));
   fprintf (output_stream, "Percent Not Measured values:     %f\n", 
            100.0*((float )ct_notmeas/(float )ct_all));   
   fprintf (output_stream, "Percent Glitch values:    %f\n", 
            100.0*((float )ct_glitch/(float )ct_all));
   fprintf (output_stream, "Percent Estimated values:    %f\n", 
            100.0*((float )ct_estimated/(float )ct_all));

   fprintf (output_stream, "\n\n\nPercentages EXCLUDING Missing values::\n");
 
   fprintf (output_stream, "\nPercent Good values:    %f\n", 
            100.0*( (float )ct_good/((float )(ct_all-ct_miss)) ) );
   fprintf (output_stream, "Percent Trace values:    %f\n",
            100.0*( (float )ct_trace/((float )(ct_all-ct_miss)) ) );

   fprintf (output_stream, "Percent Good plus Trace values:    %f\n",
            100.0*( (float )(ct_good+ct_trace)/((float )(ct_all-ct_miss)) ) );
   fprintf (output_stream, "(     NOTE: trace values are good values of less than 0.01 inches.)\n");

   fprintf (output_stream, "\nPercent Bad values:    %f\n", 
            100.0*((float )ct_bad/((float )(ct_all-ct_miss))));
   fprintf (output_stream, "Percent Dubious values:    %f\n", 
            100.0*((float )ct_dub/((float )(ct_all-ct_miss))));
 
   fprintf (output_stream, "\nPercent Unchecked values:    %f\n",
            100.0*((float )ct_unchecked/((float )(ct_all-ct_miss))));
   fprintf (output_stream, "Percent Not Measured values:     %f\n",
            100.0*((float )ct_notmeas/((float )(ct_all-ct_miss))));
   fprintf (output_stream, "Percent Glitch values:    %f\n",
            100.0*((float )ct_glitch/((float )(ct_all-ct_miss))));
   fprintf (output_stream, "Percent Estimated values:    %f\n",
            100.0*((float )ct_estimated/((float )(ct_all-ct_miss))));

   if (fclose (input_stream) == EOF)
      perror ("Can't close input_stream");

   if (fclose (output_stream) == EOF)
      perror ("Can't close output_stream");

   printf ("\nProcessing of DAILY PRECIP is complete!\n");

   }  /* main() */
