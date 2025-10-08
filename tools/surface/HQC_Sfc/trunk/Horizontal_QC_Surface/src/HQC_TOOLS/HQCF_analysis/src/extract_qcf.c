/*----------------------------------------------------------------------
 * extract_qcf.c - This code extracts qcf data in the following ways:
 *  1) Dumps specified number of records beginning at specified
 *     data time where data time is the first time in the data record.
 *
 * INPUT :
 *         User is prompted for name of qcf file to be processed.
 *         User is prompted for beginning and ending data times.
 *         User is prompted for parameter(s) to be dumped.
 *
 * OUTPUT: File with extracted data (extract.out) in specific format.
 *    (Nominal date/time, Network, stn ID, lat, lon, requested parameters)
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          both nominal and obs times.  Check location of vars in qcf recs.
 *
 * 000 27 Mar 96 lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "qcfrec.h"
#include "process_qcfrec.h"


#define MAX_CHARS  700  /* was 256 for qcf */ 

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
 * 000 27 Mar 96 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *input_stream;
   FILE         *output_stream;

   int          items;
   int          j, i;

   char		input_file_name[MAX_CHARS] = "\0";
   char		output_file_name[MAX_CHARS] = "\0";

   QCFREC       *qcfptr = NULL;

   int          rec_count = 0;

   int          beginning_hr, beginning_min, ending_hr, ending_min;
   int          StnP_flag, SLP_flag, CSLP_flag;
   int          temp_flag, Dewpt_flag, wdsp_flag, wddir_flag, gust_flag, precip_flag;


   printf ("Enter beginning name of input file:\n");
   scanf ("%s", input_file_name);
   printf ("\nInput file name was: %-s\n", input_file_name);

   sprintf (output_file_name, "extract.out");
   printf ("Output file will be named: extract.out\n");
 
   if (( input_stream = fopen(input_file_name, "r")) == NULL)
      perror ("Error: Can't open input file");
 
   if (( output_stream = fopen(output_file_name, "w")) == NULL) /* binary */
      perror ("Error: Can't open output file.");
 
   rec_count = 0;
      
   printf ("Enter beginning hour:\n");
   scanf ("%d", &beginning_hr);
   printf ("Enter beginning minute:\n");
   scanf ("%d", &beginning_min);

   printf ("Enter ending hour:\n");
   scanf ("%d", &ending_hr);
   printf ("Enter ending minute:\n"); 
   scanf ("%d", &ending_min);

   printf ("Beginning time= %-02d:%-02, Ending time= %-02d:%-02\n",
            beginning_hr, beginning_min, ending_hr, ending_min);

   printf ("Enter 1 for YES; 0 for NO::\n");

   printf ("   Extract Station Pressure & QC flag?:\n");  
   scanf ("%d", &StnP_flag);
   printf ("   Extract Sea Level Pressure & QC flag?:\n");        
   scanf ("%d", &SLP_flag);  
   printf ("   Extract Calculated Sea Level Pressure & QC flag?:\n");        
   scanf ("%d", &CSLP_flag);   

   printf ("   Extract Temperature & QC flag?:\n");        
   scanf ("%d", &temp_flag);   
   printf ("   Extract Dew Point & QC flag?:\n");        
   scanf ("%d", &Dewpt_flag);   

   printf ("   Extract Wind Speed & QC flag?:\n");
   scanf ("%d", &wdsp_flag);      
   printf ("   Extract Wind Direction & QC flag?:\n");        
   scanf ("%d", &wddir_flag);   
   printf ("   Extract Wind Gust & QC flag?:\n");         
   scanf ("%d", &gust_flag); 

   printf ("   Extract Precipitation & QC flag?:\n");
   scanf ("%d", &precip_flag);      

   /*-----------------------------------------------
    * Construct a qcfrec pointer then read the data.
    *----------------------------------------------*/
   construct_qcfptr (&qcfptr);

   while (!feof(input_stream))
      {
      reset_qcfrec( qcfptr );        /* Safe guard only */
      read_qcfrec (&input_stream, qcfptr);

      if ( feof(input_stream) 
           || (qcfptr->hour_nom > ending_hr))
         {
         if (fclose (input_stream) == EOF)
            perror ("Can't close input stream.");

         if (fclose (output_stream) == EOF)
            perror ("Can't close output_stream.");
         break;
         }

      rec_count++;

      if ( qcfptr->hour_nom   >= beginning_hr  && qcfptr->hour_nom  <=ending_hr &&
           qcfptr->minute_nom >= beginning_min && qcfptr->minute_nom <=ending_min )
         {
         fprintf( output_stream, "%2.2d/%2.2d/%2.2d %2.2d:%2.2d %-10s %-15s %10.5f %11.5f",
                  qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom,
                  qcfptr->hour_nom, qcfptr->minute_nom,
                  qcfptr->qnet,  qcfptr->statn,
                  qcfptr->lat,   qcfptr->lon);

         if (StnP_flag)
           fprintf( output_stream, " %7.2f %c",
                     qcfptr->staprs, qcfptr->staflg);

         if (SLP_flag) 
           fprintf( output_stream, " %7.2f %c",
                     qcfptr->seaprs, qcfptr->seaflg);

         if (CSLP_flag) 
           fprintf( output_stream, " %7.2f %c",
                     qcfptr->cmpsea, qcfptr->cmpflg);

         if (temp_flag) 
            fprintf( output_stream, " %7.2f %c",
                     qcfptr->temp,   qcfptr->tmpflg);

         if (Dewpt_flag) 
            fprintf( output_stream, " %7.2f %c",
                     qcfptr->dewpnt, qcfptr->dewflg);

         if (wdsp_flag) 
            fprintf( output_stream, " %7.2f %c",
                     qcfptr->wndspd, qcfptr->spdflg);

         if (wddir_flag) 
            fprintf( output_stream, " %7.2f %c",
                     qcfptr->wnddir, qcfptr->dirflg );

         if (precip_flag) 
            fprintf( output_stream, " %7.2f %c",
                     qcfptr->precip, qcfptr->prcflg);

         if (gust_flag)
            fprintf( output_stream, " %7.2f %c",
                     qcfptr->squall, qcfptr->sqlflg);

         fprintf (output_stream, "\n");

         } /* if within time */
      } /* while feof(input_stream) */

   destruct_qcfptr (&qcfptr);

   }  /* main() */
