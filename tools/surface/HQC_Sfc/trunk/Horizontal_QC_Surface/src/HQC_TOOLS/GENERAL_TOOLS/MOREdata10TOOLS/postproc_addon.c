#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * postproc_addon.c - This code takes the addon stns file
 *   creates the formatted file for stations.out.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.txt'. 
 *
 * OUTPUT: Station File addon.
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          data in specific format. 
 *          Also stn to mod is hardcoded in Main. User must name
 *          file that contains list of files that will be processed
 *          to be named 'filelist.txt'.
 * 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 07 Jun lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "process_stn_data.h"


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
 * 000 29 Apr 94 lec
 *    Created.
 *---------------------------------------------------------------------*/
int main()
   {
   /* local variables */
   FILE         *stations_out_stream;
   FILE         *input_data_stream1;
   FILE         *input_data_stream2;

   int          items;
   int          j, i;
   int          count = 3170;
   long int     rec_count = 0;

   char		input_file_name[MAX_CHARS] = "\0";

   char         newline[MAX_CHARS] = "\0";
   char         updated_line[MAX_CHARS] = "\0";
   char         stn_id[11] = "\0\0\0\0\0\0\0\0\0\0\0";
   char         junk_str[75];

   STNREC       stn_info;


   /* 
    * Read each record in.
    */
   if (( input_data_stream1 = fopen("./filelist.txt", "r")) == NULL)
      perror ("Error: Can't open filelist.txt for reading");

   fscanf (input_data_stream1, "%s", input_file_name);

   while (!feof(input_data_stream1))
      {
      if ( feof(input_data_stream1))
         {
         if (fclose (input_data_stream2) == EOF)
            perror ("Can't close input stream.");
 
         if (fclose (stations_out_stream) == EOF)
            perror ("Can't close output_stream.");

         break;
         }

      printf ("\nInput name was: %-s\n", input_file_name);

      if (( input_data_stream2 = fopen(input_file_name, "r")) == NULL)
         perror ("Error: Can't open input file");
 
      if (( stations_out_stream = fopen("stations_addon.out", "w")) == NULL)
         perror ("Error: Can't open output file.");

      printf ("Begin processing data\n");

      while (!feof(input_data_stream2))
         {
         for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';

         read_record( &input_data_stream2, newline);

         if ( feof(input_data_stream2) || !strncmp(newline, "\0",1) )
            {
            if (fclose (input_data_stream2) == EOF)
               perror ("Can't close input stream.");

            if (fclose (stations_out_stream) == EOF)
               perror ("Can't close stations output_stream.");

            break;
            }

         rec_count++;
         count++;

         printf ("\nnewline: %-sxxx\n", newline);


         for (j=0;j<75;j++)junk_str[j] = '\0';
 
         for (j=0;j<16;j++)stn_info.project[j] = '\0';
         for (j=0;j<19;j++)stn_info.stnid_ext[j] = '\0';
         for (j=0;j<51;j++)stn_info.name[j] = '\0';
         for (j=0;j<9;j++)stn_info.begin_date[j] = '\0';
         for (j=0;j<9;j++)stn_info.end_date[j] = '\0';
         for (j=0;j<3;j++)stn_info.country[j] = '\0';
         for (j=0;j<3;j++)stn_info.state[j] = '\0';
         for (j=0;j<4;j++)stn_info.county[j] = '\0';
         for (j=0;j<15;j++)stn_info.frequency[j] = '\0';

         /*
          * Pick out data write to output files.
          */
         strncpy ( stn_info.project, &newline[0], 15);
 
         stn_info.stnid_int = count;
 
         for (j=0;j<75;j++)junk_str[j] = '\0';
         strncpy (junk_str, &newline[79],6);
         stn_info.lat = atof (junk_str);
 
         for (j=0;j<75;j++)junk_str[j] = '\0';
         strncpy (junk_str, &newline[88],7);
         stn_info.lon = atof (junk_str);
 
         stn_info.occur = 0;
         stn_info.accuracy = 2;
 
         strncpy (stn_info.name, &newline[33], 27);
         strncpy (stn_info.begin_date, "19920201",8);
         strncpy (stn_info.end_date, "19920430",8);
 
         strncpy (stn_info.country, "US",2);
         strncpy (stn_info.state, &newline[76],2);
         strncpy (stn_info.county, "???",3);
 
         stn_info.time_zone = 99.99;
 
         stn_info.dst_switch = '?';
 
         for (j=0;j<75;j++)junk_str[j] = '\0';
         strncpy (junk_str, &newline[22],5);

         if (!strncmp (junk_str, "WDPN", 4))
            stn_info.platform = 1;
         else if (!strncmp (junk_str, "NWS", 3))
            stn_info.platform = 54;
         else if (!strncmp (junk_str, "CLASS", 5)) 
            stn_info.platform = 53;
         else if (!strncmp (junk_str, "FLAT", 4)) 
            stn_info.platform = 100;
         else
            printf ("UNKNOWN platform type!!!!\n"); 

 
         strncpy (stn_info.frequency, "FREQ",4);
 
         for (j=0;j<75;j++)junk_str[j] = '\0';
         strncpy(junk_str, &newline[98],4);
         stn_info.elev = atof (junk_str);
 
         stn_info.fixed_mobile = 'f';

#if 0
      printf ("stn_info.project: %-sxxx::\n",  stn_info.project);
      printf ("stn_info.stnid_int: %dxxx::\n",  stn_info.stnid_int);
      printf ("stn_info.stnid_ext: %-sxxx::\n",  stn_info.stnid_ext);
      printf ("stn_info.lat: %fxxx::\n",  stn_info.lat);
      printf ("stn_info.lon: %fxxx::\n",  stn_info.lon);
      printf ("stn_info.occur: %dxxx::\n",  stn_info.occur);
      printf ("stn_info.accuracy: %dxx::\n", stn_info.accuracy);
      printf ("stn_info.name: %-sxxx::\n",  stn_info.name);
      printf ("stn_info.begin_date: %-sxxx::\n",  stn_info.begin_date);
      printf ("stn_info.end_date: %-sxxx::\n",  stn_info.end_date);
      printf ("stn_info.country: %-sxxx::\n",  stn_info.country);
      printf ("stn_info.state: %-sxxx::\n",  stn_info.state);
      printf ("stn_info.county: %-sxxx::\n",  stn_info.county);
      printf ("stn_info.time_zone: %fxxx::\n",  stn_info.time_zone);
      printf ("stn_info.dst_switch: %cxxx::\n",  stn_info.dst_switch);
      printf ("stn_info.platform: %dxx::\n",  stn_info.platform);
      printf ("stn_info.frequency: %-sxxx::\n",  stn_info.frequency);
      printf ("stn_info.elev: %fxxx::\n",  stn_info.elev);
      printf ("stn_info.fixed_mobile: %cxxx::\n", stn_info.fixed_mobile);
#endif      
         /*
          * Write out station recs.
          */ 
         fprintf( stations_out_stream,
        "%-15s %10d %10.5f %11.5f %3d %5d *%-50s %-8s %-8s %-2s %-2s %-3s TTT.00 %-1c %4d %-15s %9.1f %1c\n",
            stn_info.project,
            stn_info.stnid_int,
            stn_info.lat,
            stn_info.lon,
            stn_info.occur,
            stn_info.accuracy,
            stn_info.name,
            stn_info.begin_date,
            stn_info.end_date,
            stn_info.country,
            stn_info.state,
            stn_info.county,
/*             stn_info.time_zone, */
            stn_info.dst_switch,
            stn_info.platform,
            stn_info.frequency,
            stn_info.elev,
            stn_info.fixed_mobile);

         } /* while feof(input_data_stream2) */

      fscanf (input_data_stream1, "%s", input_file_name);

      } /* while feof(input_data_stream1) */
 
   if (fclose (input_data_stream1) == EOF)
      perror ("Can't close input_data_stream1.");

   printf ("Number of recs processed: %d\n", rec_count);
   }  /* main() */
