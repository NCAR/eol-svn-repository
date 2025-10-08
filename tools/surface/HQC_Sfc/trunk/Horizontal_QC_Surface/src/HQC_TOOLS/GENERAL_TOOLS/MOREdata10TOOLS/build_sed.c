#ifndef lint
static char *rcsid = "$Id$";
#endif


/*
 * $Log$
 *
 */

/*----------------------------------------------------------------------
 * build_sed.c - This code builds a sed script to set the occurances
 *   in a specified file.
 *
 * INPUT : List of files to be processed in a file named :
 *         'filelist.txt'. This should be a list of files
 *         with the qcf format (i.e., *.0qc). 
 *
 * OUTPUT: a sed script
 *
 * WARNING: Currently, s/w in Main is set to process file containing
 *          data in specific format. 
 *          There's a lot of hardcoded stuff in this s/w.
 *
 * 000 24 May lec
 *    Created.
 *---------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define MAX_CHARS 700

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
   FILE         *stations_in_stream;
   FILE         *stn_id_in_stream;
   FILE         *output_stream;

   int          items;
   int          j, i;
   long int     rec_count = 0;

   char         int_id[5] = "\0\0\0\0\0";

   char         newline[MAX_CHARS] = "\0";
   char         newline2[MAX_CHARS] = "\0";

   char         stn_id_ext[11] = "\0\0\0\0\0\0\0\0\0\0\0";
   char         junk_str[75];
   int          occ = 0;
   char         latlonstr[25] = "\0";



   if (( output_stream = fopen("./sedit.sc", "w")) == NULL)
      perror ("Error: Can't open sedit.sc");

   /* 
    * Both files must be sorted in same order.
    */
   if (( stations_in_stream = fopen("GIDS1_master_stn_list_best", "r")) == NULL)
      perror ("Error: Can't open GIDS1_master_stn_list");

   if (( stn_id_in_stream = fopen("GIDS1_master_stn_id_best", "r")) == NULL)
      perror ("Error: Can't open GIDS1_master_stn_id");


      printf ("Begin processing data\n");

      while (!feof(stations_in_stream) && !feof(stn_id_in_stream))
         {
         for (j=0;j<MAX_CHARS;j++)newline[j] = '\0';
         for (j=0;j<MAX_CHARS;j++)newline2[j] = '\0';

         read_record( &stations_in_stream, newline);
         read_record( &stn_id_in_stream, newline2);

         if ( (feof(stn_id_in_stream) || !strncmp(newline2, "\0",1)) ||
              (feof(stn_id_in_stream) || !strncmp(newline2, "\0",1)) )
            {
            if (fclose (stations_in_stream) == EOF)
               perror ("Can't close stations input stream.");

            if (fclose (stn_id_in_stream) == EOF)
               perror ("Can't close stn id input_stream.");

            if (fclose (output_stream) == EOF)
               perror ("Can't close output_stream.");

            break;
            }

         rec_count++;

         /* printf ("\nnewline: %-sxxx\n", newline);
         printf ("\nnewline2: %-sxxx\n", newline2); */


         for (j=0;j<75;j++)junk_str[j] = '\0';
         for (j=0;j<25;j++)latlonstr[j] = '\0';
         for (j=0;j<11;j++)stn_id_ext[j] = '\0';
         for (j=0;j<5;j++)int_id[j] = '\0';
         occ = 0;
 

         /*
          * Pick out data and write to output file.
          */
         strncpy (latlonstr, &newline[27],22 );
         strncpy (stn_id_ext, &newline2[27], 10);

         junk_str[0] = newline[52];
         junk_str[1] = '/0';
         occ = atoi(junk_str);

         /*printf ("stn_id_ext: %-sxxx\n", stn_id_ext);
         printf ("latlonstr: %-sxxx\n", latlonstr);
         printf ("occ: %d\n", occ); */
 
         strncpy (int_id, &newline[22], 4);
 
         /*
          * Write out sed rec. Only write out if occ is > 0.
          */ 
         if (occ > 0)
            {

            /* Use following statement for PQCF format  - int_id added for debug.*/
            fprintf( output_stream,
                     "sed 's/%-5s %-15s %-s   0/%-15s %-s   %d/g' <GIDS1_DailyPrcpCmp.pqcf >GDPC.pqcf \n",
                     int_id, stn_id_ext, latlonstr, stn_id_ext, latlonstr, occ); 

            fprintf( output_stream, "mv GDPC.pqcf  GIDS1_DailyPrcpCmp.pqcf\n");
#if 0

            /* Use following statement for COOP summary of the Day format */ 
            fprintf( output_stream,
                     "sed 's/%-5s %-10s %-s   0/%-10s %-s   %d/g' <dly_coop.dqc >DS.dqc \n",
                     int_id, stn_id_ext, latlonstr, stn_id_ext, latlonstr, occ);

            fprintf( output_stream, "mv DS.dqc  dly_coop.dqc\n");
#endif
            }

         } /* while feof input streams */

   printf ("Number of recs processed: %d\n", rec_count);
   }  /* main() */
