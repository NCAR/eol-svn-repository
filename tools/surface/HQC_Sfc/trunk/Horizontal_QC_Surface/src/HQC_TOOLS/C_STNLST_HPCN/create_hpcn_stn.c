/*--------------------------------------------------------
 * create_hpcn_stns - This s/w converts some hpcn stns
 *    file to the hplains.stns file required to convert
 *    the HPCN data.
 *
 * Input: input file to be translated
 *
 * Output: new_hplains_stn.out
 *
 * Mar 96 lec
 *   Create.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "local.h"

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/

/*----------------------------------------------------------------------
 *read_record() - Generically reads in a record of max length of
 *    MAX_CHARS. 
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
 * dms_to_deg - This code converts degress/minutes/seconds 
 *              to decimal degrees. User must accommodate western/southern
 *              hemispheres, etc.
 *
 * 000  01 Dec 93 lec
 *    Created.
 *---------------------------------------------------------------------*/
 void dms_to_deg( /*in*/ char  dms[10],
                  /*out*/float *dec_deg)

   {
   float    deg, min, sec;
   char     tmpstr[10] = "\0\0\0\0\0\0\0\0\0\0";

   /*
    * Pick out dms and convert to decimal.
    */
   strncpy(tmpstr, &dms[0], 3);
   tmpstr[3] = '\0';
   deg = atof(tmpstr);

   strcpy(tmpstr,"");
   strncpy(tmpstr, &dms[4], 2); /* Changed for HPCN !!!! */
   tmpstr[2] = '\0';
   min = atof(tmpstr);

   if (min > 59.0) printf ("min greater than 59!\n");

   sec = 0.0;

   min = min + sec/60.0;

   *dec_deg = deg + min/60.0;

   } /* dms_to_dec()*/


/*----------------------------------------------------------------------
 * main() - This is the main program which controls the basic
 *     processing and control.
 *
 * Mar 96 lec
 *   Created.
 *---------------------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   FILE     *data_input_stream;
   FILE     *hpcn_output_stream;

   char     data_input_file_name[NAMELEN] = "\0";
   char     hpcn_output_file_name[NAMELEN] = "new_hplains.stns\0";

   int      ii = 0;

   float    dec_deg = 0.0;
   float    lat = 0.0;
   float    lon = 0.0;
   float    elev = 0.0;

   char     newline[MAX_CHARS];
   char     junk_str[26] = "\0";

   char     state[3] = "\0\0\0";

   char     stn_num_str[4] = "\0\0\0\0";
   int      stn_num = 0;

   char     stn_name[19] = "\0";



   printf ("Begin create_hpcn_stns processing! \n");
   printf ("Input name of stn file to process: \n");
   scanf ("%s", data_input_file_name);

   /*
    * Open the stn info input file and the two output files.
    */
   if (( data_input_stream = fopen(data_input_file_name, "r")) == NULL)
      perror ("Error: Can't open data_input_stream");

   /*
    * Open the ascii output file for writing.
    */
   if (( hpcn_output_stream = fopen(hpcn_output_file_name, "w")) == NULL)
      perror ("Error: Can't open station output_file");


   /*
    * Process all the data in the input file.
    */
   while (!feof(data_input_stream))
      {
      read_record( &data_input_stream, newline);
      printf ("newline: %-sxxx\n", newline);
 
      if ( feof(data_input_stream) || !strncmp(newline, "\0",1)
          || !strncmp(newline, "  ",2) )
         {
         break;
         }


      strncpy (stn_num_str, "\0\0\0\0", 4);
      strncpy(stn_num_str, &newline[7], 3);
      stn_num_str[3] = '\0';
      stn_num = atoi(stn_num_str);

      for (ii = 0; ii<19; ii++)
         stn_name[ii] = '\0';

      strncpy(stn_name, &newline[13], 18);
      stn_name[18] = '\0';
 
      strncpy (state, "\0\0\0", 3); 
      strncpy(state, &newline[32], 2);  
      state[2] = '\0'; 


      /*
       * Convert lat/lon dms to dec.
       */
      strncpy(junk_str, &newline[63], 6);
      junk_str[6] = '\0';
      dms_to_deg(junk_str, &dec_deg);
      lat = dec_deg;
 
      strncpy(junk_str, &newline[73], 6);
      junk_str[6] = '\0';
      dms_to_deg(junk_str, &dec_deg);
      lon = dec_deg;

      strncpy(junk_str, &newline[54], 4); 
      junk_str[4] = '\0'; 
      elev = atof(junk_str); 


     /*----------------------------------
      * Write to output file.
      *----------------------------------*/
     fprintf (hpcn_output_stream, "HPCN  %03d   %-26s %-2s %8.2f %9.2f   %04.0f  6\n",
              stn_num, stn_name, state, lat, lon, elev); 

      } /* while input data */

   /*
    * Close all input and output stream.
    */
   if (fclose (data_input_stream) == EOF)
      perror ("Can't close data_input_stream");

   if (fclose (hpcn_output_stream) == EOF)
      perror ("Can't close hpcn_output_stream");

   printf ("create_hpcn_stns processing complete!\n");
   }  /* main() */
