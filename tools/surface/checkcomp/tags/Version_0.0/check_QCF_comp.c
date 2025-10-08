/*--------------------------------------------------------
 * check_QCF_comp - Main module which controls the processing
 *  that checks a composite made up of QCF records.
 *  This program assumes the following:
 *     - Data records are in QC format (QCF).
 *     - Data is to be sorted by date/time and then lat/lon.
 *     - Each input file is a day file and contains on the
 *       data for that day.
 *     - Input file names contain: YYMMDD
 *       where YY = year, MM = month, DD = day. File suffix is
 *       generally qcf (can be 0qc).
 *     - Data records should be unique.
 *
 *  This program checks for the following:
 *     - File name matches date of data in file.
 *     - Time (hr/min) is increasing in file.
 *     - If times are the same, then lat/lon in increasing.
 *     - All hours (00 through 23) are present in file.
 *     - File contains only data at specified frequency.
 *     - Duplicate data records.
 *
 *  General statistics are printed to stnd output whether
 *  or not any errors are found in data:
 *     - Number of records processed.
 *     - First and last times in file.
 *     - List of all different Networks present in data. 
 *
 * Input: input file to be checked (YYMMDD.xxx).
 *
 * Input: File named "data_freq.in". This file contains
 *        only two lines. The first line contains one 
 *        integer number that is the data frequency 
 *        expected in the input file. If twenty minute data
 *        is being processed then create a file named
 *        data_freq.in with the number "20" on the first line.
 *        The second line contains the position in the
 *        input file name that the date (i.e., YYMMDD) begins.
 *        If the file is named 940401.qcf, then the second
 *        line should contain a zero. If the file is named
 *        OKMESO5_940401.qcf, then the second line of file
 *        data_freq.in should contain the number 8.
 *
 * Output: information to standard output.
 *
 * 3 Mar 95 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "qcfrec.h"
#include "process_qcfrec.h"

#define  NAMELEN   256
#define  DEBUG     0

typedef  char STRING10[11];

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*----------------------------------------------------------------------
 * main() - This is the main program which controls the basic
 *     processing and control.
 *
 * 3 Mar 95 lec
 *   Created.
 *---------------------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   FILE     *data_input_stream;
   FILE     *data_freq_input_stream;

   char     data_input_file_name[NAMELEN] = "\0";
   char     data_freq_input_file_name[NAMELEN] = "data_freq.in\0";

   char     match_name[7] = "\0\0\0\0\0\0\0";
   char     input_file_name_date[7] = "\0\0\0\0\0\0\0";

   long int recs_read = 0;
   long int time_in_minutes = 0;

   int      i = 0;
   int      data_freq = 20;
   int      date_pos = 0;  /* Char position in input name where date begins. */

   int      current_max_networks = 0;

   int      first_nom_yr = 0;
   int      first_nom_mo = 0;
   int      first_nom_day = 0;
   int      first_nom_hr = 0;
   int      first_nom_min = 0;
   int      last_nom_yr = 0;
   int      last_nom_mo = 0;
   int      last_nom_day = 0;
   int      last_nom_hr = 0;
   int      last_nom_min = 0;

   int      prev_hr = -1;
   int      prev_min = 0;

   float    prev_lat = -1.0;
   float    prev_lon = -1.0;

   STRING10 prev_qnet = "\0\0\0\0\0\0\0\0\0\0";
   char     prev_stnID[16] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";


   STRING10 networks[100];

   QCFREC   *qcfptr = NULL;


   /*
    * Expect input file name on the command line.
    */
   if (argc != 2)
      {
      printf ("Usage: check_QCF_comp <input_file>\n");
      exit(1);
      }

   strcpy (data_input_file_name, argv[1]);


   printf ("---------------------------------------------------\n");
   printf ("Check_QCF_comp processing Composite file: %-s\n", data_input_file_name);


   /*
    * Open the input files read expected data frequency.
    */
   open_file (data_input_file_name, "r", &data_input_stream);
   open_file (data_freq_input_file_name, "r", &data_freq_input_stream);

   fscanf (data_freq_input_stream, "%d", &data_freq);
   printf ("Expected data frequency is %d\n", data_freq);

   fscanf (data_freq_input_stream, "%d", &date_pos);
   printf ("Expect date to begin at position %d in input file name\n", date_pos);
   printf ("(Character positions begin at zero.)\n");

   /*
    * Extract date from input file name.
    */
   strncpy (input_file_name_date, &data_input_file_name[date_pos], 6);


   /*
    * Construct a qcfrec pointer
    */
   construct_qcfptr (&qcfptr);
 

   /*
    * Initialize  ptr records.
    */
   reset_qcfrec (qcfptr);


   /*
    * Process all the data in the input file.
    */
   while (!feof(data_input_stream))
      {
      read_qcfrec( &data_input_stream, qcfptr);
      recs_read++;
 
      /*
       * Determine first and last date/time in file.
       */
      if (recs_read == 1)
         {
         first_nom_yr = qcfptr->year_nom;
         first_nom_mo = qcfptr->month_nom;
         first_nom_day = qcfptr->day_nom;
         first_nom_hr = qcfptr->hour_nom;
         first_nom_min = qcfptr->minute_nom;
         }
      else
         { 
         last_nom_yr = qcfptr->year_nom;
         last_nom_mo = qcfptr->month_nom;
         last_nom_day = qcfptr->day_nom;
         last_nom_hr = qcfptr->hour_nom; 
         last_nom_min = qcfptr->minute_nom;
         } 


      /*
       * Verify that file name matches date of data in the file.
       */
      sprintf (match_name, "%02d%02d%02d\0", 
               qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom);

      if (strncmp( match_name, input_file_name_date, 6))
         printf ("ERROR: Mismatch between file name (%-s) and data time (%-s) in file at rec %ld.\n",
                 data_input_file_name, match_name, recs_read);


      /*
       * Look for duplicate records.
       */
      if (!strncmp(prev_stnID, qcfptr->statn, 15) && !strncmp(prev_qnet, qcfptr->qnet, 10)  &&
          (prev_min == qcfptr->minute_nom) && (prev_hr == qcfptr->hour_nom))
         printf ("ERROR: Duplicate records at rec %ld\n", recs_read);         

      strncpy(prev_stnID, qcfptr->statn, 15);
      strncpy(prev_qnet, qcfptr->qnet, 10);


      /* 
       * Check that the lats and lons increasing.  That is
       * the data are also sorted by lat/lon after time.
       */
      if (last_nom_hr == prev_hr &&  last_nom_min == prev_min && qcfptr->lat < prev_lat)         
         printf ("ERROR: Latitudes are decreasing from %10.5f at rec %ld to %10.5f at rec %ld.\n", 
                 prev_lat, recs_read-1, qcfptr->lat, recs_read); 

      if (last_nom_hr == prev_hr && last_nom_min == prev_min && qcfptr->lat == prev_lat && 
          prev_lon > qcfptr->lon) 

         printf ("ERROR: Latitudes match BUT Longitudes are decreasing from %10.5f at rec %ld to %10.5f at rec %ld.\n",
                 prev_lon, recs_read-1, qcfptr->lon, recs_read); 
 
      prev_lat = qcfptr->lat;
      prev_lon = qcfptr->lon; 


      /*
       * Assuming day files in the composite,
       * just ensure that time is increasing
       * according to the specified data frequency.
       */
#if DEBUG
      printf ("prev_hr = last_nom_hr:: %d, %d\n", prev_hr, last_nom_hr);
#endif

      if (last_nom_hr == prev_hr && last_nom_min < prev_min)
         printf ("ERROR: Minute field is decreasing from %d at rec %ld to %d at rec %ld.\n",
                 prev_min, recs_read-1, last_nom_min, recs_read);

      if (last_nom_hr > prev_hr && (last_nom_hr - prev_hr) > 1)
         printf ("WARNING: More than one HR missing between rec %ld (HR: %d) and rec %ld (HR: %d).\n",
                 recs_read-1, prev_hr, recs_read, last_nom_hr);

      if (last_nom_hr < prev_hr)
         printf ("ERROR: Hour field is decreasing from %d at rec %ld to %d at rec %ld.\n", 
                 prev_hr, recs_read-1, last_nom_hr, recs_read);


      prev_min = last_nom_min;
      prev_hr = last_nom_hr;

      time_in_minutes = last_nom_hr*60 + last_nom_min;

      if ((time_in_minutes % data_freq) != 0)    /* remainder should always be zero */
         printf ("ERROR: Wrong frequency data at rec %ld\n", recs_read);


      /*
       * Save a list of all different network types.
       */
      if (recs_read == 1)
         {
         strncpy (networks[0], qcfptr->qnet,10);
         current_max_networks++;
#if DEBUG
         printf ("First rec - just copy qnet into networks. current_max_networks = %d\n",
                  current_max_networks);
#endif
         }
      else
         for (i=0;i<current_max_networks;i++)
            {
#if DEBUG
            printf ("Compare qcfptr->qnet (%-s) and networks[%d] (%-s)\n",
                    qcfptr->qnet, i, networks[i]);
#endif

            if (!strncmp(networks[i], qcfptr->qnet,10))
               {
#if DEBUG
               printf ("Found qnet match at i = %ld\n",i);
#endif
               break;
               }

            if ((i+1) == current_max_networks)   /* If looked at all & no match, then save */
               {
#if DEBUG
               printf ("Add qnet to list\n");
#endif
               strncpy (networks[i+1], qcfptr->qnet,10);
               current_max_networks++;
               break;
               }

            } /* for num_networks */


      if ( feof(data_input_stream) )
         break;

      /*
       * Reset the stn ptr.
       */
       reset_qcfrec (qcfptr);

      } /* while input data */

   /*
    * Free ptr and Close input and output stream.
    */
   destruct_qcfptr (&qcfptr);

   close_file ( &data_input_stream);
   close_file ( &data_freq_input_stream);


   printf ("CHECK_QCF_COMP stats for file %-s:\n", data_input_file_name);
   printf ("   Number records processed: %ld\n", recs_read);
   printf ("   First time in file: %02d/%02d/%02d %02d:%02d\n", 
               first_nom_yr, first_nom_mo, first_nom_day, 
               first_nom_hr, first_nom_min);
   printf ("   Last time in file: %02d/%02d/%02d %02d:%02d\n", 
               last_nom_yr, last_nom_mo, last_nom_day, 
               last_nom_hr, last_nom_min);
   printf ("   Found following %d Networks present in file:\n",
               current_max_networks);

   for (i=0;i<current_max_networks;i++)
      printf ("      %-s\n", networks[i]);


   }  /* main() */
