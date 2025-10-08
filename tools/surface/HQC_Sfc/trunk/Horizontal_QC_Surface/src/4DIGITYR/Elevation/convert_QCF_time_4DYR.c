/*--------------------------------------------------------
 * convert_QCF_time_4DYR.c -  Convert Solar time to UTC 
 *    or vice versa. Handles 4 Digit Years.
 *
 * WARNING: Before converting UTC data to SOLAR data
 *   remove all solar_*.* files. Before converting
 *   SOLAR data to UTC data remove all utc_*.* files.
 *   Else this s/w will write over some of these files
 *   and cat to the end of other files. The result is
 *   incorrect output files. See the other WARNINGs
 *   below.
 *
 *   When converting from UTC to SOLAR time, if the 
 *   frequency of the data is equal to or less frequent
 *   than 15 minutes (e.g., hourly), then this s/w attempts
 *   to open every output file that a single
 *   source/input file would write into (i.e., 2 day's
 *   worth of files - current day and previous day).
 *   By opening all these files, this cuts down
 *   significantly on the number of opens and closes
 *   performed. However, if the data frequency is higher
 *   than 15 minutes, a sufficient number of files can not
 *   be opened. Note that C s/w only allows a single process 
 *   a max of 252 open files at any one time. The FILE
 *   structure uses an unsigned char to store the file
 *   number - limiting its value. So, for higher freq.
 *   data, this s/w currently opens and closes files
 *   as it needs them.  For converting from SOLAR to UTC,
 *   the s/w always opens and closes files as it needs them.
 *   In future, this s/w could be updated to open 2 day's
 *   worth of files when it doesn't exceed 252 files.
 *
 *   Usage: convert_QCF_time_4DYR <initialization_input_file>
 *
 *   Note: This s/w should properly handle data that begins
 *   in one year and rollsover into next year. Rollover
 *   into next century should also be handled properly.
 * 
 * WARNING:
 *    This s/w has not been tested for all possible
 *    data frequencies. Always check the output carefully!
 * WARNING:
 *    This s/w has NOT been tested with a reference longitude
 *    in the middle of the data's longs NOR with a longitude
 *    WEST of all longs in the composite.  Suggest that user
 *    always select a reference lon EAST of Area of Interest.
 * WARNING:
 *    Array network_freq[] and MAX_NETWORKS should always be
 *    checked for each project. This array contains a list
 *    of all network names that could occur in the input data
 *    files. Beware that the name AND the associated frequency
 *    match for the input data. For instance, even thought
 *    ARMSFC1 is actually one minute data, for ESOP96 we only
 *    generated ARMSFC1 hourly data. So, the frequency had
 *    to be updated to 60. MAX_NETWORKS equals the number
 *    of types in the network_freq[] array. This s/w will 
 *    issue a warning if it detects a network in the data that
 *    is not listed in the list of acceptable networks.
 *
 * Assumptions:
 *  - Assumes specific format for user specified input file. (4Digit YR)
 *  - Assumes specific format for user specified input file name. (4Digit YR)
 *  - Input data is in QCF format and sorted by date/time.
 *  - When converting from UTC to SOLAR, file suffix is 0qc.
 *  - When converting from SOLAR to UTC, file suffix is qcf.
 *  - That no two stations in a single network have the
 *    same ID. We use network:ID as a unique identifier.
 *  - That the reference Longitude is to the EAST of all
 *    stations in the data being processed. This is not really
 *    assumed by the s/w. It's just that other situations
 *    have not been completely tested.
 *  - That the cosort utility is available during execution.
 *  - That a cosort_4DYR.input/control file named sort.inp is located
 *    in the execution area. See Input for contents of sort_4DYR.inp.
 *  - That the execution area is free of solar_*.* files when
 *    running in UTC to SOLAR mode and free of utc_*.* files
 *    when running in SOLAR to UTC mode.
 *
 * Input:
 *    The *.0qc for *.qcf files to have times shifted.
 *
 * Input:
 *    sort_4DYR.inp file which determines final QCF sort. Typically this
 *    is a sort by nominal time, lat, lon. A sample sort_4DYR.inp file
 *    to control cosort would be :
 *
 * sort            # Action
 * 0               # Record length (Variable)
 * 1               # Number of input files
 * input.qcf       # input file
 * 7               # Number of keys
 * ascending       # Key 1 direction nominal year
 * fixed           #       location
 * 1               #       starting column
 * 4               #       length
 * numeric         #       format
 * external        #       value interpretation
 * ascending       # Key 2 direction nominal month
 * fixed           #       location
 * 6               #       starting column
 * 2               #       length
 * numeric         #       format
 * external        #       value interpretation
 * ascending       # Key 3 direction nominal day    
 * fixed           #       location
 * 9               #       starting column
 * 2               #       length
 * numeric         #       format
 * external        #       value interpretation
 * ascending       # Key 4 direction nominal hour
 * fixed           #       location
 * 12              #       starting column
 * 2               #       length
 * numeric         #       format
 * external        #       value interpretation
 * ascending       # Key 5 direction nominal minute
 * fixed           #       location
 * 15              #       starting column
 * 2               #       length
 * numeric         #       format
 * external        #       value interpretation
 * ascending       # Key 6 direction lat
 * fixed           #       location
 * 62              #       starting column
 * 10              #       length
 * numeric         #       format
 * external        #       value interpretation 
 * ascending       # Key 7 direction lon
 * fixed           #       location
 * 73              #       starting column
 * 11              #       length
 * numeric         #       format
 * external        #       value interpretation
 * file            # Output
 * input.srt       # Output file name 
 *#-- end cosort example--
 *
 * Input:
 *    User must indicate name of input control file when
 *    executing this program. This input control file
 *    contains 
 *
 *          Definition                        (var) (type)
 *  -----------------------------------------------------------------
 *  Line 1: Project Name                      (proj_name) (string)
 *  Line 2: Conversion Type Flag (0 or 1)     (UTC_to_SOLAR)(1=UTC to SOLAR; 0=SOLAR to UTC)(int)
 *  Line 3: Highest Data Frequency in minutes (input_data_freq) (integer)
 *  Line 4: Mixed Frequency flag (0 or 1)     (mixed_freq) (0=1 freq in data; 1=mltpl freq)(int)
 *  Line 4: Current year of start of data     (current_yr) (YYYY integer)
 *  Line 5: First day to process              (begin_date) (YYYYMMDD string)
 *  Line 6: Last day to process               (end_date) (YYYYMMDD string)
 *  Line 7: Input file pathname               (inpathname) (string)
 *  Line 8: Output file pathname              (outpathname) (string)
 *  Line 9: Reference Longitude               (reference_lon) (float)
 *
 *
 * A sample initialization input file would be:
 *
 * GCIP/EAOP99NW            Project Name.             (25 characters only.)
 * 1                        Conversion type. 1 = UTC to SOLAR; 0 = SOLAR to UTC.
 * 60                       Data frequency (minutes). (Set to highest freq in input) (input_data_freq)
 * 0                        Mixed Frequency flag. 0 = 1 freq in data; 1 = multiple freq in data.
 * 1999                     Century/Year data was collected. (Integer YYYY) (current_year)
 * 19991220                 Begin date of data to process YYYYMMDD (begin_date)
 * 20000109                 End   date of data to process YYYYMMDD (end_date)  
 * /work/NEWQC/src/4DIGITYR/DATA/  Path name (and general name prefix) of 0qc files to process.
 * /work/NEWQC/src/4DIGITYR/SOLAR/ Path name of output 0qc files.
 * -50.00                     Reference Longitude
 * 
 * -- End input file example---
 *
 * Output:
 *   QCF files whose first nominal times have been converted either from
 *   UTC to Solar time (solar_*.0qc) or from Solar time to UTC (utc_*.qcf).
 *
 * 16 Jan 96 lec
 *   Created.
 * 12 Feb 98 lec
 *   Updated MAX_NETWORKS and network_freq[] to contain new networks
 *   and network frequencies, etc. for ESOP96 hourly data.
 *   Updated comments.
 * 4/8 Dec 98 lec
 *   Updated WPN to NPN for NOAA Profiler Network. Updated ISWS to
 *   ISWS/ICN. Make corrections to s/w to handle year rollover correctly.
 * 1 Feb 99 lec
 *   Added DATSAV2, DATSAV2M, GLERL, GLERLM, WI_AWON to accepted
 *   networks list. Updated MAX_NETWORKS to be 21.
 * 15 August 2002 lec
 *   Updated s/w, sort files, etc. to handle 4 digit years. WARNING:
 *   this updated s/w has not been tested for all possible cases. Please
 *   check the output carefully.
 * 24 April 2003 lec
 *   Added 3 more networks to network list: ABLE_AWS, NCAR_supp, and
 *   LDMSFCMETR. Increased size of network array to be 28.
 * 28 June 2003 lec
 *   Added 9 more networks to network list. Increased network array
 *   size of network array to be 37. Add info to some debug lines.
 * 2 July 2003 lec
 *   Added 1 more network (HOMESTEAD ISS). Increased network array
 *   size of network array to be 38.
 * 18 Oct 2003 lec
 *   Added 11 more networks (ARS and all the MADIS (ms:)). Increased 
 *   network array size of network array to be 49.
 * 23 Apr 2004 lec
 *   Added 15+ networks for BAMEX AOI/TOI.
 * 08 Jun 2004 lec
 *   Added 3 more networks for BAMEX AOI/TOI.
 * 02 Jan 2007 lec
 *   Add 11 more networks for TREX AOI/TOI. Now there are 84 recongnized
 *   networks.
 * 04/05 Jan 2007 lec
 *   Add 2 more networks for TREX AOI/TOI. Now there are 86 networks.
 *   For TREX2006 only, ASOS 1min is converted to 5min, so set the
 *   frequency on the ASOS network to be 5min. Must change back for 
 *   other projects!!
 * Jan 2007 lec
 *   Change network from "DRI05" to "DRI_AWS05 and 
 *   "Leeds_AWS5" to "LeedsAWS05". Changed TREX network to be TREX05.
 * Feb 2007 lec
 *   Added "MISS05" network. Now 87 recognized networks.
 * Mar 2007 lec
 *   Added "GBUAPCD60", "ISS05", "ISFF05", "ChLkASOS" networks. 
 *   Now 90 recognized networks.
 * Apr 2007 lec
 *   Added "ISS05M" for Mobile ISS in TREX. Now 91 networks recoginzed.
 *--------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "local.h"
#include "process_qcfrec_4DYR.h"
#include "qcfrec.h"
#include "date_4DYR.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * NOTE: Some #if statements have been turned on
 *       permanently by setting #if 1.
 *
 * Set DELETE_INTERMEDIATE_FILES to 1 to delete all
 * intermediate files. Set DELETE_INTERMEDIATE_FILES to 0
 * to retain intermediate files. Warning: Beware of
 * space usage if this flag is set to 0.
 *
 * MAX_NETWORKS should equal the size of the array named
 *    network_freq[]. Add new networks to this array.
 *
 * DEG_TO_MIN is the conversion factor to convert from
 * degrees to minutes.
 *-------------------------------------------------------*/
#define  DEBUG    0
#define  DELETE_INTERMEDIATE_FILES     1
#define  MAX_NETWORKS 91
#define  DEG_TO_MIN 4.0

/*
 * Not enough for minute data. Only enough
 * to handle data with 5 min as highest
 * frequency. Assuming that never need more
 * than 2 days worth of 5 minute files open
 * at any one time. Note that because C used
 * an unsigned char to store the file # on
 * the FILE structure, C users are limited to
 * opening 252 files at one time. This max 
 * number of files will work for all data
 * frequencies of 15 minutes or lower (hrly,
 * 20min, 30min, etc.). 
 */
#define  MAX_NUM_FILES_OPEN  252

typedef char  FILE_NAME_STR [NAMELEN_MAX];


/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
   void increment_qcfptr_nomtime( /*in*/ QCFREC *qcfptr,
                                  /*in*/ int  current_year,
                                  /*in*/ int  data_freq,
                                  /*in*/ int  num_min_to_subtract );
   void decrement_qcfptr_nomtime( /*in*/ QCFREC *qcfptr,
                                  /*in*/ int  current_year,
                                  /*in*/ int  data_freq,
                                  /*in*/ int  num_min_to_subtract );
#else /*!__STDC__*/
   int main ();
   void increment_qcfptr_nomtime();
   void decrement_qcfptr_nomtime();
#endif /*__STDC__*/

/*---------------------------------------------------------
 * increment_qcfptr_nomtime() - increments (first) nominal
 *   time of incoming qcfptr.
 *
 * 16 Jan 96 lec
 *   Created.
 *--------------------------------------------------------*/
void  increment_qcfptr_nomtime( /*in*/ QCFREC *qcfptr, 
                                /*in*/ int  current_year,
                                /*in*/ int  data_freq,
                                /*in*/ int  num_min_to_add )
   { 
   int  hour = 0;
   int  days = 0;
   int  current_hr = 0;
   int  current_min = 0;
 
   long YYYYJJJ = 0;
   long updated_YYYYJJJ = 0;
 
   char year_str[5] = "\0\0\0\0\0";
   char month_str[3] = "\0\0\0";
   char day_str[3] = "\0\0\0";
   char date_YYYYMMDD[9] = "\0\0\0\0\0\0\0\0\0";
 
 
#if DEBUG
   printf ("   Enter increment: num_min_to_add = %d, current_year = %d\n",
           num_min_to_add, current_year);
   printf ("   qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n",
           qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom,
           qcfptr->minute_nom);  
#endif
 
   qcfptr->minute_nom = qcfptr->minute_nom + num_min_to_add;

   if (qcfptr->minute_nom <60)
      return;
   else
     {
     /*-----------------------------------
      * Rollover into next hr.
      *----------------------------------*/ 
     current_min = qcfptr->minute_nom;
     hour = (int)(current_min/60);                  /* allow increments of > 1 hr */

     qcfptr->hour_nom = qcfptr->hour_nom + hour;
     qcfptr->minute_nom = current_min - hour*60;    /* find remainder minutes */
     current_hr = qcfptr->hour_nom;

#if DEBUG
     printf ("   New hour is %d and new minutes are %d\n",
         qcfptr->hour_nom, qcfptr->minute_nom);
#endif
     }

   if (current_hr >= 24)
     {
     /*------------------------------------
      * Rollover into next day(s). 
      * Convert date to easier to handle
      * form. Compute number of days to add.
      *-----------------------------------*/
     days = (int)(current_hr/24);                /* allow increments of > 1 hr */
     qcfptr->hour_nom = current_hr - days*24;    /* find remainder hours */

     sprintf (date_YYYYMMDD, "%04d%02d%02d\0", qcfptr->year_nom,
              qcfptr->month_nom, qcfptr->day_nom);
 
     date_to_YYYYJJJ(date_YYYYMMDD, current_year, &YYYYJJJ);

#if DEBUG
     printf ("   before increment: date_YYYYMMDD, YYYYJJJ:: %-s  %d\n",
             date_YYYYMMDD, YYYYJJJ);
#endif
     updated_YYYYJJJ = add_num_to_YYYYJJJ( YYYYJJJ, days);
     YYYYJJJ_to_date ( updated_YYYYJJJ, date_YYYYMMDD);
 
#if DEBUG
     printf ("   after increment: date_YYYYMMDD:: %-s\n", date_YYYYMMDD);
#endif
 
     strncpy (year_str,  &date_YYYYMMDD[0], 4);
     strncpy (month_str, &date_YYYYMMDD[4], 2);
     strncpy (day_str,   &date_YYYYMMDD[6], 2);
 
     qcfptr->year_nom  = atoi (year_str);
     qcfptr->month_nom = atoi (month_str);
     qcfptr->day_nom   = atoi (day_str);
      
     } /* current_hr >=24 */

   return;

   }/* increment_qcfptr_nomtime()*/


/*---------------------------------------------------------
 * decrement_qcfptr_nomtime() - decrements (first) nominal
 *   time of incoming qcfptr.
 *
 * 16 Jan 96 lec
 *   Created.
 *--------------------------------------------------------*/
void  decrement_qcfptr_nomtime( /*in*/ QCFREC *qcfptr,
                                /*in*/ int  current_year,
                                /*in*/ int  data_freq,
                                /*in*/ int  num_min_to_subtract )
   {
   int  hour = 0;
   int  minutes = 0;
   int  real_minutes = 0;

   long YYYYJJJ = 0;
   long updated_YYYYJJJ = 0;

   char year_str[5] = "\0\0\0\0\0";
   char month_str[3] = "\0\0\0";
   char day_str[3] = "\0\0\0";
   char date_YYYYMMDD[9] = "\0\0\0\0\0\0\0\0\0";


#if DEBUG
   printf ("   Enter decrement: data_freq=%d, num_min_to_subtract = %d, current_year = %d\n",
           data_freq, num_min_to_subtract, current_year);
   printf ("   qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n",
           qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom,
           qcfptr->minute_nom);
#endif

   /*----------------------------------
    * If can do simple subtraction, do
    * so and return.
    *---------------------------------*/
   if ( num_min_to_subtract < qcfptr->minute_nom)
      {
      qcfptr->minute_nom = qcfptr->minute_nom - num_min_to_subtract;
      return;
      }

   /*-------------------------------------
    * Convert hours and minutes to sum.
    * If required, convert date YYYYMMDD to 
    * internal YYYYJJJ form and hour,
    * minutes to seconds to make dates
    * and times easier to update.
    *-------------------------------------*/
   minutes = qcfptr->hour_nom*60 + qcfptr->minute_nom;

   if (minutes < num_min_to_subtract)
      {
      /*----------------------------------------
       * Decrement date - handle month and year
       * rollbacks. The following seems like a
       * lot of contorsions to go through...later
       * can work on speeding this up. Hopefully,
       * won't execute this section too often.
       *-----------------------------------------*/
      sprintf (date_YYYYMMDD, "%04d%02d%02d\0", qcfptr->year_nom,
               qcfptr->month_nom, qcfptr->day_nom);

      date_to_YYYYJJJ(date_YYYYMMDD, current_year, &YYYYJJJ);
#if DEBUG
      printf ("   before update: date_YYYYMMDD, YYYYJJJ:: %-s  %d\n",
              date_YYYYMMDD, YYYYJJJ); 
#endif
      updated_YYYYJJJ = subtract_num_from_YYYYJJJ( YYYYJJJ, 1);

      YYYYJJJ_to_date ( updated_YYYYJJJ, date_YYYYMMDD);

#if DEBUG
      printf ("   after update: date_YYYYMMDD:: %-s\n", date_YYYYMMDD);
#endif

      strncpy (year_str,  &date_YYYYMMDD[0], 4);
      strncpy (month_str, &date_YYYYMMDD[4], 2);
      strncpy (day_str,   &date_YYYYMMDD[6], 2);

      qcfptr->year_nom  = atoi (year_str);
      qcfptr->month_nom = atoi (month_str);
      qcfptr->day_nom   = atoi (day_str);

      /*----------------------------------
       * Take time away from previous day.
       *---------------------------------*/
      minutes = (24 + qcfptr->hour_nom)*60 + qcfptr->minute_nom;
      }

#if DEBUG
   printf( "   before subtract: minutes = %d\n", minutes);
#endif
   minutes = minutes - num_min_to_subtract;

#if DEBUG
   printf( "   after subtract: minutes = %d\n", minutes); 
#endif  
   /*---------------------------------------
    * Now convert back to hours and minutes.
    *--------------------------------------*/
   hour = (int)(minutes/60);
#if DEBUG
   printf( "Convert back(1): hour= %d, minutes=%d\n", hour, minutes);
#endif 

/*ORIG:   real_minutes  = (minutes/60.0 - hour)*60.0; */

   real_minutes  = (int)(((minutes/60.0 - hour)*60.0) + 0.5);

#if DEBUG
   printf( "Convert back(2a):(int)(minutes/60.0) = %d,  (float)(minutes/60.0) = %f\n", (int)(minutes/60.0), (float)(minutes/60.0));

   printf( "Convert back(2)-HERE: hour = %d, minutes = %d, (int)real_minutes=%d\n", hour, minutes, real_minutes);
#endif 

   real_minutes = (int)((real_minutes/((float)data_freq)) + 0.5);

#if DEBUG
   printf( "Convert back(3): real_minutes=%d\n", real_minutes);
#endif

   qcfptr->minute_nom  = data_freq*real_minutes;
   qcfptr->hour_nom =  hour;

#if DEBUG
   printf( "Convert back(4): data_freq=%d, real_minutes=%d\n", data_freq, real_minutes);
#endif


#if DEBUG
   printf ("   Exit decrement: num_min_to_subtract = %d\n",
           num_min_to_subtract);
   printf ("   qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n", 
           qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom, 
           qcfptr->minute_nom); 
#endif 
   return;

   }/* decrement_qcfptr_nomtime()*/


/*---------------------------------------------------------
 * main() - controls processing flow for time conversions.
 *
 * 16 Jan 96 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char         init_input_file_name[NAMELEN_MAX] = "\0";
   char         input_file_name[NAMELEN_MAX] = "\0";
   char         output_file_name[NAMELEN_MAX] = "\0";

   FILE         *input_stream;
   FILE         *output_stream;

   char         inpathname[NAMELEN_MAX] = "\0";
   char         outpathname[NAMELEN_MAX] = "\0";

   char         test_file_name[NAMELEN_MAX]="\0";
   FILE         *test_stream;

   char         file_suffix[4] = "0qc\0";

   FILE_NAME_STR  output_file_names[MAX_NUM_FILES_OPEN];
   FILE         *output_streams[MAX_NUM_FILES_OPEN];

   long int     recs_processed = 0;

   int          UTC_to_SOLAR = 1; /* assume default time shift to local time. Init file overrides.*/

   char         proj_name[26] = "\0";

   int          input_data_freq = 0;
   int          data_freq     = 0;
   char         date[9]       ="\0\0\0\0\0\0\0\0\0"; /* YYYYMMDD */

   char         begin_date[9] ="\0\0\0\0\0\0\0\0\0"; /* Input date to begin (QC) processing. YYYYMMDD - input value */

   char         end_date[9]   ="\0\0\0\0\0\0\0\0\0"; /* Input date to end processing. YYYYMMDD - input value */

   int          begin_year = 0;
   int          end_year = 0;
   int          qcfrec_year = 0;

   float        reference_lon = 0.0;
   float        lon_diff_min = 0.0;
   long int     factor = 0;

   QCFREC       *qcfptr;

   STRING27     stn_list[MAXNUMSTNS];
   float        stn_lons[MAXNUMSTNS];    /* Save stn lons for printing, only */
   int          time_offset[MAXNUMSTNS]; /* offset from solar ref lon. */
   int          stn_data_freq[MAXNUMSTNS]; /* stn's data freq. */

   int          jj = 0;
   int          kk = 0;
   long int     i = 0;
   long int     stn_no       = -1;
   long int     numstns      = 0;
   long int     prev_numstns = 0;

   char         current_stn[27] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
   int          current_yr   = 0;        /* YYYY */
   long int     current_date = 0;        /* YYYYJJJ */
   char         current_date_YYYYMMDD[9] = "\0\0\0\0\0\0\0\0\0";

   long int     prev_date_YYYYJJJ = 0;
   char         prev_date_YYYYMMDD[9] = "\0\0\0\0\0\0\0\0\0";

   int          first_day = 1;
   int          old_day_nom = 0;

   char         system_cmd[350] = "\0";
   char         system_cmd1[350] = "\0";
   char         system_cmd2[350] = "\0";

   int          day_offset = 0;
   int          hr = 0;
   int          min = 0;
   int          index = 0;
   int          index2 = 0;
   int          data_too_frequent = 0;

   /*
    * Following Variables contain dates of the form:
    * YYYYJJJ where YYYY is the century and yr (e.g.
    * 2001) and JJJ is the julian day (e.g., 236 for
    * 24 August 2001). Dates in this form are easily
    * incremented, compared, subracted from, etc.
    */
   long int begin_YYYYJJJ = 0;
   long int end_YYYYJJJ  = 0;

   /*
    * Vars used to determine if data rollsover into
    * next year.
    */
   char         end_YYYY_str[5]   = "\0\0\0\0\0";
   char         end_MMDD[5]       = "\0\0\0\0\0";
   char         begin_YYYY_str[5] = "\0\0\0\0\0";
   char         begin_MMDD[5]     = "\0\0\0\0\0";

   /*
    * System resource limit: RLIMIT_NOFILE = max number
    * of open files per process. Original SUN soft limit
    * was 64 files with a hard limit of 1024.
    */
   struct rlimit   mylimit;

   /*
    * If data frequency is mixed, must handle each
    * frequency properly. If new networks are encountered,
    * increase MAX_NETWORKS (defined above) and add
    * new type to network_freq[] below.
    */
   int           mixed_freq = 0;

   struct {char  network[11]; int  freq;} network_freq[ MAX_NETWORKS] = { 
                              {  "ABLE_AWS  \0",  1}, /*1min*/
                              {  "ARMSFC1   \0",  1}, /*hrly in some projects*/
                              {  "NCAR_supp \0",  1}, /*1min*/
                              {  "ASOS      \0",  5}, /*1min*//*But set converted to 5min for TREX2006*/
                              {  "AWOS_IA   \0",  1}, /*1min*/
                              {  "ChLkASOS  \0",  5}, /*1min*//*Src is 1min converted to 5min for TREX2006*/
                              {  "IA_SCH_NET\0",  1}, /*1min*/
                              {  "ISS       \0",  1}, /*1min*/
                              {  "LAIS      \0",  1}, /*1min*/
                              {  "ARS       \0",  5}, /*5min*/
                              {  "ASOS5     \0",  5}, /*5min*/
                              {  "ASUsonic  \0",  5}, /*5min*/
                              {  "AWOS1     \0",  5}, /*5min*/
                              {  "AWOSA05   \0",  5}, /*5min*/
                              {  "ChLkHAND05\0",  5}, /*5min*/
                              {  "DRI_AWS05 \0",  5}, /*5min*/
                              {  "Utah_HOBO5\0",  5}, /*5min*/
                              {  "ISFF05    \0",  5}, /*5min*/
                              {  "ISS05     \0",  5}, /*5min*/
                              {  "ISS05M    \0",  5}, /*5min*/
                              {  "LeedsAWS05\0",  5}, /*5min*/
                              {  "MISS05    \0",  5}, /*5min*/
                              {  "OKMESO5   \0",  5}, /*5min*/
                              {  "TREX05    \0",  5}, /*5min*/
                              {  "WTXMESO   \0",  5}, /*5min*/
                              {  "AWOSQ20   \0",  20},/*20min*/
                              {  "AWOSH20   \0",  20},/*20min*/
                              {  "AWOSA20   \0",  20},/*20min*/
                              {  "AIRQUAL   \0",  60},/*60min*//*VERIFY*/
                              {  "AIRQUAL_IL\0",  60},/*60min*//*VERIFY*/
                              {  "AIRQUAL_OH\0",  60},/*60min*//*VERIFY*/
                              {  "AgriMet   \0",  60},/*60min*/
                              {  "ASOSH     \0",  60},/*60min*/
                              {  "AWON      \0",  60},/*60min*//*VERIFY*/
                              {  "AWOS      \0",  60},/*60min*/
                              {  "AWOSA60   \0",  60},/*60min*/
                              {  "ChLkHAND60\0",  60},/*60min*/
                              {  "CIMIS60   \0",  60},/*60min*/
                              {  "COAGMET   \0",  60},/*60min*/
                              {  "DATSAV3   \0",  60},/*60min*/
                              {  "DATSAV3M  \0",  60},/*60min*//*Mobile*/
                              {  "GLERL     \0",  60},/*60min*/
                              {  "GBUAPCD60 \0",  60},/*60min*/
                              {  "GLERLM    \0",  60},/*60min*//*Mobile GLERLM*/
                              {  "GWMD5     \0",  60},/*60min*/
                              {  "HPLAINS   \0",  60},/*60min*/
                              {  "HPCN      \0",  60},/*60min*//*new for HPLAINS*/
                              {  "ISWS/ICN  \0",  60},/*60min*//*was ISWS*/
                              {  "KVII      \0",  60},/*60min*/
                              {  "KONZA_LTER\0",  60},/*60min*/
                              {  "LCRA      \0",  60},/*60min*/
                              {  "LDMSFCMETR\0",  60},/*60min*/
                              {  "MAWN      \0",  60},/*60min*/
                              {  "MOCAWS    \0",  60},/*60min*/
                              {  "MODOC     \0",  60},/*60min*/
                              {  "MTDOT_RWIS\0",  60},/*60min*/
                              {  "NMSU      \0",  60},/*60min*/
                              {  "NPN       \0",  60},/*60min*//*was WPDN, then WPN*/
                              {  "NRCS/SMST \0",  60},/*60min*/
                              {  "OARDC     \0",  60},/*60min*/
                              {  "PAAWS     \0",  60},/*60min*/
                              {  "PET       \0",  60},/*60min*/
                              {  "SWKSMESO  \0",  60},/*60min*/
                              {  "WI_AWON   \0",  60},/*60min*/
                              {  "ms:APRSWXN\0",  60},/*60min*/
                              {  "ms:AWX    \0",  60},/*60min*/
                              {  "ms:CODOT  \0",  60},/*60min*/
                              {  "ms:DDMET  \0",  60},/*60min*/
                              {  "ms:FL-Meso\0",  60},/*60min*/
                              {  "ms:FSL-Mis\0",  60},/*60min*/
                              {  "ms:GLDNWS \0",  60},/*60min*/
                              {  "ms:GLOBE  \0",  60},/*60min*/
                              {  "ms:GPSMET \0",  60},/*60min*/
                              {  "ms:IADOT  \0",  60},/*60min*/
                              {  "ms:IEM    \0",  60},/*60min*/
                              {  "ms:KSDOT  \0",  60},/*60min*/
                              {  "ms:MAP    \0",  60},/*60min*/
                              {  "ms:MesoWes\0",  60},/*60min*/
                              {  "ms:MNDOT  \0",  60},/*60min*/
                              {  "ms:NOS-NWL\0",  60},/*60min*/
                              {  "ms:NOS-POR\0",  60},/*60min*/
                              {  "ms:RAWS   \0",  60},/*60min*/
                              {  "ms:UDFCD  \0",  60},/*60min*/
                              {  "ms:WXforYo\0",  60},/*60min*/
                              {  "RWIS      \0",  60},/*60min*//*VERIFY*/
                              {  "RWIS_IA   \0",  60},/*60min*/
                              {  "RWIS_ND   \0",  60},/*60min*/
                              {  "RWIS_OH   \0",  60},/*60min*/
                              {  "RWIS_SD   \0",  60},/*60min*/
                              {  "TNRCC     \0",  60},/*60min*/
                              {  "WYDOT_RWIS\0",  60} }; /*60min*/

   /*------------------------------------------------------
    * Up the number of open files that a process 
    * can have attached to it. Set that number 
    * based upon the frequency of the data being procesed.
    *------------------------------------------------------*/
   if (getrlimit (RLIMIT_NOFILE, &mylimit) < 0)
      {
      printf ("ERROR: getrlimit() error for RLIMIT_NOFILE");
      exit(1);
      }

   printf ("Increase number of open files that can be attached to process to %d\n",
           MAX_NUM_FILES_OPEN); /* always print */

#if DEBUG
   if (mylimit.rlim_cur == RLIM_INFINITY)
      printf ("ORIGINAL Soft Limit: infinite\n");
   else
      printf ("ORIGINAL Soft Limit: %10ld  ", mylimit.rlim_cur);

   if (mylimit.rlim_max == RLIM_INFINITY)
      printf ("ORIGINAL Hard Limit: infinite");
   else
      printf ("ORIGINAL Hard Limit: %10ld   \n", mylimit.rlim_max);
#endif

   mylimit.rlim_cur = MAX_NUM_FILES_OPEN;  /* C LIMITS EVERY PROCESS TO ONLY 252 FILES OPEN!! */

   if (setrlimit (RLIMIT_NOFILE, &mylimit) != 0)
      {  
      printf ("ERROR: setrlimit() error for RLIMIT_NOFILE\n");
      exit(1);
      }

#if DEBUG  
   if (getrlimit (RLIMIT_NOFILE, &mylimit) < 0)
      {  
      printf ("ERROR: On second getrlimit() error for RLIMIT_NOFILE");
      exit(1);
      }   
   if (mylimit.rlim_cur == RLIM_INFINITY)
      printf ("NEW      Soft Limit: infinite\n");
   else  
      printf ("NEW      Soft Limit: %10ld  ", mylimit.rlim_cur);
 
   if (mylimit.rlim_max == RLIM_INFINITY)
      printf ("NEW      Hard Limit: infinite");
   else  
      printf ("NEW      Hard Limit: %10ld   \n", mylimit.rlim_max);
#endif 


   /*--------------------------------------------
    * Now begin processing data:
    *
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 2)
      {  
      printf ("Usage: convert_QCF_time_4DYR <initialization_input_file>\n");
      exit(1);
      }  

   strcpy (init_input_file_name, argv[1]);

   /*-----------------------------
    * Initialize from input file. 
    *---------------------------*/
   printf ("\n---- Convert QCF times to SOLAR  (4 Digit YR)----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("Initialization file name (e.g., TIMEinit.inp): %-s\n", init_input_file_name);
   
   open_file (init_input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);

   printf ("\nPARAMETER INITIALIZATION:: \n\n");

   /*----------------------------
    * Read data from input file.
    *---------------------------*/ 
   fgets(proj_name, 25, input_stream);
   proj_name[25] = '\0';
   STRIPLINE(input_stream);

   fscanf (input_stream, "%d", &UTC_to_SOLAR);STRIPLINE(input_stream);

   if (UTC_to_SOLAR)
      {
      strncpy (file_suffix, "0qc", 3);
      printf ("Converting data from UTC to SOLAR!\n");
      }
   else
      {
      strncpy (file_suffix, "qcf", 3);
      printf ("Converting data from SOLAR to UTC!\n");
      }


   fscanf (input_stream, "%d", &input_data_freq);   STRIPLINE(input_stream);
   data_freq = input_data_freq;
   day_offset = (1440/input_data_freq); /*Base this on highest frequency in data */

   fscanf (input_stream, "%d", &mixed_freq);  STRIPLINE(input_stream);
   fscanf (input_stream, "%d", &current_yr);  STRIPLINE(input_stream); /* 4 digits */
   printf ("Current year of data: %d\n",                current_yr);

   fscanf (input_stream, "%s", begin_date);   STRIPLINE(input_stream); /* YYYYMMDD */
   begin_date[8] = '\0';

   fscanf (input_stream, "%s", end_date);     STRIPLINE(input_stream); /* YYYYMMDD */

   end_date[8] = '\0';


   fscanf (input_stream, "%s", inpathname);   STRIPLINE(input_stream);
   fscanf (input_stream, "%s", outpathname);  STRIPLINE(input_stream);
   fscanf (input_stream, "%f", &reference_lon);

   printf ("Project Name (proj_name): %-s\n",           proj_name);
   printf ("Conversion Type Flag (UTC_to_SOLAR): %d\n", UTC_to_SOLAR);
   printf ("Data Frequency (data_freq): %d\n",          data_freq);
   printf ("Mixed Frequency flag (mixed_freq): %d\n",   mixed_freq);
 
   if (!mixed_freq)
      printf ("Data is not mixed frequency!\n");
   else
      printf ("Data is MIXED frequency! Must compute offsets on the fly.\n");

   printf ("Current year of data: %d\n",                current_yr);
   printf ("First day to process (begin_date) : %-s\n", begin_date);
   printf ("Last day to process (end_date)    : %-s\n", end_date);
   printf ("Input file inpathname: %-s\n",              inpathname);
   printf ("Output file outpathname: %-s\n",            outpathname);
   printf ("Reference Longitude: %f\n\n",               reference_lon);

   /*------------------------------------
    * Close input initialization file.
    *------------------------------------*/
   close_file (&input_stream, FILE_NOT_COMPRESSED);

   /*------------------------------------
    * Determine best processing path.
    * If data frequency is higher than
    * 15 minutes, then this process
    * can not open enough files (2 day's
    * worth) to take advantage of faster
    * processing method.
    *-----------------------------------*/
   if (data_freq < 15) data_too_frequent = 1;

   /*---------------------------------------------
    * Construct qcfrec pointer then read the data.
    *---------------------------------------------*/
   construct_qcfptr (&qcfptr);
 
   /*------------------------------------------------------------------
    * Convert input dates to Julian and to YYYYJJJ form for easier
    * manipulation of dates.
    *------------------------------------------------------------------
    * If end_date is less than begin_date then the data must rollover
    * through the end of the year. Also note that the year will
    * change and the century might change!
    *-----------------------------------------------------------------*/
   strncpy (end_YYYY_str, &end_date[0],4);
   strncpy (end_MMDD,   &end_date[4], 4);
   end_YYYY_str[4]  = '\0';
   end_MMDD[4]    = '\0';
   end_year = atoi (end_YYYY_str);

   strncpy (begin_YYYY_str, &begin_date[0],4);
   strncpy (begin_MMDD,   &begin_date[4],4);
   begin_YYYY_str[4] = '\0';
   begin_MMDD[4]   = '\0';
   begin_year = atoi (begin_YYYY_str);

#if DEBUG
   printf ("begin_year, end_year  = %d  %d\n",
            begin_year, end_year);
#endif

   /* Must send in correct century for each year. */
   /*---------------------------------------------------------
    * Properly set the century and year even if data rollsover
    * into next year as march through input files. 
    *--------------------------------------------------------*/
   date_to_YYYYJJJ (begin_date, begin_year, &begin_YYYYJJJ);
   date_to_YYYYJJJ (end_date,   end_year, &end_YYYYJJJ);

#if  DEBUG
   printf ("begin_date, end_date: %-s %-s\n", begin_date, end_date);
   printf ("begin_YYYYJJJ, end_YYYYJJJ: %ld %ld\n",
            begin_YYYYJJJ, end_YYYYJJJ);
#endif

   if (end_YYYYJJJ < begin_YYYYJJJ)
      {
	  printf ("WARNING: end date is less than begin date. Exit.\n");
	  exit(1);
	  }

   if (!data_too_frequent && UTC_to_SOLAR) /* not implemented for StoU, yet */
      {
      /*--------------------------------------------------
       * If the data is NOT too frequent:
       * Open all the possible output files that one src day
       * can write into! Since any single source file can
       * write output into at most two day files, Open all
       * the files in these two days. The number of files
       * depends strictly on the frequency of the data.
       * Data can be written into an output file with
       * same date as the source file and into an output
       * file with the previous day's date for the 
       * UTC to SOLAR time conversion. Data can be written into
       * an output file with same date as the source file and
       * into an output file with the next day's date for 
       * the SOLAR to UTC conversion.
       *
       * Open files for "first" previous day.
       *-------------------------------------------------*/
      if (UTC_to_SOLAR)
        prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( begin_YYYYJJJ, 1);
      else
        prev_date_YYYYJJJ = add_num_to_YYYYJJJ( begin_YYYYJJJ, 1); /* NEXT??? */

      YYYYJJJ_to_date ( prev_date_YYYYJJJ, prev_date_YYYYMMDD);
     
 
      for (hr=0; hr<24; hr++)
         {
         for (min=0; min<60; min= min+input_data_freq)
           {
           index = (hr*60 + min)/input_data_freq;

           if (UTC_to_SOLAR) 
              sprintf (output_file_names[index], "%-ssolar_%-8s_%02d%02d.%-3s\0",
                       outpathname, prev_date_YYYYMMDD, hr, min, file_suffix);
           else
              sprintf (output_file_names[index], "%-sutc_%-8s_%02d%02d.%-3s\0",
                       outpathname, prev_date_YYYYMMDD, hr, min, file_suffix);

           open_file (output_file_names[index], "w", FILE_NOT_COMPRESSED, &output_streams[index]);
#if DEBUG
           printf ("Open output file[%d]: %-sxxx\n", index, output_file_names[index]);
#endif
           } /* for all times  - previous day */
         } /* for all hours  - previous day */

#if DEBUG
      printf ("Files for first -previous(NEXT for StoU) - day are open!\n");
#endif 
      } /**** !Data_too_frequent ****/


   numstns = 0;
   first_day = 1;

   /*-----------------------------------------------------------------
    * Do Time conversion processing on all requested data. Modify the
    * first time in the QCF record from UTC to Solar time based upon
    * the input reference station.
    *----------------------------------------------------------------*/
   for(current_date=begin_YYYYJJJ; current_date<=end_YYYYJJJ; increment_YYYYJJJ(&current_date))
      {
      /*-----------------------------------------------------------------------
       * Form name of input file and open. Assume name of data being worked
       * is of the form yyyyddmm.0qc and the output file will be solar_yyyyddmm.0qc
       * for UTC to SOLAR and of the form solar_yyyyddmm.qcf for input and form
       * yyyyddmm.qcf for output of SOLAR to UTC processing.
       * All times are referenced to the input ref longitude. Choose the ref lon 
       * to be (at least) the most eastern longitude in the dataset.
       *-----------------------------------------------------------------------*/
      YYYYJJJ_to_date (current_date, date);

#if DEBUG
      printf ("\nCurrent julian date is: %ld %-s\n", current_date, date);
#endif

      if (UTC_to_SOLAR)
         sprintf (input_file_name, "%-s%-s.%-3s\0", inpathname, date, file_suffix);
      else
         sprintf (input_file_name, "%-ssolar_%-s.%-3s\0", inpathname, date, file_suffix);

      open_file (input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);

      printf ("Processing file: %-s\n", input_file_name); /* Always print */

      /*------------------------------
       * Do cleanup for previous day. 
       *------------------------------*/
      if (!first_day)
         {
         if (UTC_to_SOLAR)
            prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( current_date, 2);
         else
            prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( current_date, 1);

         YYYYJJJ_to_date (prev_date_YYYYJJJ, prev_date_YYYYMMDD);

#if DEBUG
         printf ("Not the first day - close old previous day files.\n");
#endif
        /*------------------------------------------------------------
         * If data is not too frequent, Open all files for current day.
         * Close the last current_day's prev_day's open files.
         * And reposition the file names in the open file index list.
         * For too high frequency data, just cleanup the previous
         * days files.
         *----------------------------------------------------------*/
         if (!data_too_frequent && UTC_to_SOLAR)
            { 
            for (hr=0; hr<24; hr++)
              {  
              for (min=0; min<60; min= min+input_data_freq)
                {
                index = (hr*60 + min)/input_data_freq;

                if (UTC_to_SOLAR)
                   sprintf (output_file_names[index], "%-ssolar_%-8s_%02d%02d.%-3s",
                            outpathname, prev_date_YYYYMMDD, hr, min, file_suffix);
                else
                   sprintf (output_file_names[index], "%-s%-8s_%02d%02d.%-3s",
                            outpathname, prev_date_YYYYMMDD, hr, min, file_suffix);

                close_file (&output_streams[index], FILE_NOT_COMPRESSED);
#if DEBUG
                printf ("Close output file[%d]: %-sxxx\n", index, output_file_names[index]);
#endif
                /*----------------------------------
                 * Now can overwrite that file name
                 * location. Shift everything up 
                 * one day. Move current day names
                 * into previous day locations.
                 *---------------------------------*/
                strcpy (output_file_names[index], output_file_names[index + day_offset]);
                output_streams[index] = output_streams[index + day_offset];

                } /* for all times  - previous day */
              } /* for all hours  - previous day */
#if DEBUG
            for (hr=0; hr<24; hr++)
              {  
              for (min=0; min<60; min= min+input_data_freq)
                {
                index = (hr*60 + min)/input_data_freq;
                printf ("NEW Open output file[%d]: %-sxxx\n", index, output_file_names[index]);
                } /* for all times  - previous day */
              } /* for all hours  - previous day */

             printf ("Files for NEW (NEXT) first -previous- day are open!\n");
#endif
           } /**** !data_too_frequent && UTC_to_SOLAR *****/


         /*-----------------------------------------------
          * Cat the closed files together (in time order)
          * to form day files and remove the minute files.
          * Note that the HQC processing and compute
          * sigma processing only care that like times are
          * together. Sorting within each time is not
          * important. However, when converting back to 
          * UTC time from SOLAR, the final qcf data must
          * be sorted by nominal time, lat, and then lon.
          *-----------------------------------------------*/
         if (!UTC_to_SOLAR) /* => SOLAR_TO_UTC */
            {
            for (hr=0; hr<24; hr++)
              {  
              for (min=0; min<60; min= min+input_data_freq)
                {
#if DEBUG
                printf ("Cleanup hr, min:: %d %d\n", hr, min);
#endif HERE
                /*--------------------------------------
                 * Before copying a file, determine if
                 * that file exists. If the file does
                 * exist, cp it to input.xxx, cosort
                 * that small file, and then cat that
                 * sorted small file to the end of final
                 * output file.
                 *--------------------------------------*/
                sprintf (test_file_name, "%-sutc_%-8s_%02d%02d.%-3s\0",
                         outpathname, prev_date_YYYYMMDD, hr, min, file_suffix);

                test_stream = fopen(test_file_name, "r+");

                if (test_stream!= NULL)
                   {
                   /* File exists - close it continue. */
                   fclose(test_stream);
#if DEBUG
                   printf ("File %-s exist - cp, cosort, cat it!\n", test_file_name);
#endif
                   sprintf (system_cmd, "cp %-s input.%-3s\0",
                            test_file_name, file_suffix);
                   system( system_cmd );

                /*-------------------------------------------------
                 * Sort data by Nominal time, lat, and then by lon.
                 * Sorts input.qcf into input.srt. Sort each piece
                 * then cat together into final UTC qcf file.
                 *------------------------------------------------*/
                   sprintf (system_cmd1, "cosort sort_4DYR.inp\0"); /* sorts input.qcf into input.srt */
                   system( system_cmd1 );

                   sprintf (system_cmd2, "cat input.srt >>%-sutc_%-8s.%-3s\0",
                            outpathname, prev_date_YYYYMMDD, file_suffix);
                   system( system_cmd2 );
#if DEBUG
                   printf ("Execute: xxx%-sxxx\n", system_cmd);
                   printf ("Execute: xxx%-sxxx\n", system_cmd1);
                   printf ("Execute: xxx%-sxxx\n", system_cmd2);
#endif
                   }
#if DEBUG
                else /* test_stream != NULL */
                   printf ("File %-s does not exist - look for next file\n",
                      test_file_name);
#endif
                } /* for all times in a day */
              } /* for all hours in a day */
            } /* UTC_to_SOLAR */
         else
            {
            sprintf (system_cmd, "cat %-ssolar_%-8s_*.%-3s >>%-ssolar_%-8s.%-3s\0",
                     outpathname, prev_date_YYYYMMDD, file_suffix, 
                     outpathname, prev_date_YYYYMMDD, file_suffix);
#if DEBUG
            printf ("Execute: xxx%-sxxx\n", system_cmd);
#endif
            system( system_cmd );

            } /* UTC_to_SOLAR */


         for (kk = 0;kk < 255; kk++) 
            system_cmd[kk] = '\0';

#if DELETE_INTERMEDIATE_FILES         
         if (UTC_to_SOLAR)
            sprintf (system_cmd, "/bin/rm %-ssolar_%-8s_*.%-3s\0",
                     outpathname, prev_date_YYYYMMDD, file_suffix);   
         else
            sprintf (system_cmd, "/bin/rm %-sutc_%-8s_*.%-3s\0",
                     outpathname, prev_date_YYYYMMDD, file_suffix); 
#endif

#if DEBUG 
         printf ("YES -----Execute: xxx%-sxxx\n", system_cmd); 
#endif
   
#if DELETE_INTERMEDIATE_FILES
         system( system_cmd );
#endif

         } /* !first_day */

      if (!data_too_frequent && UTC_to_SOLAR)
         {
         /*----------------------------------
          * Open files for new current day.
          *---------------------------------*/
         for (hr=0; hr<24; hr++)
           {
           for (min=0; min<60; min= min+input_data_freq)
             {
             index = ((24+hr)*60 + min)/input_data_freq; /*shifted up one day's worth*/
 
             sprintf (output_file_names[index], "%-ssolar_%-8s_%02d%02d.%-3s",
                      outpathname, date, hr, min, file_suffix);
             open_file (output_file_names[index], "w", FILE_NOT_COMPRESSED, &output_streams[index]);
#if DEBUG
             printf ("Open output file[%d]: %-sxxx\n", index, output_file_names[index]);
#endif
             } /* for all times  - current day */
           } /* for all hours  - current day */
#if DEBUG
        printf ("Files for current day are open!\n");
#endif
        } /* --- !data_too_frequent && UTC_to_SOLAR --- */


      /*--------------------------------------------------
       * We assume that the input (*.0qc) files are sorted
       * by date/time. Although it really may not matter.
       * Note that every input day can at most affect 2
       * output days (i.e., that day and the previous day).
       *-------------------------------------------------*/
      recs_processed = 0;

      while (!feof(input_stream))
         {
         reset_qcfrec( qcfptr );
         read_qcfrec (&input_stream, qcfptr);

         /*-----------------------------------------*
          * Check and reset the current year if we
          * have rolled over into the next year.
          *-----------------------------------------*/
         qcfrec_year = qcfptr->year_nom;
#if DEBUG
         printf ("current yr, qcfrec_year  = %d  %d\n",
                  current_yr, qcfrec_year);
#endif
			   
		 if (current_yr < qcfrec_year)
		    {
                    current_yr = qcfrec_year;
#if DEBUG
                    printf ("Updated current year (date rollover) to match data = %d\n",
                             current_yr);
#endif
            }

         /**************************************************
          * Following set of code is here to correct a
          * problem noticed with the IOWA AWOS1 minute data.
          * The IOWA AWOS1 data always has the gust flag set
          * to G regardless of whether or not there was a 
          * gust. Correct this mistake here. The IOWA AWOS1
          * conversion s/w has since been corrected - so
          * after V95/STORMWAVE, we should be able to remove
          * the following gust correction!
          **************************************************/
         if (!strncmp (qcfptr->qnet, "AWOS1", 5))
            {
#if 1
            printf ("Resetting AWOS1- IOWA GUST to blank!!\n");
#endif
            if (qcfptr->squall <= 0.0)
               qcfptr->sg = ' '; /* reset to Blank for -999.99 or 0.0 */
            }

         recs_processed++;

#if DEBUG
         printf ("\n---CURRENT TIME: qcfptr->hour_nom, qcfptr->minute_nom: %d %d\n",
                 qcfptr->hour_nom, qcfptr->minute_nom);
#endif

         /*------------------------------------------------------
          * Determine the "correct" solar time for this record,
          * then write the record to the proper output file:
          * solar_yyyymmdd_hhmm.0qc. Compute the difference between
          * the ref longitude and the current stations longitude.
          * Round this diff to the nearest "data_freq" amount of
          * time. Write this new time into this qcfrec and then
          * output the updated qcfrec to the correct solar file.
          *------------------------------------------------------*/
         prev_numstns = numstns;

         sprintf (current_stn, "%-10s:%-15s\0",qcfptr->qnet,qcfptr->statn);
         stn_no = determine_stn_no(stn_list, &numstns, current_stn);

         if (stn_no > MAXNUMSTNS)
            {
            printf ("WARNING: stn number exceeds MAXNUMSTNS limit!");
            exit(1);
            } 

#if DEBUG
         printf ("prev_numstns, numstns: %ld %ld.  CURRENT STN---------------->%-s\n", 
                 prev_numstns, numstns, current_stn);
#endif
         /*-----------------------------------------------------------
          * If new stn encountered, compute time offset from ref stn.
          *----------------------------------------------------------*/
         if (prev_numstns < numstns)
            {
            /*---------------------------------------------------
             * Compute time offset by differencing the longitudes
             * and converting lon diff to minutes. Round to 
             * nearest possible time in data. Compute offset only
             * once and save. Extra work required for mixed
             * frequency composites. Must determine data_freq
             * from network name.
             *--------------------------------------------------*/
            lon_diff_min = (reference_lon - qcfptr->lon)*DEG_TO_MIN;

            if (mixed_freq)
               { 
               data_freq = -1;
               for (jj = 0; jj<MAX_NETWORKS; jj++)
                  {
#if DEBUG
                  printf ("Compare xxx%-sxxx and xxx%-sxxx\n",
                           qcfptr->qnet, network_freq[jj].network);
#endif
                  if (!strncmp (qcfptr->qnet, network_freq[jj].network, 10))
                     {
                     /* Found Matching network, extract frequency */
                     data_freq = network_freq[jj].freq;
#if DEBUG
                     printf ("Found a Matching network!!!data_freq = %d\n",
                      data_freq);
#endif
                     break;
                     }
                  } /* for jj */
 
               if (data_freq == -1)
                  {
                  printf ("ERROR: Unknown network type: %-s\n", qcfptr->qnet);
                  printf ("ERROR at rec %d of file %-s\n", recs_processed, input_file_name);
                  exit (1);
                  }
 
               } /* mixed_freq */
 
            factor = (int)((lon_diff_min/((float)data_freq)) + 0.5); /* Here use stns freq */
            time_offset[stn_no]   = data_freq * factor;
            stn_data_freq[stn_no] = data_freq;
            stn_lons[stn_no]      = qcfptr->lon;
#if DEBUG
            printf ("reference_lon, qcfptr->lon, lon_diff_min: %f %f %f \n",
                     reference_lon, qcfptr->lon, lon_diff_min);
#endif
           } /* prev_numstns < numstns */

         /*-----------------------------------------------------
          * Pull out freq for this stn, when search not required
          *----------------------------------------------------*/
         data_freq = stn_data_freq[stn_no];
 
         /*--------------------------------------------------------
          * Update output qcfptr nominal times to solar times OR
          * from solar times to UTC times.
          *--------------------------------------------------------*/
         old_day_nom = qcfptr->day_nom;     /* save for comparison */

         if (UTC_to_SOLAR)
            {
            if (lon_diff_min >= 0.0)
               {
               decrement_qcfptr_nomtime(qcfptr, current_yr, stn_data_freq[stn_no], 
                                        time_offset[stn_no]);
#if DEBUG
               printf ("After decrement(): qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n",
                       qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom, 
                       qcfptr->minute_nom);
#endif
               }
            else /* Found station to the EAST of ref lon */
               {
               printf ("WARNING: Found station to the EAST of reference Lon!\n");
               printf ("   Stn: %-s at %7.2f is EAST of ref_lon= %7.2f\n",
                      stn_list[stn_no], stn_lons[stn_no], reference_lon);

               printf ("  (StoU) INCREMENT - don't decrement!!!!\n");
 
               increment_qcfptr_nomtime (qcfptr, current_yr, stn_data_freq[stn_no],
                                      time_offset[stn_no]);
#if DEBUG
               printf ("After increment(): qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n",
                       qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom,
                       qcfptr->minute_nom);
#endif   
               }
            }
        else /* SOLAR to UTC */    
            {
            if (lon_diff_min >= 0.0)
               { 
               increment_qcfptr_nomtime(qcfptr, current_yr, stn_data_freq[stn_no], 
                                        time_offset[stn_no]);
#if DEBUG
               printf ("STU: After increment(): qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n",
                       qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom,
                       qcfptr->minute_nom);
#endif   
               } 
            else /* Found station to the EAST of ref lon */
               {
               printf ("WARNING: Found station to the EAST of reference Lon!\n");
               printf ("   (STU: Stn: %-s at %7.2f is EAST of ref_lon= %7.2f\n",
                      stn_list[stn_no], stn_lons[stn_no], reference_lon);
 
               decrement_qcfptr_nomtime(qcfptr, current_yr, stn_data_freq[stn_no], 
                                       time_offset[stn_no]);
#if DEBUG
               printf ("After increment(): qcfptr->yr,mon,day,hr,min:: %04d%02d%02d %02d:%02d\n",
                       qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom,
                       qcfptr->minute_nom);
#endif   
               }
            } /* convert_QCF_time_4DYR */


         if (!data_too_frequent && UTC_to_SOLAR)
            {
            /*------------------------------------------------
             * Determine the output file to write
             * this rec into.  File should be open. Write rec.
             *-----------------------------------------------*/ 
#if DEBUG
            printf ("Compare old_day_nom, qcfptr->day_nom, hour_nom: %d %d, %d\n",
                    old_day_nom, qcfptr->day_nom, qcfptr->hour_nom);
#endif
            if (qcfptr->day_nom != old_day_nom)
               {
               /*-------------------------------
                * Must write to prev day for UtoS.
                * (5 min data = indices:0-287)
                * Hourly data = indices: 0-22.
                *------------------------------*/
#if DEBUG
               printf ("Write to prev day!\n");
#endif
               index = (qcfptr->hour_nom*60 + min)/data_freq;
               }
            else
               {
               /*------------------------------------
                * Must still be in same day for UtoS. 
                * (5 min data = indices:288-575)
                * Hourly data = indices: 23-47.
                *-----------------------------------*/
#if DEBUG
               printf ("Write to SAME day! - second half of 2 days.\n");
#endif
               index = ((24+qcfptr->hour_nom)*60 + min)/data_freq;
               }

#if DEBUG
            printf ("Write record to file[%d]: %-s\n", index-1, output_file_names[index-1]);
#endif
            write_qcfrec(&output_streams[index-1], qcfptr);

            } /* !data_too_frequent && UTC_to_SOLAR */   
         else
            {
            /*------------------------------------------------
             * Data too frequent to have 2 day's worth of
             * files open at once so....
             * Determine the name of the output file to write
             * this rec into.  Write rec and then close file.
             *-----------------------------------------------*/
            if (UTC_to_SOLAR)
               sprintf (output_file_name, "%-ssolar_%04d%02d%02d_%02d%02d.%-3s",
                        outpathname, qcfptr->year_nom, qcfptr->month_nom,
                        qcfptr->day_nom, qcfptr->hour_nom, qcfptr->minute_nom, file_suffix);
            else
               sprintf (output_file_name, "%-sutc_%04d%02d%02d_%02d%02d.%-3s",
                        outpathname, qcfptr->year_nom, qcfptr->month_nom,
                        qcfptr->day_nom, qcfptr->hour_nom, qcfptr->minute_nom, file_suffix);

            if (qcfptr->minute_nom >= 60)
               { 
               printf ("ERROR: incorrect minute of %2d at rec %d.\n", 
                        qcfptr->minute_nom, recs_processed);
               exit(1);
               } 
 
            if (qcfptr->hour_nom >= 24)
               { 
               printf ("ERROR: incorrect hour of %2d at rec %d.\n",
                        qcfptr->hour_nom, recs_processed);
               exit(1);
               } 

            /*----------------------------------------------------------
             * Open a new file, or append to an existing one for output.
             *---------------------------------------------------------*/
            open_file (output_file_name, "a", FILE_NOT_COMPRESSED, &output_stream);
#if DEBUG
            printf ("Open output file: %-sxxx\n", output_file_name);
#endif
            write_qcfrec(&output_stream, qcfptr);
    
            close_file (&output_stream, FILE_NOT_COMPRESSED);
            } /* data_too_frequent && UTC_to_SOLAR */

         if (feof(input_stream)) break;
   
         } /* while data in input file */

#if DEBUG
      printf ("Completed processing %ld recs for input_stream on %ld. GO TO NEXT DATE.\n", 
               recs_processed, current_date);
#endif
      close_file (&input_stream, FILE_NOT_COMPRESSED);

      first_day = 0;

      /*---------------------------------
       * Do cleanup on final day's files.
       *---------------------------------*/
      if (current_date == end_YYYYJJJ)
         {
#if DEBUG
      printf ("Do final days cleanup\n");
#endif
         if (UTC_to_SOLAR)
            prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( current_date, 1);
         else
            {
            prev_date_YYYYJJJ = current_date;
            increment_YYYYJJJ( &current_date );
            }

         YYYYJJJ_to_date ( prev_date_YYYYJJJ, prev_date_YYYYMMDD);

         if (!data_too_frequent && UTC_to_SOLAR)
            {
            for (hr=0; hr<24; hr++)
              {
              for (min=0; min<60; min= min+input_data_freq)
                {
                index = (hr*60 + min)/input_data_freq;
                index2 = ((24+hr)*60 + min)/input_data_freq;

                close_file (&output_streams[index], FILE_NOT_COMPRESSED);
                close_file (&output_streams[index2], FILE_NOT_COMPRESSED);
#if DEBUG
                printf ("Close output file[%d]: %-sxxx\n", index, output_file_names[index]);
                printf ("Close output file[%d]: %-sxxx\n", index2, output_file_names[index2]);
#endif
                } /* for all times  - previous day */
              } /* for all hours  - previous day */
           } /* !data_too_frequent && UTC_to_SOLAR */

         /*----------------------------------
          * Cat previous day then current day.
          *---------------------------------*/
         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';

         if (!UTC_to_SOLAR)
            {
            /*-----------------------------
             * Cosort the data back into
             * original form by: Nominal
             * time, lat, then lon.
             *-----------------------------*/
            for (hr=0; hr<24; hr++)
              {  
              for (min=0; min<60; min= min+input_data_freq)
                {
                /*--------------------------------------
                 * Before copying a file, determine if 
                 * that file exists. If the file does
                 * exist, cp it to input.xxx, cosort
                 * that small file, and then cat that
                 * sorted small file to the end of final
                 * output file.
                 *--------------------------------------*/
                sprintf (test_file_name, "%-sutc_%-8s_%02d%02d.%-3s\0",
                         outpathname, prev_date_YYYYMMDD, hr, min, file_suffix);

                test_stream = fopen(test_file_name, "r+");

                if (test_stream!= NULL)
                   {
                   /* File exists - close it continue. */
                   fclose(test_stream);
#if DEBUG
                   printf ("File %-s exist - cp, cosort, cat it!\n", test_file_name);
#endif
                   sprintf (system_cmd, "cp %-s input.%-3s\0",
                            test_file_name, file_suffix);
                   system( system_cmd );

                   sprintf (system_cmd1, "cosort sort_4DYR.inp\0"); /* sorts input.qcf into input.srt */
                   system( system_cmd1 );

                   sprintf (system_cmd2, "cat input.srt >>%-sutc_%-8s.%-3s\0",
                            outpathname, prev_date_YYYYMMDD, file_suffix);
                   system( system_cmd2 );
#if DEBUG  
                   printf ("Execute: xxx%-sxxx\n", system_cmd);
                   printf ("Execute: xxx%-sxxx\n", system_cmd1);
                   printf ("Execute: xxx%-sxxx\n", system_cmd2);
#endif
                   }
#if DEBUG
                else /* test_stream != NULL */
                   printf ("File %-s does not exist - look for next file\n",
                      test_file_name);
#endif


                /*--------------------------
                 * Now cleanup final date.
                 * if more than one day.
                 *-------------------------*/
                YYYYJJJ_to_date (current_date, current_date_YYYYMMDD);

                sprintf (test_file_name, "%-sutc_%-8s_%02d%02d.%-3s\0",
                         outpathname, current_date_YYYYMMDD, hr, min, file_suffix, file_suffix);

                test_stream = fopen(test_file_name, "r+");
 
                if (test_stream!= NULL)
                   {
                   /* File exists. Add it's data to this day file. */
                   fclose(test_stream);
#if DEBUG 
                   printf ("File %-s exist - cp, cosort, cat it!\n", test_file_name); 
#endif 
                   sprintf (system_cmd, "cp %-s input.%-3s\0",
                            test_file_name, file_suffix);
                   system( system_cmd );

                   sprintf (system_cmd1, "cosort sort_4DYR.inp\0"); /* sorts input.0qc into input.srt */
                   system( system_cmd1 );

                   sprintf (system_cmd2, "cat input.srt >>%-sutc_%-8s.%-3s\0",
                            outpathname, current_date_YYYYMMDD, file_suffix);
                   system( system_cmd2 );
#if DEBUG  
                   printf ("Execute: xxx%-sxxx\n", system_cmd);
                   printf ("Execute: xxx%-sxxx\n", system_cmd1);
                   printf ("Execute: xxx%-sxxx\n", system_cmd2);
#endif   
                   }
#if DEBUG
                else /* test_stream != NULL */
                   printf ("File %-s does not exist - look for next file\n",
                          test_file_name);
#endif
                } /* for all times in a day */
              } /* for all hours in a day */
            }
         else /* UTC_to_SOLAR */
            { 
            sprintf (system_cmd, "cat %-ssolar_%-8s_*.%-3s >>%-ssolar_%-8s.%-3s\0",
                     outpathname, prev_date_YYYYMMDD, file_suffix, 
                     outpathname, prev_date_YYYYMMDD, file_suffix);
#if DEBUG
            printf ("Execute: xxx%-sxxx\n", system_cmd);
#endif           
            system( system_cmd );

            for (kk = 0;kk < 255; kk++)
               system_cmd[kk] = '\0';

            sprintf (system_cmd, "cat %-ssolar_%-8s_*.0qc >>%-ssolar_%-8s.0qc\0",
                     outpathname, date, outpathname, date);
#if DEBUG
            printf ("Execute: xxx%-sxxx\n", system_cmd);
#endif
            system( system_cmd );

            } /* UTC_to_SOLAR */

         /*--------------------------------
          * Delete the intermediate files
          * if requested.
          *--------------------------------*/
         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';

         if (UTC_to_SOLAR)
            sprintf (system_cmd, "/bin/rm %-ssolar_%-8s_*.%-s\0",
                     outpathname, prev_date_YYYYMMDD, file_suffix);
         else
            sprintf (system_cmd, "/bin/rm %-sutc_%-8s_*.%-s\0",
                     outpathname, prev_date_YYYYMMDD, file_suffix);

#if DEBUG
         printf ("NO ----Execute: xxx%-sxxx\n", system_cmd);
#endif
 
#if DELETE_INTERMEDIATE_FILES
         system( system_cmd );
#endif
 
         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';

         if (UTC_to_SOLAR)
            sprintf (system_cmd, "/bin/rm %-ssolar_%-8s_*.%-s\0",
                     outpathname, date, file_suffix);
         else
            sprintf (system_cmd, "/bin/rm %-sutc_%-8s_*.%-s\0",
                     outpathname, current_date_YYYYMMDD, file_suffix);

#if DEBUG
         printf ("----Execute: xxx%-sxxx\n", system_cmd);
#endif
 
#if DELETE_INTERMEDIATE_FILES
         system( system_cmd );
#endif
         } /* final cleanup */      

      } /* for current_date = begin to end */

   /*----------------------------------------------
    * Write out the computed time offsets for each
    * station - for verification.
    *----------------------------------------------*/
   if (UTC_to_SOLAR)
      sprintf (output_file_name, "%-ssolar_time_offsets.txt\0", outpathname);
   else
      sprintf (output_file_name, "%-sutc_time_offsets.txt\0", outpathname);


   open_file (output_file_name, "w", FILE_NOT_COMPRESSED, &output_stream);

   fprintf (output_stream, "Stn_# Station                  Stn_Freq    Time_Offset    Stn_Lon    Ref_Lon\n");

   for (i=0;i<numstns; i++)
      fprintf (output_stream, "%5ld %-25s %-10d %-10d %10.2f %10.2f\n",
               i, stn_list[i], stn_data_freq[i], time_offset[i],
               stn_lons[i], reference_lon );
 
   close_file (&output_stream, FILE_NOT_COMPRESSED);

   /*----------------------------------------------
    * Free all pointers created during processing
    * and any left over files.
    *---------------------------------------------*/
   destruct_qcfptr (&qcfptr);

   if (!UTC_to_SOLAR)
      {
      sprintf (system_cmd, "/bin/rm input.qcf\0");
      sprintf (system_cmd1, "/bin/rm input.srt\0");
      system( system_cmd );
      system( system_cmd1 );

#if DEBUG
   printf ("----Execute: xxx%-sxxx\n", system_cmd);
   printf ("----Execute: xxx%-sxxx\n", system_cmd1);
#endif
      }

   printf ("\nTIME Processing completed on %-s %-s\n", __DATE__, __TIME__);
   }  /* main() */
