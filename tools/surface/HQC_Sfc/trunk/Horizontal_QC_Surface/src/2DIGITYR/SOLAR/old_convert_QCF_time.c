/*--------------------------------------------------------
 * convert_QCF_time.c -  Convert UTC to Solar time and
 *              vice versa. If the frequency of the data
 *   is equal to or less frequent to 15 minutes, then 
 *   this s/w attempts to open every output file that
 *   a single source/input file would write into (i.e.,
 *   2 day's worth of files - current day and previous
 *   day). By open all these files, this cuts down
 *   significantly on the number of opens and closes
 *   performed. If the data frequency is higher than
 *   15 minutes, a sufficient number of files can not
 *   be opened. C s/w only allows a single process 
 *   a max of 252 open files at any one time. The FILE
 *   structure uses an unsigned char to store the file
 *   number - limiting its value. So, for higher freq.
 *   data, this s/w currently opens and closes files
 *   as it needs them.
 *
 *   This s/w should also properly handle composite with
 *   mixed data frequency. The user must specify that
 *   the composite has data of more than one frequency
 *   in the initialization input file.
 *
 *   Usage: convert_QCF_time <initialization_input_file>
 *
 *   Note: This s/w should properly handle data that begins
 *   in one year and rollsover into next year. Rollover
 *   into next century should also be handled properly.
 * 
 * Warning:
 *    This s/w has not been tested for all possible
 *    data frequencies. Always check the output carefully!
 * Warning:
 *    This s/w has NOT been tested with a reference longitude
 *    in the middle of the data's longs NOR with a longitude
 *    WEST of all longs in the composite. 
 *
 * Assumptions:
 *  - Assumes specific format for user specified input file.
 *  - Input data is in QCF format and sorted by date/time.
 *  - Expects suffix for input file names to be .0qc. This
 *    is currently hardcoded. Can change and recompile.
 *  - That no two stations in a single network have the
 *    same ID. We use network:ID as a unique identifier.
 *  - That the reference Longitude is to the EAST of all
 *    stations in the data being processed.
 *
 * Input:
 *    User must indicate name of QC input control file when
 *    executing this program. This QC input control file
 *    contains 
 *
 *          Definition                        (var) (type)
 *  -----------------------------------------------------------------
 *  Line 1: Project Name                      (proj_name) (string)
 *  Line 2: Highest Data Frequency in minutes (input_data_freq) (integer)
 *  Line 3: Mixed data frequency flag         (mixed_freq) (0 or 1; integer)
 *  Line 4: Current year of start of data     (current_yr) (YYYY integer)
 *  Line 5: First day to process              (begin_date) (YYMMDD string)
 *  Line 6: Last day to process               (end_date) (YYMMDD string)
 *  Line 7: Input file pathname               (inpathname) (string)
 *  Line 8: Output file pathname              (outpathname) (string)
 *  Line 9: Reference Longitude               (reference_lon) (float)
 *
 * Output:
 *   QCF files whose first nominal times have been converted from
 *   UTC to Solar time. (solar_*.0qc)
 *
 * 16 Jan 96 lec
 *   Created.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "local.h"
#include "process_qcfrec.h"
#include "qcfrec.h"
#include "date.h"

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
#define  DELETE_INTERMEDIATE_FILES    0
#define  MAX_NETWORKS    8 
#define  DEG_TO_MIN 4.0

/*
 * System does not allow a process to open
 * enough files to have all files open at 
 * once for minute data. Assuming that never
 * need more than 2 days worth of files open
 * at any one time. Note that because C used
 * an unsigned char to store the file # on
 * the FILE structure, C users are limited to
 * opening 252 files at one time. This max 
 * number of files will work for all data
 * frequencies of 15 minutes or lower (hrly,
 * 20min, 30min, etc.).  That is, can open
 * 2 days worth of "minute" files all at once
 * saving on I/O processing time.
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
 
   char year_str[3] = "\0\0\0";
   char month_str[3] = "\0\0\0";
   char day_str[3] = "\0\0\0";
   char date_YYMMDD[7] = "\0\0\0\0\0\0\0";
 
 
#if DEBUG
   printf ("   Enter increment: num_min_to_add = %d, current_year = %d\n",
           num_min_to_add, current_year);
   printf ("   qcfptr->yr,mon,day,hr,min:: %02d%02d%02d %02d:%02d\n",
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
     }

   if (current_hr >= 24)
     {
     /*----------------------------------
      * Rollover into next day(s). 
      * Convert date to easier to handle
      * form. Compute number of days to add.
      *---------------------------------*/
     days = (int)(current_hr/24);                /* allow increments of > 1 hr */
     qcfptr->hour_nom = current_hr - days*24;    /* find remainder hours */

     sprintf (date_YYMMDD, "%02d%02d%02d\0", qcfptr->year_nom,
              qcfptr->month_nom, qcfptr->day_nom);
 
     date_to_YYYYJJJ(date_YYMMDD, current_year, &YYYYJJJ);

#if DEBUG
     printf ("   before increment: date_YYMMDD, YYYYJJJ:: %-s  %d\n",
             date_YYMMDD, YYYYJJJ);
#endif
     updated_YYYYJJJ = add_num_to_YYYYJJJ( YYYYJJJ, days);
     YYYYJJJ_to_date ( updated_YYYYJJJ, date_YYMMDD);
 
#if DEBUG
     printf ("   after increment: date_YYMMDD:: %-s\n", date_YYMMDD);
#endif
 
     strncpy (year_str,  &date_YYMMDD[0], 2);
     strncpy (month_str, &date_YYMMDD[2], 2);
     strncpy (day_str,   &date_YYMMDD[4], 2);
 
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

   char year_str[3] = "\0\0\0";
   char month_str[3] = "\0\0\0";
   char day_str[3] = "\0\0\0";
   char date_YYMMDD[7] = "\0\0\0\0\0\0\0";


#if DEBUG
   printf ("   Enter decrement: data_freq=%d, num_min_to_subtract = %d, current_year = %d\n",
           data_freq, num_min_to_subtract, current_year);
   printf ("   qcfptr->yr,mon,day,hr,min:: %02d%02d%02d %02d:%02d\n",
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
    * If required, convert date YYMMDD to 
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
      sprintf (date_YYMMDD, "%02d%02d%02d\0", qcfptr->year_nom,
               qcfptr->month_nom, qcfptr->day_nom);

      date_to_YYYYJJJ(date_YYMMDD, current_year, &YYYYJJJ);
#if DEBUG
      printf ("   before update: date_YYMMDD, YYYYJJJ:: %-s  %d\n",
              date_YYMMDD, YYYYJJJ); 
#endif
      updated_YYYYJJJ = subtract_num_from_YYYYJJJ( YYYYJJJ, 1);

      YYYYJJJ_to_date ( updated_YYYYJJJ, date_YYMMDD);

#if DEBUG
      printf ("   after update: date_YYMMDD:: %-s\n", date_YYMMDD);
#endif

      strncpy (year_str,  &date_YYMMDD[0], 2);
      strncpy (month_str, &date_YYMMDD[2], 2);
      strncpy (day_str,   &date_YYMMDD[4], 2);

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
   real_minutes  = (minutes/60.0 - hour)*60.0;
   real_minutes = (int)((real_minutes/((float)data_freq)) + 0.5);
   qcfptr->minute_nom  = data_freq*real_minutes;
   qcfptr->hour_nom =  hour;

#if DEBUG
   printf ("   Exit decrement: num_min_to_subtract = %d\n",
           num_min_to_subtract);
   printf ("   qcfptr->yr,mon,day,hr,min:: %02d%02d%02d %02d:%02d\n", 
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

   char         inpathname[NAMELEN_MAX];
   char         outpathname[NAMELEN_MAX];

   FILE_NAME_STR  output_file_names[MAX_NUM_FILES_OPEN];
   FILE         *output_streams[MAX_NUM_FILES_OPEN];

   long int     recs_processed = 0;

   char         proj_name[26] = "\0\0\0\0\0\0\0\0\0\0";
   int          input_data_freq = 0;
   int          data_freq     = 0;
   char         date[7]       ="\0\0\0\0\0\0\0"; /* YYMMDD */
   char         begin_date[7] ="\0\0\0\0\0\0\0"; /* Input date to begin (QC) processing. YYMMDD - input*/
   char         end_date[7]   ="\0\0\0\0\0\0\0"; /* Input date to end processing. YYMMDD - input value */
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

   char         current_stn[27] = "\0";
   int          current_yr   = 0;        /* YYYY */
   long int     current_date = 0;        /* YYYYJJJ */

   long int     prev_date_YYYYJJJ = 0;
   char         prev_date_YYMMDD[7] = "\0\0\0\0\0\0\0";

   int          first_day = 1;
   int          old_day_nom = 0;

   char         system_cmd[256] = "\0";
   int          day_offset = 0;
   int          hr = 0;
   int          min = 0;
   int          index = 0;
   int          index2 = 0;
   int          data_too_frequent = 0; /* data too freq to open 2 day's worth of files. */

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
   char         end_YY_str[3]   = "\0\0\0";
   int          end_YY   = 0;
   char         end_MMDD[5]   = "\0\0\0\0\0";
   char         begin_YY_str[3] = "\0\0\0";
   int          begin_YY = 0;
   char         begin_MMDD[5] = "\0\0\0\0\0";

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

   struct {char  network[11]; int  freq;} network_freq[ MAX_NETWORKS] = { {"ASOS5     \0",  5},
                                                                        {  "AWOSQ20   \0", 20},
                                                                        {  "AWOSH20   \0", 20},
                                                                        {  "AWOSA20   \0", 20},
                                                                        {  "AWOS1     \0",  5},
                                                                        {  "OKMESO5   \0",  5},
                                                                        {  "ARMSFC1   \0",  5},
                                                                        {  "AWOSA05   \0",  5} };


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
      printf ("Usage: convert_QCF_time <initialization_input_file>\n");
      exit(1);
      }  

   strcpy (init_input_file_name, argv[1]);

   /*-----------------------------
    * Initialize from input file. 
    *---------------------------*/
   printf ("\n---- Convert QCF times to SOLAR ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("Initialization file name (e.g., TIMEinit.inp): %-s\n", init_input_file_name);
   
   open_file (init_input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);

   printf ("\nPARAMETER INITIALIZATION:: \n\n");

   /*----------------------------
    * Read data from input file.
    *---------------------------*/ 
   fgets(proj_name, 26, input_stream);
   STRIPLINE(input_stream);

   fscanf (input_stream, "%d", &input_data_freq);   STRIPLINE(input_stream);
   data_freq = input_data_freq;
   day_offset = (1440/input_data_freq); /* Base this on highest frequency in data */

   fscanf (input_stream, "%d", &mixed_freq);  STRIPLINE(input_stream);
   fscanf (input_stream, "%d", &current_yr);  STRIPLINE(input_stream); 
   fscanf (input_stream, "%s", begin_date);   STRIPLINE(input_stream);
   begin_date[6] = '\0';
   fscanf (input_stream, "%s", end_date);     STRIPLINE(input_stream);
   end_date[6] = '\0';
   fscanf (input_stream, "%s", inpathname);   STRIPLINE(input_stream);
   fscanf (input_stream, "%s", outpathname);  STRIPLINE(input_stream);
   fscanf (input_stream, "%f", &reference_lon);

   printf ("Project Name (proj_name): %-s\n",           proj_name);
   printf ("Data Frequency (input_data_freq): %d\n",    input_data_freq);
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
   date_to_YYYYJJJ (begin_date, current_yr, &begin_YYYYJJJ);
   date_to_YYYYJJJ (end_date,   current_yr, &end_YYYYJJJ);

   strncpy (end_YY_str, &end_date[0], 2);
   strncpy (end_MMDD,   &end_date[2], 4);
   end_YY_str[2]  = '\0';
   end_MMDD[4]    = '\0';
   end_YY = atoi (end_YY_str);
 
   strncpy (begin_YY_str, &begin_date[0],2);
   strncpy (begin_MMDD,   &begin_date[2],4);
   begin_YY_str[2] = '\0';
   begin_MMDD[4]   = '\0';
   begin_YY = atoi (begin_YY_str);
 
   if (end_YY == begin_YY && atoi(end_MMDD) < atoi(begin_MMDD))
      {  
      printf ("WARNING: User specified end date is less than the begin date!\n");
      exit(1);
      }  
 
   /*---------------------------------------------------------
    * Properly set the century and year even if data rollsover
    * into next year as march through input files.
    *--------------------------------------------------------*/
   if (end_YY > begin_YY || atoi(end_MMDD) < atoi(begin_MMDD))
      {  
      /*------------------------------------
       * Data rolls from one year into next.
       *------------------------------------*/
      date_to_YYYYJJJ (end_date, current_yr+1, &end_YYYYJJJ);
      }  
   else
      {  
      /*----------------------------------
       * Data is contained in single year.
       *----------------------------------*/
      date_to_YYYYJJJ (end_date, current_yr, &end_YYYYJJJ);
      }  
 
#if  DEBUG
   printf ("begin_date, end_date: %-s %-s\n", begin_date, end_date);
   printf ("begin_YYYYJJJ, end_YYYYJJJ: %ld %ld\n",
            begin_YYYYJJJ, end_YYYYJJJ);
#endif


   if (!data_too_frequent)
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
       * file with the previous day's date.
       * If mixed frequency in data, open the max number
       * needed.
       * Open files for "first" previous day.
       *-------------------------------------------------*/
      prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( begin_YYYYJJJ, 1);
      YYYYJJJ_to_date ( prev_date_YYYYJJJ, prev_date_YYMMDD);
 
      for (hr=0; hr<24; hr++)
         {
         for (min=0; min<60; min= min+input_data_freq)
           {
           index = (hr*60 + min)/input_data_freq;
 
           sprintf (output_file_names[index], "%-ssolar_%-6s_%02d%02d.0qc\0",
                    outpathname, prev_date_YYMMDD, hr, min);

           open_file (output_file_names[index], "w", FILE_NOT_COMPRESSED, &output_streams[index]);
#if DEBUG
           printf ("Open output file[%d]: %-sxxx\n", index, output_file_names[index]);
#endif
           } /* for all times  - previous day */
         } /* for all hours  - previous day */

#if DEBUG
      printf ("Files for first -previous- day are open!\n");
#endif 
      } /* !Data_too_frequent */


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
       * is of the form yyddmm.0qc and the output file will be solar_yyddmm.0qc.
       * All times are referenced to the input ref longitude. Choose the ref lon 
       * to be (at least) the most eastern longitude in the dataset.
       *-----------------------------------------------------------------------*/
      YYYYJJJ_to_date (current_date, date);

#if DEBUG
      printf ("\nCurrent julian date is: %ld %-s\n", current_date, date);
#endif

      sprintf (input_file_name, "%-s%-s.0qc\0", inpathname, date);
      open_file (input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);

      printf ("Processing file: %-s\n", input_file_name); /* Always print */

      /*------------------------------
       * Do cleanup for previous day. 
       *------------------------------*/
      if (!first_day)
         {
         prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( current_date, 2);
         YYYYJJJ_to_date (prev_date_YYYYJJJ, prev_date_YYMMDD);

#if DEBUG
         printf ("Not the first day - close old previous day files.\n");
#endif
        /*------------------------------------------------------------
         * If data is not too frequent, Open all files for current day
         * close that last current_day's prev_day's open files.
         * And reposition the file names in the open file index list.
         * For too high frequency data, just cleanup the previous
         * days files.
         *----------------------------------------------------------*/
         if (!data_too_frequent)
            { 
            for (hr=0; hr<24; hr++)
              {  
              for (min=0; min<60; min= min+input_data_freq)
                {
                index = (hr*60 + min)/input_data_freq;

                sprintf (output_file_names[index], "%-ssolar_%-6s_%02d%02d.0qc",
                         outpathname, prev_date_YYMMDD, hr, min);
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

             printf ("Files for NEW first -previous- day are open!\n");
#endif
           } /* --- !data_too_frequent--- */


         /*-----------------------------------------------
          * Cat the closed files together (in time order)
          * to form day files and remove the minute files.
          * Note that the HQC processing and compute
          * sigma processing only care that like times are
          * together. Sorting within each time is not
          * important.
          *-----------------------------------------------*/
         sprintf (system_cmd, "cat %-ssolar_%-6s_*.0qc >>%-ssolar_%-6s.0qc\0",
                  outpathname, prev_date_YYMMDD, outpathname, prev_date_YYMMDD);
#if DEBUG
         printf ("Execute: xxx%-sxxx\n", system_cmd);
#endif
         system( system_cmd );

         for (kk = 0;kk < 255; kk++) 
            system_cmd[kk] = '\0';
         
         sprintf (system_cmd, "/bin/rm %-ssolar_%-6s_*.0qc\0",
                  outpathname, prev_date_YYMMDD);   
#if DEBUG 
         printf ("YES -----Execute: xxx%-sxxx\n", system_cmd); 
#endif
   
#if DELETE_INTERMEDIATE_FILES
         system( system_cmd );
#endif

         } /* !first_day */

      if (!data_too_frequent)
         {
         /*----------------------------------
          * Open files for new current day.
          *---------------------------------*/
         for (hr=0; hr<24; hr++)
           {
           for (min=0; min<60; min= min+input_data_freq)
             {
             index = ((24+hr)*60 + min)/input_data_freq; /*shifted up one day's worth*/
 
             sprintf (output_file_names[index], "%-ssolar_%-6s_%02d%02d.0qc",
                      outpathname, date, hr, min);
             open_file (output_file_names[index], "w", FILE_NOT_COMPRESSED, &output_streams[index]);
#if DEBUG
             printf ("Open output file[%d]: %-sxxx\n", index, output_file_names[index]);
#endif
             } /* for all times  - current day */
           } /* for all hours  - current day */
#if DEBUG
        printf ("Files for current day are open!\n");
#endif
        } /* --- !data_too_frequent--- */


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

         recs_processed++;

#if DEBUG
         printf ("\n---CURRENT TIME: qcfptr->hour_nom, qcfptr->minute_nom: %d %d\n",
                 qcfptr->hour_nom, qcfptr->minute_nom);
#endif
         /*------------------------------------------------------
          * Determine the "correct" solar time for this record,
          * then write the record to the proper output file:
          * solar_yymmdd_hhmm.0qc. Compute the difference between
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
          * Update output qcfptr nominal times to solar times.
          *--------------------------------------------------------*/
         old_day_nom = qcfptr->day_nom;     /* save for comparison */

         if (lon_diff_min >= 0.0)
            {
            decrement_qcfptr_nomtime (qcfptr, current_yr, stn_data_freq[stn_no],
                                      time_offset[stn_no]);

#if DEBUG
            printf ("After decrement(): qcfptr->yr,mon,day,hr,min:: %02d%02d%02d %02d:%02d\n",
                   qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom, 
                   qcfptr->minute_nom);
#endif
            }
         else /* Found station to the EAST of ref lon */
            {
            printf ("WARNING: Found station to the EAST of reference Lon!\n");
            printf ("   Stn: %-s at %7.2f is EAST of ref_lon= %7.2f\n",
                    stn_list[stn_no], stn_lons[stn_no], reference_lon);
            printf ("   INCREMENT - don't decrement!!!!\n");

            increment_qcfptr_nomtime (qcfptr, current_yr, stn_data_freq[stn_no], 
                                      time_offset[stn_no]);
#if DEBUG
            printf ("After increment(): qcfptr->yr,mon,day,hr,min:: %02d%02d%02d %02d:%02d\n",
                   qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom, qcfptr->hour_nom,
                   qcfptr->minute_nom);
#endif   
            }

         if (!data_too_frequent)
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
#if DEBUG
               printf ("Write to prev day!\n");
#endif
               /*-------------------------------
                * Must write to prev day.
                * (5 min data = indices:0-287)
                * Hourly data = indices: 0-22.
                *------------------------------*/
               index = (qcfptr->hour_nom*60 + min)/data_freq;
               }
            else
               {
#if DEBUG
               printf ("Write to SAME day! - second half of 2 days.\n");
#endif
               /*-------------------------------
                * Must still be in same day. 
                * (5 min data = indices:288-575)
                * Hourly data = indices: 23-47.
                *------------------------------*/
               index = ((24+qcfptr->hour_nom)*60 + min)/data_freq;
               }
#if DEBUG
            printf ("Write record to file[%d]: %-s\n", index-1, output_file_names[index-1]);
#endif
            write_qcfrec(&output_streams[index-1], qcfptr);

            } /* !data_too_frequent */   
         else
            {
            /*------------------------------------------------
             * Data too frequent to have 2 day's worth of
             * files open at once so....
             * Determine the name of the output file to write
             * this rec into.  Write rec and then close file.
             *-----------------------------------------------*/
            sprintf (output_file_name, "%-ssolar_%02d%02d%02d_%02d%02d.0qc",
                     outpathname, qcfptr->year_nom, qcfptr->month_nom,
                     qcfptr->day_nom, qcfptr->hour_nom, qcfptr->minute_nom);

            if (qcfptr->minute_nom >= 60)
               {
               printf ("ERROR: incorrect minute of %2d.\n",qcfptr->minute_nom);
               exit(1); 
               }

            if (qcfptr->hour_nom >= 24)
               { 
               printf ("ERROR: incorrect hour of %2d.\n",qcfptr->hour_nom);
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
            } /* data_too_frequent */

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
         prev_date_YYYYJJJ = subtract_num_from_YYYYJJJ( current_date, 1);
         YYYYJJJ_to_date ( prev_date_YYYYJJJ, prev_date_YYMMDD);

         if (!data_too_frequent)
            {
            for (hr=0; hr<24; hr++)
              {
              for (min=0; min<60; min= min+data_freq)
                {
                index = (hr*60 + min)/data_freq;
                index2 = ((24+hr)*60 + min)/data_freq;

                close_file (&output_streams[index], FILE_NOT_COMPRESSED);
                close_file (&output_streams[index2], FILE_NOT_COMPRESSED);
#if DEBUG
                printf ("Close output file[%d]: %-sxxx\n", index, output_file_names[index]);
                printf ("Close output file[%d]: %-sxxx\n", index2, output_file_names[index2]);
#endif
                } /* for all times  - previous day */
              } /* for all hours  - previous day */
           } /* !data_too_frequent */

         /*----------------------------------
          * Cat previous day and current day.
          *---------------------------------*/
         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';
 
         sprintf (system_cmd, "cat %-ssolar_%-6s_*.0qc >>%-ssolar_%-6s.0qc\0",
                  outpathname, prev_date_YYMMDD, outpathname, prev_date_YYMMDD);
#if DEBUG
         printf ("Execute: xxx%-sxxx\n", system_cmd);
#endif           
         system( system_cmd );


         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';

         sprintf (system_cmd, "cat %-ssolar_%-6s_*.0qc >>%-ssolar_%-6s.0qc\0",
                  outpathname, date, outpathname, date);
#if DEBUG
         printf ("Execute: xxx%-sxxx\n", system_cmd);
#endif
         system( system_cmd );


         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';

         sprintf (system_cmd, "/bin/rm %-ssolar_%-6s_*.0qc\0",
                  outpathname, prev_date_YYMMDD);
#if DEBUG
         printf ("YES ----Execute: xxx%-sxxx\n", system_cmd);
#endif

#if DELETE_INTERMEDIATE_FILES 
         system( system_cmd );
#endif

         for (kk = 0;kk < 255; kk++)
            system_cmd[kk] = '\0';

         sprintf (system_cmd, "/bin/rm %-ssolar_%-6s_*.0qc\0",
                  outpathname, date);
#if DEBUG
         printf ("YES ----Execute: xxx%-sxxx\n", system_cmd);
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
   strncpy (output_file_name, "./solar_time_offsets.txt\0", 25);

   open_file (output_file_name, "w", FILE_NOT_COMPRESSED, &output_stream);

   fprintf (output_stream, "Stn_# Station                    Stn_Freq  Time_Offset    Stn_Lon    Ref_Lon\n");

   for (i=0;i<numstns; i++)
      fprintf (output_stream, "%5ld %-25s %10d %-10d %10.2f %10.2f\n", 
               i, stn_list[i], stn_data_freq[i], time_offset[i],
               stn_lons[i], reference_lon );

   close_file (&output_stream, FILE_NOT_COMPRESSED);

   /*----------------------------------------------
    * Free all pointers created during processing.
    *---------------------------------------------*/
   destruct_qcfptr (&qcfptr);

   printf ("\nTIME Processing completed on %-s %-s\n", __DATE__, __TIME__);
   }  /* main() */
