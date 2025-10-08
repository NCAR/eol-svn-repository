/*--------------------------------------------------------
 * compute_sigmasq_4DYR - Computes sigma squared (i.e., variance)
 *  for stns/data in QCF format. Variances are written to
 *  output files which are later read and used during the
 *  the final Horizontal Quality Control processing. See
 *  WARNINGS section below. This version handles 4 Digit Years
 *  and was created from V1.0 of compute_sigmasq.c.
 *
 *  Usage: compute_sigmasq_4DYR <initialization_input_file>
 *
 *  This program (using input info about project/data dates)
 *  opens, extracts data from, and closes the input QCF files and
 *  then computes variances for all parameters at all times and at 
 *  all stations in the QCF day files. This is done beginning
 *  at the user specified begin date through the end date. A
 *  SIGMA_PERIOD number of days (i.e., # of parameter values) is 
 *  used to compute each variance. 
 *
 *  Compute_sigmasq_4DYR() computes sigma values for the following
 *  parameters:
 *                Calculated Sea Level Pressure,
 *                Sea Level Pressure,
 *                Temperature,
 *                Dew Point Temperature,
 *                Wind speed,
 *                Wind Direction.
 *
 *  The variances (or sigma squared values) are later used
 *  by the Horizontal Quality Control (HQC) program named
 *  main_qc.c. See that software for a detailed description
 *  of the HQC processing.
 *
 *  Compute_sigmasq_4DYR() should always be executed with the
 *  same initialization input file that will be used for
 *  the HQC processing. Begin and end dates can be modified to
 *  to control the amount of data processed, but all other
 *  input parameter should remain the same.
 *       
 *  WARNING: The Horizontal Quality Control (HQC) program
 *      (main_qc) may update/overwrite the sigma_sq files
 *      created by compute_sigmasq_4DYR. See the header description
 *      in main_qc for more details. It is wise to make a 
 *      copy of the original sigma_sq files as a safe guard.
 *
 *  WARNING: Ensure that the constant MAXNUMSTNS is equal to
 *      or greater than the number of unique stations in the
 *      data being processed. 
 *
 *  Assumptions:
 *   -  Input data is in QCF format and sorted by date/time.
 *      These should be the exact same files that main_qc()
 *      file uses to perform Horizontal Quality Control.
 *   -  Assumes specific format for user specified input file.
 *   -  Hours run from 0 to 23 not 1 to 24.
 *   -  Expects day files whose names are of the form:
 *      yyyymmdd.0qc (suffix currently hardcoded). Actually only
 *      assumes that yyyymmdd immediately preceeds '.0qc' suffix.
 *   -  gQC.h contains definitions for the limits of some
 *      array sizes. These values should be set appropriately
 *      for each project. These arrays were created to 
 *      shorten the coding time, only. Possibly a better
 *      programming method could be used here to allow
 *      more flexiblity (possible pointers and dynamic
 *      space allocation).
 *   -  That no two stations in a single network have the
 *      same ID. We use network:ID as a unique identifier.
 *
 *  Input:
 *    The *.0qc files that will be quality controlled using
 *    the HQC (main_qc.c) program.
 *
 *  Input:
 *    User must indicate name of QC input control file when
 *    executing this program. This QC input control file
 *    contains several input values, limits, dates,
 *    etc. required by the horizontal QC program (main_qc). 
 *    This program picks and chooses only those values
 *    needed from this input file. The format of this input
 *    file is as follows:
 * 
 *          Definition                        (var) (type)
 *  ----------------------------------------------------------------- 
 *  Line 1: Project Name                      (proj_name) (25 chars)
 *  Line 2: Distance fn method                (pmethod) (float)
 *  Line 3: Area of Influence in km           (1/min_weight) (float)
 *  Line 4: Data Frequency                    (data_freq) (integer)
 *  Line 5: Current year of start of data     (current_yr) (YYYY string)
 *  Line 6: First day of Project              (proj_begin_date) (YYYYMMDD string)
 *  Line 7: Last  day of Project              (proj_end_date) (YYYYMMDD string)
 *  Line 8: First day to QC/compute variance  (begin_date) (YYYYMMDD string)
 *  Line 9: Last day to QC/compute variance   (end_date) (YYYYMMDD string)
 *  Line 10: Input file pathname              (pathname) (string)
 *  Line 11: Sigma file pathname              (sigpathname) (string)
 *  Line 12: Precip Unlikely (B) Gross Limit  (bmax) (float)
 *  Line 13: Precip Dubious Gross Limit       (dmin) (float)
 *  Line 14: Alpha Unlikely & Dubious for stn pres      (two floats)
 *  Line 15: Alpha Unlikely & Dubious for sea lvl pres  (two floats)
 *  Line 16: Alpha Unlikely & Dubious for calc slp      (two floats)
 *  Line 17: Alpha Unlikely & Dubious for temp          (two floats)
 *  Line 18: Alpha Unlikely & Dubious for dew point     (two floats)
 *  Line 19: Alpha Unlikely & Dubious for wind speed    (two floats)
 *  Line 20: Alpha Unlikely & Dubious for wind dir      (two floats)
 *
 *  Output:
 *    Sigma Squared files to be used in HQC processing. (*.sig) 
 *
 *    This program can create numerous sigmasq (variance) files.  The
 *    number of files created depends on several things. BEWARE that
 *    depending on the number of days being processed, number of stns
 *    in the data, etc, etc, the user MAY need to limit the
 *    number of days for which the sigma files are created
 *    at during one processing session. The user can create only
 *    those sigma files required to do Horizontal QC for a 
 *    few days at a time. Then do the HQC. Save off the
 *    output files and repeat the process if space is limited.
 *    It is wise to save all the variance files (somewhere) if
 *    possible. This allows the user to rerun HQC without
 *    recreating the sigma_sq or variance files. Output file names
 *    will have the form:
 *                   yyyyjjjhhmm.sig 
 * 
 *    where yyyy is year, jjj is julian date, hh is hour, mm is
 *    minute for which the contained sigmasq (variance) values
 *    are valid. These files will be located in the directory specified
 *    as the output directory in the QC input control file. The
 *    horizontal QC s/w (main_qc.c) assumes that the variance
 *    files are named using this convention. To save space, this
 *    program compresses (via gnuzip) the output variance files.
 *    Note that the HQC program also assumes that these files are
 *    compressed.
 *
 *    The form of each output variance file is:
 *
 *          Definition                        (var) (type)
 *  -----------------------------------------------------------------
 *  Line 1: Number of stations at this time   (numstns) (int)
 *  Line 2->xx: InternalID  Network:Stn ID    (i, stn_list[i][27]) (int, string)
 *  Line xx+1: Comment line                   ("stn_no parm_no  sigma value")
 *  Line xx+2: InternalID  Parm# Variance     (i, j, sigma_sq[i][j]) (int, int, float)
 *
 * (Line xx+2 is repeated for each variance for parameter (0-6 (j)) for each stn.)
 *
 *  Example of variance file:
 *   1
 *     0 ASOSH     :E02
 *   stn_no parm_no  sigma value
 *       0     0 33.714584
 *       0     1 33.271523
 *       0     2 67.128426
 *       0     3 29.454615
 *       0     4 38.043491
 *       0     5 3.950475
 *       0     6 13018.718750
 *  (end example)
 *
 * 09 Dec 94 lec
 *   Created from original Horizontal QC test s/w.
 * 06/08 Jun 95 lec
 *   Some comment mods/cleanup only.
 * 11/13 Sep 95 lec
 *   Updated s/w to accept name of QC input file as
 *   line input. Added more info in header. Updated s/w
 *   to handle data that rollsover into next year.
 * 5 Oct 95 lec
 *   Updated s/w to use network:stnID combo as the internal
 *   stn ID. Station ID alone is not necessarily unique.
 * 31 Oct 95 lec
 *   Updated s/w to properly pick out year from incremented
 *   time - affects only data that rollsover into next year.
 *   Also s/w can now handle multiple hour/odd minute
 *   increments. This is not typically seen in surface data.
 * 07 Nov 95 lec
 *   Upgraded s/w to use totally different method for
 *   ingesting data from daily QCF files. Use to process
 *   day after day, hr 0 through 23 for each day. To reduce
 *   the number of file opens/closes, we now process through
 *   the hours 00 to 23 saving off previously read input
 *   data for SIGMA PERIOD days. For each new day, only 
 *   need to read data from next file and properly write-
 *   over oldest value saved off for each parameter.
 * 28 Nov 95 lec
 *   Modified output so that if station drops out, stn
 *   numbering in output file will still be sequential.
 * 12 Feb 98 lec
 *   Updated comments.
 * 04 Mar 98 lec
 *   Added warning if variance is computed as zero to
 *   4 digits. Warning tells StnID, parm, var value,
 *   and var output file name. This should help ID
 *   conversion problems or stations with problems.
 * 19 August 2002 lec
 *   Update s/w to handle 4 digit years.
 * 14 Sep 2004 lec
 *   Update s/w to not use values previously flagged Bad
 *   'B' in variance calculations for any parameter.
 * 02 Jan 2007 lec
 *   Added explict include for string.h.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "local.h"
#include "process_qcfrec_4DYR.h"
#include "qcfrec.h"
#include "gQC_4DYR.h"
#include "date_4DYR.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * Set DEBUG2 to 1 for even more debug type output to
 * screen.
 *-------------------------------------------------------*/
#define   DEBUG  0
#define   DEBUG2  0
#define   DEBUGBADVAL 0

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() -  Controls variance (sigma squared) computations.
 *
 * 09 Dec 94 lec
 *   Created.
 *--------------------------------------------------------*/
int main( argc, argv)
int argc;
char *argv[];
   {
   /* local variables */
   char		init_input_file_name[NAMELEN_MAX]="\0";
   char		input_file_name[NAMELEN_MAX]="\0";
   char         sigma_output_file_name[NAMELEN_MAX] = "\0";

   FILE		*init_input_stream;
   FILE		*input_stream;
   FILE         *sigma_output_stream;

   long int     fileRecNo[MAX_NUM_PROJ_DAYS];
   long int     fileno;

   char		pathname[NAMELEN_MAX];
   char		sigpathname[NAMELEN_MAX];

   char         compress_cmd [NAMELEN_MAX+8];

   QCFREC       *qcfptr; 

   int           i,ii,j,jj,k,kk = 0;
   int           data_freq   = 0;
   long          stn_no      = -1;
   long          numstns     = 0;
   long          output_numstns = 0;
   long          output_ct      = 0;
   int           first_file;
   
   char          current_stn[27] = "\0";
   char          NULL27[27] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
   char          proj_name[26] = "\0";
   float         ave, svar, adev, sdev = 0.0;

   int           writeover_position = 0;
 
   int           current_yr   = 0; /* YYYY */
   int           hour         = 0;
   int           current_hr   = 0;
   int           current_min  = 0;
   long int      current_date = 0; /* YYYYJJJ */
   char          current_date_str[8]= "\0\0\0\0\0\0\0\0"; /* YYYYJJJ */

   char          date[9]      = "\0\0\0\0\0\0\0\0\0"; /*YYYYMMDD */
 
   char          proj_begin_date[9] ="\0\0\0\0\0\0\0\0\0"; /* Date that project begins. QC program uses next
                                                          date (i.e., begin_date) to individually QC days.
                                                          YYYYMMDD - input value */ 
   char          proj_end_date[9] ="\0\0\0\0\0\0\0\0\0";   /* Date that project ends. YYYYMMDD - input value*/
   char          begin_date[9] ="\0\0\0\0\0\0\0\0\0";      /* Input date to begin processing. YYYYMMDD-input value*/
   char          end_date[9]   ="\0\0\0\0\0\0\0\0\0";      /* Input date to end processing. YYYYMMDD-input value */
 
   /* 
    * Following Variables contain dates of the form:
    * YYYYJJJ where YYYY is the century and yr (e.g. 
    * 2001) and JJJ is the julian day (e.g., 236 for 
    * 24 August 2001). Dates in this form are easily 
    * incremented, compared, subracted from, etc. 
    */ 
   long int proj_begin_YYYYJJJ = 0;
   long int begin_YYYYJJJ      = 0;
   long int end_YYYYJJJ        = 0;
   long int stop_YYYYJJJ       = 0; /* Must stop processing 1 day short of end */
   long int end_check_YYYYJJJ  = 0; /* Used to ensure that there are at least 
                                       SIGMA_PERIOD days in proj. */

   long int end_processing_date;
   long int last_date_processed;

   long int begin_processing_date; /* Beginning YYYYJJJ date of current days for which
                                      sigmasq is being computed. This var increments
                                      along one day at a time through the complete
                                      project period or the specified time. It increments
                                      one day, then sigmasq's are computed for all stns
                                      and all parameters at all times over the current
                                      sigma period. These new sigmasq's are valid for
                                      date = process_jdate+SIGMA_PERIOD only. The only
                                      exception to this rule is that the first set of
                                      sigmasq's computed are used to QC data from day
                                      1 of the project to day = begin_date+SIGMA_PERIOD+1.
                                      In other words, if SIGMA_period is 30 days, the
                                      variances (sigmasq's) that are computed using data
                                      from the first 30 days are used to QC the first 31
                                      days of the project. You must use the sigma values
                                      valid for day 31 to QC the project's first 31 days,
                                      unless you have data preceding the beginning of the
                                      project. */
 
   /*
    * Vars used to determine if data rollsover into
    * next year.
    */
   int          data_rollsover  = FALSE;  /* Boolean flag indicating if data rollsover
                                             from one year into the next. */
   char         end_YYYY_str[5]   = "\0\0\0\0\0";
   int          end_YYYY          = 0;
   char         end_MMDD[5]     = "\0\0\0\0\0";
   char         begin_YYYY_str[5] = "\0\0\0\0\0";
   int          begin_YYYY        = 0;
   char         begin_MMDD[5]   = "\0\0\0\0\0";
 
   /*
    * stn_list        - list of possibly active stns.
    * date_last_seen  - Matches stn_list[] to show last date stns 
    *                   were seen in data at current_hr,current_min.
    * stn_data    - data from which sigmas are computed.
    * stn_numdata - number of data points in stn_data by parameter.
    * sigma_sq    - variances for each stn and each parameter at a specified time.
    */
   STRING27  stn_list[MAXNUMSTNS];
   long int  date_last_seen[MAXNUMSTNS]; /* YYYYJJJ */
   long int  start_sigma_period;         /*YYYYJJJ */

   float stn_data[MAXNUMSTNS][NUMQCPARMS][SIGMA_PERIOD];

   int   stn_numdata[MAXNUMSTNS][NUMQCPARMS] = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0}, 
                                                {0,0,0,0,0,0,0}};

   float sigma_sq[MAXNUMSTNS][NUMQCPARMS]    = {{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}, {0,0,0,0,0,0,0},{0,0,0,0,0,0,0},
                                                {0,0,0,0,0,0,0}};

   /*
    * Expect input file name on the command line.
    */
   if (argc != 2)
      {  
      printf ("Usage: compute_sigmasq_4DYR <initialization_input_file>\n");
      exit(1);
      }  

   strcpy (init_input_file_name, argv[1]);

   /*---------------------------------------------------------------
    * Initialize from input file. This should be the same
    * file that is used during Horizontal Quality Control processing.
    *
    * Read only what is required for the compute sigma sq processing.
    *---------------------------------------------------------------*/
   printf ("\n---- Compute Sigma Square ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("QC Initialization file name (e.g., QCinit.inp) was: %-s\n", init_input_file_name);
   
   open_file (init_input_file_name, "r", FILE_NOT_COMPRESSED, &init_input_stream);

   printf ("\nPARAMETER INITIALIZATION:: \n\n");

   /*-----------------------------------------
    * Read data from input file (QCinit.inp).
    *----------------------------------------*/
   fgets(proj_name, 25, init_input_stream);
   proj_name[25] = '\0'; 
   STRIPLINE(init_input_stream);
   STRIPLINE(init_input_stream);                /*Skip pmethod*/
   STRIPLINE(init_input_stream);                /*Skip min_weight*/
   fscanf (init_input_stream, "%d", &data_freq);      STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%d", &current_yr);     STRIPLINE(init_input_stream); 
   fscanf (init_input_stream, "%s", proj_begin_date); STRIPLINE(init_input_stream);
   proj_begin_date[8] = '\0';
   fscanf (init_input_stream, "%s", proj_end_date);   STRIPLINE(init_input_stream);
   proj_end_date[8] = '\0';
   fscanf (init_input_stream, "%s", begin_date);      STRIPLINE(init_input_stream);
   begin_date[8] = '\0';
   fscanf (init_input_stream, "%s", end_date);        STRIPLINE(init_input_stream);
   end_date[8] = '\0';
   fscanf (init_input_stream, "%s", pathname);        STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%s", sigpathname);
   
   printf ("Project Name (proj_name): %-s\n", proj_name);
   printf ("Data Frequency (data_freq): %d\n", data_freq);
   printf ("Current year of data: %d\n", current_yr);
   printf ("First day of Project (proj_begin_date): %-s\n", proj_begin_date);
   printf ("Last day of Project  (proj_end_date): %-s\n", proj_end_date);
   printf ("First day to QC (begin_date) : %-s\n", begin_date);
   printf ("Last day to QC (end_date)    : %-s\n", end_date);
   printf ("Input file pathname: %-s\n", pathname);
   printf ("Sigma file pathname: %-s\n\n", sigpathname);

   /*------------------------------------
    * Close QC input initialization file.
    *------------------------------------*/
   close_file (&init_input_stream, FILE_NOT_COMPRESSED);

   /*-----------------------------------------------
    * Construct a qcfrec pointer then read the data.
    *----------------------------------------------*/
   construct_qcfptr (&qcfptr);

   /*------------------------------------------------------------------
    * Convert input dates to Julian and to YYYYJJJ form for easier
    * manipulation of dates.
    *------------------------------------------------------------------
    * If end_date is less than begin_date then the data must rollover
    * through the end of the year. Also note that the year will
    * change and the century might change! Remember to increment the
    * current_yr!
    *-----------------------------------------------------------------*/
   data_rollsover  = FALSE;

   date_to_YYYYJJJ (proj_begin_date, current_yr, &proj_begin_YYYYJJJ);
   date_to_YYYYJJJ (begin_date, current_yr, &begin_YYYYJJJ);

   strncpy (end_YYYY_str, &end_date[0], 4);
   strncpy (end_MMDD,   &end_date[4], 4);
   end_YYYY_str[4]  = '\0';
   end_MMDD[4]    = '\0';
   end_YYYY = atoi (end_YYYY_str);
    
   strncpy (begin_YYYY_str, &begin_date[0],4);
   strncpy (begin_MMDD,   &begin_date[4],4);
   begin_YYYY_str[4] = '\0';
   begin_MMDD[4]   = '\0';
   begin_YYYY = atoi (begin_YYYY_str);

   if (end_YYYY == begin_YYYY && atoi(end_MMDD) < atoi(begin_MMDD))
      {
      printf ("WARNING: User specified end date is less than the begin date!\n");
      exit(1);
      }
 
   /*---------------------------------------------------------
    * Properly set the century and year even if data rollsover
    * into next year. Note that in compute_sigmasq_4DYR we use only
    * end date not the project end date for processing.
    *--------------------------------------------------------*/
   if (end_YYYY > begin_YYYY || atoi(end_MMDD) < atoi(begin_MMDD))
      {  
      /*------------------------------------
       * Data rolls from one year into next.
       *------------------------------------*/
      data_rollsover = TRUE;
      }  
 
   date_to_YYYYJJJ (end_date, end_YYYY, &end_YYYYJJJ);
   stop_YYYYJJJ = subtract_num_from_YYYYJJJ( end_YYYYJJJ, 1);

   /*-----------------------------
    * Do one more check on dates.
    *-----------------------------*/
   end_check_YYYYJJJ = subtract_num_from_YYYYJJJ( end_YYYYJJJ, SIGMA_PERIOD);

#if DEBUG
   printf ("proj_begin_YYYYJJJ, end_YYYYJJJ, end_check_YYYYJJJ: %ld %ld %ld\n",
            proj_begin_YYYYJJJ, end_YYYYJJJ, end_check_YYYYJJJ);
#endif
 
   if (end_check_YYYYJJJ < proj_begin_YYYYJJJ)
      {  
      printf ("WARNING: User specified end date is within SIGMA_PERIOD days\n");
      printf ("         of proj_begin date. User must have at least SIGMA_PERIOD\n");
      printf ("         days worth of data! Reset end date to equal proj_begin date\n");
      printf ("         plus SIGMA_PERIOD OR shorten SIGMA_PERIOD.\n");
      exit(1);
      }  


   /*-------------------------------------
    * Reinitialize the station data and 
    * variables. stn_numdata and stn_data
    * are initialized below. fileRecNo
    * retains location in file for all
    * project days.
    *------------------------------------*/
   for (kk=0;kk<MAX_NUM_PROJ_DAYS;kk++)
      fileRecNo[kk] = 0;

   current_hr  = 0;
   current_min = -1*data_freq;

   stn_no      = -1;
   writeover_position = -1;

   /*--------------------------------------------
    * For a particular time, generate all the
    * variance files for all days in the project
    * for that time. Then move to the next time.
    * Assume that hours run from 0 to 23.
    *--------------------------------------------*/
   while (current_hr <= 23)
      {
      current_min = current_min + data_freq;
 
      if (current_min >= 60)
         {
         hour = (int)(current_min/60);         /* allow increment of > 1 hr */
         current_hr = current_hr + hour;
         current_min = current_min - hour*60;  /* find remainder minutes */
         }
 
#if DEBUG
      printf ("current_hr, current_min: %d %d\n", current_hr, current_min);
#endif
 
      if (current_hr >= 24)
         break;

      last_date_processed = begin_YYYYJJJ;

      /*------------------------------------------
       * Process current time's data for all days.
       *------------------------------------------*/
      while (last_date_processed < stop_YYYYJJJ)
         {
#if DEBUG
         printf ("(loop TOP) last_date_processed: %ld\n", last_date_processed);
#endif
         /*---------------------------------------------------
          * First must fill the first SIGMA_PERIOD number of
          * days worth of data into the data array for this
          * time. Once this is done, only have to pick up next
          * day's values for this time to recompute that day's
          * sigmasq values. Saves lots of I/O.
          *---------------------------------------------------*/
         if ( last_date_processed == begin_YYYYJJJ)
            {
            /*--------------------------------------
             * Set dates to fill stn_data with first
             * SIGMA_PERIOD days worth of data.
             *-------------------------------------*/
            begin_processing_date = begin_YYYYJJJ;
            end_processing_date = add_num_to_YYYYJJJ (begin_YYYYJJJ, SIGMA_PERIOD);

            fileno     = -1;
            stn_no     = -1;
            first_file = 1;
            numstns    = 0;
            writeover_position = 0;

#if DEBUG
            printf ("Fill first SIGMA_PERIOD # days in stn_data. writeover_position: %d\n",
                    writeover_position);
#endif
            /*---------------------------------------
             * Reinitialize the station data 
             * since we are re-reading first SIGMA_
             * PERIOD days. It is possible that stns
             * or data are missing in middle of data.
             * In embedded for loop: only retain
             * SIGMA_PERIOD amt of data at any time.
             *--------------------------------------*/
            for (j=0;j<MAXNUMSTNS;j++)
              {  
              date_last_seen[j] = -999;
              strncpy (stn_list[j], NULL27, 27);

              for (k=0;k<NUMQCPARMS;k++)
                 {
                 stn_numdata[j][k] = 0;
                 for (kk=0;kk<SIGMA_PERIOD; kk++)
                    stn_data[j][k][kk] = -999.99;
                 }
               }   
            } /* need to fill data array */
         else
            {
            /*---------------------------------------
             * Else just pick up the next days data
             * and write over appropriate oldest days
             * data. Then ready to compute this next
             * days sigmasq values. Note that call to
             * increment_YYYYJJJ() actually modifies
             * last_date_processed. Variable 
             * writeover_position is incremented in
             * loop below.
             *--------------------------------------*/
            begin_processing_date = increment_YYYYJJJ (&last_date_processed);
            end_processing_date = add_num_to_YYYYJJJ (begin_processing_date, 1);

            if (writeover_position >= SIGMA_PERIOD)
               writeover_position = 0;
#if DEBUG 
            printf ("Fill ONLY new position in stn_data array. writeover_postion: %d\n",
                   writeover_position); 
            printf ("Reset stn_data[writeover_position] = -999.99. Decrement stn_numdata\n");
#endif 
            /*--------------------------------------
             * Blank out this writeover_position, so
             * if no data available for that day, the
             * position is correctly set to missing.
             *---------------------------------------*/
            for (j=0;j<MAXNUMSTNS;j++)
               {  
               for (k=0;k<NUMQCPARMS;k++)
                  {
                  if (stn_data[j][k][writeover_position] > -990.00)
                     {
                     stn_numdata[j][k]--;
                     stn_data[j][k][writeover_position] = -999.99;
                     }
                  }
               }   

            } /* if (last_date_processed == begin_YYYYJJJ) */

#if DEBUG
      printf("bfr crntdate LOOP-begin_processing_date, end_processing_date, writeover_position: %ld %ld %d\n",
             begin_processing_date, end_processing_date, writeover_position);
#endif

         /*------------------------------------------------------------
          * Store off the data values to form sigma for a particular
          * station, parameter, and time. To form sigma for stn ILE at
          * time 00:20, only use data at stn ILE at times 00:20 on
          * these SIGMA_PERIOD (e.g., 30) days. Ingest only the 
          * data for the requested days. For the first SIGMA_PERIOD 
          * number of days, must get initial set of data by reading
          * data from all of these days. Once this initial set is read
          * in, it is saved (for each new current time). Thereafter,
          * only need to read data from one/next file and place this
          * new data into appropriate location in saved data array.
          * Then we can compute the variances with a minimum of I/O.
          * (I.E., don't have to reopen, read, and close data from
          * past files.)
          *------------------------------------------------------------*/
         for (current_date=begin_processing_date; current_date<end_processing_date; 
                  increment_YYYYJJJ(&current_date))
            {
            fileno++;

            /*----------------------------------------------
             * Form name of input file and open. Assume name
             * is of the form (pathname)(*)yyddmm.0qc. Must
             * pick year out of incremented date, since may
             * have rolled over into next yr while processing
             * files. Convert YYYYJJJ dates yymmdd form.
             *----------------------------------------------*/
            YYYYJJJ_to_date (current_date, date);

            sprintf (input_file_name, "%-s%-s.0qc\0", pathname, date);
            open_file (input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);

#if DEBUG
            printf ("\nCurrent_date is: %ld %-s\n", current_date, date);
            printf ("Open file: %-sxxx\n", input_file_name);
            printf ("current_hr, current_min, writeover_position, fileno: %d %d %d %d\n", 
                     current_hr, current_min, writeover_position, fileno);
#endif
            /*---------------------------------------------------------
             * Go to the last rec read in this file. Use Random Access.
             * Then Begin reading and read until hit next time.
             *---------------------------------------------------------*/
            if (fseek(input_stream, fileRecNo[fileno],0) !=0)
               { 
               printf ("Error: Can't move file pointer to fileRecNo = %d\n", 
                       fileRecNo[fileno]);
               exit(1);
               } 
 
            while (!feof(input_stream))
               { 
               fileRecNo[fileno] = ftell(input_stream);

               reset_qcfrec( qcfptr );        /* Safe guard only */
               read_qcfrec (&input_stream, qcfptr);
#if DEBUG
               printf ("(After read_qcfrec) FileRecNo[%ld]: %ld\n", fileno,fileRecNo[fileno]);
               printf ("\nRead qcfrec - loop top\n");
               printf ("qcfptr->statn, qcfptr->hour_nom, qcfptr->minute_nom: %-s  %d %d\n",
                        qcfptr->statn, qcfptr->hour_nom, qcfptr->minute_nom);
#endif
               /*---------------------------------------------------------------
                * Compute sigmasq for all stns at a particular time and for all
                * parameters. Write this info to a file with a name indicating
                * time sigmas are valid. QC will read this output file. Fseek
                * above uses random file access to Skip past all lesser times.
                *
                * If first_file then just increment stn_no because there should
                * not be any stn repeats within a time freq. All stns should
                * be unique. The stn_list array is a reference list of internal
                * stn numbers to external stn names.
                *--------------------------------------------------------------*/
               if ( qcfptr->hour_nom > current_hr) break;
               if ((qcfptr->hour_nom == current_hr) && (qcfptr->minute_nom > current_min)) break;
 
               if ((qcfptr->hour_nom == current_hr) && (qcfptr->minute_nom == current_min))
                  {
                  sprintf (current_stn, "%-10s:%-15s\0",qcfptr->qnet,qcfptr->statn);
 
                  if (first_file)
                     {
                     stn_no++;
                     numstns++;
                     strncpy (stn_list[stn_no], current_stn, 27);
                     }
                  else
                     {
                     /*--------------------------------------
                      * Add to stn_list if not already there. 
                      *--------------------------------------*/
                     stn_no = determine_stn_no(stn_list, &numstns, current_stn);
                     }

                  date_last_seen[stn_no] = current_date;
#if DEBUG
                  printf ("(det_stn_no)current_stn, numstns, stn_no, stn_list[stn_no]: %-s %ld, %d %-s\n",
                           current_stn, numstns, stn_no, stn_list[stn_no]);
#endif
                  /*---------------------------------------------
                   * Assign current parameter values. Even
                   * assign (writeover) if new value is missing.
                   * Only increment parameter counters if new
                   * value is replacing a missing value. Only
                   * decrement the parameter counters if replacing
                   * a non-missing value with a missing value.
                   *
                   * The second 'if' in each of the following
                   * 'else' portions should never occur since
                   * we now reset writeover position to -999.
                   *
                   * As of Sept 2004, do not include previously
                   * flagged Bad (B) values in the sigmasq calcs.
                   * S/w resets writeover_position to missing, so
                   * leave as missing if encounter Bad parameter.
                   *---------------------------------------------*/
#if DEBUG 
                  printf ("writeover_position: %d\n", writeover_position);
                  printf ("(BEFORE)qcfptr->staprs, stn_data[ORIG], stn_numdata[stn_no][stnprs]:: %f %f %d\n", 
                          qcfptr->staprs, stn_data[stn_no][stnprs][writeover_position], 
                          stn_numdata[stn_no][stnprs] );
#endif
#if DEBUGBADVAL
                  printf ("staprs Value and flag (%7.2f, %1c).\n", qcfptr->staprs, qcfptr->staflg);
#endif
                  /* -- Station Pressure -- */
                  if  (qcfptr->staflg != 'B')
                     {
                     if (qcfptr->staprs >0.00)
                        if (stn_data[stn_no][stnprs][writeover_position] < -990.00) 
                           stn_numdata[stn_no][stnprs]++;
                     else
                        if (stn_data[stn_no][stnprs][writeover_position] > -990.00)
                           stn_numdata[stn_no][stnprs]--;

                     stn_data[stn_no][stnprs][writeover_position] = qcfptr->staprs;
                     }
                  /* else Bad value, leave as reset -999.99 values. */

#if DEBUG
                  printf ("(AFTER)qcfptr->staprs, stn_data[ORIG], stn_numdata[stn_no][stnprs]:: %f %f %d\n",
                          qcfptr->staprs, stn_data[stn_no][stnprs][writeover_position], 
                          stn_numdata[stn_no][stnprs] );
#endif 
#if DEBUGBADVAL
                  printf ("seaprs Value and flag (%7.2f, %1c).\n", qcfptr->seaprs, qcfptr->seaflg);
#endif
                  /* -- Sea Level Pressure -- */
                  if  (qcfptr->seaflg != 'B')
                     {
                     if (qcfptr->seaprs >0.00)
                        if (stn_data[stn_no][slp][writeover_position] < -990.00)
                           stn_numdata[stn_no][slp]++;
                     else
                        if (stn_data[stn_no][slp][writeover_position] > -990.00)
                           stn_numdata[stn_no][slp]--;

                     stn_data[stn_no][slp][writeover_position] = qcfptr->seaprs;
                     }
                  /* else Bad value, leave as reset -999.99 values. */

#if DEBUGBADVAL
                  printf ("cmpsea Value and flag (%7.2f, %1c).\n", qcfptr->cmpsea, qcfptr->cmpflg);
#endif
                  /* -- Calculated Sea Level Pressure -- */
                  if  (qcfptr->cmpflg != 'B')
                     {
                     if (qcfptr->cmpsea >0.00)
                        if (stn_data[stn_no][cslp][writeover_position] < -990.00)
                           stn_numdata[stn_no][cslp]++;
                     else
                        if (stn_data[stn_no][cslp][writeover_position] > -990.00)
                           stn_numdata[stn_no][cslp]--;

                     stn_data[stn_no][cslp][writeover_position] = qcfptr->cmpsea;
                     }
                  /* else Bad value, leave as reset -999.99 values. */

#if DEBUGBADVAL
                  printf ("temp Value and flag (%7.2f, %1c).\n", qcfptr->temp, qcfptr->tmpflg);
#endif
                  /* -- Temperature -- */
                  if  (qcfptr->tmpflg != 'B')
                     {
                     if (qcfptr->temp >-999.00)
                        if (stn_data[stn_no][temp][writeover_position] < -990.00)
                           stn_numdata[stn_no][temp]++;
                     else
                        if (stn_data[stn_no][temp][writeover_position] > -990.00)
                           stn_numdata[stn_no][temp]--;

                     stn_data[stn_no][temp][writeover_position] = qcfptr->temp;
#if DEBUGBADVAL
                     printf ("Good Temp Val Add In (%7.2f) at hour:min:: %d:%d.\n", qcfptr->temp, qcfptr->hour_nom, qcfptr->minute_nom);
#endif
                     }
                  /* else Bad value, leave as reset -999.99 values. */

#if DEBUGBADVAL
                  printf ("dewpnt Value and flag (%7.2f, %1c).\n", qcfptr->dewpnt, qcfptr->dewflg);
#endif
                  /* -- Dew Point Temperature -- */
                  if  (qcfptr->dewflg != 'B')
                     {
                     if (qcfptr->dewpnt >-999.00)
                        if (stn_data[stn_no][dewpt][writeover_position] < -990.00)
                           stn_numdata[stn_no][dewpt]++;
                     else
                        if (stn_data[stn_no][dewpt][writeover_position] > -990.00)
                           stn_numdata[stn_no][dewpt]--;

                     stn_data[stn_no][dewpt][writeover_position] = qcfptr->dewpnt;
                     }
                  /* else Bad value, leave as reset -999.99 values. */

#if DEBUGBADVAL
                  printf ("wndspd Value and flag (%7.2f, %1c).\n", qcfptr->wndspd, qcfptr->spdflg);
#endif
                  /* -- Wind Speed -- */
                  if  (qcfptr->spdflg != 'B')
                     {
                     if (qcfptr->wndspd >-999.00)
                        if (stn_data[stn_no][windsp][writeover_position] < -990.00)
                           stn_numdata[stn_no][windsp]++;
                     else
                        if (stn_data[stn_no][windsp][writeover_position] > -990.00)
                           stn_numdata[stn_no][windsp]--;

                     stn_data[stn_no][windsp][writeover_position] = qcfptr->wndspd;
                     }
                  /* else Bad value, leave as reset -999.99 values. */

#if DEBUGBADVAL
                  printf ("wnddir Value and flag (%7.2f, %1c).\n", qcfptr->wnddir, qcfptr->dirflg);
#endif
                  /* -- Wind Direction -- */
                  if  (qcfptr->dirflg != 'B')
                     {
                     if (qcfptr->wnddir >-999.00)
                        if (stn_data[stn_no][winddir][writeover_position] < -990.00)
                           stn_numdata[stn_no][winddir]++;
                     else
                        if (stn_data[stn_no][winddir][writeover_position] > -990.00)
                           stn_numdata[stn_no][winddir]--;

                     stn_data[stn_no][winddir][writeover_position] = qcfptr->wnddir;
                     }
                  /* else Bad value, leave as reset -999.99 values. */

                  } /* time match */

               } /* while data in current day's file.*/
#if DEBUG2
            printf ("Found %ld temp data points for current_stn: %-sxxx\n",
                     stn_numdata[0][temp], stn_list[0]);
#endif
            first_file = 0;

            close_file (&input_stream, FILE_NOT_COMPRESSED);

            last_date_processed = current_date;

            /*-------------------------------------
             * Update the writeover position. It
             * varies between 0 and SIGMA_PERIOD-1.
             * Constant for the qcf day file being
             * processed.
             *-------------------------------------*/
            writeover_position++;
#if DEBUG
            printf ("(End curr_date LOOP)Increment writeover_position to %d, numstns= %ld\n", 
                     writeover_position, numstns);
#endif 
            } /* for current_date */

         /*------------------------------------
          * Don't break here; Want to continue
          * and check next day for this time.
          *-----------------------------------*/
         if (numstns == 0) continue;
 
         /*----------------------------------------------------------------
          * Write number of stations in station list and the station list
          * to the sigma output file. Note that the station list could vary
          * for each time period. In general, it should just get longer or
          * include more stations. But, stations can drop in and out at any
          * time.
          *----------------------------------------------------------------*/
         sprintf (sigma_output_file_name, "%-s%07ld%02d%02d.sig",
                  sigpathname, end_processing_date,
                  current_hr, current_min);
 
#if DEBUG
         printf ("OPEN sigma_output_file_name: xxx%-sxxx\n", 
                 sigma_output_file_name);
#endif

         open_file (sigma_output_file_name, "w", FILE_NOT_COMPRESSED, &sigma_output_stream);

         /*-----------------------------------------------
          * Preprocess output stn_list. Only output those
          * stns that were seen in at least one of the days
          * used to compute these sigmasq. Do any 
          * renumbering that's required.
          *----------------------------------------------*/
         start_sigma_period = subtract_num_from_YYYYJJJ(end_processing_date, SIGMA_PERIOD);

         output_numstns = 0;

         for (ii=0;ii<numstns;ii++)
            if (start_sigma_period <= date_last_seen[ii]  && 
                date_last_seen[ii] < end_processing_date)
               output_numstns++;

#if DEBUG
         printf ("start_sigma_period, numstns, output_numstns: %d  %ld  %ld\n", 
                 start_sigma_period, numstns, output_numstns);
#endif
         fprintf (sigma_output_stream, "%5ld\n", output_numstns);

         output_ct = 0;
         for (ii=0;ii<numstns;ii++)                        
            {
            /*-------------------------------
             * Only print active stations.
             *------------------------------*/
            if (start_sigma_period <= date_last_seen[ii]  &&
                date_last_seen[ii] < end_processing_date)
               {
               fprintf (sigma_output_stream, "%5ld %-27s\n", output_ct, stn_list[ii]);
               output_ct++;
               }
            }
 
         fprintf (sigma_output_stream, "stn_no parm_no  sigma value\n");
 
 
         /*-----------------------------------------------------------
          * Compute variance (variance = std*std = sigma squared)
          * for a particular stn and parameter over the sigma
          * period. Do all parameters for all stns at time t.
          *
          * First sigmas computed are valid for the first SIGMA_PERIOD
          * plus one day (e.g., If SIGMA_PERIOD is 30 days, then the
          * first set of sigmas computed using days 1 through day 30
          * should be used to QC all the data from days 1 through day
          * 31 of the project.  The name of the sigma file indicates
          * the day and time the data is valid. The QC program must
          * be smart enough to know to reuse the first set of sigmas
          * for a longer period.
          *
          * Stations periodically come and go in the data. When
          * a station first appears, sigma sq can not be computed.
          * Once "enough" data points have been collected
          * for this station, the variances can be computed. These
          * variances for this new station are then valid (as when
          * a project first starts) for the first SIGMA_PERIOD+1 days.
          * "Enough" points is defined as any number between 15 and
          * SIGMA_PERIOD for surface data as collected in VORTEX,
          * STORMWAVE, etc. The value 15 is then set to be the
          * MINIMUM_NUM_VALS that must occur in a SIGMA_PERIOD
          * number of days. These MINIMUM_NUM_VALS do not have to
          * be on consecutive days. The QC program must be smart
          * enough to locate and use the appropriate set of sigma sq
          * values when stations come and go during a project. The
          * MINIMUM_NUM_VALS and SIGMA_PERIOD variables are defined in
          * the gQC.h header file. These two variables could be
          * project dependent.
          *-----------------------------------------------------------*/
         output_ct = -1;

         for (i=0;i<numstns;i++)
           {
           /*-----------------------------------------------
            * Only print if seen during this SIGMA_PERIOD.
            *----------------------------------------------*/
           if (start_sigma_period <= date_last_seen[i]  &&
               date_last_seen[i] < end_processing_date)
              {
              output_ct++;

              for (j=0;j<NUMQCPARMS;j++)
                 { 
                 if (stn_numdata[i][j] >= MINIMUM_NUM_VALS)
                    {
                    sigma_sq[i][j]=0.0;
                    ave=svar=adev=sdev=0.0;
#if DEBUG2
                    for (kk=0;kk<SIGMA_PERIOD; kk++)
                       printf ("output_ct= %d, stn_data[%d][%d][%d] = %7.2f\n", 
                               output_ct, i, j, kk, stn_data[i][j][kk]);

                    printf ("CALL determin_sigma!!! current_hr, current_min: %d %d\n", 
                             current_hr, current_min);
#endif
                    /*-----------------------------------------------------------
                     * Always pass in complete SIGMA_PERIOD amount of data.
                     * Let determine_sigma() fn excluded mixed in missing values.
                     *----------------------------------------------------------*/
                    determine_sigma(stn_data[i][j], SIGMA_PERIOD, &ave, &svar, &adev, &sdev);
                    sigma_sq[i][j] = svar;

                    /*------------------------------------------
                     * Ensure the output numbering is sequential
                     * by printing output_ct instead of i.
                     * Printing i only works if all stations are
                     * active (i.e., seen in this SIGMA_PERIOD).
                     *------------------------------------------*/ 
                    fprintf (sigma_output_stream, "%5d %5d %f\n", output_ct, j, sigma_sq[i][j]);

                    /*------------------------------------------
                     * Issue a warning if the variance is 0.00.
                     * This means the values for the current
                     * parameter was constant, and this may 
                     * point to a software conversion problem.
                     *-----------------------------------------*/
                    if ((int)(sigma_sq[i][j] * 10000) == 0)
                       printf ("WARNING: ZERO SigmaSq Val (%f) computed for stn=%-15s, parm=%d in %-s\n",
                       sigma_sq[i][j], stn_list[i], j, sigma_output_file_name);
  
#if DEBUG2
                    printf ("(var)output_ct=%d, sigma_sq[%d][%d] = %f\n", 
                            output_ct,i, j, sigma_sq[i][j]);
                    printf ("stn (%d) parm (%d), ave, svar, adev, sdev: %f %f %f %f\n\n",
                            i, j, ave, svar, adev, sdev);
#endif
                    }
#if DEBUG         
                 else /* MINIMUM_NUM_VALS */
                    {
                    /*----------------------------------------------------
                     * Could write out missing variance to output file,
                     * but not writing missing values saves space.
                     *---------------------------------------------------*/
                    if (j !=1)    /* 1 = calc slp */
                       {
                       printf ("stn_numdata[%d][%d]=%d not >MINIMUM_NUM_VALS\n",i,j,stn_numdata[i][j]);
                       printf ("Current SIGMA FILE: %-s, Current stn#: %d\n", sigma_output_file_name, i);
                       }
                    } /* MINIMUM_NUM_VALS */
#endif
                 } /* for all parameters j */
               } /* if stn seen in this time period */
#if DEBUG
            else
               {
               printf ("stn_list[%d] = %-s -- NOT CURRENTLY ACTIVE!\n", i, stn_list[i]);
               }
#endif
            } /* for all stns (i) */

         if (output_ct != output_numstns-1)
            {
            printf ("ERROR in %-s : output_ct != output_numstns\n", 
                    sigma_output_file_name);
            exit(1);
            }

         close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);

         sprintf (compress_cmd, "gzip %-s\0", sigma_output_file_name);
         system (compress_cmd);

         } /* while (last_date_processed < end_YYYYJJJ) */

#if DEBUG
      printf ("GO TO NEXT TIME!!!!!\n");
#endif

      } /* while current_hr <= 23 */

#if DEBUG
   printf ("Compute next times sigmas - for all days!\n");
#endif

   destruct_qcfptr (&qcfptr);
 
   printf ("\nCompute Sigma Square processing ended on %-s %-s\n", __DATE__, __TIME__);
 
   }  /* compute_sigmasq_4DYR() */
