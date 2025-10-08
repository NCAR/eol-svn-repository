/*--------------------------------------------------------
 * compute_sigmasq - Computes sigma squared (i.e., variance)
 *  for stns/data in QCF format. Variances are written to
 *  output files which are later read and used during the
 *  the final Horizontal Quality Control processing. See
 *  WARNINGS section below.
 *
 *  Usage: compute_sigmasq <initialization_input_file>
 *
 *  This program (using input info about project/data dates)
 *  opens, extracts data from, and closes the input QCF files and
 *  then computes variances for all parameters at all times and at 
 *  all stations in the QCF day files. This is done beginning
 *  at the user specified begin date through the end date. A
 *  SIGMA_PERIOD number of days (i.e., # of parameter values) is 
 *  used to compute each variance. 
 *
 *  Compute_sigmasq() computes sigma values for the following
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
 *  Compute_sigmasq() should always be executed with the
 *  same initialization input file that will be used for
 *  the HQC processing. Begin and end dates can be modified to
 *  to control the amount of data processed, but all other
 *  input parameter should remain the same.
 *       
 *  WARNINGS: The Horizontal Quality Control (HQC) program
 *      (main_qc) may update/overwrite the sigma_sq files
 *      created by compute_sigmasq. See the header description
 *      in main_qc for more details. It is wise to make a 
 *      copy of the original sigma_sq files as a safe guard.
 *
 *  Assumptions:
 *   -  Input data is in QCF format and sorted by date/time.
 *      These should be the exact same files that main_qc()
 *      file uses to perform Horizontal Quality Control.
 *   -  Assumes specific format for user specified input file.
 *   -  Hours run from 0 to 23 not 1 to 24.
 *   -  Expects day files whose names are of the form:
 *      yymmdd.0qc (suffix currently hardcoded). Actually only
 *      assumes that yymmdd immediately preceeds '.0qc' suffix.
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
 *  Line 6: First day of Project              (proj_begin_date) (YYMMDD string)
 *  Line 7: Last  day of Project              (proj_end_date) (YYMMDD string)
 *  Line 8: First day to QC/compute variance  (begin_date) (YYMMDD string)
 *  Line 9: Last day to QC/compute variance   (end_date) (YYMMDD string)
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
 *    Numerous sigmasq (variance) files. BEWARE that depending
 *    on the number of days being processed, number of stns
 *    in the data, etc, etc, the user may need to limit the
 *    number of days for which the sigma files are created
 *    at during one processing session. Create only those
 *    sigma files required to do Horizontal QC for a 
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
 *    The form of all output variance files is:
 *
 *          Definition                        (var) (type)
 *  -----------------------------------------------------------------
 *  Line 1: Number of stations at this time   (numstns) (int)
 *  Line 2->xx: Internal ID Network:Stn ID    (i, stn_list[i][27]) (int, string)
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
 * 10 Nov 95 lec
 *   Changed some ints to longs, such as all YYYYJJJ dates,
 *   and some loop flags used for numstns and stn_no.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>

#include "local.h"
#include "process_qcfrec.h"
#include "qcfrec.h"
#include "gQC.h"
#include "date.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * Set DEBUG2 to 1 for even more debug type output to
 * screen.
 *-------------------------------------------------------*/
#define   DEBUG  0
#define   DEBUG2 0 

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

   long int     fileRecNo[SIGMA_PERIOD];
   long int     fileno;

   char		pathname[NAMELEN_MAX];
   char		sigpathname[NAMELEN_MAX];

   char         compress_cmd [NAMELEN_MAX+8];

   QCFREC       *qcfptr; 
   long int     i, ii, jj  = 0;

   int           j,k,kk = 0;
   int           data_freq   = 0;
   STRING27      stn_list[MAXNUMSTNS];
   long          stn_no      = -1;
   long          numstns     = 0;
   int           first_file;
   
   char          current_stn[27] = "\0";
   char          NULL27[27] = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
   char          proj_name[26] = "\0\0\0\0\0\0\0\0\0\0";
   float         ave, svar, adev, sdev = 0.0;
 
   int           current_yr   = 0; /* YYYY */
   int           hour         = 0;
   int           current_hr   = 0;
   int           current_min  = 0;
   long int      current_date = 0; /* YYYYJJJ */

   char          date[7]      = "\0\0\0\0\0\0\0"; /*YYMMDD */
 
   char          proj_begin_date[7] ="\0\0\0\0\0\0\0"; /* Date that project begins. QC program uses next
                                                          date (i.e., begin_date) to individually QC days.
                                                          YYMMDD - input value */ 
   char          proj_end_date[7] ="\0\0\0\0\0\0\0";   /* Date that project ends. YYMMDD - input value*/
   char          begin_date[7] ="\0\0\0\0\0\0\0";      /* Input date to begin processing. YYMMDD-input value*/
   char          end_date[7]   ="\0\0\0\0\0\0\0";      /* Input date to end processing. YYMMDD-input value */
 
   long int      process_jdate     = 0;           /* Beginning YYYYJJJ date of current days for which
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
    * Following Variables contain dates of the form:
    * YYYYJJJ where YYYY is the century and yr (e.g. 
    * 2001) and JJJ is the julian day (e.g., 236 for 
    * 24 August 2001). Dates in this form are easily 
    * incremented, compared, subracted from, etc. 
    */ 
   long int proj_begin_YYYYJJJ = 0;
   long int begin_YYYYJJJ = 0;
   long int end_YYYYJJJ  = 0;
   long int end_process_YYYYJJJ = 0;
   long int end_sigma_period_date = 0;  

   /*
    * Vars used to determine if data rollsover into
    * next year.
    */
   int          data_rollsover = FALSE;   /* Boolean flag indicating if data rollsover
                                             from one year into the next. */
   char         end_YY_str[3]   = "\0\0\0";
   int          end_YY   = 0;
   char         end_MMDD[5]   = "\0\0\0\0\0";
   char         begin_YY_str[3] = "\0\0\0";
   int          begin_YY = 0;
   char         begin_MMDD[5] = "\0\0\0\0\0";
 
   /*
    * stn_data    - data from which sigmas are computed.
    * stn_numdata - number of data points in stn_data by parameter.
    * sigma_sq    - variances for each stn and each parameter at a specified time.
    */
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
      printf ("Usage: compute_sigmasq <initialization_input_file>\n");
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
   
   open_file (init_input_file_name, "r", &init_input_stream);

   printf ("\nPARAMETER INITIALIZATION:: \n\n");

   /*-----------------------------------------
    * Read data from input file (QCinit.inp).
    *----------------------------------------*/
   fgets(proj_name, 26, init_input_stream);
   STRIPLINE(init_input_stream);
   STRIPLINE(init_input_stream);                /*Skip pmethod*/
   STRIPLINE(init_input_stream);                /*Skip min_weight*/
   fscanf (init_input_stream, "%d", &data_freq);      STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%d", &current_yr);     STRIPLINE(init_input_stream); 
   fscanf (init_input_stream, "%s", proj_begin_date); STRIPLINE(init_input_stream);
   proj_begin_date[6] = '\0';
   fscanf (init_input_stream, "%s", proj_end_date);   STRIPLINE(init_input_stream);
   proj_end_date[6] = '\0';
   fscanf (init_input_stream, "%s", begin_date);      STRIPLINE(init_input_stream);
   begin_date[6] = '\0';
   fscanf (init_input_stream, "%s", end_date);        STRIPLINE(init_input_stream);
   end_date[6] = '\0';
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
    * into next year. Note that in compute_sigmasq we use only
    * end date not the project end date for processing.
    *--------------------------------------------------------*/
   if (end_YY > begin_YY || atoi(end_MMDD) < atoi(begin_MMDD))
      {  
      /*------------------------------------
       * Data rolls from one year into next.
       *------------------------------------*/
      data_rollsover = TRUE;

      date_to_YYYYJJJ (end_date, current_yr+1, &end_YYYYJJJ);
      }  
   else
      {  
      /*----------------------------------
       * Data is contained in single year.
       *----------------------------------*/
      date_to_YYYYJJJ (end_date, current_yr, &end_YYYYJJJ);
      }  
 

   /*------------------------------------------------------------------------
    * Process all data, forming sigmasq (variance) values for all 
    * days of project and all times in each day. Note that variable process_jdate
    * is the beginning julian date (YYYYJJJ) of each of the SIGMA_PERIODs
    * for which sigmasq's are computed. The end_date is most likely the
    * stop date of the project, but the user may decide to create only
    * a portion of the sigmasq files due to space contraints. The size of a 
    * (representative) variance file times number of days times 24hrs
    * divided by data frequency indicates space needed for all sigmasq files
    * for a project.
    *------------------------------------------------------------------------*/
   end_process_YYYYJJJ = subtract_num_from_YYYYJJJ( end_YYYYJJJ, SIGMA_PERIOD);

#if DEBUG
   printf ("proj_begin_YYYYJJJ, end_YYYYJJJ, end_process_YYYYJJJ: %ld %ld %ld\n",
           proj_begin_YYYYJJJ, end_YYYYJJJ, end_process_YYYYJJJ);
#endif

 
   if (end_process_YYYYJJJ < proj_begin_YYYYJJJ)
      {  
      printf ("WARNING: User specified end date is within SIGMA_PERIOD days\n");
      printf ("         of proj_begin date. User must have at least SIGMA_PERIOD\n");
      printf ("         days worth of data! Reset end date to equal proj_begin date\n");
      printf ("         plus SIGMA_PERIOD OR shorten SIGMA_PERIOD.\n");
      exit(1);
      }  

   for(process_jdate=proj_begin_YYYYJJJ; process_jdate<=end_process_YYYYJJJ; increment_YYYYJJJ(&process_jdate))
      {
#if DEBUG
      printf ("------------processing data for date: %ld\n", process_jdate);
#endif

      for (kk=0;kk<SIGMA_PERIOD;kk++)
         fileRecNo[kk] = 0;

      current_hr  = 0;
      current_min = -1*data_freq;

      while (current_hr <= 23) /* assumes hrs run 0 to 23 */
         {
         /*-------------------------------
          * Reinitialize the station data.
          *------------------------------*/
         stn_no     = -1;
         first_file = 1;
         numstns    = 0;

         for (jj=0;jj<MAXNUMSTNS;jj++)
            {
            strncpy (stn_list[jj], NULL27, 27);

            for (k=0;k<NUMQCPARMS;k++)
               {
               stn_numdata[jj][k] = 0;
               for (kk=0;kk<SIGMA_PERIOD; kk++)
                  stn_data[jj][k][kk] = -999.99; /* was 0.0 */
               }
            }

         /*------------------------------------------------------------
          * Store off the data values to form sigma for a particular
          * station, parameter, and time. To form sigma for stn ILE at
          * time 00:20, only use data at stn ILE at times 00:20 on
          * these SIGMA_PERIOD (e.g., 30) days. 
          *------------------------------------------------------------*/
         current_min = current_min + data_freq;

         if (current_min >= 60)
            {
            hour = (int)(current_min/60);         /* allow increment of > 1 hr */
            current_hr = current_hr + hour;
            current_min = current_min - hour*60; /* find remainder minutes */
            }

#if DEBUG
         printf ("current_hr, current_min: %d %d\n", current_hr, current_min);
#endif

         if (current_hr >= 24)  /* was == */
            break;

         fileno = -1;

         /*------------------------------------------------------------
          * Form sigmasq (variances) for all 7 QC parameters (qcparms).
          *------------------------------------------------------------*/
         end_sigma_period_date = add_num_to_YYYYJJJ (process_jdate, SIGMA_PERIOD);

         for (current_date=process_jdate; current_date<end_sigma_period_date; increment_YYYYJJJ(&current_date))
            {
            fileno++;

            /*----------------------------------------------
             * Form name of input file and open. Assume name
             * is of the form (pathname*)yyddmm.0qc. Must
             * pick year out of incremented date, since may
             * have rolled over into next yr while processing
             * SIGMA_PERIOD # files. Convert YYYYJJJ dates
             * yymmdd form.
             *----------------------------------------------*/
            YYYYJJJ_to_date (current_date, date);
#if DEBUG
            printf ("\nCurrent julian date is: %d %-s\n", current_date, date);
#endif
            sprintf (input_file_name, "%-s%-s.0qc\0", pathname, date);
            open_file (input_file_name, "r", &input_stream);
#if DEBUG
            printf ("Open file: %-sxxx\n", input_file_name); 
#endif
#if DEBUG2 
            printf ("current_hr, current_min: %d %d\n", current_hr, current_min);
#endif
            /*---------------------------------------------------------
             * Go to the last rec read in this file. Use Random Access.
             * Then Begin reading and read until hit next time.
             *---------------------------------------------------------*/
            if (fseek(input_stream, fileRecNo[fileno],0) !=0) /*go there*/
               {
               printf ("Error: Can't move file pointer to fileRecNo = %d\n", fileRecNo[fileno]);
               exit(1);
               }

            while (!feof(input_stream))
               {
               fileRecNo[fileno] = ftell(input_stream);
#if DEBUG2
               printf ("FileRecNo[%ld]: %ld\n", fileno,fileRecNo[fileno]);
#endif

               reset_qcfrec( qcfptr );   /* Safe guard only */
               read_qcfrec (&input_stream, qcfptr);
#if DEBUG2
               printf ("\nRead qcfrec - loop top\n");
               printf ("qcfptr->statn, qcfptr->hour_nom, qcfptr->minute_nom: %-s  %d %d\n", 
                        qcfptr->statn, qcfptr->hour_nom, qcfptr->minute_nom);
#endif
               /*---------------------------------------------------------------
                * Compute sigmasq for all stns at a particular time and for all
                * parameters. Write this info to a file with a name indicating
                * time sigmas are valid. QC will read this output file. Fseek
                * above uses random file access to Skip past all lesser times.
                *--------------------------------------------------------------*/
               if ( qcfptr->hour_nom > current_hr) break;
               if ((qcfptr->hour_nom == current_hr) && (qcfptr->minute_nom > current_min)) break;

               if ((qcfptr->hour_nom == current_hr) && (qcfptr->minute_nom == current_min))
                  {
                  sprintf (current_stn, "%-10s:%-15s\0",qcfptr->qnet,qcfptr->statn);

                  if (first_file)
                     {
                     stn_no++; /* should not be stn repeats within a time freq. All stns unique.*/
                     numstns++;
                     strncpy (stn_list[stn_no], current_stn, 27); /* keep ref list of nums to names */
                     }
                  else
                     {
                     /*--------------------------------------
                      * Add to stn_list if not already there.
                      *--------------------------------------*/
                     stn_no = determine_stn_no(stn_list, &numstns, current_stn);
                     }
#if DEBUG
                  printf ("current_stn, numstns, stn_no, stn_list[stn_no]: %-sxxx %ld, %ld %-sxxx\n",
                           current_stn, numstns, stn_no, stn_list[stn_no]);
#endif

                  /* -- Station Pressure -- */
                  if (qcfptr->staprs >0.00)
                     {
                     stn_data[stn_no][stnprs][stn_numdata[stn_no][stnprs]] = qcfptr->staprs;
                     stn_numdata[stn_no][stnprs]++;
                     }

                  /* -- Sea Level Pressure -- */
                  if (qcfptr->seaprs >0.00)
                     {
                     stn_data[stn_no][slp][stn_numdata[stn_no][slp]] = qcfptr->seaprs;
                     stn_numdata[stn_no][slp]++;
                     }

                  /* -- Calculated Sea Level Pressure -- */
                  if (qcfptr->cmpsea >0.00)
                     { 
                     stn_data[stn_no][cslp][stn_numdata[stn_no][cslp]] = qcfptr->cmpsea;
                     stn_numdata[stn_no][cslp]++;
                     }
 
                  /* -- Temperature -- */
                  if (qcfptr->temp >-999.00)
                     { 
                     stn_data[stn_no][temp][stn_numdata[stn_no][temp]] = qcfptr->temp;
                     stn_numdata[stn_no][temp]++;
                     }
 
                  /* -- Dew Point Temperature -- */
                  if (qcfptr->dewpnt >-999.00)
                     { 
                     stn_data[stn_no][dewpt][stn_numdata[stn_no][dewpt]] = qcfptr->dewpnt;
                     stn_numdata[stn_no][dewpt]++;
                     }
 
                  /* -- Wind Speed -- */
                  if (qcfptr->wndspd >-999.00)
                     { 
                     stn_data[stn_no][windsp][stn_numdata[stn_no][windsp]] = qcfptr->wndspd;
                     stn_numdata[stn_no][windsp]++;
                     }
 
                  /* -- Wind Direction -- */
                  if (qcfptr->wnddir >-999.00)
                     { 
                     stn_data[stn_no][winddir][stn_numdata[stn_no][winddir]] = qcfptr->wnddir;
                     stn_numdata[stn_no][winddir]++;
                     }

                  } /* time match */
 
               } /* while data in current day's file.*/
#if DEBUG2
            printf ("Found %ld temp data points for current_stn: %-sxxx\n", 
                     stn_numdata[0][temp], stn_list[0]);
#endif

            first_file = 0;
            close_file (&input_stream);


            } /* for SIGMA_PERIOD days */

         if (numstns == 0) break;

         /*----------------------------------------------------------------
          * Write number of stations in station list and the station list
          * to the sigma output file. Note that the station list could vary
          * for each time period. In general, it should just get longer or
          * include more stations.
          *----------------------------------------------------------------*/
         sprintf (sigma_output_file_name, "%-s%07d%02d%02d.sig",
                  sigpathname, end_sigma_period_date,
                  current_hr, current_min);

#if DEBUG
         printf ("open sigma_output_file_name: xxx%-sxxx\n", sigma_output_file_name);
#endif
 
         open_file (sigma_output_file_name, "w", &sigma_output_stream);
 
         fprintf (sigma_output_stream, "%5ld\n", numstns);
 
         for (ii=0;ii<numstns;ii++)
               fprintf (sigma_output_stream, "%5ld %-27s\n", ii, stn_list[ii]);

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
          * Once "enough" # of data points have been collected 
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
         for (i=0;i<numstns;i++)
           {
           for (j=0;j<NUMQCPARMS;j++)
               {
               if (stn_numdata[i][j] >= MINIMUM_NUM_VALS)
                  {
                  sigma_sq[i][j]=0.0;
                  ave=svar=adev=sdev=0.0;
#if DEBUG2
                  for (kk=0;kk<SIGMA_PERIOD; kk++)
                     printf ("stn_data[%d][%d][%d] = %7.2f\n", i, j, kk, stn_data[i][j][kk]);
#endif
                  determine_sigma(stn_data[i][j], stn_numdata[i][j], &ave, &svar, &adev, &sdev);
                  sigma_sq[i][j] = svar;

                  fprintf (sigma_output_stream, "%5ld %5d %f\n", i, j, sigma_sq[i][j]);
#if DEBUG2
                  printf ("(var)sigma_sq[%ld][%d] = %f\n", i, j, sigma_sq[i][j]);
                  printf ("stn (%ld) parm (%d), ave, svar, adev, sdev: %f %f %f %f\n\n", 
                          i, j, ave, svar, adev, sdev); 
#endif
                  }
               else
                  {
#if  DEBUG
                  /*----------------------------------------------------
                   * Could write out missing to output file, 
                   * but not writing missing values saves space. 
                   *---------------------------------------------------*/
                  if (j !=1)
                     {
                     printf ("stn_numdata[%ld][%d]=%d not >MINIMUM_NUM_VALS\n",i,j,stn_numdata[i][j]);
                     printf ("Current SIGMA FILE: %-s, Current stn#: %ld\n", sigma_output_file_name, i);
                     }
#endif
                  }

               } /* for all parameters j */
            } /* for all stns (i) */

         close_file (&sigma_output_stream);

         sprintf (compress_cmd, "gzip %-s\0", sigma_output_file_name);
         system (compress_cmd);

#if DEBUG
         printf ("GO TO NEXT TIME!!!!!\n");
#endif
         } /* while current_hr <= 23 */

#if DEBUG
      printf ("Compute next days sigmas!\n");
#endif
      } /* for process_jdate - stepping through all days in project */

   destruct_qcfptr (&qcfptr);

   printf ("\nCompute Sigma Square processing ended on %-s %-s\n", __DATE__, __TIME__);

   }  /* compute_sigmasq() */
