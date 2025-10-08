/*--------------------------------------------------------
 * main_qc_4DYR.c -  Performs Horizontal Quality Control (HQC).
 *              (See WARNINGS below.) Now handles 4 digit year.
 *
 *   Usage: main_qc_4DYR <initialization_input_file>
 *
 *   Or:  (runQC_4DYR > runQC_4DYR.log) >& /dev/null&
 *
 *      where runQC contains: 
 *          /usr/bin/nice -3 main_qc_4DYR QCinit_4DYR.inp
 *
 *   The second usage places the process at a lower
 *   priority of 3, will not stop when the user logs out,
 *   and dumps extraneous error messages to null (bit bucket).
 *   Extraneous error messages are generated everytime
 *   main_qc_4DYR closes a file that was not scanned completely.
 *   This is a function of way we now open/close sigmasq files.
 *   It is not an error, since we do not want to read each
 *   file to the end, if we know that what we are looking
 *   for is not in that file. So, we dump these errors
 *   to null.
 *
 *   This s/w performs HQC by applying previously
 *   computed variance values and a user-specified distance
 *   formula to each input parameter for all times and
 *   all stations. The variances are computed by 
 *   executing the compute_sigma program. (See that 
 *   program for more information.) Main_qc.c reads
 *   these variance values and combines these with user-
 *   specified alpha values to set limits that determine
 *   whether each input parameter is good (G), dubious (D), or 
 *   unlikely (B).  In the final processing step, main_qc_4DYR
 *   applies a set of logical checks to certain input
 *   parameters. These logical checks may override 
 *   previously set quality control flags. See that 
 *   section of the s/w for a detailed description.
 *
 *   Main_qc applies HQC to the following set of six
 *   parameters:  
 *                Calculated Sea Level Pressure,
 *                Sea Level Pressure,
 *                Temperature,
 *                Dew Point Temperature,
 *                Wind speed,
 *                Wind Direction.
 *
 *   The Quality Control (QC) flag determined for the
 *   Calculated Sea Level Pressure (CSLP) is applied to the
 *   Sea Level pressure and station pressure provided the 
 *   CSLP QC flag is not missing, unchecked, etc. If the CSLP
 *   is one of these unsuitable values but the Sea Level
 *   pressure has a QC flag that is not missing, etc., the
 *   the Sea Level pressure QC flag is used to set the
 *   station pressure QC flag. The user may also set
 *   the defined "QC_STNPRS" flag (see below in s/w) to
 *   cause main_qc_4DYR to perform HQC on the station
 *   pressure. Even if this flag is turned on, one of the
 *   Sea Level Pressure QC flags may override the 
 *   station pressure QC flag.
 * 
 *   Gross limit checks are applied to the precipitation
 *   values. The precipitation gross limit checks apply
 *   user-specified limits. (See bmax and dmin.) If the
 *   the precipitation value is greater than or equal to bmax,
 *   that precip QC flag is set to unlikely (B). If the precip
 *   value is less than bmax, but greater than or equal to dmin,
 *   the precip QC flag is set to dubious (D). If the precip
 *   value is less than dmin, the precip QC flag is set
 *   to good (G).
 *                
 *   This s/w automatically locates the "best" sigma squared
 *   (variance) value for each stn/time/parameter set. Using
 *   the date and time from this set, this HQC s/w determines 
 *   the only file where the compute_sigmasq program would have placed
 *   the desired sigma sq value.  If compute_sigmasq could not 
 *   compute a variance for this stn/time/parameter set, no
 *   value will have been written to that sigma sq file. When this 
 *   HQC s/w can not find a variance in that first sigma sq file,
 *   this s/w searches ahead in time (but no farther than
 *   SIGMA_PERIOD days) through subsequent sigma sq files. The search 
 *   continues until the HQC s/w locates the first non-missing
 *   variance for this stn/time/parameter set. If a sigmasq
 *   value can not be located within SIGMA_PERIOD days, the
 *   parameter can not be Quality Controlled.  To prevent
 *   multiple searches for the same stn/time/parameter set,
 *   this s/w updates the first sigma sq file that would
 *   be searched with the located siqmasq value. If the sigmasq 
 *   value could not be located, a value of -888.88 will be
 *   written to the updated sigma sq file for this stn/time/
 *   parameter set. Note that the number of days searched has
 *   been limited to SIGMA_PERIOD days to minimize crossing
 *   seasons. Applying sigma values computed with summer
 *   data to spring values would be undesirable. See below for
 *   a format description of the sigma_sq files.
 *
 *   In the input file, the user specifies the distance function
 *   and the maximum area of influence used during HQC.
 *   The distance function is used to set weights for all
 *   stations surrounding the station currently being
 *   quality controlled. To maximize flexibility, this
 *   s/w computes and applies these distances continually
 *   as data is read and processed. If all stations
 *   are fixed in location, then it would be possible to do
 *   these computations only once. These distances or weights
 *   could be retained at the expense of using (potentially)
 *   lots of space. Experience may show, however, that
 *   it is wise to pay the price of recomputing these
 *   distances each time period. It is already known
 *   that stations come and go (and even move) in
 *   the data as time passes. This section of the process
 *   may need to be modified/changed if processing time is
 *   too large. Also remember that due to the curvature of the earth,
 *   the distance between station A and station B does not
 *   necessarily equal the distance between station B to 
 *   station A. It depends on the path you follow. This means 
 *   that you can not simply retain a single distance value.
 *       
 *   Note: This s/w should properly handle data that begins
 *   in one year and rollsover into next year. Rollover
 *   into next century should also be handled properly.
 * 
 * WARNINGS:
 *   When required as described above, this s/w will 
 *   update (i.e., rewrite) portions of the incoming
 *   sigma_sq files. Each time an update occurs, this
 *   s/w currently saves off a backup (*.bak) copy of
 *   the sigma_sq file before the update is performed.
 *   This means that if several updates are performed
 *   on a single sigma_sq file, a difference between the
 *   two files will only show the last completed change.
 *   So, it is wise to SAVE OFF A COPY OF THE ORIGINAL
 *   SIGMA_SQ FILES, else the user might have to rerun
 *   the compute_sigmasq program to recreate the original
 *   files.
 *
 * Assumptions:
 *  - Assumes specific format for sigma squared input file
 *    named: yyyyjjjhhmm.sig where yyyy is the year,
 *    jjj is the julian date, hh is the hour,
 *    and mm is the minute. Name indicates date and time
 *    for which variances in file are valid.
 *  - Assumes specific format for user specified input file.
 *  - Input data is in QCF format and sorted by date/time.
 *    These should be the exact same files that compute_sigmasq
 *    file used to compute the sigmasq (variance) values.
 *  - Expects suffix for input file names to be .0qc. This
 *    is currently hardcoded. Could change and recompile.
 *  - Expects sigma_sq files to be gzipped. If files are
 *    not gzipped, a warning will be issued, but the s/w
 *    will continue unharmed. After the s/w has read
 *    the sigma_sq file, it will gzip the file. See UNIX_ENV flag.
 *  - That no two stations in a single network have the
 *    same ID. We use "network:ID" as a unique identifier.
 *  - That all Mobile stations can be identified by their
 *    network of either "DATSAV2M" or "GLERLM".
 *
 * Input:
 *    User must indicate name of QC input control file when
 *    executing this program. This QC input control file
 *    contains several input values, limits, dates,
 *    etc. required by the horizontal QC program. The format
 *    for this input file is as follows:
 *
 *          Definition                        (var) (type)
 *  -----------------------------------------------------------------
 *  Line 1: Project Name                      (proj_name) (25 chars)
 *  Line 2: Distance fn method                (pmethod) (float)
 *  Line 3: Area of Influence in km           (1/min_weight) (float)
 *  Line 4: Data Frequency                    (data_freq) (integer)
 *  Line 5: Current year of start of data     (current_yr) (YYYY string) Must match proj_begin_date's year.
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
 * This program also expects sigma_sq files as input. These sigma_sq
 * or variance files have file names of the form:
 *                   yyyyjjjhhmm.sig
 *
 *    where yyyy is year, jjj is julian date, hh is hour, mm is
 *    minute for which the contained sigmasq (variance) values
 *    are valid. And will be located in the directory specified
 *    as the output directory in the QC input control file. 
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
 *     0 E02
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
 * Output:
 *   Quality Controlled QCF files. (*.qcf)
 *
 *   This s/w will also update (or completely rewrite when
 *   necessary) a sigma_sq file when a search had to be
 *   performed on a missing sigma_sq value. If a sigma_sq
 *   file is rewritten, a .bak file is retained. Differencing
 *   the sigma_sq file and it's .bak file will on show the
 *   lastest update. It is advised (when it is reasonable)
 *   to retain a copy elsewhere of the original sigma_sq files.
 *
 * 10 Dec 94 lec
 *   Created.
 * 06/08 Jun 95 lec
 *   Upgrade s/w with mods specified by scientific staff.
 *   These upgrades include allowing different alpha
 *   values for each parameter AND changing the logical
 *   quality control checks performed at the end of this
 *   program.
 * 11/12 Sep 95 lec
 *   Updated s/w to accept name of QC input file as
 *   line input. Added more info in header. Updated s/w
 *   to handle data that rollsover into next year.
 * 29 Sep-2 Oct 95 lec
 *   Updated s/w to call fn to locate_sigmasq when sigma_sq
 *   value is not available in expected sigma_sq file. Add
 *   s/w to rewrite sigmasq file once a missing sigmasq
 *   value has been located in a later sigma_sq file.
 * 01 Nov 95 lec
 *   Upgraded s/w to use YYYYJJJ dates and fns. Now s/w
 *   matches more closely with compute_sigmasq.c.
 * 10 Nov 95 lec 
 *   Updated some ints to long ints. Such as all YYYYJJJ
 *   dates.
 * 30 Nov 95 lec
 *   Updated s/w to use "cat", popen, and pclose for UNIX
 *   environments to prevent having to uncompress/compress
 *   sigma files each time. Original s/w that does
 *   compression can be turned on by setting UNIX_ENV to
 *   0 in local.h.
 * 16/29 May 96 lec
 *   Added s/w to ensure that if only a single qcf record
 *   occurs on the last time of an input file, that one
 *   record will be written to the output qcf file.
 * 13 Jun 96/6 Aug 96 lec
 *   Added record_count and ALWAYS_PRINT variables to 
 *   show progress as files are HQC'd. Fixed record_count
 *   in August.
 * 04 Mar 97 lec
 *   More general cleanup. C++ compiler (CC) used to 
 *   id other warnings about s/w. Added <string.h>, Removed
 *   i,j,k from main program - not used. Added include to
 *   dist.h. Removed last \0 from some initializers. 
 *   Literature indicates that this will happen automatically
 *   and C++ doesn't like this...so remove. Note that the
 *   current C++ compiler has not yet implemented and so
 *   does not allow the initialization of variables at
 *   the time they are defined. This code should be
 *   compiled under a regular C compiler.
 * 12 Feb 98 lec
 *   Updated Usage above and added comments on extraneaous
 *   errors, etc. Recompiled without errors.
 * 04 Mar 98 lec
 *   Correct word "parameter" spelling in searching message.
 * 12 Jan/11 Feb 99 lec
 *   Added s/w to skip Mobile stations. Currently, all mobile
 *   stations are either in the "DATSAV2M" or "GLERLM" networks.
 *   Networks containing mobile stations in the future must
 *   be added to the s/w below. Note that during the data
 *   conversion process, an "M" is added to the end of
 *   the network name. All other non-mobile stations in
 *   that same network would not have "M" added.
 * 11/12 Mar 99 lec
 *   Cleaned up first section that converts dates to 
 *   YYYYJJJ and retested.
 * 5 May 99 lec
 *   Updated s/w checks that uses CSLP or SLP QC flag to
 *   override station pressure QC flag. Now, DATSAV2(M) 
 *   stn pressure is calculated and can have a QC flag of
 *   I which should never be overridden.
 * 19 August 2002 lec
 *   Update to handle 4 digit year. Update calls to
 *   locate_sigmasq() to now include proj_begin_date.
 * Sept 2002 lec
 *   Consider updating s/w in the following areas:
 *     Winter precip, searching algorithm, doing multi-
 *     pass processing where values determined to be Bad
 *     are dropped and not allowed to affect subsequent
 *     HQC passes. Handle previously set Estimated values
 *     such as T, Td, Wdir, etc.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "local.h"
#include "process_qcfrec_4DYR.h"
#include "qcfrec.h"
#include "gQC_4DYR.h"
#include "locate_sigmasq_4DYR.h"
#include "dist.h"
#include "date_4DYR.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 * NOTE: Some #if statements have been turned on
 *       permanently by setting #if ALWAYS_PRINT.
 *
 * Set QC_STNPRS to 1 to allow quality control processing
 * to occur for station pressure. Typically this is not
 * done because we use the Calculated Sea Level or Sea 
 * Level pressure QC flags to set the station pressure
 * QC flag. The user may still set this flag to 1 to
 * allow Horizontal QC to be applied to station pressure.
 * Note that the logical check that propagates the sea
 * level QC flags will override the station pressure
 * QC flag set by HQC.
 *-------------------------------------------------------*/
#define  ALWAYS_PRINT   1
#define  DEBUG     0
#define  DEBUG2    0
#define  QC_STNPRS 0 

/* local functions */
#ifdef __STDC__
   int main (int argc, char *argv[]);
#else /*!__STDC__*/
   int main ();
#endif /*__STDC__*/


/*---------------------------------------------------------
 * main() - controls Horizontal Quality Control processing flow.
 *
 * 10 Dec 94 lec
 *   Created.
 * 04 Mar 97 lec
 *   Updated call to main to include arg defs inline.
 *--------------------------------------------------------*/

int main( int argc, char *argv[])
   {
   /* local variables */
   char         init_input_file_name[NAMELEN_MAX] = "\0";
   char         input_file_name[NAMELEN_MAX] = "\0";
   char         sigma_output_file_name[NAMELEN_MAX] = "\0";
   char         qcf_output_file_name[NAMELEN_MAX] = "\0";

   FILE         *init_input_stream;
   FILE         *input_stream;
   FILE         *sigma_output_stream;
   FILE         *qcf_output_stream;

   char         pathname[NAMELEN_MAX];
   char         sigpathname[NAMELEN_MAX];

   char         compression_cmd [NAMELEN_MAX+10]= "\0";
   char         junk_char = '\0';
   char         current_stn[27]= "\0";

   QCFREC       *qcfptr;
   QCFREC       *current_qcfptr;

   QCFREC       *qcfptr_array[MAXNUMSTNS];

   long int     ii, mm, zz, yyy = 0; 
   long int     record_count = 0;

   int          jj,kk,z,yy,xx,xxy = 0;
   char         proj_name[26] = "\0\0\0\0\0\0\0\0\0\0";
   int          data_freq   = 0;
   STRING27     stn_list[MAXNUMSTNS];
   long         stn_no      = -1;
   long         numstns     = 0;
   long         numstns_inp = 0;
   long         numsearches = 0;   /* number of searches of sigmasq vals performed. */
   long         index       = 0;
   long         print_order[MAXNUMSTNS];

   int          pmethod;    /* input value - determine weight fn */
   float        min_weight; /* input value - determines area of influence around each stn */

   float        bmax;       /* input value - 'Bad' Gross limit for precip QC. */
   float        dmin;       /* input value - 'Dubious' Gross limit for precip QC. */

   float        alpha0, alpha1;

   float        alpha_sq[NUMQCPARMS][2] = { {0.0,0.0},{0.0,0.0},    /* input values - G,D,B limits */
                                            {0.0,0.0},{0.0,0.0},
                                            {0.0,0.0},{0.0,0.0},
                                            {0.0,0.0} };
   float        A [MAXNUMSTNS][4];
 
   float        theta_obs;                        /* observations begin QC'd */
   float        theta_o[NUMQCPARMS][MAXNUMSTNS];  /* observations at stns in array A[][] */
   int          numtheta_o[NUMQCPARMS];           /* number of theta_o's in theta_o array */
   char         qcflag;                           /* Val returned by determine_qcflag() */
 
   double       x,y = 0.0;

   int          saved_hour_nom = 0;    /* for check of last rec in file. */
   int          saved_minute_nom = 0;

   int          current_yr   = 0; /* YYYY */
   int          current_hr   = 0;
   int          current_min  = 0;
   long int     current_date = 0; /* YYYYJJJ */

   char         date[9]      = "\0\0\0\0\0\0\0\0\0"; /* YYYYMMDD */
 
   char         proj_begin_date[9] ="\0\0\0\0\0\0\0\0\0"; /* Date project began. This is the date
                                                         Compute_sigmasq used to calc variances.
                                                         From "proj_begin_date" to "project_begin_date+
                                                         SIGMA_PERIOD+1", the set of sigma files named
                                                         and dated "project_begin_date+SIGMA_PERIOD"
                                                         are used. After that date each day has
                                                         recomputed variances. YYYYMMDD - input value */

   char         proj_end_date[9] ="\0\0\0\0\0\0\0\0\0";   /* Date that project ends. YYYYMMDD - input value */
   char         begin_date[9] ="\0\0\0\0\0\0\0\0\0"; /* Input date to begin (QC) processing. YYYYMMDD - input*/
   char         end_date[9]   ="\0\0\0\0\0\0\0\0\0"; /* Input date to end processing. YYYYMMDD - input value */
 

   /*
    * Following Variables contain dates of the form:
    * YYYYJJJ where YYYY is the century and yr (e.g.
    * 2001) and JJJ is the julian day (e.g., 236 for
    * 24 August 2001). Dates in this form are easily
    * incremented, compared, subracted from, etc.
    */
   long int proj_begin_YYYYJJJ = 0;
   long int proj_end_YYYYJJJ = 0;
   long int project_length = 0;
   long int processing_length = 0;
   long int begin_YYYYJJJ = 0;
   long int end_YYYYJJJ  = 0;
   long int end_sigma_period_date = 0;

   /*
    * Vars used to determine if data rollsover into
    * next year.
    */
   char         end_YYYY_str[5]   = "\0\0\0\0\0";
   int          end_YYYY   = 0;
   char         end_MMDD[5]   = "\0\0\0\0\0";
   char         begin_YYYY_str[5] = "\0\0\0\0\0";
   int          begin_YYYY = 0;
   char         begin_MMDD[5] = "\0\0\0\0";

   int          proj_begin_YYYY = 0;
   char         proj_begin_YYYY_str[5] = "\0\0\0\0\0";
   int          proj_end_YYYY = 0;
   char         proj_end_YYYY_str[5] = "\0\0\0\0\0";
 
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



   /*--------------------------------------------
    * Expect input file name on the command line.
    *--------------------------------------------*/
   if (argc != 2)
      {  
      printf ("Usage: main_qc_4DYR <initialization_input_file>\n");
      exit(1);
      }  

   strcpy (init_input_file_name, argv[1]);

   /*----------------------------------------------------------
    * Initialize from input file. This should be the same
    * file that is used during compute_sigmasq processing.
    *----------------------------------------------------------*/
   printf ("\n---- Horizontal Quality Control (HQC) ----\n");
   printf ("Processing began on %-s %-s\n", __DATE__, __TIME__);
   printf ("Warning: Only DATSAV2M and GLERLM are recognized as Mobile stns.\n");
   printf ("Warning: Project_length must be *GREATER THAN* SIGMA_PERIOD for searching to occur.\n");
   printf ("QC Initialization file name (e.g., QCinit.inp): %-s\n", init_input_file_name);
   
   open_file (init_input_file_name, "r", FILE_NOT_COMPRESSED, &init_input_stream);

   printf ("\nPARAMETER INITIALIZATION:: \n\n");

   /*-----------------------------------------
    * Read data from input file (QCinit.inp).
    *----------------------------------------*/ 
   fgets(proj_name, 25, init_input_stream);
   proj_name[25] = '\0'; 
   STRIPLINE(init_input_stream);

   fscanf (init_input_stream, "%d", &pmethod);        STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%f", &min_weight);     STRIPLINE(init_input_stream);
   min_weight = 1.0/min_weight;         /* invert for HQC processing */
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
   fscanf (init_input_stream, "%s", sigpathname);     STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%f", &bmax);           STRIPLINE(init_input_stream);
   fscanf (init_input_stream, "%f", &dmin);           STRIPLINE(init_input_stream);


   printf ("Project Name (proj_name): %-s\n", proj_name);
   printf ("Distance fn method (pmethod): %d\n", pmethod);
   printf ("Area of Influence in km (1/min_weight): %8.3f\n", 1.0/min_weight); /* print km */
   printf ("Data Frequency (data_freq): %d\n", data_freq);

   printf ("Current year of data: %d\n", current_yr); /* Always set to same as Project Begin date year. */
   printf ("First day of Project (proj_begin_date): %-s\n", proj_begin_date);
   printf ("Last day of Project  (proj_end_date): %-s\n", proj_end_date);
   printf ("First day to QC (begin_date) : %-s\n", begin_date);
   printf ("Last day to QC (end_date)    : %-s\n", end_date);
   printf ("Input file pathname: %-s\n", pathname);
   printf ("Sigma file pathname: %-s\n\n", sigpathname);
   printf ("Precipitation Gross Limits (Unlikely, Dubious): %f %f\n\n", bmax, dmin);
   printf ("Alpha_sq Indices: 0=stn pres, 1=slp, 2=cslp, 3=temp, 4=dw pt, 5=wnd sp, 6=wnd dir\n");

   /*--------------------------------------------------------
    * Read default bad & dubious alpha limit values for each
    * parameter. Save the square of the alpha values. Squared
    * values are required by Horizontal QC process done in
    * determine_qcflag() function.
    *--------------------------------------------------------*/
   for (yy=0;yy<NUMQCPARMS;yy++)
      {
      fscanf (init_input_stream, "%f %f", &alpha0, &alpha1); 
      STRIPLINE(init_input_stream);
      alpha_sq[yy][0] = alpha0*alpha0;
      alpha_sq[yy][1] = alpha1*alpha1;

      printf ("alpha_sq[%2d][0] = %4.2f alpha_sq[%2d][1] = %4.2f\n",
               yy, alpha_sq[yy][0], yy, alpha_sq[yy][1]);
      }

   /*------------------------------------
    * Close QC input initialization file.
    *------------------------------------*/
   close_file (&init_input_stream, FILE_NOT_COMPRESSED);

   /*---------------------------------------------
    * Begin Horizontal Quality Control processing.
    *--------------------------------------------*/
   /*---------------------------------------------
    * Construct qcfrec pointers then read the data.
    *---------------------------------------------*/
   construct_qcfptr (&qcfptr);
   construct_qcfptr (&current_qcfptr);
 
   for (ii=0;ii<MAXNUMSTNS;ii++)
      {
      construct_qcfptr ( &qcfptr_array[ii]);
      reset_qcfrec( qcfptr_array[ii] );
      print_order[ii] = 0;
      }

   /*-------------------------------------------------------------
    * Convert input dates to Julian and to YYYYJJJ form for easier
    * manipulation of dates. Assume that the current_yr is ALWAYS
    * the current century and year (YYYY) of the proj_begin_date.
    *
    * Note that the begin_date and end_date of processing and
    * the proj_end_date could occur in the next year. And that
    * they year and century could change during the project &/or
    * processing time. But this s/w assumes that we will never 
    * be processing more than one years worth of data. That is,
    * that the year &/or century can only change once and dates
    * are calculated once here at the beginning of the s/w.
    *
    * The above may not be a problem anymore since we are now
    * putting in the full 4 digit year in the input.
    *------------------------------------------------------------*/
   /*------------------------------------------------------------
    * Pick out the Year the PROJECT begins. User provided century.
    *------------------------------------------------------------*/
   strncpy (proj_begin_YYYY_str, &proj_begin_date[0], 4);
   proj_begin_YYYY_str[4]  = '\0';
   proj_begin_YYYY = atoi (proj_begin_YYYY_str);

   date_to_YYYYJJJ (proj_begin_date, proj_begin_YYYY, &proj_begin_YYYYJJJ); /* given by user */

   /*------------------------------------
    * Pick out the Year the PROJECT ends. 
    * May roll to next yr/century.
    *-----------------------------------*/
   strncpy (proj_end_YYYY_str, &proj_end_date[0], 4);
   proj_end_YYYY_str[4]  = '\0';
   proj_end_YYYY = atoi (proj_end_YYYY_str);

#if  ALWAYS_PRINT
   printf ("\n(1)proj_end_YYYY, proj_end_YYYY_str, proj_end_date: %ld %-s %-s\n",
                 proj_end_YYYY, proj_end_YYYY_str, proj_end_date);
#endif  


   /*---------------------------------------------------------------
    * Pick out the Year the PROCESSING begins. User provided century. 
    * Parse the month/day too. Then below compute YYYYJJJ date.
    *---------------------------------------------------------------*/
   strncpy (begin_YYYY_str, &begin_date[0], 4);
   begin_YYYY_str[4]  = '\0';
   begin_YYYY = atoi (begin_YYYY_str);

   strncpy (begin_MMDD, &begin_date[4],4);
   begin_MMDD[4]   = '\0';


   /*-----------------------------------------------------------
    * We assume that we will not process more than one year's
    * worth of data, so the dates could only rollover at most
    * one year. So the years must either be equal OR one yr off.
    * If not equal assume rollover into next year.
    *
    * Note: Now with 4 digit years and since we extract the year
    *   from the begin and end dates, the above assumption may
    *   not be a restriction.
    *
    * Compute YYYYJJJ date for PROCESSING begin date.
    *-----------------------------------------------------------*/
   date_to_YYYYJJJ (begin_date, begin_YYYY, &begin_YYYYJJJ);
     

   /*-----------------------------------------------------
    * Pick out the Processing end yr, month/day. Warn user
    * if end date is earlier than begin date.
    *----------------------------------------------------*/
   strncpy (end_YYYY_str, &end_date[0], 4);
   strncpy (end_MMDD,   &end_date[4], 4);
   end_YYYY_str[4]  = '\0';
   end_MMDD[4]    = '\0';
   end_YYYY = atoi (end_YYYY_str);

#if  ALWAYS_PRINT
   printf ("\n(2)end_YYYY, end_YYYY_str, end_date: %d %-s %-s\n"
,
             end_YYYY, end_YYYY_str, end_date);
#endif

   if (end_YYYY == begin_YYYY && atoi(end_MMDD) < atoi(begin_MMDD))
      {  
      printf ("WARNING: User specified end date is less than the begin date!\n");
      exit(1);
      }  

 
   /*---------------------------------------------------------
    * Determine the century, year, and YYYYJJJ dates for
    * the END of the Project and the END of the Processing.
    *
    * Properly set the century and year even if data rollsover
    * into next year. Assume rolls to next yr if no match.
    * Originally did following check:
    *   if (end_YYYY > begin_YYYY || atoi(end_MMDD) < atoi(begin_MMDD))
    *
    * But since we assume that we can only roll over one yr
    * to the next, think not equal check is sufficient. Also,
    * assume that the Processing end date can not be later
    * than the Project end date, so if the Processing end 
    * date rolled into the next yr then so did the Project
    * end date. However, the Processing end date might be
    * in same year, but the Project end date could be in next yr.
    *
    * Now that we have 4 digit year passed in, the above assumption
    * may no longer be true. We extract the year from the input
    * value and it will always be correct.
    *--------------------------------------------------------*/
    if (end_YYYY > proj_begin_YYYY)
      {  
      /*------------------------------------
       * Data rolls from one year into next.
       *------------------------------------*/
#if DEBUG 
      printf ("--------End rolls to next year!!! end_YYYY, begin_YYYY: %d %d\n", end_YYYY, begin_YYYY);
#endif
      }  
   else
      {  
      /*----------------------------------
       * Data is contained in single year.
       *----------------------------------*/
#if DEBUG
      printf ("--------Data all in same year: end_YYYY, begin_YYYY: %d %d\n",  end_YYYY, begin_YYYY);
#endif
      }  

   date_to_YYYYJJJ (end_date, end_YYYY, &end_YYYYJJJ);

    /*----------------------------------------------
     * Determine if Project ends in next year.
     * Correctly compute YYYYJJJ for end of Project.
     *---------------------------------------------*/
    if (proj_end_YYYY > proj_begin_YYYY)
      {
      /*---------------------------------------
       * Project rolls from one year into next.
       *---------------------------------------*/
#if DEBUG 
      printf ("-PROJECT rolls to next year: proj_end_YYYY, proj_begin_YYYY: %d %d\n",  proj_end_YYYY, proj_begin_YYYY);
#endif
      }
    else
      {
      /*------------------------------------
       * Project is contained in single year.
       *------------------------------------*/
#if DEBUG 
      printf ("-PROJECT all in same year: proj_end_YYYY, proj_begin_YYYY: %d %d\n",  proj_end_YYYY, proj_begin_YYYY);
#endif
      }  

/*HERE*/

   date_to_YYYYJJJ (proj_end_date, proj_end_YYYY, &proj_end_YYYYJJJ);

#if  ALWAYS_PRINT
   printf ("\nproj_end_date, proj_end_YYYY, end_YYYY, proj_end_YYYYJJJ: %-s %d %d %ld\n",
             proj_end_date, proj_end_YYYY, end_YYYY, proj_end_YYYYJJJ);
#endif

   /*--------------------------------------------------------------
    * Compute length of project in days. Compute processing length.
    * Basically: project_length = end_YYYYJJJ - begin_YYYYJJJ;
    *--------------------------------------------------------------*/
   processing_length = subtract_YYYYJJJ_from_YYYYJJJ(begin_YYYYJJJ, end_YYYYJJJ);
   project_length = subtract_YYYYJJJ_from_YYYYJJJ(proj_begin_YYYYJJJ, proj_end_YYYYJJJ);


   /*------------------------------------------------------------------
    * When sigmasq (variance) values are computed (by another program),
    * the first "sigma period" number of days are used to compute the
    * first set of sigmasq (variance) files. When HQC'ing any day within
    * that first "sigma period" number of days, this HQC s/w must use
    * the single set of variance files computed for that time period.
    * See compute_sigmasq.c for more info.  "end_sigma_period_date"
    * is the last date in that first of files that must use the single
    * set of variance files. the SIGMA_PERIOD value is set by the
    * user but is usually set to 30 days. The end period date is always
    * an offset from the project begin date, not the processing begin date.
    *------------------------------------------------------------------*/
   end_sigma_period_date = add_num_to_YYYYJJJ (proj_begin_YYYYJJJ, SIGMA_PERIOD);



#if  ALWAYS_PRINT
   printf ("\nproj_begin_date, proj_end_date, begin_date, end_date: %-s %-s %-s %-s\n",
            proj_begin_date, proj_end_date, begin_date, end_date);

   printf ("proj_begin_YYYYJJJ, proj_end_YYYYJJJ, begin_YYYYJJJ, end_YYYYJJJ, end_sigma_period_date, project_length, processing_length: \n      %ld       %ld       %ld     %ld     %ld     %ld   %ld\n",
           proj_begin_YYYYJJJ, proj_end_YYYYJJJ, begin_YYYYJJJ, end_YYYYJJJ, 
           end_sigma_period_date, project_length, processing_length);
#endif



      /*-----------------------------------------------------------------
       * Do Quality Control processing on all requested data. Read proper
       * sigma file (according to date and time of data being QC'd) and
       * stn_list info.
       *----------------------------------------------------------------*/
      for(current_date=begin_YYYYJJJ; current_date<=end_YYYYJJJ; increment_YYYYJJJ(&current_date))
         {
         /*-----------------------------------------------------------------
          * Form name of input file and open. Assume name of data being QC'd
          * is of the form yyddmm.0qc and the QC'd file will be yyddmm.qcf.
          *----------------------------------------------------------------*/
         YYYYJJJ_to_date (current_date, date);
#if ALWAYS_PRINT
         printf ("\nCurrent julian date is: %ld %-s\n", current_date, date);
#endif
         sprintf (input_file_name, "%-s%-s.0qc\0", pathname, date);
         open_file (input_file_name, "r", FILE_NOT_COMPRESSED, &input_stream);

         sprintf (qcf_output_file_name, "%-s%-s.qcf\0", pathname, date);
         open_file (qcf_output_file_name, "w", FILE_NOT_COMPRESSED, &qcf_output_stream);

#if DEBUG
         printf ("Open input file: %-s\n", input_file_name);
         printf ("Open output file: %-s\n", qcf_output_file_name);
#endif
#if ALWAYS_PRINT
         printf ("Processing file: %-s\n", input_file_name); /* Always print */
#endif
         numstns = 0;

         reset_qcfrec( qcfptr );
         read_qcfrec (&input_stream, qcfptr);
         record_count = 1;

         /*--------------------------------------------------
          * We assume that the input (*.0qc) files are sorted
          * by date/time.
          *-------------------------------------------------*/
         while (!feof(input_stream))
            {
            current_hr = qcfptr->hour_nom;
            current_min = qcfptr->minute_nom;
#if DEBUG
            printf ("------GET NEXT TIME: current_hr, current_min: %d %d\n", 
                    current_hr, current_min);
#endif
            /*---------------------------------------------------
             * Open, read the sigmasq (variance) file which
             * contains the variances for all stns at time
             * t. For the first SIGMA_PERIOD+1 days use the
             * sigmasq's computed during the first SIGMA_PERIOD.
             *---------------------------------------------------*/
#if DEBUG
            printf ("current_date, end_sigma_period_date: %ld %ld\n",
                     current_date, end_sigma_period_date);
#endif
            if (current_date <= end_sigma_period_date)
               {
               sprintf (sigma_output_file_name, "%-s%07ld%02d%02d.sig",
                        sigpathname, end_sigma_period_date,
                        current_hr, current_min);
               }
            else
               {
               sprintf (sigma_output_file_name, "%-s%07ld%02d%02d.sig",
                        sigpathname, current_date,
                        current_hr, current_min);
               }

            /*--------------------------------------- 
             * In UNIX environment, don't uncompress 
             * (gunzip) the file. Just use a pipe to 
             * access the data in the file. In all 
             * other environments, uncompress and use 
             * fopen() as usual. 
             *---------------------------------------*/ 
#if UNIX_ENV
            open_file (sigma_output_file_name, "r", FILE_COMPRESSED,
                       &sigma_output_stream);
#else
            sprintf (compression_cmd, "gunzip %-s.gz", sigma_output_file_name);
            system (compression_cmd);
            open_file (sigma_output_file_name, "r", FILE_NOT_COMPRESSED, 
                       &sigma_output_stream);
#endif

            fscanf (sigma_output_stream, "%ld\n", &numstns_inp);

#if DEBUG
            printf ("open_file: %-sxxx\n", sigma_output_file_name);
            printf ("excuting cmd:%-sxxx\n", compression_cmd);
            printf ("numstns_inp: %ld\n", numstns_inp);
#endif

            for (ii=0;ii<numstns_inp;ii++)
               {
               fscanf (sigma_output_stream, "%d%1c", &jj, &junk_char);
               if (feof(sigma_output_stream))
                  {
                  printf ("WARNING(1): hit premature End Of File on %-s.\n",
                          sigma_output_file_name);
                  printf ("WARNING(1): Some additional searching will be required for sigmas.\n");
                  }

               fgets (stn_list[jj], 27, sigma_output_stream);
#if DEBUG2
               printf ("stn_list[%d] = xxx%-sxxx\n", jj, stn_list[jj]);
#endif
               }

            /*--------------------------------------------------
             * Initialize all sigma_sq values to missing -999.99)
             * even beyond what will be read.
             *--------------------------------------------------*/
            for (ii=0;ii<MAXNUMSTNS;ii++)
               for (kk=0;kk<=NUMQCPARMS;kk++) sigma_sq[ii][kk] = -999.990000;


            STRIPLINE (sigma_output_stream); /* Skip the comment line. */
            STRIPLINE (sigma_output_stream); /* Skip the comment line. */

            ii = 0;
            kk = 0;
            while (!feof(sigma_output_stream))
               {
               fscanf (sigma_output_stream, "%ld%d", &ii, &kk);
               if (feof(sigma_output_stream))
                  {
#if DEBUG
                  if (ii < numstns_inp-1)
                     {
                     printf ("WARNING(2): hit premature End Of File on %-s.\n", 
                             sigma_output_file_name);
                     printf ("WARNING(2): Some additional searching will be required for sigmas.\n");
                     }
                  else
#endif
                     break;
                  }

               fscanf (sigma_output_stream, "%f", &sigma_sq[ii][kk]);
               if (feof(sigma_output_stream)) break;
#if DEBUG2
               printf ("(read)sigma_sq[%ld][%d] = %f\n", ii, kk, sigma_sq[ii][kk]);
#endif
               } /* data in file */


            /*---------------------------------------
             * In UNIX environment, have used a
             * pipe open to access compressed
             * file without using time to uncompress.
             * Close_file() will then close pipe.
             * If file is not compressed, close_file()
             * close normally with fclose command.
             *----------------------------------------*/
#if UNIX_ENV 
            close_file (&sigma_output_stream, FILE_COMPRESSED);
#else
            close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
            sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
            system (compression_cmd);
#endif

            numstns = 0;

            for (z=0;z<NUMQCPARMS;z++)
               numtheta_o[z] = 0;

            for (xx=0;xx<MAXNUMSTNS;xx++)
               {
               A[xx][0] = -999.99; /*latitude*/
               A[xx][1] = -999.99; /*longitude*/
               A[xx][2] = 0.0;     /*weight - generally a function of distance.*/
               A[xx][3] = 0.0;     /*distance*/
               }

            for (xx=0;xx<NUMQCPARMS;xx++)
               for (yyy=0;yyy<MAXNUMSTNS;yyy++)
                  theta_o[xx][yyy] = -999.99;    /* reinit to missing! Prevents adding
                                                    in bad values when stn in area of 
                                                    influence, yet data value missing, 
                                                    0.00,etc.*/


            while ( (qcfptr->hour_nom == current_hr) && (qcfptr->minute_nom == current_min))
               {
               /*--------------------------------------------------------------
                * Determine internal stn number from stn_list. Note that at 
                * this point numstns should not be changed by the following fn.
                * The sigma file should contain all stns encountered at this
                * time. All stations should already be identified.
                *--------------------------------------------------------------*/
               sprintf (current_stn, "%-10s:%-15s\0",qcfptr->qnet,qcfptr->statn);
               stn_no = determine_stn_no(stn_list, &numstns_inp, current_stn);

               if (stn_no > MAXNUMSTNS)
                  {
                  printf ("WARNING: stn number exceeds MAXNUMSTNS limit!");
                  exit(1);
                  }
            
               /*------------------------------------------------------
                * Retain knowledge of the input order, so can print out
                * in same order...not in stn_no order.
                *-----------------------------------------------------*/
               print_order[numstns] = stn_no;
#if DEBUG
               printf ("current_stn: xxx%-sxxx\n", current_stn);
               printf ("qcfptr->statn, stn_no: %-sxxx %d\n", qcfptr->statn, stn_no);
               printf ("qcfptr->lat, lon: %7.2f %7.2f\n", qcfptr->lat, qcfptr->lon);
#endif
               copy_qcfrec (qcfptr_array[stn_no],qcfptr); 
   
               /*-------------------------------------------------------
                * Fill up A's lat/lons and theta_o (Observation) values.
                * Keep track of how many Obs values stored for each
                * parameter in numtheta_o array.
                *------------------------------------------------------*/
               A[stn_no][0] = qcfptr->lat;
               A[stn_no][1] = qcfptr->lon;

#if QC_STNPRS
               /* -- Station Pressure. -- */
               if (qcfptr->staprs >0.00)
                  {
                  theta_o[stnprs][stn_no] = qcfptr->staprs;
                  numtheta_o[stnprs]++;
                  }
#endif 
               /* -- Sea Level Pressure. -- */
               if (qcfptr->seaprs >0.00)
                  {
                  theta_o[slp][stn_no] = qcfptr->seaprs;
                  numtheta_o[slp]++;
                  }

               /* -- Calculated Sea Level Pressure.  -- */
               if (qcfptr->cmpsea >0.00)
                  {
                  theta_o[cslp][stn_no] = qcfptr->cmpsea;
                  numtheta_o[cslp]++;
                  }
 
               /* -- Temperature  -- */
               if (qcfptr->temp >-999.00)
                  {
                  theta_o[temp][stn_no] = qcfptr->temp;
                  numtheta_o[temp]++;
                  }
 
               /* -- Dew Point Temperature  -- */
               if (qcfptr->dewpnt >-999.00)
                  {
                  theta_o[dewpt][stn_no] = qcfptr->dewpnt;
                  numtheta_o[dewpt]++;
                  }
 
               /* -- Wind Speed  -- */
               if (qcfptr->wndspd >-999.00)
                  {
                  theta_o[windsp][stn_no] = qcfptr->wndspd;
                  numtheta_o[windsp]++;
                  }
 
               /* -- Wind Direction -- */
               if (qcfptr->wnddir >-999.00)
                  {
                  theta_o[winddir][stn_no] = qcfptr->wnddir;
                  numtheta_o[winddir]++;
                  }

               numstns++;   /* Actually equals the number of recs at this time!!
                               NOTE: Stn nums are NOT necessarily in order.    */

               reset_qcfrec( qcfptr );
   
               if (feof(input_stream)) break;

               read_qcfrec (&input_stream, qcfptr);
               record_count++;

               /*
                * Output message to show progress.
                */
               if (record_count%10000 == 0)
                  printf ("Processed %10ld records.\n", record_count);
            
               } /* while qcfptr times equal current times.*/

            /*------------------------------------------------------
             * At this point have read in all data for current time.
             * Plus first rec of next set of times from input 0qc.
             *-----------------------------------------------------*/
#if DEBUG2
            printf ("Completed filling A now compute qcflag. numstns, numstns_inp: %ld %ld\n", 
                     numstns, numstns_inp);
#endif

            /*-------------------------------------------------------------------------
             * Compute distances and weights only once. They apply for all parameters 
             * at this time at a particular stn. Beware applying to more times, cause 
             * at different times may have different set of stns (or some stns may
             * drop out on some parameters).
             *
             * Distances vary according to location of reference stn. 
             * Loop through stations letting each in turn be the ref stn when
             * QCing that stns data.
             *
             * Do numstns_inp not numstns cause must check all possible stns to see if
             * data read. Using numstns may cause last statns to not get QC'd.
             *------------------------------------------------------------------------*/
            for (mm=0;mm<numstns_inp;mm++)
               {
               copy_qcfrec(current_qcfptr, qcfptr_array[mm]);
   
               /*----------------------------------------------------
                * Don't process blank stns. Not all possible stns may
                * occur at each time. Stations come and go in data.
                *---------------------------------------------------*/
               if (!strncmp(current_qcfptr->statn,"               \0",15)) continue;

               /*--------------------------------------------------------
                * Don't process Mobile stations. Mobile stations
                * should always have an 'M' at the end of the network
                * name. Currently, the only networks containing Mobile
                * stations are "DATSAV2M" and "GLERLM". Skip all stations
                * with these networks. Add all other Mobile network names
                * here.
                *--------------------------------------------------------*/
               if (!strncmp(current_qcfptr->qnet,"DATSAV2M\0",8)) continue;
               if (!strncmp(current_qcfptr->qnet,"GLERLM\0",6)) continue;

#if DEBUG
               if (!strncmp(current_qcfptr->qnet,"DATSAV2M\0",8) ||
			       !strncmp(current_qcfptr->qnet,"GLERLM\0",6)  )
                  {
                  printf ("\nFound Mobile Station: %-10s:%-15s\n", 
				          current_qcfptr->qnet, current_qcfptr->statn);
                  }
#endif


               sprintf (current_stn, "%-10s:%-15s\0",current_qcfptr->qnet,current_qcfptr->statn);

               stn_no = determine_stn_no(stn_list, &numstns_inp, current_stn);
#if DEBUG
               printf ("\nCompute dist and weights for Current stn, stn_no: %-s %d<---------------\n", 
                       current_stn, stn_no);
#endif

               /*------------------------------------------------------------
                *  Let current station be the reference station.
                *  Compute distances from ref station to all other 
                *  stations and put in the last/4th position of A or A[,3]. 
                *
                * NOTE: Because of the shape and curvature of the earth, the
                *  ll2xydrv() function will compute potentially different
                *  distances between two stations depending on which is allowed
                *  to be the reference station. It is important to let
                *  the current station be the reference station each
                *  time. Theoretically, when all stns on the surface
                *  are fixed, these computations could be performed 
                *  once and saved. However, since each distance between
                *  any two stations must be computed in both directions,
                *  the storage required to save distances for a large number
                *  of stations could be significant. Here we choose to 
                *  perform the computations each time to allow for maximum
                *  flexiblity in station movement, etc. 
                *
                *  Don't process against Missing (-999.99) stations.
                *  Must check numstns_inp here, too. OR Missing sigma_sq values.
                *-------------------------------------------------------------*/
               for (ii=0;ii<numstns_inp;ii++)
                  {
                  if (A[ii][0]>-990.00 && A[ii][1] >-990.00)
                     {
                     ll2xydrv( current_qcfptr->lat, 
                               current_qcfptr->lon, 
                               &x, &y, 
                               A[ii][0], A[ii][1]); 
                     A[ii][3]=sqrt(x*x+y*y);             /* distance */
#if DEBUG3was2
                     printf ("x, y, A[%ld][3]: %lf %lf %f\n", ii, x, y, A[ii][3]); 
#endif
                     }
#if DEBUG3was2
                  else
                     printf ("Station (%ld) is missing. no lat/lon pair = no distance\n",ii );
#endif
                  }
  
               /*---------------------------------------------------
                * Determine "weights" (influence) of other stations.
                * The user selects the method (eqn) used to determine
                * the weight of each station by setting the pmethod
                * input value in the input file.
                *
                * (Ensure dist/weight is 0.0 if lat/lon = -999.99)
                *--------------------------------------------------*/
               weight(A, numstns_inp, pmethod);

               /*-----------------------------------------------------
                * Determine QC flag for all parameters at current stn.
                * Allow s/w to not QC station pressure since this vars
                * QC flag is reset by either Calc Sea Lvl Press or Sea
                * Lvl Press's QC flag.
                *
                * jj - Type of data begin processed. (0=Stn Press,
                *      1=Sea Lvl Press, 2=Calc Sea Lvl Press, 3=Temp,
                *      4=Dew Point, 5=Wind Speed, 6=Wind Direction.)
                *----------------------------------------------------*/
               for (jj=0;jj<NUMQCPARMS;jj++)
                  {
#if !QC_STNPRS
/*                if (jj == 0)
                     printf ("Currently NOT QCing station pressure data, only.\n"); */

                  if (jj != 0) {  /* Don't process stn pressure data, unless requested. */
#endif
                  /*-----------------------------------------------
                   * Pick out current observation to QC (theta_obs)
                   * QC flag (qcflag) to pass to fns.
                   *-----------------------------------------------*/
                  switch (jj) {
                    case 0:   /*Station Pressure*/
                    theta_obs = current_qcfptr->staprs;
                    qcflag = current_qcfptr->staflg;
                    break;
                  case 1:  /*Sea Level Pressure*/
                    theta_obs = current_qcfptr->seaprs;
                    qcflag = current_qcfptr->seaflg;
                    break;
                  case 2:  /*Computed Sea Level Pressure*/
                    theta_obs = current_qcfptr->cmpsea;
                    qcflag = current_qcfptr->cmpflg;
                    break;
                  case 3:  /*Temperature*/
                    theta_obs = current_qcfptr->temp;
                    qcflag = current_qcfptr->tmpflg;
                    break;
                  case 4:  /*Dew Point*/
                    theta_obs = current_qcfptr->dewpnt;
                    qcflag = current_qcfptr->dewflg;
                    break;
                  case 5:  /*Wind Speed*/
                    theta_obs = current_qcfptr->wndspd;
                    qcflag = current_qcfptr->spdflg;
                    break;
                  case 6:  /*Wind Direction*/
                    theta_obs = current_qcfptr->wnddir;
                    qcflag = current_qcfptr->dirflg;
                    break;
                  default:
                    printf ("Error: Unknown parameter");
                  } /* switch */

                  /*----------------------------------------------------
                   * Apply Horizontal Quality Control to each parameter.
                   *
                   * The user controls the limits which determine whether
                   * a value is Good (G), Dubious (D), or Unlikely (B)
                   * by setting the alpha input values read from the
                   * specified input file. Remember, the user specifies
                   * the alpha values which are then squared and passed
                   * on to the following function.
                   *
                   * If parameter being QC is not missing, but the sigma_sq
                   * value is missing, s/w must search ahead in time and
                   * locate the "best" sigma_sq value. To find the "best"
                   * (next, non-missing) sigma_sq value, search ahead by
                   * days considering only sigma's for current stn and
                   * current time. Never search farther than SIGMA_PERIOD
                   * days. Beyond SIGMA_PERIOD days may cross seasons
                   * and sigma_sq values may not be valid. It is possible
                   * that a preprocessor could do the searching work prior
                   * to running HQC (main_qc_4DYR), but this might cause lots
                   * unnecessary work to be done.
                   *
                   * NOTE: only need to locate a sigma_sq if we are going 
                   *   to QC that parameter. Can't QC missing values. 
                   *   Also must have more than 1 value of this parameter
                   *   type at this time (considering all stns). 
                   *
                   * AND finally, there's no need to search ahead if the
                   * project is only SIGMA_PERIOD days long. In this case
                   * only one set of sigmasq (variance) files exist and
                   * these files are being used to HQC the whole period.
                   * There are no other files to search!
                   *---------------------------------------------------*/
                  if ((theta_obs > -990.00) && (numtheta_o[jj] >1) &&
                      qcflag != 'M' && qcflag != 'N' && qcflag != 'X' &&
                      qcflag != 'C' && qcflag != 'I'  )
                     {
#if DEBUG
                     printf ("Check project_length >? SIGMA_PERIOD: %ld %d\n",
                             project_length, SIGMA_PERIOD);
#endif
                     if ((sigma_sq[stn_no][jj] < -990.00) && 
                         (project_length > SIGMA_PERIOD))
                        {
                        numsearches++; /* for stats only */
/* #if DEBUG */
#if ALWAYS_PRINT
                        printf ("SEARCHING (%d) for sigmasq PAST file %-s at hr,min: %d %d \n",
                                numsearches, sigma_output_file_name, current_hr, current_min);
                        printf ("for parameter, stn_no, stn: %d %d %-s\n",         
                                jj, stn_no, current_stn);
#endif
#if DEBUG
                        printf ("\nSEARCHING for sigma_sq value. Current_hr (%d) Current_min (%d)\n",
                                current_hr, current_min);
                        printf ("sigfilename, sigpath, proj_end_YYYYJJJ: xxx%-sxxx,\nxxx%-sxxx, %ld\n",
                                 sigma_output_file_name, sigpathname, proj_end_YYYYJJJ);
                        printf ("statn, stn_no, jj, sigma_sq[stn_no][jj]: xxx%-sxxx, %d %d %f\n",
                                 current_stn, stn_no, jj, sigma_sq[stn_no][jj]);
#endif
                        /*--------------------------------------
                         * Try to locate a non-missing sigma_sq.
                         * Return -888.88 if a valid sigma_sq
                         * can NOT be located. The value -888.88
                         * is recognized by other QC s/w to mean
                         * that a search was performed, but a
                         * valid sigma_sq value could not be found.
                         * So, don't attempt this search again.
                         *--------------------------------------*/
                        locate_sigmasq( sigma_output_file_name, sigpathname,
                                        proj_begin_YYYYJJJ,
                                        proj_end_YYYYJJJ, current_stn,
                                        jj, sigma_sq[stn_no]);
#if DEBUG
/* #if ALWAYS_PRINT  */
                        printf ("(AFTER SEARCHING)sigma_sq[%d][%d] = %f\n", 
                                 stn_no, jj, sigma_sq[stn_no][jj]);
                        for (xxy=0;xxy<NUMQCPARMS;xxy++)
                           printf ("(AFTER) sigma_sq[%d][%d] = %f for current_stn=%-s\n",
                                   stn_no, xxy,sigma_sq[stn_no][xxy],current_stn);
                        printf ("\n");
#endif

                        /*--------------------------------------------
                         * Update the current sigma_sq file so that
                         * we never have to search for this parameter's
                         * sigma_sq value again. Taking the time
                         * to rewrite this sigmasq file only 
                         * makes sense during the first SIGMA_PERIOD
                         * days, since we know that those files will
                         * be used SIGMA_PERIOD+1 times.  OR if we
                         * find that we rerun HQC several times on a
                         * dataset/composite, may want to just rewrite
                         * files regardless of day. For these initial
                         * runs/testing, rewrite files every time. Note
                         * how frequently this is done. To SAVE TIME
                         * only allow file rewrite if within SIGMA_PERIOD
                         * days.
                         *--------------------------------------------*/
                        rewrite_sigmasq_file( sigma_output_file_name,
                                              numstns_inp, stn_list, sigma_sq);
                        }

                     /*----------------------------------------
                      * Sigma_sq val of -999.99 indicates that
                      * the value is missing and a search for
                      * another valid sigma_sq has not been
                      * performed. If sigma_sq is -888.88, then
                      * a search was performed, but a valid
                      * sigma_sq value could not be located.
                      *---------------------------------------*/
                     if (sigma_sq[stn_no][jj] < -880.00)
                        {
#if DEBUG
                        printf ("Cant QC this parameter - couldn't locate sigma_sq[%d][%d]=%f\n",
                               stn_no, jj, sigma_sq[stn_no][jj]);
#endif
                        continue; /* to QC next parameter */
                        }
#if DEBUG
                     printf ("Call determine_qcflag. stn_no = %d, jj = %d\n", stn_no, jj);
                     printf ("numtheta_o[%d] = %d, sigma_sq[%d][%d] = %f\n",
                             jj, numtheta_o[jj], stn_no, jj, sigma_sq[stn_no][jj]);
#endif

                     determine_qcflag ( A,
                                        theta_o[jj],
                                        numstns_inp,
                                        sigma_sq[stn_no][jj],
                                        min_weight,
                                        alpha_sq,
                                        theta_obs,
                                        jj,
                                        &qcflag);

                    /*------------------------------------------------
                     * Assign Quality Control flag for this parameter.
                     *-----------------------------------------------*/
                    switch (jj) {
                       case 0:
                         current_qcfptr->staflg = qcflag;
                         break;
                       case 1:
                         current_qcfptr->seaflg = qcflag;
                         break;
                       case 2:
                         current_qcfptr->cmpflg = qcflag;
                         break;
                       case 3:
                         current_qcfptr->tmpflg = qcflag;
                         break;
                       case 4:
                         current_qcfptr->dewflg = qcflag;
                         break;
                       case 5:
                         current_qcfptr->spdflg = qcflag;
                         break;
                       case 6:
                         /*----------------------------------------------------
                          * Dubious is worse case allowed for wind direction
                          * when have light and variable winds. Only reset wind
                          * direction QC flag Bad/Unlikely flag.
                          *----------------------------------------------------*/
                         if (current_qcfptr->wndspd < 10.0 && qcflag =='B')
                            {
                            current_qcfptr->dirflg = 'D';
#if DEBUG2
                            printf ("Light and Var winds: Reset qcflag from B to D!\n");
#endif 
                            }
                         else
                            current_qcfptr->dirflg = qcflag;
                         break;
                       default:
                         printf ("Error: Attempt to set qcflag for unknown parameter");
                       } /* switch */

                     }
#if DEBUG2
                  else

                     printf ("numtheta_o[%d] = %d. sigma_sq[%d][%d]= %f. --NO QC FOR (1)-- stn=%-s\n",
                              jj, numtheta_o[jj], stn_no, jj, sigma_sq[stn_no][jj],
                              current_stn);
#endif

#if !QC_STNPRS
                  } /* QC station pressure only when requested. */
#endif
                  } /* for QC all parameters (NUMQCPARMS) (jj)*/

           
               /*--------------------------------------------------------------
                * Extraneous/logical QC checks.
                *--------------------------------------------------------------
                * Gross limit check the precip. Update the flags!
                * This limit check only works properly if data is all at
                * same frequency in the composite. VORTEX data was not this way!
                *
                * B overrides E, but D does NOT. Estimated values are
                * are only reset if found to be bad.
                *
                * Do not gross limit check the precip if the QC flag is any
                * of the following:
                *
                * Missing (M), Not Available (N), Glitch (X), or
                * Exceeds format size (C). Can't be an (I) since precip is
                * not a computed parameter.
                *
                * For some projects with WINTER time data, the precip flag
                * may have already been set to D or Dubious for precip gage
                * types which do not reliably record during freezing temps.
                * The winter period would have been identified by the 
                * scientific staff and the conversion s/w would only have
                * set the precip flag to D for those specific months. 
                * Do NOT reset that D flag to be anything better than D.
                * This is not yet implemtented in this HQC s/w. It is
                * reworked by some post processing s/w which runs after the
                * the HQC.
                *--------------------------------------------------------------*/
               if (current_qcfptr->prcflg != 'M' && current_qcfptr->prcflg != 'N' &&
                   current_qcfptr->prcflg != 'X' && current_qcfptr->prcflg != 'C')
                  {
                  if (current_qcfptr->precip < -990.00)
                     current_qcfptr->prcflg = 'M';
                  else if (current_qcfptr->precip >= bmax || current_qcfptr->precip< 0.0)
                     current_qcfptr->prcflg = 'B';
                  else if (current_qcfptr->precip >= dmin && current_qcfptr->prcflg != 'E')
                     current_qcfptr->prcflg = 'D';
                  else
                     if (current_qcfptr->prcflg != 'E') current_qcfptr->prcflg = 'G';
                  }

#if DEBUG2
               if (current_qcfptr->precip >0.0 && current_qcfptr->prcflg == 'G' &&
                   current_qcfptr->clamt1 == 0)
                  printf ("LOGICAL: Precip in clear air deteched. Set prcflg = D!!\n");
#endif

               /*-------------------------------------
                * Can't have precip in clear air.
                *------------------------------------*/
               if (current_qcfptr->precip >0.0 && current_qcfptr->prcflg == 'G' &&
                   current_qcfptr->clamt1 == 0)
                  current_qcfptr->prcflg = 'D';


               /*-----------------------------------------------------
                * If first cloud layer is present then next
                * levels should be Not available instead of Missing.
                * Reset ceiling height flags and cloud amount qc flags.
                *-----------------------------------------------------*/
               if (current_qcfptr->c1flg == 'U' || current_qcfptr->c1flg == 'E')
                  {
                  if (current_qcfptr->c2flg == 'M')current_qcfptr->c2flg = 'N';
                  if (current_qcfptr->c3flg == 'M')current_qcfptr->c3flg = 'N';
                  if (current_qcfptr->ca2flg == 'M')current_qcfptr->ca2flg = 'N';
                  if (current_qcfptr->ca3flg == 'M')current_qcfptr->ca3flg = 'N';
#if DEBUG2
                  printf ("LOGICAL: First cld layer present. set others to N not M!\n");
#endif
                  }

#if DEBUG2
               if (current_qcfptr->sqlflg == 'M' && current_qcfptr->spdflg != 'M')
                  printf ("LOGICAL: Squall/Gust winds should be N not M!!\n");
#endif

               /*----------------------------------------------
                *  Squall/Gust winds should be Not Available (N)
                *  instead of Missing unless winds are missing.
                *----------------------------------------------*/
               if (current_qcfptr->sqlflg == 'M' && current_qcfptr->spdflg != 'M')
                  current_qcfptr->sqlflg = 'N';


               /*--------------------------------------------------------------
                * Dew point should be less than or equal to temperature.
                * Else flag both as dubious. Don't change M,N,C,I,X, B,D flags.
                *--------------------------------------------------------------*/
               if ((current_qcfptr->dewpnt > current_qcfptr->temp) &&
                   (current_qcfptr->dewpnt > -999.00 && current_qcfptr->temp > -999.00))
                  {
                  if (current_qcfptr->tmpflg == 'G') current_qcfptr->tmpflg = 'D';
                  if (current_qcfptr->dewflg == 'G') current_qcfptr->dewflg = 'D';
#if DEBUG2
                  printf ("LOGICAL: Dew point is > temp...reset flags to at least D!!\n");
#endif
                  }

               /*---------------------------------------------
                * Wind speed must be >=0.0 and not missing.
                *--------------------------------------------*/
               if (current_qcfptr->wndspd <0.0 && current_qcfptr->wndspd > -999.00)
                  current_qcfptr->spdflg = 'B';
#if DEBUG2
               if (current_qcfptr->wndspd <0.0 && current_qcfptr->wndspd > -999.00)
                  printf ("LOGICAL: Wind speed must be >=0.0!!\n");
#endif

               /*-----------------------------------------------
                * Wind dir must be >0 && < 360 and not missing.
                *----------------------------------------------*/
               if ( current_qcfptr->wnddir > -999.00 && 
                    (current_qcfptr->wnddir <0.0 || current_qcfptr->wnddir >360.0))
                  current_qcfptr->dirflg = 'B'; 
#if DEBUG2
               if ( current_qcfptr->wnddir > -999.00 && 
                    (current_qcfptr->wnddir <0.0 || current_qcfptr->wnddir >360.0))
                  printf ("LOGICAL: Wind dir must be >0 or <360!!\n"); 
#endif 

               /*-----------------------------------------------------
                * If Wind speed is 0 (calm), set wind dir = 0.
                * AND set wind dir QC flag same as wind speed QC flag.
                *----------------------------------------------------*/
               if ( current_qcfptr->wndspd == 0.0)
                  {
                  current_qcfptr->wnddir = 0.0;
                  current_qcfptr->dirflg = current_qcfptr->spdflg;
#if DEBUG2
                  printf ("LOGICAL: Wind speed =0.0. Reset wind dir and qc flags!!\n"); 
#endif 
                  }

               /*-----------------------------------------------------------------
                * Use Calculated sea level pressure to flag the station
                * pressure and sea level pressures if they are present...
                * If CSLP is not avail for some reason but SLP is available,
                * use the SLP flag to set the Stn Pressure flag. If neither
                * CSLP or SLP are available, then leave Stn Pressure flag
                * as 'U' for unchecked. Do this setting of QC flags
                * as long as the CSLP (of SLP) flag != M,N,X,I,C or U. Note that
                * I for insufficient can only occur on computed values such as
                * CSLP or dew point. The DATSAV2(M) stn pressure is also computed.
                *
                * NOTE: There is no need to even QC Stn Pressures since they
                *   can not really be compared. The s/w above that does these
                *   Stn Pressure comparisons has been commented out only...not
                *   yet removed....just in case. Also, we still allow (in 
                *   compute_sigma.c) sigma values to be computed and printed
                *   for stn pressure even though they are not needed for HQC.
                *   This is because this whole process is still under
                *   development and should remain as flexible as possible.
                *-----------------------------------------------------------------*/
               if (current_qcfptr->cmpflg !='M' && current_qcfptr->cmpflg !='N' &&
                   current_qcfptr->cmpflg !='X' && current_qcfptr->cmpflg !='U' &&
                   current_qcfptr->cmpflg !='I' && current_qcfptr->cmpflg !='C')
                  {
                  if (current_qcfptr->seaflg !='M' && current_qcfptr->seaflg !='N' &&
                      current_qcfptr->seaflg !='X' && current_qcfptr->seaflg !='C')
                     current_qcfptr->seaflg = current_qcfptr->cmpflg;

                  if (current_qcfptr->staflg !='M' && current_qcfptr->staflg !='N' &&
                      current_qcfptr->staflg !='X' && current_qcfptr->staflg !='C' &&
					  current_qcfptr->staflg !='I') 
                     current_qcfptr->staflg = current_qcfptr->cmpflg;
#if DEBUG2
                  printf ("LOGICAL: Reset pressure QC flags using CSLP qcflag!!\n"); 
#endif 
                  }
               else if (current_qcfptr->seaflg !='M' && current_qcfptr->seaflg !='N' &&
                        current_qcfptr->seaflg !='X' && current_qcfptr->seaflg !='C' &&
                        current_qcfptr->seaflg !='U')
                  {
                  if (current_qcfptr->staflg !='M' && current_qcfptr->staflg !='N' &&
                      current_qcfptr->staflg !='X' && current_qcfptr->staflg !='C' &&
					  current_qcfptr->staflg !='I')
                     current_qcfptr->staflg = current_qcfptr->seaflg;
#if DEBUG2
                  printf ("LOGICAL: Reset pressure QC flags using Sea Lvl P qcflag!!\n");  
#endif  
                  }


               /*-------------------------------------------
                * Save QC'd rec in array for later printing.
                *------------------------------------------*/
               copy_qcfrec(qcfptr_array[mm], current_qcfptr);

               } /* for all stns at current hour and minute. (mm) */

            /*----------------------------------------------
             * Save last time written for test below.
             *---------------------------------------------*/
            saved_hour_nom =  qcfptr_array[print_order[0]]->hour_nom;
            saved_minute_nom =  qcfptr_array[print_order[0]]->minute_nom;
#if DEBUG
            printf ("Last printed time was time::  %d:%d\n", saved_hour_nom, saved_minute_nom);
#endif

            /*-----------------------------------------------
             * Write out chunk of QC'd qcfrecs for this time.
             * In the order read. Save first qcfptr time for
             * final time check of last rec below.
             *-----------------------------------------------*/
            for (zz=0;zz<numstns;zz++)    
               {
               index = print_order[zz];
               write_qcfrec (&qcf_output_stream, qcfptr_array[index]);
               reset_qcfrec (qcfptr_array[index]); /* blank out for next time */
               }

            } /* while data in input file  - Read next set of data for next time.*/

         /*-----------------------------------------------
          * It is possible that at the end of any file, a
          * single (last) record could occur at the last
          * time in the file. This record can not be HQC'd
          * since you must have more than one record at a
          * time, but it still needs to be written (un-QC'd)
          * to the qcf output file. Note that if more than
          * record occurs at any time, the data is written
          * to the output file. Compare times with data
          * just written to output file. Make sure there's
          * a station and not just a blanked out qcfptr
          * which would cause a "blanked" qcf rec written
          * to the end of each processed file.
          *-----------------------------------------------*/
#if DEBUG
         printf ("Compare times: %d:%d and (SAVED) %d:%d\n",
                 qcfptr->hour_nom, qcfptr->minute_nom, saved_hour_nom, saved_minute_nom);
#endif
         if ((qcfptr->hour_nom != saved_hour_nom ||
             qcfptr->minute_nom != saved_minute_nom) &&
             strncmp(qcfptr->statn,"               \0",16) )
            {
#if ALWAYS_PRINT 
            printf ("Found single record %-s %-s at last time in 0qc file. %d:%d compared to saved %d:%d\n",
                    qcfptr->qnet, qcfptr->statn, 
                    qcfptr->hour_nom, qcfptr->minute_nom,
                    saved_hour_nom, saved_minute_nom);
#endif
            write_qcfrec (&qcf_output_stream, qcfptr);
            }


#if ALWAYS_PRINT
         printf ("Completed processing %10ld records for date %ld. GO TO NEXT DATE.\n", 
                  record_count, current_date);
#endif
         close_file (&input_stream, FILE_NOT_COMPRESSED);
         close_file (&qcf_output_stream, FILE_NOT_COMPRESSED);

         } /* for current_date = begin to end */


   /*----------------------------------------------
    * Free all pointers created during processing.
    *---------------------------------------------*/
   destruct_qcfptr (&qcfptr);
   destruct_qcfptr (&current_qcfptr);

   for (ii=0;ii<MAXNUMSTNS;ii++)
      destruct_qcfptr (&qcfptr_array[ii]);

   printf ("\nHQC - main_qc_4DYR() completed with %ld searches for sigmasq values.\n",
          numsearches); /* Always print */
   printf ("NOTE: Each search may have required searching forward through up to %d files.(Not including the day being HQC'd)\n", SIGMA_PERIOD+1);
   printf ("\nProcessing completed on %-s %-s\n", __DATE__, __TIME__);
   }  /* main() */
