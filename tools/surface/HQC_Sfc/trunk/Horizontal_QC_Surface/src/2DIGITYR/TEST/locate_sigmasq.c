/*--------------------------------------------------------
 * >>>> V2.0 >>>> NEW VERSION of SEARCHING <<<<<<<<<<<<<<<
 *
 * locate_sigmasq - This module contains s/w that supports
 *   locating the (NEXT) best sigmasq value for the
 *   requested parameter and time.
 *
 * 10/15 Nov 95 lec
 *   Updated some ints to long ints. Cleanup
 * 15 Apr 96 lec
 *   Update searching s/w to always search the complete
 *   last SIGMA PERIOD (30) day period of the project when
 *   searching for a sigmasq within those last SIGMA PERIOD
 *   days. For example, if request received to search for
 *   a sigmasq for data from day Apr 29 (next to last day 
 *   of project), then updated s/w will search sigmasq
 *   files for days Apr 1 through Apr 30...not just
 *   Apr 29 and 30.
 * 04 Mar 97 lec
 *   Added include for string.h. Removed additional \0
 *   at end of char string initialzations.
 * 17 Feb 98 lec
 *   Added signal.h to "Broken Pipe" signal handling.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "local.h"
#include "locate_sigmasq.h"
#include "gQC.h"
#include "date.h"

/*-------------------------------------------------------
 * Set DEBUG to 1 for debug type output to screen
 * during any run. Set to 0 to prevent debug output.
 *-------------------------------------------------------*/
#define  DEBUG  0

/*---------------------------------------------------------
 * rewrite_sigmasq_file() - This fn rewrites an existing
 *   sigma_sq (variance) file with updated sigma_sq values.
 *   This fn prevents the HQC s/w from multiple searches
 *   (using locate_sigmasq()) for the same sigma sq value.
 *
 * 29 Sep 95 lec
 *   Created.
 * 01 Nov 95 lec
 *   Updated to use fseek to skip first part of sigmasq 
 *   files.
 *--------------------------------------------------------*/
void rewrite_sigmasq_file( /*in*/ char      sigma_output_file_name[NAMELEN_MAX],
                           /*in*/ long int  numstns_inp,
                           /*in*/ STRING27  stn_list[MAXNUMSTNS],
                           /*in*/ float     sigma_sq[MAXNUMSTNS][NUMQCPARMS])
   {
   /* local variables */
   FILE         *sigma_output_stream;

   long int     ii = 0;
   int          jj, kk = 0;
   long int     numstns_in_file = 0;

   char         copy_cmd[NAMELEN_MAX] = "\0";
   char         compression_cmd[NAMELEN_MAX] = "\0";
   long int     offset2, offset3 = 0;
 

#if DEBUG
   printf ("---Enter rewrite_sigmasq_file() ---\n");
   printf ("Rewrite file: xxx%-sxxx\n", sigma_output_file_name);
#endif

   /*------------------------------------------
    * At this point, sigmasq file needs to be
    * uncompressed. Make a copy before update
    * just for save keeping. The copy statement
    * can be deleted in the future. Don't use
    * pipe cmds (popen/pclose) to access these
    * sigmasq files, since we must write to the
    * files.
    *------------------------------------------*/
   sprintf (copy_cmd, "cp %-s.gz %-s.bak.gz\0",           /* Name does not include .gz */
            sigma_output_file_name, sigma_output_file_name);
#if DEBUG
   printf ("executing copy_cmd: %-sxxx\n", copy_cmd);
#endif
   system (copy_cmd);

   sprintf (compression_cmd, "gunzip %-s.gz\0",
            sigma_output_file_name);
#if DEBUG
   printf ("executing compression_cmd: %-sxxx\n", compression_cmd);
#endif
   system (compression_cmd);

   /*--------------------------------------------------
    * Read how many stns are currently listed in
    * this sigma file. If numstns_inp > numstns_in_file
    * then need to update the sig file with the
    * stns encountered on this day. This can occur
    * since we use the previous SIGMA_PERIOD days
    * to compute the next day's variances. If on
    * that next day new stns occur, the compute_
    * sigma program would not have know about them
    * and so would not have added them in to this
    * sigma file. This s/w needs to add them in
    * to prevent future searching.
    *-------------------------------------------------*/
   open_file (sigma_output_file_name, "r+", FILE_NOT_COMPRESSED,
              &sigma_output_stream); /* open for read/write */

   fscanf (sigma_output_stream, "%ld\n", &numstns_in_file); 
 
#if DEBUG    
   printf ("numstns_inp, numstns_in_file: %ld %ld\n", 
           numstns_inp, numstns_in_file);
#endif

   if (numstns_in_file < numstns_inp)
      {
      /*-----------------------------------------------
       * Need to rewrite every section of the file.
       *-----------------------------------------------*/
      close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
      open_file (sigma_output_file_name, "w", FILE_NOT_COMPRESSED,
                 &sigma_output_stream);/* open to write only */

#if DEBUG
      printf ("Must completely rewrite the sigma file!!!\n");
#endif    
	
      fprintf (sigma_output_stream, "%5ld\n", numstns_inp);

      for (ii=0;ii<numstns_inp;ii++)
         fprintf (sigma_output_stream, "%5ld %-27s\n", ii, stn_list[ii]);

      fprintf (sigma_output_stream, "stn_no parm_no  sigma value\n");
      }
   else
      {
      /*------------------------------------------------
       * Just need to update a single variable on one
       * station.
       *-----------------------------------------------*/
      STRIPLINE(sigma_output_stream);
      offset2 = ftell (sigma_output_stream); /* 2 lines down from top */
      STRIPLINE(sigma_output_stream);
      offset3 = ftell (sigma_output_stream); /* 3 lines down from top */

#if DEBUG
      printf ("offset3, offset2, (offset3-offset2): %ld %ld %ld\n",
               offset3, offset2, (offset3-offset2));
#endif

      if (fseek(sigma_output_stream, (numstns_inp-2)*(offset3-offset2), SEEK_CUR) !=0) /*skip 1st section*/
         {
         printf ("Error(1): Problems calling fseek in rewrite_sigmasq()\n");
         exit(1);
         }

      STRIPLINE (sigma_output_stream); /* strip comment line */

#if DEBUG
      printf ("Only need to rewrite last section of sigmasq file.\n");
      printf ("After STRIPLINE - numstns_inp = %ld\n", numstns_inp);
#endif

      /*----------------------------------------------
       * C requires that an fseek (or ftell, etc)
       * call be made between switching I/O operations.
       *----------------------------------------------*/
#if DEBUG
      printf ("call fseek\n");
#endif
      if (fseek(sigma_output_stream, 0, SEEK_CUR) !=0) /*go nowhere!*/
         { 
         printf ("Error(2): Problems calling fseek in rewrite_sigmasq()\n");
         exit(1);
         }
      } /* if numstns_in_file < numstns_inp */

   /*----------------------------------------------
    * Write updated sigmasq values to sigmasq file.
    * Only have to rewrite the second half of the
    * file. First half is list of stns should never
    * change.
    *----------------------------------------------*/
   for (ii=0;ii<numstns_inp;ii++)
      {
      for (jj=0;jj<NUMQCPARMS;jj++)
          { 
          if (sigma_sq[ii][jj] > -990.00)
             {
             fprintf (sigma_output_stream, "%5ld %5d %f\n", ii, jj, sigma_sq[ii][jj]);
#if DEBUG
             printf ("Write updated sigmasq vals:: %5ld %5d %f\n", ii, jj, sigma_sq[ii][jj]);
#endif
             }
          } /* for all parameters jj */
       } /* for all stns (ii) */
 
    close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);

    sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
    system (compression_cmd);

#if DEBUG 
    printf ("(End rewrite) executing compression_cmd: %-sxxx\n", compression_cmd);
#endif
 
   } /* rewrite_sigmasq_file() */


/*---------------------------------------------------------
 * locate_sigmasq() - This fn searches forward (day by day)
 *   from the input beginning date until it locates the
 *   first non-missing sigmasq value for the input stn,
 *   parameter, and time. This fn will NOT search farther
 *   than SIGMA_PERIOD days, since beyond that many days
 *   seasons might be crossed. If seasons are crossed, the
 *   sigmasq (variance) values may not be valid. Within
 *   the last SIGMA_PERIOD days, all SIGMA_PERIOD days
 *   will be searched.
 *
 *   The beginning year, julian date, hour, and minute 
 *   can be extracted from the input sigmasq file name.
 *   
 * 27 Sep 95 lec
 *   Created.
 * 01 Nov 95 lec
 *   Upgraded call since main_qc now uses YYYYJJJ dates.
 *   Get best sigmasq values for all parameters for
 *   requested stn. This potentially saves lots of time.
 * 30 Nov 95 lec
 *   Upgrade s/w to use pipes to open compressed sigmasq
 *   files. This allows s/w to bypass some system calls
 *   and will save time in UNIX environments.
 * 17 Feb 98 lec
 *   Upgraded s/w to catch "Broken Pipe" signal. This
 *   occurs when we open a pipe to a sigmasq file searching
 *   for a particular station's sigmasq values. If we
 *   determine that this sigmasq value is not in the file
 *   being currently searched, then we don't want to search
 *   (and waste more time) in that file. We want to close
 *   the file and start searching the next file. Since
 *   in the UNIX environment, we opened the file using
 *   a pipe, the system issues a "Broken Pipe" signal
 *   if we attempt to close the pipe before we hit EOF.
 *   We now make a call to "signal (SIGPIPE, SIG_IGN);"
 *   telling the system to IGNore pipe signals. The
 *   user can also just dump all these errors and not
 *   make a call to signal by adding the ">&/dev/null"
 *   to the "(runQC > runQC.log) >& /dev/null &) call.
 *   See main_QC.c. The call to signal handles this
 *   without the user's knowledge.
 *--------------------------------------------------------*/
void locate_sigmasq(/*in*/  char     sigfile_last_searched[NAMELEN_MAX], /* (pathname)YYYYJJJHHMM.sig */
                    /*in*/  char     sigpathname[NAMELEN_MAX], /* Sigmasq file pathname */
                    /*in*/  long int project_end_YYYYJJJ,      /* Year/jdate that project ends */
                    /*in*/  char     input_stn[27],            /* Stn needing sigma_sq value */
                    /*in*/  int      data_type,                /* Type of data being QC'd (temp, etc.) */
                    /*out*/ float    sigma_sq[NUMQCPARMS])     /* Variance array for current stn */
   {
   /* local variables */
   char         next_sigma_file_name[NAMELEN_MAX] = "\0";
   FILE         *next_sigma_file_stream;

   char         sigma_output_file_name[NAMELEN_MAX] = "\0";
   FILE         *sigma_output_stream;

   long int     ii = 0;
   int          jj, kk, ll = 0;

   int          match_found = FALSE;

   int          path_length = 0;
   int          lesser_stn_length = 0;
   long int     stn_no      = -1;
   long int     numstns_inp = 0;
   float        best_sigma_sq = -888.88000;

   int          numread = 0;
   int          ferr_int = 0;

   char         junkchar;
   char         compression_cmd [NAMELEN_MAX+100] = "\0";
   char         begin_YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";
   char         begin_YYYY_str[5] = "\0\0\0\0";

   long int     begin_YYYYJJJ;
   long int     begin_YYYY;
   long int     stop_YYYYJJJ;
   long int     current_YYYYJJJ;
   int          file_closed;
   int          number_of_days_searched = 0; /* always =< SIGMA_PERIOD */
   int          continue_search = 1;  /* true means < SIGMA_PERIOD days were searched -keep going.*/

   STRING27     stn_list[MAXNUMSTNS];

#if DEBUG
   printf ("---Enter locate_sigmasq---, input_stn:xxx%-sxxx\n", input_stn);
   for (jj=0;jj<NUMQCPARMS;jj++)
      printf ("(entry) sigma_sq[%d] = %f for input_stn=%-s\n",
                jj,sigma_sq[jj],input_stn);
#endif

   /*-----------------------------------------
    * Handle "Broken Pipe" errors. Tell system
    * to ignore pipe signals.
    *----------------------------------------*/
/*    signal (SIGPIPE, SIG_IGN); */

   /*-----------------------------------------
    * Convert input dates to workable numbers.
    *-----------------------------------------*/
   path_length = strlen (sigpathname);

   strncpy (begin_YYYYJJJ_str, &sigfile_last_searched[path_length], 7);
   begin_YYYYJJJ_str[7] = '\0';

#if DEBUG
   printf ("last date searched: %-s\n", begin_YYYYJJJ_str);
#endif

   begin_YYYYJJJ = atol (begin_YYYYJJJ_str);
   increment_YYYYJJJ (&begin_YYYYJJJ); /* Begin search at NEXT file */

   strncpy (begin_YYYY_str, &sigfile_last_searched[path_length], 4); 
   begin_YYYY_str[4] = '\0'; 
   begin_YYYY = atoi (begin_YYYY_str);

   stop_YYYYJJJ = add_num_to_YYYYJJJ (begin_YYYYJJJ, SIGMA_PERIOD);


   /*------------------------------------
    * While loop allows for "special"
    * handling of searches, such as
    * what occurs within last SIGMA_PERIOD
    * days of project. 
    *------------------------------------*/
   while (continue_search)
     {
#if DEBUG
   printf ("SEARCH from/to: begin_YYYYJJJ, begin_YYYY, stop_YYYYJJJ, project_end_YYYYJJJ: %ld %d %ld %ld\n",
            begin_YYYYJJJ, begin_YYYY, stop_YYYYJJJ, project_end_YYYYJJJ);
#endif

     /*--------------------------------------------
      * Stop search if any of the following occur:
      *   - Located requested sigmasq.
      *   - Have searched all SIGMA_PERIOD days but
      *     can't find non-missing sigmasq for
      *     requested time.
      *--------------------------------------------*/
     sigma_sq[data_type] = -888.88;

     for( current_YYYYJJJ = begin_YYYYJJJ;  current_YYYYJJJ <= stop_YYYYJJJ;
          increment_YYYYJJJ (&current_YYYYJJJ) )
        {  
#if DEBUG
        printf ("\n(locate) Current date is: %ld\n", current_YYYYJJJ);
#endif

        if (current_YYYYJJJ > project_end_YYYYJJJ) break;

        /*--------------------------------------------------------
         * Open next sigma sq file and search for requested
         * station. If stn not in this file OR requested
         * parameter's sigma not in file, go to next sigmasq file.
         * Save any other parameter's sigmasq values along the way
         * for this stn and time to prevent future searches.
         * Note that only the year (YYYY) and julian day (JJJ)
         * change between each file. Not the time or the suffix.
         *-------------------------------------------------------*/
        number_of_days_searched++;
        strcpy (sigma_output_file_name, sigfile_last_searched);

        sprintf (&sigma_output_file_name[path_length], "%7ld%-8s", 
                 current_YYYYJJJ, &sigfile_last_searched[path_length+7]);

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
#if DEBUG
        printf ("(1)executing cmd:%-sxxx\n", compression_cmd);
#endif

        numread = fscanf (sigma_output_stream, "%ld\n", &numstns_inp);
        if (numread == EOF)
           {
           printf ("ERROR: fscanf failed on first attempt to read %-s.\n",
                    sigma_output_file_name);
           exit(1);
           }

        file_closed = FALSE;
 
#if DEBUG
        printf ("numstns_inp: %ld\n", numstns_inp);
#endif
        match_found = 0;

        for (ii=0;ii<numstns_inp;ii++)
           {
           numread = fscanf (sigma_output_stream, "%ld%1c", &ii,&junkchar);
           if (feof(sigma_output_stream) || numread == EOF)
              {
              printf ("ERROR(searching): hit premature End Of File on %-s.\n",
                       sigma_output_file_name);
              exit(1);
              }
 
           fgets (stn_list[ii], 27, sigma_output_stream);
           if (feof(sigma_output_stream))
              {
              printf ("ERROR(searching): hit premature End Of File on %-s.\n",
                       sigma_output_file_name);
              exit(1);
              }

           if (match_found) continue; /* Must position file ptr at end of first section. */

           if (strlen(stn_list[ii]) <= strlen(input_stn))
              lesser_stn_length = strlen(stn_list[ii]);
           else
              lesser_stn_length = strlen(input_stn);

#if DEBUG
           printf ("Comparing stn_list[%ld], input_stn: xxx%-sxxx xxx%-sxxx\n",
                    ii, stn_list[ii], input_stn);
#endif
           if (!match_found && !strncmp(stn_list[ii], input_stn, lesser_stn_length))
              {
              /*-------------------------------------
               * Found a match  - don't break! Need 
               * to read to next section of file. 
               *------------------------------------*/
              match_found = 1;
              stn_no = ii;
#if DEBUG
              printf ("   STN FOUND MATCH stn_no, stn_list[%d]:%d xxx%sxxx. Now locate variance.\n", 
                     stn_no, stn_no, stn_list[stn_no]);
#endif
              }
#if DEBUG
           printf ("   stn_list[%ld] =  %sxxx\n", ii, stn_list[ii]);
#endif
           }  /* for ii < numstns_inp */

        sigma_sq[data_type] = -888.88;

        /*---------------------------------------
         * If station match not located in this
         * sigmasq file, then skip rest of file
         * and continue searching next day file.
         *--------------------------------------*/
        if (!match_found)
           {
#if DEBUG
           printf ("Stn match NOT found!\n");
#endif
           stn_no = -999;

           /*---------------------------------------
            * In UNIX environment, have used a
            * pipe open to access compressed
            * file without using time to uncompress.
            * Close_file() will then close pipe.
            * If file is not compressed, close_file()
            * close normally with fclose command.
            *
            * Following close could give "Broken Pipe"
            * signal, if have not read complete file.
            *----------------------------------------*/
#if UNIX_ENV
           close_file (&sigma_output_stream, FILE_COMPRESSED);
#else
           close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
           sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
           system (compression_cmd);
#endif
#if DEBUG
           printf ("No stn match was found! Close file AND %-s\n", compression_cmd);
           printf ("(1)NOTE: SEARCH NEXT SIG FILE - Stn OR parm's variance NOT located in sigfile:: \n %-s\n",
                    sigma_output_file_name);
#endif
           continue;    /* affects for current_YYYYJJJ loop */
           }

        /*---------------------------------------
         * To get here, must have located stn in
         * in top part of current sigmasq file. 
         * Now begin search for sigmasq values
         * for this station.
         *--------------------------------------*/
#if DEBUG
        printf ("STRIP 2 LINES\n");
#endif
        STRIPLINE (sigma_output_stream); /* Skip to the comment line. */
        STRIPLINE (sigma_output_stream); /* Skip the comment line. */

        /*--------------------------------------------------------
         * Can't just STRIPLINES down to where match found,
         * cause sigma file only contains non-missing variances.
         * Just because the stn is listed in the top part of the
         * sigmasq file doesn't mean there will be a non-missing
         * sigmasq value in the second half of that file. It does
         * mean that stn was encountered at least once during
         * that time period when compute_sigmasq() was doing
         * the original sigmasq computations.
         *-------------------------------------------------------*/ 
        jj = -999;
        kk = -999;
#if DEBUG
        printf ("Begin while (!feof(sigma_output_stream))\n");
#endif
        while (!feof(sigma_output_stream))
           { 
           numread = fscanf (sigma_output_stream, "%d%d", &jj, &kk);
#if DEBUG
           printf ("fscanf -- numread = %d\n", numread);
           printf ("(-1) jj=%d, kk=%d, best_sigma_sq = %f\n", jj, kk, best_sigma_sq);
           printf ("(-1) stn_no = %d, sigma_sq[%d]= %f\n", stn_no, kk, sigma_sq[kk]);
#endif
           if (feof(sigma_output_stream) != 0 || numread == EOF) break;

           numread = fscanf (sigma_output_stream, "%f", &best_sigma_sq);
           if (feof(sigma_output_stream) !=0 || numread == EOF) break;

#if DEBUG
           printf ("(1) jj=%d, kk=%d, best_sigma_sq = %f\n", jj, kk, best_sigma_sq);
           printf ("(1) stn_no = %d, sigma_sq[%d]= %f\n", stn_no, kk, sigma_sq[kk]);
#endif
           if (jj > stn_no)
/*            continue; if read everything then no broken pipe error issued  - this works*/
              break;    /* Potentially leads to "Broken Pipe" signal. */

           if (jj < stn_no)
              {
#if DEBUG
              printf ("jj < stn_no - continue!\n");
#endif
              continue; /* assumes numbers always in increasing order. */
              }

           /*-------------------------------------------
            * Once the data for current stn is located,
            * save off best values for ALL parameters.
            * This should save s/w from doing multiple
            * searches. Only overwrite missing values.
            *------------------------------------------*/
           if (jj == stn_no)
              {
#if DEBUG
              printf ("(jj=stn_no) jj=%d, kk=%d, best_sigma_sq = %f\n", jj, kk, best_sigma_sq);
              printf ("(jj=stn_no) stn_no = %d, sigma_sq[%d]= %f\n", stn_no, kk, sigma_sq[kk]);
#endif 

              if ((sigma_sq[kk] < -880.00) && (best_sigma_sq > -880.00))/*Don't drop first val located*/
                 {
                 sigma_sq[kk] = best_sigma_sq;
#if DEBUG
                 printf ("(SAVE 1st encounter) sigma_sq[%d] = %f for stn_no=%d, parameter=%d\n",
                         kk,sigma_sq[kk],jj,kk);
#endif
                 }

              /*------------------------------------------
               * Now that current stn has been located,
               * check for best values for all parameters.
               *------------------------------------------*/
              while (!feof(sigma_output_stream))
                 {
                 numread = fscanf (sigma_output_stream, "%d%d", &jj, &kk);
#if DEBUG
                 printf ("(11)numread: %d\n", numread);
#endif
                 if (feof(sigma_output_stream) != 0 || numread == EOF) break; /* hit EOF */
                 if (jj > stn_no) break;   /* gone too far  - potential "Broken Pipe" signal!!!*/

                 numread = fscanf (sigma_output_stream, "%f", &best_sigma_sq);
#if DEBUG
                 printf ("(22)numread: %d\n", numread);
#endif
                 if (feof(sigma_output_stream) != 0 || numread == EOF) break;
#if DEBUG
                 printf ("(Inner loop)   jj=%d, kk=%d, best_sigma_sq = %f\n", jj, kk, best_sigma_sq);
                 printf ("(Inner loop)   stn_no = %d, sigma_sq[%d]=%f\n", stn_no, kk, sigma_sq[kk]);
#endif
                 if ((sigma_sq[kk] < -880.00) && (best_sigma_sq > -880.00)) /*only save first good val encountered */
                    {
                    sigma_sq[kk] = best_sigma_sq;
#if DEBUG
                    printf (" (2)FOUND sigma_sq[%d] = %f for stn_no=%d, parameter=%d\n",
                            kk,sigma_sq[kk],jj,kk);
#endif
                    }

                 if (jj == stn_no && kk >= NUMQCPARMS-1) break;/*done searching-found or not-"Broken Pipe"*/

                 } /*while !feof -  collecting this stn's sigmasq vals */
#if DEBUG
              printf ("End while collecting this stns sig vals.\n");
#endif

              /*---------------------------------------
               * In UNIX environment, have used a
               * pipe open to access compressed
               * file without using time to uncompress.
               * Close_file() will then close pipe.
               * If file is not compressed, close_file()
               * close normally with fclose command.
               *
               * If have not read all sigmasq file, the
               * following closes could lead to the
               * "Broken Pipe" signal.
               *----------------------------------------*/
#if UNIX_ENV
              close_file (&sigma_output_stream, FILE_COMPRESSED);
#else
              close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
              sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
              system (compression_cmd);
#endif
              file_closed = TRUE;

              if (sigma_sq[data_type] > -880.00)
                 {
#if DEBUG
                 printf ("Found requested sigmasq in file %-s in %d days.\n", 
                         sigma_output_file_name, number_of_days_searched);
                 printf ("   FOUND sigma_sq[%d] = %f for stn_no=%-s, parameter=%d\n", 
                         data_type, sigma_sq[data_type], input_stn, data_type);
#endif
                 return;
                 }
              } /* jj == stn_no */

           /*--------------------------------
            * Following checks break from
            * outer while data in file loop.
            *-------------------------------*/
           if (jj == stn_no && kk >= NUMQCPARMS-1) break;
           if (feof(sigma_output_stream) != 0 || numread == EOF) break;

           if (jj > stn_no) /* Assume data in numbered order!! */
              {
#if DEBUG
              printf ("(22)end of while data in file: jj > stn_no, file_closed = %d\n", file_closed);
#endif
              break; /* potential "Broken Pipe" signal */
              }
           } /* while data in file */


        if (!file_closed)
           {
           /*------------------------------------------------------
            * In UNIX environment, have used a pipe open to access
            * compressed file without using time to uncompress. 
            * Close_file() will then close pipe. If file is not
            * compressed, close_file() close normally with fclose
            * command. When accessing the sigma file with a 
            * pipe (popen/pclose/FILES_COMPRESSED), a "Broken Pipe"
            * signal is generated by the system if pclose is called
            * before EOF is hit in the file. If the complete file is 
            * read, then no signal is generated. Trap this signal and
            * discard since to save processing time we want to just
            * close the file without reading the whole file. Seems
            * like system should allow closing pipe before EOF, but
            * system designers didn't allow this. It's not an error,
            * more like a warning about the pipe.
            *------------------------------------------------------*/
#if UNIX_ENV
           close_file (&sigma_output_stream, FILE_COMPRESSED);
#else
           close_file (&sigma_output_stream, FILE_NOT_COMPRESSED);
           sprintf (compression_cmd, "gzip %-s\0", sigma_output_file_name);
           system (compression_cmd);
#endif
           }

        if (sigma_sq[data_type] > -880.00) break; /* Found what searching for, so break */

#if  DEBUG
        if (sigma_sq[data_type] > -880.00)
           printf ("Found requested sigmasq in file %-s after searching %d days.\n", 
                   sigma_output_file_name, number_of_days_searched);
        else
           printf ("NOTE: SEARCH NEXT SIG FILE - Stn OR parm's variance NOT located in sigfile:: \n %-s\n",
                    sigma_output_file_name);
#endif
        } /* for current_YYYYJJJ  */

     /*--------------------------------------------------
      * S/w always searches forward in time. If less than
      * SIGMA_PERIOD days located between begin and end
      * of project and sigma_sq was not located, then 
      * need to continue searching back at project end
      * minus SIGMA_PERIOD # days. This second search
      * ends where first search began. As in all other
      * searches, the max number days searched is 
      * SIGMA_PERIOD days.
      *-------------------------------------------------*/
     if ((number_of_days_searched < SIGMA_PERIOD-1) && 
         (sigma_sq[data_type] < -880.00))
       { 
       begin_YYYYJJJ = subtract_num_from_YYYYJJJ(project_end_YYYYJJJ, (SIGMA_PERIOD-1));
       stop_YYYYJJJ = add_num_to_YYYYJJJ (begin_YYYYJJJ, (SIGMA_PERIOD-number_of_days_searched-2));
       continue_search = 1;

#if DEBUG
       printf ("LESS than SIGMA_PERIOD days were searched - Hit project end. sigma_sq[%d]=%f\n",
              data_type, sigma_sq[data_type]);
       printf ("ONLY %d days were searched.\n", number_of_days_searched);
       printf ("CONTINUE Search. NEW begin/stop dates: %ld %ld\n", 
               begin_YYYYJJJ, stop_YYYYJJJ);
#endif
       }
     else
       {
#if DEBUG
       printf ("SEARCHED ALL POSSIBLE DAYS - end search.\n");
#endif
       number_of_days_searched++; /* this var doesn't incld first day srched */
       continue_search = 0;
       }
     } /* while (continue_search)


   /*---------------------------------------------------
    * One last thing to save some searching time.
    * If we have searched the complete SIGMA_PERIOD
    * days and couldn't find the sigmasq value we
    * were searching for, that sigmasq will be set
    * to -888.88 at this point. So....if after that
    * search, the other parameters still have
    * missing values, they can be set to -888.88, too.
    *--------------------------------------------------*/
   if (sigma_sq[data_type] < -880.00 && (number_of_days_searched >=SIGMA_PERIOD-1))
      {
      for (ll=0; ll<NUMQCPARMS; ll++)
         if (sigma_sq[ll] < -990.00)
           {
#if DEBUG
           printf ("Set parameter %d to -888.88\n", ll);
#endif
           sigma_sq[ll] = -888.88;
           }
      }

   if (sigma_sq[data_type] < -880.00) 
    printf ("Fn locate_sigmasq() could NOT locate requested sigmasq within %d days!(Inclds orig day)\n",
              number_of_days_searched);

#if DEBUG
   printf ("End locate_sigmasq: file_closed = %d\n", file_closed);
#endif
   }  /* locate_sigmasq() */
