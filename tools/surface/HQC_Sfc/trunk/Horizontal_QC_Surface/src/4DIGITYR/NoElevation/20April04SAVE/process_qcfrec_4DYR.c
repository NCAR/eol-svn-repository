/*-------------------------------------------------------
 * process_qcfrec_4DYR.c - module containing functions to 
 *    process qcf records.
 *
 * 04 Mar 97 lec
 *   Added include for string.h.
 * 15 August 2002 lec
 *   Update s/w to handle 4 digit years in data.
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
 
#include "qcfrec.h"
#include "process_qcfrec_4DYR.h"

#define debug 0

/*------------------------------------------------------- 
 * construct_qcfptr - creates pointer to qcf record.
 *
 *-------------------------------------------------------*/ 
void construct_qcfptr ( /*in/out*/  struct qcfrec **qcfptr )
   {
   struct qcfrec *qptr;

   /*
    * Construct a qcfrec pointer
    */
   qptr = (QCFREC *) malloc (sizeof(QCFREC));
 
   if (qptr == NULL)
      perror ("Error: Can't malloc QCFREC space!");

   *qcfptr = qptr;
   }

/*-------------------------------------------------------
 * destruct_qcfptr - destroys space occupied by input
 *     qcf pointer.
 *
 *-------------------------------------------------------*/
void destruct_qcfptr  ( /*in*/   struct qcfrec **qcfptr )
   {
   struct qcfrec *qptr;

   qptr = *qcfptr;
   free (qptr);
   *qcfptr = NULL;
   }


/*-------------------------------------------------------  
 * reset_qcfrec.c - reinitializes the input qcf record. 
 *    Real values are set to -999.99 for missing value.
 *    Other data types are set to blank,
 *    zero, or special missing value codes, if any
 *    exist.
 *
 *  Input: qcfptr is the pointer to a structure of 
 *         type qcfrec (qcf record variables)
 *
 *  Output: qcfptr which has been reinitialized.
 *
 *-------------------------------------------------------*/
void reset_qcfrec ( /*in/out*/  QCFREC *qcfptr )
   {
   qcfptr->year_nom = 0;
   qcfptr->month_nom = 0;
   qcfptr->day_nom = 0;
   qcfptr->hour_nom = 0;
   qcfptr->minute_nom = 0;

   qcfptr->year = 0;
   qcfptr->month = 0;
   qcfptr->day = 0;
   qcfptr->hour = 0;
   qcfptr->minute = 0;

   strncpy( qcfptr->qnet, "          \0",11);
   strncpy( qcfptr->statn,"               \0",16);

   qcfptr->lat = -999.99;
   qcfptr->lon = -999.99;
   qcfptr->occur = 0;
   qcfptr->staelv = -999.99;

   qcfptr->staprs = -999.99;
   qcfptr->staflg = 'M';
   qcfptr->seaprs = -999.99;
   qcfptr->seaflg = 'M';
   qcfptr->cmpsea = -999.99;
   qcfptr->cmpflg = 'M';

   qcfptr->temp = -999.99;
   qcfptr->tmpflg = 'M';
   qcfptr->dewpnt = -999.99;
   qcfptr->dewflg = 'M';

   qcfptr->wndspd = -999.99;
   qcfptr->spdflg = 'M';
   qcfptr->wnddir = -999.99;
   qcfptr->dirflg = 'M';

   qcfptr->precip = -999.99;
   qcfptr->prcflg = 'M';

   qcfptr->sg = ' ';
   qcfptr->squall = -999.99;
   qcfptr->sqlflg = 'M';

   qcfptr->prswea = -999;
   qcfptr->pwflg = 'M';

   qcfptr->visib = -999.99;
   qcfptr->visflg = 'M';

   qcfptr->celht1 = -999.99;
   qcfptr->celfg1 = 15;
   qcfptr->c1flg = 'M';
   qcfptr->clamt1 = 15;
   qcfptr->ca1flg = 'M';

   qcfptr->celht2 = -999.99;
   qcfptr->celfg2 = 15;
   qcfptr->c2flg = 'M';
   qcfptr->clamt2 = 15;
   qcfptr->ca2flg = 'M';

   qcfptr->celht3 = -999.99;
   qcfptr->celfg3 = 15;
   qcfptr->c3flg = 'M';
   qcfptr->clamt3 = 15;
   qcfptr->ca3flg = 'M';

   } /* reset_qcfrec() */

/*-------------------------------------------------------
 * copy_qcfrec.c - copies data from qcfptr1 to qcfptr.
 *
 *  Input: qcfptr is the pointer to a structure of
 *         type qcfrec (qcf record variables)
 *
 *  Output: qcfptr2.
 *
 *-------------------------------------------------------*/
void copy_qcfrec ( /*in/out*/  QCFREC *qcfptr,
                   /*in*/      QCFREC *qcfptr1 )
   {
   qcfptr->year_nom = qcfptr1->year_nom;
   qcfptr->month_nom = qcfptr1->month_nom;
   qcfptr->day_nom = qcfptr1->day_nom;
   qcfptr->hour_nom = qcfptr1->hour_nom;
   qcfptr->minute_nom = qcfptr1->minute_nom;
    
   qcfptr->year = qcfptr1->year;
   qcfptr->month = qcfptr1->month;
   qcfptr->day = qcfptr1->day;
   qcfptr->hour = qcfptr1->hour;
   qcfptr->minute = qcfptr1->minute;
    
   strncpy( qcfptr->qnet, qcfptr1->qnet,11);
   strncpy( qcfptr->statn, qcfptr1->statn,16);
    
   qcfptr->lat = qcfptr1->lat;
   qcfptr->lon = qcfptr1->lon;
   qcfptr->occur = qcfptr1->occur;
   qcfptr->staelv = qcfptr1->staelv;

   qcfptr->staprs = qcfptr1->staprs;
   qcfptr->staflg = qcfptr1->staflg;
   qcfptr->seaprs = qcfptr1->seaprs;
   qcfptr->seaflg = qcfptr1->seaflg;
   qcfptr->cmpsea = qcfptr1->cmpsea;
   qcfptr->cmpflg = qcfptr1->cmpflg;

   qcfptr->temp   = qcfptr1->temp;
   qcfptr->tmpflg = qcfptr1->tmpflg;
   qcfptr->dewpnt = qcfptr1->dewpnt;
   qcfptr->dewflg = qcfptr1->dewflg;
 
   qcfptr->wndspd = qcfptr1->wndspd;
   qcfptr->spdflg = qcfptr1->spdflg;
   qcfptr->wnddir = qcfptr1->wnddir;
   qcfptr->dirflg = qcfptr1->dirflg;
 
   qcfptr->precip = qcfptr1->precip;
   qcfptr->prcflg = qcfptr1->prcflg;
 
   qcfptr->sg     = qcfptr1->sg;
   qcfptr->squall = qcfptr1->squall;
   qcfptr->sqlflg = qcfptr1->sqlflg;
 
   qcfptr->prswea = qcfptr1->prswea;
   qcfptr->pwflg  = qcfptr1->pwflg;
 
   qcfptr->visib  = qcfptr1->visib;
   qcfptr->visflg = qcfptr1->visflg;
 
   qcfptr->celht1 = qcfptr1->celht1;
   qcfptr->celfg1 = qcfptr1->celfg1;
   qcfptr->c1flg  = qcfptr1->c1flg;
   qcfptr->clamt1 = qcfptr1->clamt1;
   qcfptr->ca1flg = qcfptr1->ca1flg;
 
   qcfptr->celht2 = qcfptr1->celht2;
   qcfptr->celfg2 = qcfptr1->celfg2;
   qcfptr->c2flg  = qcfptr1->c2flg;
   qcfptr->clamt2 = qcfptr1->clamt2;
   qcfptr->ca2flg = qcfptr1->ca2flg;

   qcfptr->celht3 = qcfptr1->celht3;
   qcfptr->celfg3 = qcfptr1->celfg3;
   qcfptr->c3flg  = qcfptr1->c3flg;
   qcfptr->clamt3 = qcfptr1->clamt3;
   qcfptr->ca3flg = qcfptr1->ca3flg;
 
   } /* copy_qcfrec() */
 

/*--------------------------------------------------------
 * write_qcfrec.c - writes a qcf record to an output file
 *
 * Input: output_stream is pointer to output file.
 *        &qcfptr is the pointer to a structure of
 *        type qcfrec (qcf record variables)
 *
 * Output: &qcfptr data is written to specified output stream.        
 *
 *-------------------------------------------------------*/
void write_qcfrec ( /*in/out*/ FILE    **output_stream, 
                    /*in*/     QCFREC  *qcfptr )
   {
   FILE   *out_stream;

   out_stream = *output_stream;

   fprintf( out_stream, "%4.4d/%2.2d/%2.2d %2.2d:%2.2d ", 
            qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom,
            qcfptr->hour_nom, qcfptr->minute_nom );

   fprintf( out_stream, "%4.4d/%2.2d/%2.2d %2.2d:%2.2d ", 
            qcfptr->year, qcfptr->month, qcfptr->day,
            qcfptr->hour, qcfptr->minute );
 
   fprintf( out_stream, "%-10s %-15s %10.5f %11.5f %3d %7.2f ",   /* updated statn size*/
            qcfptr->qnet,  qcfptr->statn, 
            qcfptr->lat,   qcfptr->lon, 
            qcfptr->occur, qcfptr->staelv);

   fprintf( out_stream, "%7.2f %c %7.2f %c %7.2f %c ", 
            qcfptr->staprs, qcfptr->staflg, 
            qcfptr->seaprs, qcfptr->seaflg,
            qcfptr->cmpsea, qcfptr->cmpflg );

   fprintf( out_stream, "%7.2f %c %7.2f %c %7.2f %c %7.2f %c ", 
            qcfptr->temp,   qcfptr->tmpflg, 
            qcfptr->dewpnt, qcfptr->dewflg,
            qcfptr->wndspd, qcfptr->spdflg,
            qcfptr->wnddir, qcfptr->dirflg );

   fprintf( out_stream, "%7.2f %c %c %7.2f %c %4d %c %8.2f %c ", 
            qcfptr->precip, qcfptr->prcflg,
            qcfptr->sg,     qcfptr->squall, qcfptr->sqlflg,
            qcfptr->prswea, qcfptr->pwflg,
            qcfptr->visib,  qcfptr->visflg );

   fprintf(out_stream,"%7.2f %2d %c %2d %c %7.2f %2d %c %2d %c %7.2f %2d %c %2d %c\n", 
    qcfptr->celht1, qcfptr->celfg1, qcfptr->c1flg, qcfptr->clamt1, qcfptr->ca1flg,
    qcfptr->celht2, qcfptr->celfg2, qcfptr->c2flg, qcfptr->clamt2, qcfptr->ca2flg,
    qcfptr->celht3, qcfptr->celfg3, qcfptr->c3flg, qcfptr->clamt3, qcfptr->ca3flg);

    *output_stream = out_stream;

   } /* write_qcfrec() */


/*--------------------------------------------------------
 * read_qcfrec.c - reads a qcf record from an input file
 * 
 * Input: input_stream is pointer to input file.
 *        &qcfptr is the pointer to a structure of
 *        type qcfrec (qcf record variables)
 * 
 * Output: QCF input data is written to specified ptr.
 *
 * 28 Nov 95 lec
 *  Combined fscanf statements into minimum number
 *  of calls.
 *-------------------------------------------------------*/
void read_qcfrec ( /*in/out*/ FILE    **input_stream,
                   /*in*/     QCFREC  *qcfptr )
   {
   char   junk[50];
   int    ii;
   FILE   *in_stream;

   in_stream = *input_stream;

   fscanf( in_stream, "%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c",
            &qcfptr->year_nom, &junk[0], &qcfptr->month_nom, &junk[1],
            &qcfptr->day_nom,  &junk[2], &qcfptr->hour_nom,  &junk[3],
            &qcfptr->minute_nom, &junk[4],
            &qcfptr->year, &junk[0], &qcfptr->month, &junk[1],
            &qcfptr->day,  &junk[2], &qcfptr->hour,  &junk[3],
            &qcfptr->minute, &junk[4] );

   fgets (qcfptr->qnet, 11, in_stream);
   qcfptr->qnet[10] = '\0';
 
   junk[0] = fgetc(in_stream);
 
   fgets (qcfptr->statn, 16, in_stream); 
   qcfptr->statn[15] = '\0';
 
   junk[0] = fgetc(in_stream);

   fscanf( in_stream,
   "%10f %11f %3d %7f %7f %1c %7f %1c %7f %1c %7f %1c %7f %1c %7f %1c %7f %1c %7f %1c%1c%1c%1c%7f %1c%1c%4d%1c%1c %8f %1c %7f %2d %1c %2d %1c %7f %2d %1c %2d %1c %7f %2d %1c %2d %1c\n",
           &qcfptr->lat,    &qcfptr->lon, 
           &qcfptr->occur,  &qcfptr->staelv,
           &qcfptr->staprs, &qcfptr->staflg,
           &qcfptr->seaprs, &qcfptr->seaflg, 
           &qcfptr->cmpsea, &qcfptr->cmpflg, 
           &qcfptr->temp,   &qcfptr->tmpflg,
           &qcfptr->dewpnt, &qcfptr->dewflg,
           &qcfptr->wndspd, &qcfptr->spdflg,
           &qcfptr->wnddir, &qcfptr->dirflg,
           &qcfptr->precip, &qcfptr->prcflg,
           &junk[0], &qcfptr->sg, &junk[1],
           &qcfptr->squall, &qcfptr->sqlflg,
           &junk[2], &qcfptr->prswea, &junk[3], &qcfptr->pwflg,
           &qcfptr->visib,  &qcfptr->visflg,
           &qcfptr->celht1, &qcfptr->celfg1, &qcfptr->c1flg,
           &qcfptr->clamt1, &qcfptr->ca1flg,
           &qcfptr->celht2, &qcfptr->celfg2, &qcfptr->c2flg,
           &qcfptr->clamt2, &qcfptr->ca2flg,
           &qcfptr->celht3, &qcfptr->celfg3, &qcfptr->c3flg,
           &qcfptr->clamt3, &qcfptr->ca3flg );

#if debug
   printf("%4.4d/%2.2d/%2.2d %2.2d:%2.2d\n",
            qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom,
            qcfptr->hour_nom, qcfptr->minute_nom );

   printf("%4.4d/%2.2d/%2.2d %2.2d:%2.2d\n",
            qcfptr->year, qcfptr->month, qcfptr->day,
            qcfptr->hour, qcfptr->minute );

   printf("xxx%-10sxxx xxx%-15sxxx %10.5f %11.5f %3d %7.2f\n",
            qcfptr->qnet,  qcfptr->statn,
            qcfptr->lat,   qcfptr->lon,
            qcfptr->occur, qcfptr->staelv);

   printf("%7.2f %c %7.2f %c %7.2f %c\n",
            qcfptr->staprs, qcfptr->staflg,
            qcfptr->seaprs, qcfptr->seaflg,
            qcfptr->cmpsea, qcfptr->cmpflg );

   printf("%7.2f %c %7.2f %c %7.2f %c %7.2f %c\n",
            qcfptr->temp,   qcfptr->tmpflg,
            qcfptr->dewpnt, qcfptr->dewflg,
            qcfptr->wndspd, qcfptr->spdflg,
            qcfptr->wnddir, qcfptr->dirflg );

   printf("%7.2f %c xxx%cxxxxx%7.2fxx %c %4d %c xx%8.2fxx %c\n",
            qcfptr->precip, qcfptr->prcflg,
            qcfptr->sg,     qcfptr->squall, qcfptr->sqlflg,
            qcfptr->prswea, qcfptr->pwflg,
            qcfptr->visib,  qcfptr->visflg );
 
   printf("xxx%7.2fxxx %2d %c %2d %c xxx%7.2fxxx %2d %c %2d %c xxx%7.2fxxx %2d %c %2d %c\n",
    qcfptr->celht1, qcfptr->celfg1, qcfptr->c1flg, qcfptr->clamt1, qcfptr->ca1flg,
    qcfptr->celht2, qcfptr->celfg2, qcfptr->c2flg, qcfptr->clamt2, qcfptr->ca2flg,
    qcfptr->celht3, qcfptr->celfg3, qcfptr->c3flg, qcfptr->clamt3, qcfptr->ca3flg);
 
#endif

    *input_stream = in_stream;

   } /* read_qcfrec() */

/*--------------------------------------------------------
 * open_file.c - Opens the requested file, if it exists.
 *
 * Input: path name of file to be opened.
 *
 * Output: Opened file stream.
 *-------------------------------------------------------*/
void open_file ( /*in*/  char   *path_name,
                 /*in*/  char   *mode,
                 /*in*/  int    file_compressed,
                 /*out*/ FILE   **opened_file )
   {
   char   cat_cmd [NAMELEN_MAX+10] = "\0";

#if UNIX_ENV
   /*----------------------------------------
    * In UNIX environment, open a pipe to the 
    * compressed file. The gunzip -c command
    * leaves the original file untouched and
    * only uncompress the file to the stdout.
    * This allows the s/w to skip the system
    * calls that compress and uncompress
    * each file. The gunzip cmd assumes the
    * file has a .gz suffix. If not in UNIX
    * environment or file is not compressed,
    * just open the file regularly.
    *----------------------------------------*/
   if (file_compressed)
      {
      sprintf (cat_cmd, "gunzip -c %-s", path_name);

/*    printf ("file_compressed = TRUE! popen with cat_cmd: xxx%-sxxx\n", cat_cmd);  DEBUG */

      *opened_file = popen (cat_cmd, mode);
      }
   else
      *opened_file = fopen (path_name, mode);
   
#else
   *opened_file = fopen (path_name, mode);
#endif

   if (*opened_file == NULL)
      {
      fprintf (stderr, "Error: Can NOT open %-s.\n", path_name);
      exit(1);
      }

   } /* open_file()*/

/*--------------------------------------------------------
 * close_file.c - Closes the requested file.
 * 
 * Input: Stream to be closed.
 * 
 *-------------------------------------------------------*/
void close_file (/*in*/ FILE   **opened_file,
                 /*in*/ int    file_compressed)
   {
   int   stat = 0;

   /*---------------------------------------------
    * If in UNIX environment and the file was
    * compressed, file has been opened via the
    * popen (pipe) cmd. If not in UNIX environ,
    * or file not compressed, just close normally.
    *---------------------------------------------*/
#if UNIX_ENV
   if (file_compressed)
      {
      stat = pclose(*opened_file);
/*    printf ("UNIX_ENV --(1) close_file: file_compressed - use pclose\n"); */

      if (stat !=0 && stat !=EOF && stat != 36096)
         printf (" WARNING: file_compressed -> pclose stat = %d\n", stat); /* debug */

      if (stat == EOF)
        {
        fprintf (stderr, "Error: Can NOT pclose opened file. Status = %d.\n", stat); 
        exit(1); 
        } 
      }
   else
      {
/*    printf ("UNIX_ENV --(2) close_file: file_NOT_compressed - use normal fclose\n"); */
      if (fclose (*opened_file) == EOF)
         {   
         fprintf (stderr, "Error: Can NOT close opened file.\n");
         exit(1);
         }
      }
#else
/* printf ("NOT UNIX_ENV - (3) close_file: file_NOT_compressed - use normal fclose\n"); */
   if (fclose (*opened_file) == EOF)
     {
     fprintf (stderr, "Error: Can NOT close opened file.\n");
     exit(1);
     }
#endif 

#if debug
   printf ("end close_file\n");
#endif
   } /* close_file() */


/*---------------------------------------------------------
 * determine_stn_no() - Determines internal stn number.
 *
 * 09 Dec 94 lec
 *   Created.
 *--------------------------------------------------------*/
int determine_stn_no(/*in*/ STRING27 stn_list[MAXNUMSTNS],
                     /*in*/ long     *numstns,
                     /*in*/ char     statn[27])
   {
   long int i=0;
   int      len;
   long int nums;

#if debug 
   printf ("Enter determine_stn_no. numstns, statn: %ld %-sxxx\n",
           *numstns, statn);
#endif
 
   nums = *numstns;
   for (i=0;i<nums;i++)
     {
     /*
      * Only compare with what is stored. Since names can be
      * multiple words, strlen return should always be 27 and
      * should always compare complete string. During testing
      * older fns may have generated stn names shorter than
      * 27 chars (was 15 chars name only w/o network).
      */
     len = strlen (stn_list[i]);

#if debug
     printf ("statn, i, stn_list[i], len[i] : %-sxxx %d %-sxxx %d\n", 
             statn, i, stn_list[i], len);
#endif
     if (!strncmp(stn_list[i], statn,len)) /* len was = 27 */
        {
#if debug
        printf ("Found a match, i = %ld\n", i);
#endif
        return(i);
        }
     }
 
   /*
    * Else found new station. Add to stn_list and increment
    * number of stations. Note that nums is incremented one
    * extra in the loop above.
    */
   strncpy (stn_list[nums], statn, 27);

#if debug 
   printf ("numstns = %ld, stn_list[%ld] = %-sxxx\n",
            *numstns, nums, stn_list[nums]);
#endif
 
   *numstns = nums+1;

#if debug
   printf ("Exit determin_stn_no. Add stn to stn list in determine_stn_no!!! *numstns: %ld\n", *numstns);
#endif 

   return (nums); /* return the element in the array, and number of stns above.*/
 
   }/*determine_stn_no()*/
