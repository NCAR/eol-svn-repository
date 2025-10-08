/*-------------------------------------------------------
 * process_qcfrec.c - module containing functions to 
 *    process qcf records.
 *
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
 
#include "qcfrec.h"
#include "process_qcfrec.h"

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

   fprintf( out_stream, "%2.2d/%2.2d/%2.2d %2.2d:%2.2d ", 
            qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom,
            qcfptr->hour_nom, qcfptr->minute_nom );

   fprintf( out_stream, "%2.2d/%2.2d/%2.2d %2.2d:%2.2d ", 
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
 *-------------------------------------------------------*/
void read_qcfrec ( /*in/out*/ FILE    **input_stream,
                   /*in*/     QCFREC  *qcfptr )
   {
   char   junk[50];
   int    ii;
   FILE   *in_stream;

   in_stream = *input_stream;

   fscanf( in_stream, "%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c",
            &qcfptr->year_nom, &junk[0], &qcfptr->month_nom, &junk[1], 
            &qcfptr->day_nom,  &junk[2], &qcfptr->hour_nom,  &junk[3],
            &qcfptr->minute_nom, &junk[4] );

   fscanf( in_stream, "%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c",
            &qcfptr->year, &junk[0], &qcfptr->month, &junk[1],
            &qcfptr->day,  &junk[2], &qcfptr->hour,  &junk[3],
            &qcfptr->minute, &junk[4] );

   for (ii=0;ii<10;ii++)   /* use fgets */
      qcfptr->qnet[ii] = fgetc (in_stream);
 
   qcfptr->qnet[10] = '\0';
 
   junk[0] = fgetc(in_stream);
 
   for (ii=0;ii<15;ii++)  /* use fgets */
      qcfptr->statn[ii] = fgetc (in_stream);
 
   qcfptr->statn[15] = '\0';
 
   junk[0] = fgetc(in_stream);
 
   fscanf( in_stream, "%10f%1c%11f%1c%3d%1c%7f%1c",
            &qcfptr->lat,  &junk[2], &qcfptr->lon,  &junk[3],
            &qcfptr->occur, &junk[4], &qcfptr->staelv, &junk[5]);

#if debug 
   printf("%2.2d/%2.2d/%2.2d %2.2d:%2.2d\n",
            qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom,
            qcfptr->hour_nom, qcfptr->minute_nom );
 
   printf("%2.2d/%2.2d/%2.2d %2.2d:%2.2d\n",
            qcfptr->year, qcfptr->month, qcfptr->day,
            qcfptr->hour, qcfptr->minute );
 
   printf("%-10s %-15s %10.5f %11.5f %3d %7.2f\n",
            qcfptr->qnet,  qcfptr->statn,
            qcfptr->lat,   qcfptr->lon,
            qcfptr->occur, qcfptr->staelv);
#endif

   fscanf( in_stream, "%7f%1c%1c%1c%7f%1c%1c%1c%7f%1c%1c%1c",
            &qcfptr->staprs, &junk[0], &qcfptr->staflg, &junk[1],
            &qcfptr->seaprs, &junk[2], &qcfptr->seaflg, &junk[3],
            &qcfptr->cmpsea, &junk[4], &qcfptr->cmpflg, &junk[5] );

   fscanf( in_stream, "%7f%1c%1c%1c%7f%1c%1c%1c%7f%1c%1c%1c%7f%1c%1c%1c",
            &qcfptr->temp,   &junk[0], &qcfptr->tmpflg, &junk[1],
            &qcfptr->dewpnt, &junk[2], &qcfptr->dewflg, &junk[3],
            &qcfptr->wndspd, &junk[4], &qcfptr->spdflg, &junk[5],
            &qcfptr->wnddir, &junk[6], &qcfptr->dirflg, &junk[7] );

   fscanf( in_stream, "%7f%1c%1c%1c%1c%1c%7f%1c%1c%1c%4d%1c%1c%1c%8f%1c%1c%1c",
            &qcfptr->precip, &junk[0], &qcfptr->prcflg, &junk[1],
            &qcfptr->sg,     &junk[2], &qcfptr->squall, &junk[3], 
            &qcfptr->sqlflg, &junk[4],
            &qcfptr->prswea, &junk[5], &qcfptr->pwflg,  &junk[6],
            &qcfptr->visib,  &junk[7], &qcfptr->visflg, &junk[8] );

 fscanf(in_stream,"%7f%1c%2d%1c%1c%1c%2d%1c%1c%1c%7f%1c%2d%1c%1c%1c%2d%1c%1c%1c%7f%1c%2d%1c%1c%1c%2d%1c%1c\n",
    &qcfptr->celht1, &junk[0], &qcfptr->celfg1, &junk[1],
    &qcfptr->c1flg,  &junk[2], &qcfptr->clamt1, &junk[3], &qcfptr->ca1flg, &junk[4],
    &qcfptr->celht2, &junk[5], &qcfptr->celfg2, &junk[6], &qcfptr->c2flg,  &junk[7],
    &qcfptr->clamt2, &junk[8], &qcfptr->ca2flg, &junk[9],
    &qcfptr->celht3, &junk[10], &qcfptr->celfg3, &junk[11],
    &qcfptr->c3flg,  &junk[12], &qcfptr->clamt3, &junk[13], &qcfptr->ca3flg);

    *input_stream = in_stream;

#if debug
   printf("%7.2f %c %7.2f %c %7.2f %c\n",
            qcfptr->staprs, qcfptr->staflg,
            qcfptr->seaprs, qcfptr->seaflg,
            qcfptr->cmpsea, qcfptr->cmpflg );

   printf("%7.2f %c %7.2f %c %7.2f %c %7.2f %c\n",
            qcfptr->temp,   qcfptr->tmpflg,
            qcfptr->dewpnt, qcfptr->dewflg,
            qcfptr->wndspd, qcfptr->spdflg,
            qcfptr->wnddir, qcfptr->dirflg );

   printf("%7.2f %c %c %7.2f %c %4d %c %8.2f %c\n",
            qcfptr->precip, qcfptr->prcflg,
            qcfptr->sg,     qcfptr->squall, qcfptr->sqlflg,
            qcfptr->prswea, qcfptr->pwflg,
            qcfptr->visib,  qcfptr->visflg );
 
   printf("%7.2f %2d %c %2d %c %7.2f %2d %c %2d %c %7.2f %2d %c %2d %c\n",
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
                 /*out*/ FILE   **opened_file )
   {
   *opened_file = fopen (path_name, mode);

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
void close_file (/*in*/ FILE   **opened_file)
   {
   if (fclose (*opened_file) == EOF)
     {
     fprintf (stderr, "Error: Can NOT close opened file.\n");
     exit(1);
     }
 
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
      * multiple words, strlen return should always be 15 and
      * should always compare complete string. During testing
      * older fns may have generated stn names shorter than
      * 15 chars.
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
