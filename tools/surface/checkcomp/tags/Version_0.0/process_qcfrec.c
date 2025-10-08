/*-------------------------------------------------------
 * process_qcfrec.c - module containing functions to 
 *    process qcf records.
 *
 *-------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
 
#include "qcfrec.h"
#include "process_qcfrec.h"

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

   strncpy( qcfptr->qnet, "          \0",10);
   strncpy( qcfptr->statn,"               \0",15);

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

   fprintf( output_stream, "%2.2d/%2.2d/%2.2d %2.2d:%2.2d ", 
            qcfptr->year_nom, qcfptr->month_nom, qcfptr->day_nom,
            qcfptr->hour_nom, qcfptr->minute_nom );

   fprintf( output_stream, "%2.2d/%2.2d/%2.2d %2.2d:%2.2d ", 
            qcfptr->year, qcfptr->month, qcfptr->day,
            qcfptr->hour, qcfptr->minute );
 
   fprintf( output_stream, "%-10s %-15s %10.5f %11.5f %3d %7.2f ", 
            qcfptr->qnet,  qcfptr->statn, 
            qcfptr->lat,   qcfptr->lon, 
            qcfptr->occur, qcfptr->staelv);

   fprintf( output_stream, "%7.2f %c %7.2f %c %7.2f %c ", 
            qcfptr->staprs, qcfptr->staflg, 
            qcfptr->seaprs, qcfptr->seaflg,
            qcfptr->cmpsea, qcfptr->cmpflg );

   fprintf( output_stream, "%7.2f %c %7.2f %c %7.2f %c %7.2f %c ", 
            qcfptr->temp,   qcfptr->tmpflg, 
            qcfptr->dewpnt, qcfptr->dewflg,
            qcfptr->wndspd, qcfptr->spdflg,
            qcfptr->wnddir, qcfptr->dirflg );

   fprintf( output_stream, "%7.2f %c %c %7.2f %c %4d %c %8.2f %c ", 
            qcfptr->precip, qcfptr->prcflg,
            qcfptr->sg,     qcfptr->squall, qcfptr->sqlflg,
            qcfptr->prswea, qcfptr->pwflg,
            qcfptr->visib,  qcfptr->visflg );

   fprintf(output_stream,"%7.2f %2d %c %2d %c %7.2f %2d %c %2d %c %7.2f %2d %c %2d %c\n", 
    qcfptr->celht1, qcfptr->celfg1, qcfptr->c1flg, qcfptr->clamt1, qcfptr->ca1flg,
    qcfptr->celht2, qcfptr->celfg2, qcfptr->c2flg, qcfptr->clamt2, qcfptr->ca2flg,
    qcfptr->celht3, qcfptr->celfg3, qcfptr->c3flg, qcfptr->clamt3, qcfptr->ca3flg);

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

/*was:   fscanf( in_stream, "%10s%1c%15s%1c%10f%1c%11f%1c%3d%1c%7f%1c",
            qcfptr->qnet, &junk[0], qcfptr->statn, &junk[1],
            &qcfptr->lat,  &junk[2], &qcfptr->lon,  &junk[3],
            &qcfptr->occur, &junk[4], &qcfptr->staelv, &junk[5]);
*/

   for (ii=0;ii<10;ii++)
      qcfptr->qnet[ii] = fgetc (in_stream);

   qcfptr->qnet[10] = '\0';

   junk[0] = fgetc(in_stream);

   for (ii=0;ii<15;ii++) 
      qcfptr->statn[ii] = fgetc (in_stream);   

   qcfptr->statn[15] = '\0'; 

   junk[0] = fgetc(in_stream);

   fscanf( in_stream, "%10f%1c%11f%1c%3d%1c%7f%1c",
            &qcfptr->lat,  &junk[2], &qcfptr->lon,  &junk[3],
            &qcfptr->occur, &junk[4], &qcfptr->staelv, &junk[5]);

/*-------------
   printf("%-10s %-15s %10.5f %11.5f %3d %7.2f\n",
            qcfptr->qnet,  qcfptr->statn,
            qcfptr->lat,   qcfptr->lon,
            qcfptr->occur, qcfptr->staelv);

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

 -------- */

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

/*----
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
 
----*/

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
