/*----------------------------------------------------------
 * date.h - Header file for date.c
 *
 *---------------------------------------------------------*/
#ifndef DATE_H
#define DATE_H

#ifdef __STDC__

   extern void date_to_julian(/*in*/  char date[7],
                              /*in*/  int  yr,   /* four places - 1994 */
                              /*out*/ int  *julian_date);

   extern void julian_to_date(/*in*/  int  julian_date,
                              /*in*/  int  yr,   /* four places - 1994 */
                              /*out*/ char date[7]);

   extern void date_to_YYYYJJJ(/*in*/  char date_YYMMDD[7],
                               /*in*/  int  yr,   /* four places - 1994 */
                               /*out*/ long int  *YYYYJJJ);

   extern void YYYYJJJ_to_date(/*in*/  long int   YYYYJJJ,
                               /*out*/ char  date_YYMMDD[7]);

   extern void convert_to_YYYYJJJ(/*in*/  int  julian_date,
                                  /*in*/  int  yr,   /* four places - 1994 */
                                  /*out*/ long int  *YYYYJJJ);

   extern long int increment_YYYYJJJ(/*in*/ long int  *YYYYJJJ);

   extern long int add_num_to_YYYYJJJ(/*in*/ long int  YYYYJJJ,
                                      /*in*/ int  num_to_add);

   extern int subtract_YYYYJJJ_from_YYYYJJJ(/*in*/ long int  begin_YYYYJJJ,
                                     /*in*/ long int  end_YYYYJJJ);

   extern long int subtract_num_from_YYYYJJJ(/*in*/ long int  YYYYJJJ,
                                             /*in*/ int  num_to_subtract);
#else /*!__STDC__*/

   extern void date_to_julian();
   extern void julian_to_date();
   extern void date_to_YYYYJJJ();
   extern void YYYYJJJ_to_date();
   extern void convert_to_YYYYJJJ();
   extern long int increment_YYYYJJJ();
   extern long int add_num_to_YYYYJJJ();
   extern int subtract_YYYYJJJ_from_YYYYJJJ();
   extern long int subtract_num_from_YYYYJJJ();

#endif /*__STDC__*/

#endif /* DATE_H */
