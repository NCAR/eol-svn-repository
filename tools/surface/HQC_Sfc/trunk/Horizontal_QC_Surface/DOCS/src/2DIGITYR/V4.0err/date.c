/*-------------------------------------------------------
 * date - This module contains the functions to handle
 *   date conversions. Where Y indicates year, M indicates
 *   month, D indicates day, and J indicates Julian day
 *   in the naming conventions.
 *
 * circa 1994 lec
 *   Created.
 * 04 Mar 97 lec
 *   Corrected incorrect access to junk[3] variable. Was
 *   accessing 1 element beyond the size allotted. Removed
 *   a \0 from each initialization. These should be added
 *   automatically by C/C++. Literature indicates this
 *   is true.
 * 04 Mar 97 lec
 *   Added include for string.h and stdio.h.
 *-------------------------------------------------------*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "date.h"

#define DEBUG  0

/*-------------------------------------------------------
 * date_to_julian - convert date string to julian date.
 *                  (YYMMDD to JJJ)
 *-------------------------------------------------------*/
void date_to_julian(/*in*/  char date[7], /* YYMMDD */
                    /*in*/  int  yr,      /* four places 1994*/
                    /*out*/ int  *julian_date)
   {   
   int   leap_yr = 0;
   int   mo = 0;
   int   day = 0;
   char  junk[3]= "\0\0";  /* This var has positions 0,1,2 (not 3) */

   static int prev_days[12] = {0,   31, 59, 90,120,151,
                               181,212,243,273,304,334};

   /*
    * Pick out mo, day out of input date string. 
    */
   strncpy (junk, &date[2],2); 
   junk[2] = '\0';
   mo = atoi (junk); 

   strncpy (junk, &date[4],2); 
   junk[2] = '\0';
   day = atoi (junk); 

   /*
    * Compute Julian Day and return.
    */
   switch (mo)  {
      case 1:
         *julian_date = day;
         break;

      case 2:
         *julian_date = day + prev_days[1];
         break;

      case 3:
      case 4:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
      case 10:
      case 11:
      case 12:
         /*
          * Determine if Leap year.
          */
         if ((yr%4==0 && yr%100 !=0 || yr%400==0))
            leap_yr = 1;

         *julian_date = day + prev_days[mo-1] + leap_yr;

         break;

      default:
         perror ("Bad month in date_to_julian()"); 
         break;

      } /* switch */

   } /* date_to_julian() */

/*-------------------------------------------------------
 * julian_to_date - convert julian_date to date string.
 *                  (JJJ to YYMMDD)
 *-------------------------------------------------------*/
void julian_to_date(/*in*/  int  julian_date,
                    /*in*/  int  yr,   /* four places - 1994 */
                    /*out*/ char date[7])
   {
   int   leap_yr = 0;
   int   mo, day = 0;

   char  year[5]= "\0\0\0\0";

   static int prev_days[12] = {0,   31, 59, 90,120,151,
                               181,212,243,273,304,334};

   if (julian_date >366)
      {
      printf ("Error: Julian date > 366!\n");
      exit(1);
      }

   /*
    * Determine if Leap year.
    */
   if ((yr%4==0 && yr%100 !=0 || yr%400==0) )
      leap_yr = 1;


   /*
    * Determine the month.
    */
   if (julian_date < 32)
      {
      mo = 1;
      day = julian_date;
      }
   else if (julian_date <60)
      {
      mo = 2;
      day = julian_date - 31;
      }
   else if (leap_yr && julian_date ==60)
      {
      mo = 2;
      day = 29;
      }
   else
      {
      for (mo=2; mo<12; mo++)   /* start at mar */
         if (julian_date <= prev_days[mo]+leap_yr) break;


      /*
       * Determine day.
       */
      if (leap_yr && mo>1 && julian_date >59)
         day = julian_date - prev_days[mo-1]-leap_yr;
      else
         day = julian_date - prev_days[mo-1];
   }

   /*
    * Form date string.
    */
   sprintf( year, "%d", yr);
   
   sprintf (date, "%c%c%02d%02d\0", year[2], year[3], mo, day);
 
   } /* julian_to_date() */


/*-------------------------------------------------------
 * date_to_YYYYJJJ() - converts YYMMDD format to
 *   internal "YYYYJJJ" integer format: YYYYJJJ.
 *   (YYMMDD to YYYYJJJ)
 *-------------------------------------------------------*/
void date_to_YYYYJJJ(/*in*/  char date_YYMMDD[7],
                     /*in*/  int  yr,   /* four places - 1994 */
                     /*out*/ long int  *YYYYJJJ)
   {
   int    date_JJJ;

   date_to_julian (date_YYMMDD, yr, &date_JJJ);
   convert_to_YYYYJJJ (date_JJJ, yr, YYYYJJJ);

   } /* date_to_YYYYJJJ() */

/*------------------------------------------------------- 
 * YYYYJJJ_to_date() - converts internal YYYYJJJ format
 *   to YYMMDD format. (YYYYJJJ to YYMMDD)
 *-------------------------------------------------------*/ 
void YYYYJJJ_to_date(/*in*/  long int   YYYYJJJ,
                     /*out*/ char  date_YYMMDD[7])
   {
   char  YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";
   char  JJJ_str[4] = "\0\0\0";
   char  YYYY_str[5] = "\0\0\0\0";

   int   JJJ = 0;  /* Julian day */
   int   YYYY = 0; /* Year       */

    /*--------------------------------------------
     * Parse out julian day and year from YYYYJJJ.
     *-------------------------------------------*/
    sprintf (YYYYJJJ_str, "%ld", YYYYJJJ);
    strncpy (JJJ_str, &YYYYJJJ_str[4], 3);
    JJJ_str[3] = '\0';
    JJJ = atoi( JJJ_str );
 
    strncpy (YYYY_str, &YYYYJJJ_str[0], 4);
    YYYY_str[4] = '\0';
    YYYY = atoi( YYYY_str );
 
    julian_to_date (JJJ, YYYY, date_YYMMDD); /* form YYMMDD date */

   } /* YYYYJJJ_to_date() */ 


/*-------------------------------------------------------
 * convert_to_YYYYJJJ() - converts 4 char yr and julian
 *   date to internal "YYYYJJJ" integer format: YYYYJJJ
 *   This integer dates are easily incremented through a
 *   year and compared to see what dates are later than
 *   others.
 *-------------------------------------------------------*/
void convert_to_YYYYJJJ(/*in*/  int  julian_date,
                        /*in*/  int  yr,   /* four places - 1994 */
                        /*out*/ long int  *YYYYJJJ)
   {
   char  YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";

   sprintf (YYYYJJJ_str, "%04d%03d\0", yr, julian_date);

   *YYYYJJJ = atol (YYYYJJJ_str);

#if DEBUG
   printf ("convert(): YYYYJJJ_str, YYYYJJJ:: xxx%-sxxx, %ld\n",
            YYYYJJJ_str, *YYYYJJJ);
#endif   
   } /* convert_to_YYYYJJJ() */

/*-------------------------------------------------------
 * increment_YYYYJJJ() - converts integer yr and julian
 *   date to internal "YYYYJJJ" integer format: YYYYJJJ
 *   These integer dates are easily incremented through a
 *   year and compared to see what dates are later than 
 *   others. 
 *-------------------------------------------------------*/
long int increment_YYYYJJJ(/*in*/ long int  *YYYYJJJ)
   {
   int   leap_yr = 0;

   char  year_str[5]= "\0\0\0\0";
   int   year = 0;

   char  julian_date_str[4]= "\0\0\0";
   int   julian_date = 0;

   char  YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";


   /*
    * Pick out the year and julian date.
    */
   sprintf (YYYYJJJ_str, "%ld\0", *YYYYJJJ);

   strncpy (year_str, YYYYJJJ_str, 4);
   year_str[4] = '\0';
   year = atoi(year_str);

   strncpy (julian_date_str, &YYYYJJJ_str[4], 3);
   julian_date_str[3] = '\0';
   julian_date = atoi(julian_date_str);

#if DEBUG
   printf ("(increment) YYYYJJJ, year, julian_date:: %ld %d %d\n",
            *YYYYJJJ, year, julian_date);
#endif

   /*
    * Determine if Leap year.
    */
   if ((year%4==0 && year%100 !=0 || year%400==0) )
      leap_yr = 1;

   julian_date++;

#if DEBUG
   printf ("(before rollover) year, julian_date, leap_yr: %d %d %d\n",
           year, julian_date, leap_yr);
#endif

   if (!leap_yr && julian_date >365)
      {
#if DEBUG
      printf ("rollover into next year!!!!!\n");
#endif
      julian_date = julian_date - 365;
      year++;
      }
 
   if (leap_yr && julian_date > 366)
      {
#if DEBUG
      printf ("rollover into next year from LEAP yr!!!!!\n");
#endif
      julian_date = julian_date - 366;
      year++;
      }

   sprintf (YYYYJJJ_str, "%04d%03d\0", year, julian_date); 

#if DEBUG
   printf ("(increment)YYYYJJJ_str, atol(YYYYJJJ_str):: xxx%-sxxx, %ld\n",
            YYYYJJJ_str, atol(YYYYJJJ_str));
#endif

   *YYYYJJJ = atol(YYYYJJJ_str);
 
   return  *YYYYJJJ;
 
   } /* increment_YYYYJJJ() */

/*-------------------------------------------------------
 * add_num_to_YYYYJJJ() - adds integer number to input
 *   YYYYJJJ ("YYYYJJJ" integer format: YYYYJJJ) and
 *   yields another YYYYJJJ.
 *-------------------------------------------------------*/
long int add_num_to_YYYYJJJ(/*in*/ long int  YYYYJJJ,
                            /*in*/ int  num_to_add)
   {
   int   leap_yr = 0;
 
   char  year_str[5]= "\0\0\0\0";
   int   year = 0;
 
   char  julian_date_str[4]= "\0\0\0";
   int   julian_date = 0;
 
   char  YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";
 
 
   /*
    * Pick out the year and julian date.
    */
   sprintf (YYYYJJJ_str, "%0ld\0", YYYYJJJ);
 
   strncpy (year_str, YYYYJJJ_str,4);
   year_str[4] = '\0';
   year = atoi(year_str);
 
   strncpy (julian_date_str, &YYYYJJJ_str[4],3);
   julian_date_str[3] = '\0';
   julian_date = atoi(julian_date_str);

#if DEBUG
   printf ("(add) YYYYJJJ, year, julian_date, num_to_add:: %ld %d %d %d\n",
            YYYYJJJ, year, julian_date, num_to_add);
#endif
 
   /*
    * Determine if Leap year.
    */
   if ((year%4==0 && year%100 !=0 || year%400==0) )
      leap_yr = 1;

   julian_date = julian_date + num_to_add;

   if (!leap_yr && julian_date >365)
      {  
#if DEBUG
      printf ("rollover into next year!!!!!\n");
#endif
      julian_date = julian_date - 365;
      year++;
      }

   if (leap_yr && julian_date > 366)
      {
#if DEBUG
      printf ("rollover into next year from LEAP yr!!!!!\n");
#endif
      julian_date = julian_date - 366;
      year++;
      }

   sprintf (YYYYJJJ_str, "%04d%03d\0", year, julian_date);

#if DEBUG
   printf ("(end of add) YYYYJJJ_str, atol(YYYYJJJ_str):: xxx%-sxxx, %ld\n",
            YYYYJJJ_str, atol(YYYYJJJ_str));
#endif

   return  atol (YYYYJJJ_str);

   } /* add_num_to_YYYYJJJ() */



/*-------------------------------------------------------
 * subtract_YYYYJJJ_from_YYYYJJJ() - subtracts two dates
 *   and determine the number of days between the two.
 *   Yields an integer number.
 *
 * 11 March 99 lec
 *   Created.
 *-------------------------------------------------------*/
int subtract_YYYYJJJ_from_YYYYJJJ(/*in*/ long int  begin_YYYYJJJ,
                                  /*in*/ long int  end_YYYYJJJ)
   {
   char  begin_year_str[5]= "\0\0\0\0";
   int   begin_year = 0;
   char  begin_julian_date_str[4]= "\0\0\0";
   int   begin_julian_date = 0;

   char  end_year_str[5]= "\0\0\0\0";
   int   end_year = 0;
   char  end_julian_date_str[4]= "\0\0\0";
   int   end_julian_date = 0;

   char  begin_YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";
   char  end_YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";

   int   days_in_begin_year=365;  
   int   days_in_end_year=365;  

   int   diff = 0;

   /*
    * Pick out the years and julian dates.
    */
   sprintf (begin_YYYYJJJ_str, "%ld\0", begin_YYYYJJJ);
   sprintf (end_YYYYJJJ_str, "%ld\0", end_YYYYJJJ);
 
   strncpy (begin_year_str, begin_YYYYJJJ_str,4);
   begin_year_str[4] = '\0';
   begin_year = atoi(begin_year_str);

   strncpy (end_year_str, end_YYYYJJJ_str,4);
   end_year_str[4] = '\0'; 
   end_year = atoi(end_year_str);  

   strncpy (begin_julian_date_str, &begin_YYYYJJJ_str[4],3);
   begin_julian_date_str[3] = '\0';
   begin_julian_date = atoi(begin_julian_date_str);

   strncpy (end_julian_date_str, &end_YYYYJJJ_str[4],3); 
   end_julian_date_str[3] = '\0'; 
   end_julian_date = atoi(end_julian_date_str); 
 
#if DEBUG 
   printf ("(subtract two) begin: YYYYJJJ, year, julian_date:: %ld %d %d\n",
            begin_YYYYJJJ, begin_year, begin_julian_date);
   printf ("(subtract two) end: YYYYJJJ, year, julian_date:: %ld %d %d\n",
            end_YYYYJJJ, end_year, end_julian_date);   
#endif

   /*-------------------------------------------
    * If years match just determine number
    * days between dates. Else more complicated
    *------------------------------------------*/
   if (begin_year == end_year)
      {
      diff = (end_julian_date - begin_julian_date)+1;
      return (diff);
      }
   else
      {
      if ((begin_year%4==0 && begin_year%100 !=0 || begin_year%400==0) )
          days_in_begin_year++;

      diff = (days_in_begin_year - begin_julian_date)+1 + end_julian_date;
      }
 
#if DEBUG
   printf ("(subtract two) diff:: %d\n", diff);
#endif
 
   return (diff);
   }


/*-------------------------------------------------------
 * subtract_num_from_YYYYJJJ() - subtracts integer number
 *   from input YYYYJJJ ("YYYYJJJ" integer format: YYYYJJJ)
 *   and yields another YYYYJJJ.
 *
 * 4 Dec 98 lec
 *   Updated to correctly handle rollback from Jan 1 to
 *   previous year.
 *-------------------------------------------------------*/
long int subtract_num_from_YYYYJJJ(/*in*/ long int  YYYYJJJ,
                                   /*in*/ int  num_to_subtract)
   {
   int   leap_yr = 0;
   int   prev_yr = 0;

   char  year_str[5]= "\0\0\0\0";
   int   year = 0;

   char  julian_date_str[4]= "\0\0\0";
   int   julian_date = 0;

   char  YYYYJJJ_str[8] = "\0\0\0\0\0\0\0";


   /*
    * Pick out the year and julian date.
    */
   sprintf (YYYYJJJ_str, "%ld\0", YYYYJJJ);

   strncpy (year_str, YYYYJJJ_str,4);
   year_str[4] = '\0';
   year = atoi(year_str);
 
   strncpy (julian_date_str, &YYYYJJJ_str[4],3);
   julian_date_str[3] = '\0';
   julian_date = atoi(julian_date_str);

#if DEBUG 
   printf ("(subtract) YYYYJJJ, year, num_to_subtract, julian_date:: %ld %d %d %d\n",
            YYYYJJJ, year, num_to_subtract, julian_date);
#endif
 
   /*
    * Determine if previous year was Leap year.
    */
   prev_yr = year-1;

   if ((prev_yr%4==0 && prev_yr%100 !=0 || prev_yr%400==0) )
      leap_yr = 1;

#if DEBUG
   printf ("(subtract1) YYYYJJJ, year, num_to_subtract, julian_date:: %ld %d %d %d\n",
            YYYYJJJ, year, num_to_subtract, julian_date);
#endif
 
 
   julian_date = julian_date - num_to_subtract;

#if DEBUG
   printf ("(subtract2) YYYYJJJ, year, num_to_subtract ,julian_date:: %ld %d %d %d\n",
            YYYYJJJ, year, num_to_subtract, julian_date);
#endif
 
#if DEBUG
   printf ("julian_date <= 0?\n");
#endif

   if (julian_date <= 0)
      {
#if DEBUG
      printf ("Roll back to previous year during subtract\n");
#endif

      year--;

      if (!leap_yr) 
         julian_date = julian_date + 365;
      else
         julian_date = julian_date + 366;
      }  
 
   sprintf (YYYYJJJ_str, "%04d%03d\0", year, julian_date);

#if DEBUG 
   printf ("(end of subtract) YYYYJJJ_str, atol(YYYYJJJ_str), year,julian_date:: xxx%-sxxx,  %ld %d %d\n",
            YYYYJJJ_str, atol(YYYYJJJ_str), year, julian_date);
#endif 
   return  atol (YYYYJJJ_str);

   } /* subtract_num_from_YYYYJJJ() */
