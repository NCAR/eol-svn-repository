#include "stdio.h"
#include "date.h"
main()
{
   int           current_yr   = 0;
   int           current_date = 0; /* Julian date */

   char          date[7]      = "\0\0\0\0\0\0\0";
   int           jdate = 0;
   int           year=0;


        for(;;)
           {
	   printf("Enter current_yr (neg to quit):\n");
   	   scanf("%d", &current_yr);
           if (current_yr <0) break;

           printf("Enter current_date (julian):\n"); 
           scanf("%d", &current_date);
 
           printf ("Current year is: %d\n", current_yr);
           printf ("Current julian date is: %d\n", current_date);

           julian_to_date (current_date, current_yr, date);

           printf ("Converted date is: xxx%-sxxx\n", date);

           date_to_julian (date, current_yr, &jdate);
           printf ("Converted back to julian: %d\n", jdate);

           } /*for */
}

