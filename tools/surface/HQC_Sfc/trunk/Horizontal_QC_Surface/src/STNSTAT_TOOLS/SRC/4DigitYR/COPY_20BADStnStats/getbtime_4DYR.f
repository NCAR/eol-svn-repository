C************************************************************************
C
C function: getbtime_4DYR()
C description: form a string of date and time
C
C              format of name is:  YYYYDDDHHMM
C
C                        where YYYY : year			(YYYY= 0-9999)
C                              DDD : julian day of year         (DDD= 1-366)
C                              HH : hour			(HH= 0-23)
C                              MM : minute			(MM= 0-59)
C
C returns:  the date/time in the above format
C 9 Sept 2002 lec
C   Update to handle 4 Digit Year. Corrected check for Leap Year.
C   It was not checking the century.
C************************************************************************

      character*11 function getbtime_4DYR(yr,mo,day,hr,min)

      implicit none

      include 'debug.h'

      integer yr,mo,day,hr,min,modays(12),mdays,i
      data modays /31,28,31,30,31,30,31,31,30,31,30,31/

      if (debug) write(0,*) 'getbtime_4DYR: yr, mo, day, hr, min:',
     +  yr,mo,day,hr,min

      mdays=0

      do i=1,mo-1
         mdays=mdays+modays(i)
      end do

      if (debug) write(0,*) 'getbtime_4DYR: mdays after loop:',mdays

C     Written in C the leap yr check is
C       if ((yr%4==0 && yr%100 !=0 || yr%400==0))
C            leap_yr = 1; 

      if(mo.gt.2 .and. 
     + (mod(yr,4).eq.0 .and. mod(yr,100).ne.0 .or.
     +  mod(yr,400).eq.0)) then		!allow for leap year
            if (debug) write(0,*) 'getbtime_4DYR: Leap Year!'
            mdays=mdays+1
      endif

      if (debug) write(0,*) 'getbtime_4DYR: yr, mdays_day, hr, min', 
     +            yr,mdays+day,hr,min

      write(getbtime_4DYR,101) yr,mdays+day,hr,min

      do i=1,11
         if(getbtime_4DYR(i:i) .eq. ' ') getbtime_4DYR(i:i)='0'
      end do

      return

101   format(i4,i3,i2,i2)
      end
