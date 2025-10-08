C************************************************************************
C
C function: getbtime()
C description: form a string of date and time
C
C              format of name is:  YYDDDHHMM
C
C                        where YY : year			(YY= 0-99)
C                             DDD : julian day of year (DDD= 1-366)
C                              HH : hour			(HH= 0-23)
C                              MM : minute			(MM= 0-59)
C
C returns:  the date/time in the above format
C************************************************************************

      character*9 function getbtime(yr,mo,day,hr,min)

      implicit none

      integer yr,mo,day,hr,min,modays(12),mdays,i
      data modays /31,28,31,30,31,30,31,31,30,31,30,31/

      mdays=0

      do i=1,mo-1
         mdays=mdays+modays(i)
      end do

      if(mo.gt.2 .and. mod(yr,4).eq.0) then		!allow for leap year
            mdays=mdays+1
      endif

      write(getbtime,101) yr,mdays+day,hr,min

      do i=1,9
         if(getbtime(i:i) .eq. ' ') getbtime(i:i)='0'
      end do

      return

101   format(i2,i3,i2,i2)
      end
