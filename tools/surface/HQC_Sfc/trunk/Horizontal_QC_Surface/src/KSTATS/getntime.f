C************************************************************************
C
C function: getntime()
C description: return the next hour from the one passed as a parameter
C
C              format of name is:  YYDDDHHMM
C
C                        where YY : year
C                             DDD : julian day of year
C                              HH : hour
C                              MM : minute  (always 00)
C returns:  the date/time in the above format
C************************************************************************
      character*9 function getntime(btime)

      implicit none

      character*9 btime  ! input fname
      character*2  min
      integer yr,hr,mdays,i

      read(btime,101) yr,mdays,hr,min ! parse input file name
101   format(i2,i3,i2,a2)

      hr=hr+1
      if(hr.gt.23) then
         hr=0
         mdays=mdays+1
         if(mod(yr,4).eq.0 .and. mdays.gt.366) then	!leap year
            mdays=1
            yr=yr+1
         else if(mdays .gt. 365) then			!non leap year
            mdays=1
            yr=yr+1
         endif
      endif

      write(getntime,101) yr,mdays,hr,"00"

      do i=1,9
         if(getntime(i:i) .eq. ' ') getntime(i:i)='0'
      end do
      return

      end
