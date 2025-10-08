      subroutine juliancvt ( iyear, imonth, iday, jdate)

c--------------------------------------------------------------------
c
c     juliancvt.f - convert date given to julian day 
c
c
c     written by Lia Pennington
c       based on julian.f written by Ronald A. Murdock
c
c     creation date:  17 June 1994
c
c     Input to subroutine:
c        iyear - integer year (last two digits only) 
c                (use 92 for 1992, etc.)
c        imonth - integer month number (1 = jan, 12 = dec, etc.)
c        iday   - integer day number (12 = may12, 31 = dec31, etc.)

c     Returned by subroutine:
c        jdate - integer julian date
c--------------------------------------------------------------------

      integer jdate, iyear, imonth, iday, imax
      integer itest
      logical leapyr

      leapyr = .false.

      itest = mod(iyear,4)
      if ( itest .eq. 0 ) leapyr = .true.

      if ( leapyr ) then
         imax = 366
      else
         imax = 365
      end if


      if ( leapyr ) then
         if ( imonth .eq. 1) then !January
            jdate = iday
         else if ( imonth .eq. 2 ) then !february
            jdate  = iday + 31
         else if ( imonth .eq. 3 ) then !march
            jdate  = iday + 60
         else if ( imonth .eq. 4 ) then !april
            jdate  = iday + 91
         else if ( imonth .eq. 5 ) then !may
            jdate  = iday + 121
         else if ( imonth .eq. 6 ) then !june
            jdate  = iday + 152
         else if ( imonth .eq. 7 ) then !july
            jdate  = iday + 182
         else if ( imonth .eq. 8 ) then !august
            jdate  = iday + 213
         else if ( imonth .eq. 9 ) then !september
            jdate  = iday + 244
         else if ( imonth .eq. 10 ) then !october
            jdate  = iday + 274
         else if ( imonth .eq. 11 ) then !november
            jdate  = iday + 305
         else if ( imonth .eq. 12 ) then !december
            jdate  = iday + 335
         end if

      else  !this is not a leap year
         if ( imonth .eq. 1) then !January
            jdate = iday
         else if ( imonth .eq. 2 ) then !february
            jdate  = iday + 31
         else if ( imonth .eq. 3 ) then !march
            jdate  = iday + 59
         else if ( imonth .eq. 4 ) then !april
            jdate  = iday + 90
         else if ( imonth .eq. 5 ) then !may
            jdate  = iday + 120
         else if ( imonth .eq. 6 ) then !june
            jdate  = iday + 151
         else if ( imonth .eq. 7 ) then !july
            jdate  = iday + 181
         else if ( imonth .eq. 8 ) then !august
            jdate  = iday + 212
         else if ( imonth .eq. 9 ) then !september
            jdate  = iday + 243
         else if ( imonth .eq. 10 ) then !october
            jdate  = iday + 273
         else if ( imonth .eq. 11 ) then !november
            jdate  = iday + 304
         else if ( imonth .eq. 12 ) then !december
            jdate  = iday + 334
         end if

      end if

9999  return
      end
