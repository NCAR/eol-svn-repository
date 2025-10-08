      subroutine julian ( jdate, iyear, imonth, iday )
c
c     julian.f - given julian date and year, returns month and day
c                as integers
c
c
c     written by Ronald A. Murdock
c
c     creation date:  13May92
c
c     Input to subroutine:
c        jdate - integer julian date
c        iyear - integer year (last two digits only) 
c                (use 92 for 1992, etc.)

c     Returned by subroutine:
c        imonth - integer month number (1 = jan, 12 = dec, etc.)
c        iday   - integer day number (12 = may12, 31 = dec31, etc.)


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

      if ( jdate .lt. 1 .or. jdate .gt. imax ) then
c        bad julian date - notify use and bail out
         write(6, '('' error - bad Julian date of '',i5)' ) jdate
         write(6, 
     *   '('' Julian date must be between 1 and '',i3)' ) imax
         imonth = 0
         iday = 0
         goto 9999
      end if



      if ( leapyr ) then
         if ( jdate .ge. 1 .and. jdate .le. 31 ) then       !january
            imonth = 1
            iday   = jdate
         else if ( jdate .ge. 32 .and. jdate .le. 60 ) then !february
            imonth = 2
            iday   = jdate - 31
         else if ( jdate .ge. 61 .and. jdate .le. 91 ) then !march
            imonth = 3
            iday   = jdate - 60
         else if ( jdate .ge. 92 .and. jdate .le. 121 ) then !april
            imonth = 4
            iday   = jdate - 91
         else if ( jdate .ge. 122 .and. jdate .le. 152 ) then !may
            imonth = 5
            iday   = jdate - 121
         else if ( jdate .ge. 153 .and. jdate .le. 182 ) then !june
            imonth = 6
            iday   = jdate - 152
         else if ( jdate .ge. 183 .and. jdate .le. 213 ) then !july
            imonth = 7
            iday   = jdate - 182
         else if ( jdate .ge. 214 .and. jdate .le. 244 ) then !august
            imonth = 8
            iday   = jdate - 213
         else if ( jdate .ge. 245 .and. jdate .le. 274 ) then !september
            imonth = 9
            iday   = jdate - 244
         else if ( jdate .ge. 275 .and. jdate .le. 305 ) then !october
            imonth = 10
            iday   = jdate - 274
         else if ( jdate .ge. 306 .and. jdate .le. 335 ) then !november
            imonth = 11
            iday   = jdate - 305
         else !december
            imonth = 12
            iday   = jdate - 335
         end if

      else  !this is not a leap year
         if ( jdate .ge. 1 .and. jdate .le. 31 ) then       !january
            imonth = 1
            iday   = jdate
         else if ( jdate .ge. 32 .and. jdate .le. 59 ) then !february
            imonth = 2
            iday   = jdate - 31
         else if ( jdate .ge. 60 .and. jdate .le. 90 ) then !march
            imonth = 3
            iday   = jdate - 59
         else if ( jdate .ge. 91 .and. jdate .le. 120 ) then !april
            imonth = 4
            iday   = jdate - 90
         else if ( jdate .ge. 121 .and. jdate .le. 151 ) then !may
            imonth = 5
            iday   = jdate - 120
         else if ( jdate .ge. 152 .and. jdate .le. 181 ) then !june
            imonth = 6
            iday   = jdate - 151
         else if ( jdate .ge. 182 .and. jdate .le. 212 ) then !july
            imonth = 7
            iday   = jdate - 181
         else if ( jdate .ge. 213 .and. jdate .le. 243 ) then !august
            imonth = 8
            iday   = jdate - 212
         else if ( jdate .ge. 244 .and. jdate .le. 273 ) then !september
            imonth = 9
            iday   = jdate - 243
         else if ( jdate .ge. 274 .and. jdate .le. 304 ) then !october
            imonth = 10
            iday   = jdate - 273
         else if ( jdate .ge. 305 .and. jdate .le. 334 ) then !november
            imonth = 11
            iday   = jdate - 304
         else  !december
            imonth = 12
            iday   = jdate - 334
         end if

      end if

9999  return
      end
