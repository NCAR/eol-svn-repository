      subroutine intchk (char,len,flag,ios)
      character*20 char
      integer len
      integer ios
      integer i
      logical flag

      ios = 0

      if ( len .lt. 1 .or. len .gt. 20 ) then
         write(6, '('' intchk: problem in length of string'')' )
         write(6,
     *   '('' intchk: string length must be >= 1 and <= 20'')' )
         write(6, '('' intchk: string length is '',i3)' ) len
         flag = .false.
         ios = 1
         goto 9999

      end if

      do i = 1, len
         if ( ichar ( char(i:i)) .ge. 48 .and.
     *        ichar ( char(i:i)) .le. 57 ) then
            flag = .true.
         else
            flag = .false.
            goto 9999
         end if
      end do

9999  continue
      return
      end
