      subroutine pwea(token, lentok, ipw, ios)

c     pwea.f - parses visibility/present weather/obstructions to vision token
c              from ASOS SAO observation and returns
c              a present weather code flag value
c
c     written by Ronald A. Murdock
c     April 15, 1992
c
c     modified by Lia Pennington
c     May 16, 1994
c
c     Input parameters:
c       token - string to be parsed
c       lentok - length of token
c       lognum - fortran unit number for log file
c
c    Returned parameters:
c       ipw   - integer code flag value for present weather
c       ios   - zero if everything OK, one if something wrong
c
c
c     LOG OF PROGRAM CHANGES
c
c     Creation date:  4/15/92
c
c
c     4/23/92 - commented out all messages to log file except errors

c     4/30/92 - added COMMON /BUGBLK/
c             - added calls to msg family of subroutines for debugging
c
c     04jun92 - added additional documentation on present weather codes
c               although some of that is not used inside pwea.f
c
c     16may94 - modified code to match only a substring of the present
c               weather string in case additional symbols were added
c               by an observer.
c
c*****!*****************************************************************
c23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
c*****!*****************************************************************
c

      character*1  blank ! one-byte blank
      character*10 varfmt !variable format string
      character*3  pwstr  !present weather string

      character*10 token !the character string containing visibility
c                         present weather, and obstructions to vision
c                         (this is what must be parsed)
c     character*1  char1 ! one byte generic character string
c     character*80 char80 !generic 80-byte general purpose string

      integer lentok  !length in bytes of token
      integer ipw     !code flag value for present weather
      integer ios     !zero if everything OK, one if not OK
      integer lognum  !fortran unit number for log file
      integer zero    !the value zero
      integer ibeg    !starting byte of present weather within token
      integer iend    !ending byte of present weather within token
      integer lenpw   !length of present weather string within token
      integer i


      logical debug1, debug2, debug3, screen  !debug level variables

C*****!****************************************************************!

      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!

      blank = ' '
      zero  = 0

      ios = zero


      call msg('pwea:entering pwea',3)
      call msg('pwea:token and lentok are on next 2 lines',3)
      call msg(token,3)
      call msgi('token length = ',3,lentok)

c     first check for a missing value
c     (this should never happen - vipr should catch this)
c     (just in case however)
c     token length would have to be one byte
c     and token would be 'M'
      if ( lentok .eq. 1 .and. token(1:1) .eq. 'M' ) then
c        this is a missing value flag
c        set values accordingly and exit pwea
         ipw = -999
         write(lognum, '('' pwea:missing value for token'')' )
         write(lognum, '('' pwea:vipr should have caught this'')' )
         write(6, '('' pwea:missing value for token'')' )
         write(6, '('' pwea:vipr should have caught this'')' )
         ios = 1
         goto 9999
      else
      end if




c     determine if last byte of token is an obstruction to vision flag
c     if so, reset the pointer used for the end of the token
      if ( token(lentok:lentok) .eq. 'F' .or. 
     *     token(lentok:lentok) .eq. 'H' ) then
         iend = lentok - 1
      else
         iend = lentok
      end if
c     write(lognum,
c    *'('' pwea: present weather string ends in byte number '',
c    *i2)' ) iend


c     find the first byte of a permissible present weather string

c     the list of possible present weather codes is listed below

c     present weather code           Code Value

c     P+  = heavy precip                142
c     P   = precip                      141
c     P-  = light precip                141
c     R+  = heavy rain                  163
c     R   = rain                        162
c     R-  = light rain                  161
c     ZR+ = heavy freezing rain         166
c     ZR  = freezing rain               165
c     ZR- = light freezing rain         161
c     S+  = heavy snow                  173
c     S   = snow                        172
c     S-  = light snow                  171

c     The following information is reported here for completeness,
c     but it is not used inside of pwea.f

c     Fog/Haze are reported only if no significant weather observed otherwise

c     Fog                               130

c     Haze (>= 1 km visibility)         104
c     Haze (<  1 km visibility)         105

c     No significant weather observed   100

c     if present weather is in this string, then it must start with
c     one of the following four letters - P, R, Z, or S
c     search from beginning of token thru iend looking for one of these
      do i = 1, iend
c        write(lognum,
c    *   '('' pwea: checking '',A1,
c    *   '' in token('',i1,'':'',i1,'')'' )' ) token(i:i), i, i

         if ( token(i:i) .eq. 'P' .or. token(i:i) .eq. 'R' 
     *   .or. token(i:i) .eq. 'Z' .or. token(i:i) .eq. 'S' ) then

c           found the start of a present weather string
c           set ibeg to this position
            ibeg = i
c           write(lognum, '('' pwea: present weather string starts '',
c    *      '' in byte number '',i1)' ) ibeg
            goto 1000
         end if

      end do

c     no present weather string found
c     set values accordingly and exit
      ipw = 100
      goto 9999


1000  continue

c     calculate the length of the present weather string
c     length is (last byte - first byte) + 1
c     length must be either 1,2, or 3

      lenpw = (iend - ibeg ) + 1
      if ( lenpw .ge. 1 .and. lenpw .le. 3 ) then
c        everything is OK
      else
         iend = ibeg + 2
         lenpw = (iend - ibeg ) + 1

c        wrong length - send warning message
c        call msg('pwea:wrong length for present weather string',0)
c        call msg('pwea:calculated length for string is on next line',0)
c        call msgi('pwea:calculated string length = ',0,lenpw)
c        call msg('pwea:token is on next line',0)
c        call msg(token,0)
c        ios = 1
c        goto 9999
      end if



c     read the present weather part of token into a separate string
c     this will make comparisons a bit easier

      pwstr = token(ibeg:iend)

c     first build a variable format string for the read statement
c     write(char80, '(I1)' ) lenpw
c     read (char80, '(A1)' ) char1
c     varfmt = '(A'//char1//')'
c     read ( token(ibeg:iend), varfmt, iostat = ios, 
c    *end = 8000, err = 8000 ) 
c     if ( ios .ne. 0 ) then
c       call msg('pwea:error reading token',0)
c       call msgi('pwea:read returned ios = ',0,ios)
c       goto 8000
c     end if


c     now we have the present weather string in pwstr
c     check for a match against the list

      if ( pwstr(1:2) .eq. 'R+' ) then
         ipw = 163
         goto 9999

      else if ( pwstr(1:2) .eq. 'R-' ) then
         ipw = 161
         goto 9999

      else if ( pwstr(1:1) .eq. 'R' ) then
         ipw = 162
         goto 9999

      else if ( pwstr(1:2) .eq. 'S+' ) then
         ipw = 173
         goto 9999

      else if ( pwstr(1:2) .eq. 'S-' ) then
         ipw = 171
         goto 9999

      else if ( pwstr(1:1) .eq. 'S' ) then
         ipw = 172
         goto 9999

      else if ( pwstr(1:3) .eq. 'ZR+' ) then
         ipw = 166
         goto 9999

      else if ( pwstr(1:3) .eq. 'ZR-' ) then
         ipw = 164
         goto 9999

      else if ( pwstr(1:2) .eq. 'ZR' ) then
         ipw = 165
         goto 9999

      else if ( pwstr(1:2) .eq. 'P+' ) then
         ipw = 142
         goto 9999

      else if ( pwstr(1:2) .eq. 'P-' ) then
         ipw = 141
         goto 9999

      else if ( pwstr(1:1) .eq. 'P' ) then
         ipw = 141
         goto 9999

      else
c        nothing matches - observer entered non-automated symbol
c        set to no-significant weather

         ipw = 100

         write(lognum, 
     *   '('' pwea: present weather string not on list'')' )
         write(lognum, '('' pwea: pwstr = '',A3)' ) pwstr
         write(lognum, '('' pwea: token = '',A10)' ) token
         write(*, 
     *   '('' pwea: present weather string not on list'')' )
         write(*, '('' pwea: pwstr = '',A3)' ) pwstr
         write(*, '('' pwea: token = '',A10)' ) token

         goto 9999
      end if


      goto 9999
c     skip over the error messages from the read statement


8000  continue
      write(lognum, '('' pwea: error reading pwstr'')' )
      write(lognum, '('' pwea: token = '',a10)' ) token
      write(lognum, '('' pwea: ibeg = '',i2)' ) ibeg
      write(lognum, '('' pwea: iend = '',i2)' ) iend
      write(lognum, '('' pwea: varfmt = '',A10)' ) varfmt
      write(6, '('' pwea: error reading pwstr'')' )
      write(6, '('' pwea: token = '',a10)' ) token
      write(6, '('' pwea: ibeg = '',i2)' ) ibeg
      write(6, '('' pwea: iend = '',i2)' ) iend
      write(6, '('' pwea: varfmt = '',A10)' ) varfmt
      ios = 1
      goto 9999



9999  continue
      call msg('pwea:leaving pwea now',3)
c     exit the subroutine now
      return
      end
