
            subroutine vis(token, lentok, visib, ios)

c     vis.f - parses visibility/present weather/obstructions-to-vision token
c              from ASOS SAO observation and returns
c              visibility in meters as a real number
c
c     written by Ronald A. Murdock
c     April 15, 1992
c
c     modified by Lia Pennington  April 28th, 1994
c
c    Input parameters:
c       token - string to be parsed
c       lentok - length of token
c
c    Returned parameters:
c       visib - visibility in meters as a real number
c       ios   - zero if everything OK, one if something wrong
c
c
c     LOG OF PROGRAM CHANGES
c
c     Creation date:  4/15/92
c
c     4/23/92 - commented out all messages to log file except errors

c     16jun92 - added bugblk common block and associated type statements
c             - added capability to handle '<.25' token (from Handar AWOS)
c             - added capability to handle decimal visibilities such as
c               3.5, 1.75, 0.5, 0.25  (from Handar AWOS)
c
c     28apr94 - added code to handle missing visibility when no 'M' 
c               is present.
c             - added function declaration.
c
c
c*****!*****************************************************************
c23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
c*****!*****************************************************************
c

      character*1  blank ! one-byte blank
      character*1  char1 !generic one-byte character variable
      character*5  vs ! holds visibility string
      character*80 char80 !generic 80-byte general purpose string
      character*10 varfmt !variable format string


      character*10 token !the character string containing visibility
c                         present weather, and obstructions to vision
c                         (this is what must be parsed)

      real    visib
      real    temp1, temp2, temp3, temp4

      integer lentok  !length in bytes of token

      integer ios     !zero if everything OK, one if not OK
      integer lognum  !fortran unit number for log file
      integer zero    !the value zero
      integer i

      integer lenvis   !length of visibility string within token
      integer islash  !location of slash within string
      integer ipoint  !location of decimal point within string

      logical debug1, debug2, debug3, screen  !debug level variables

C*****Declare functions used.
      real         cmi2me
      external     cmi2me

C*****!****************************************************************!

      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!



      blank = ' '
      zero  = 0
      vs = '     '  !five blanks
      ios = zero

c*****!*****************************************************************


      call msgc('vis:token = ',3,token,'a10')
      call msgi('vis:lentok= ',3,lentok)



c     first check for a missing value
c     (this should never happen - vipr should catch this)
c     (just in case however)
c     token length would have to be one byte
c     and token would be 'M'
      if ( lentok .eq. 1 .and. token(1:1) .eq. 'M' ) then
c        this is a missing value flag
c        set values accordingly and exit vis

         call msg('vis:error - missing value for token',0)
         call msg('vis:vipr should have caught this',0)

         visib = -999.99
         ios = 1
         goto 9999
      else
      end if

c*****!*****************************************************************

c     comments on the visibility part of token
c
c     (assuming 'M' has already been caught and processed)
c     assuming 'MM' has also been caught
c
c     first byte must be a digit or '<'
c     last byte must be a digit or '+'
c
c     other bytes must be digits or '/' or '.'
c
c     maximum length of visibility string appears to be 4 bytes
c     could be 5 bytes if '<31/2' ever appears although this
c     does not seem likely.  if it happens, this subroutine will
c     not be able to process it correctly anyway!
c     Allow for 5 bytes just in case
c
c     when moving through token from left to right on a byte-by-byte
c     basis, a blank or an alphabetic character signals that you are
c     outside of the visibility portion of the string
c

c*****!*****************************************************************

c     parse through token, left to right, putting each byte of the
c     visibility string into a one-byte array for ease in
c     subsequent processing
c
c     a blank or alphabetic character signals end of string
c
c     also determine the number of bytes in the visibility string

      do i = 1, lentok

c        check for blank
         if ( token(i:i) .eq. blank ) then   !end of string
c           write(lognum, '('' vis: detected blank'')' )
            lenvis = i - 1
c           write(lognum, '('' vis: lenvis = '',i2)' ) lenvis
            go to 1000
         else
         end if

c        check for upper case alphabetic character
         if ( ichar ( token(i:i) ) .ge. 65 .and.
     *        ichar ( token(i:i) ) .le. 90 ) then  !end of string
c           write(lognum, '('' vis: detected upper case letter'')' )
            lenvis = i - 1
c           write(lognum, '('' vis: lenvis = '',i2)' ) lenvis
            go to 1000
         else
         end if

         vs(i:i) = token(i:i) !otherwise copy visibility string into
c                              the vs variable
c        write(lognum,
c    *   '('' vis: copying byte number '',i1,'' from token to vs'')' ) i
c        write(lognum,
c    *   '('' vis: byte '',i1, '' = '',A1)' ) i, vs(i:i)
      end do


c*****!*****************************************************************
c
c     if you reach this statement, then computer did not find
c     a blank or an upper-case alphabetic character
c
c    in that case, the entire token is the visibility string
c
c    set lenvis to lentok

      lenvis = lentok

c
c     then copy token into vs
c
      do i = 1, lentok
        vs(i:i) = token(i:i)
      end do
      call msg('vis:set vs equal to token',3)
      call msgc('vis: vs = ',3,vs,'a5')
      call msgi('vis: lenvis = ',3,lenvis)

c*****!*****************************************************************


1000  continue

c     now have the length of the visibility string

c     a bit of debug info here....


      call msgc('vis: vs = ',3,vs,'a5')
      call msgi('vis: vs string length = ',3,lenvis)

c     If there is no visibility, then exit the program.
c     - lia
      if ( lenvis .eq. 0 ) then
          visib = -999.99
          goto 9999
      end if


c*****!*****************************************************************

c     verify that length of visibility string is 4 bytes or less
c     if not, warn user
c     program not prepared for greater length

      if ( lenvis .gt. 4 ) then
         call msg('vis: error - visibility string gt 4 bytes',0)
         call msgc('vis: token = ',0,token,'A10')

         ios = 1
         goto 9999
      end if

c*****!*****************************************************************

c     check the first byte for '<'
c     if found, check for '<1/4' string and process as a unit
c     if '<1/4' is not found, then try '<.25'

c     if '<' in first byte, but neither '<1/4' nor '<.25' is found,
c     then generate an error message.


c     code is not prepared for any other 'less than' string

      if ( vs(1:1) .eq. '<' ) then
c        visibility string starts with '<' - check what follows
         call msg('vis: string starts with <',3)

         if ( vs(1:4) .eq. '<1/4' ) then
c           visibility is less than one fourth
c           set visib to appropriate value in meters
            visib = 200.
            call msg('vis:visibility string is <1/4',3)
            goto 9999

         else if ( vs(1:4) .eq. '<.25' ) then
c           visibility is less than one fourth
c           set visib to appropriate value in meters
            visib = 200.
            call msg('vis:visibility string is <.25',3)
            goto 9999

         else
c           trouble! - some other number here
c           warn user and bail out

            call msg('vis:error-Unexpected visibility starting with <',
     *      0)
            call msg('vis: vis only handles <1/4 or <.25 strings',0)
            call msgc('vis: token = ',0,token,'A10')

            ios = 1
            goto 9999
         end if
      else
c        visibility string does not start with '<'
      end if

c*****!*****************************************************************

c     check the last byte of the visibility string for a plus sign
c     if '+' is found, then check for string '10+'
c     if '10+' is found, process as a unit
c
c     if '+' in last byte, but '10+' is not found, then generate
c     an error message.
c     code is not prepared for any other string ending with '+'

      if ( vs(lenvis:lenvis) .eq. '+' ) then
c        visibility string ends with '+' - check preceding bytes


c        quick check to prevent crashes
c        be sure that lenvis - 2 is greater than zero
         if ( (lenvis - 2 ) .ge. 1 ) then
c           proceed - everything is OK

         else
c           something wrong here - warn user
c           byte position (lenvis - 2) is not reachable in vs

            call msg('vis:error-unexpectedly short string ends with +',
     *      0)
            call msgc('vis: token = ',0,token,'A10')
            ios = 1
            goto 9999
         end if

         if ( vs( (lenvis -2):lenvis) .eq. '10+' ) then
c           visibility is ten plus miles
c           set visib to appropriate value in meters
            visib = 18000.
            call msg('vis:visibility is 10+ miles',3)
            goto 9999

         else
c           trouble! - some other number here
c           warn user and bail out

            call msg('vis:error-Unexpected visibility ends with +',0)
            call msg('vis: vis only handles 10+ string',0)
            call msgc('vis: token = ',0,token,'A10')
            ios = 1
            goto 9999
         end if

      else
c        visibility string does not end with '+'
      end if

c*****!*****************************************************************

c     at this point assume all strings with '+' at end or '<' at start
c     are taken care of - this only leaves whole numbers and/or fractions



c     check for string length of one byte
c     if so, then simply read the number

      if ( lenvis .eq. 1 ) then
         read ( vs(1:1), '(F1.0)', err=90 ) visib
         call msgr('vis: visibility in miles = ',3,visib,'F5.2')
         visib = cmi2me(visib)  !convert miles into meters
         goto 9999

90       ios = 1
         goto 9999
      else
c        not a single byte - requires more parsing
      end if

c*****!*****************************************************************

c     visibility is not a single byte - more parsing needed
c     visibility probably has a slash or a decimal point
c     (could also have two digits such as 10)

c     search through visibility looking for a slash
c     record the position if one is found

      islash = 0
      do i = 1, lenvis
         if ( vs(i:i) .eq. '/' ) islash = i !found slash position
      end do

c     search through visibility looking for a decimal point
c     record the position if one is found

      ipoint = 0
      do i = 1, lenvis
         if ( vs(i:i) .eq. '.' ) ipoint = i !found decimal point position
      end do

c*****!*****************************************************************

c     slash must be in byte number 2 or 3
c     nothing else is acceptable at this point

      if ( islash .ne. 0 ) then
         if ( islash .eq. 2 .or. islash .eq. 3 ) then
c           proceed - everything OK
         else
c           trouble - unexpected location for slash
c           warn user and bail out

            call msg('vis: error - expected slash in byte 2 or 3',0)
            call msgi('vis: byte location = ',0,islash)
            call msgc('vis: token = ',0,token,'A10')
            ios = 1
            goto 9999
         end if
      end if

c*****!*****************************************************************

c     if decimal point exists,
c     decimal point must be in byte number 2

      if ( ipoint .ne. 0 ) then
         if ( ipoint .eq. 2 ) then
c           proceed - everything OK
         else
c           trouble - unexpected location for decimal point
c           warn user and bail out

            call msg('vis: error - expected decimal point in byte 2',0)
            call msgi('vis: decimal point location = ',0,ipoint)
            call msgc('vis: token = ',0,token,'A10')
            ios = 1
            goto 9999
         end if
      end if
c*****!*****************************************************************
c     check if any slash was found
c     if no slash was found, build format and read number

c     this also works for the decimal point entries
c     even if decimal point was found, proceed
c     location of decimal point does not matter

      if ( islash .eq. zero ) then
         write ( char80, '(i1)' ) lenvis
         read  ( char80, '(A1)' ) char1
         varfmt = '(F'//char1//'.0)'
         read ( vs(1:lenvis), varfmt, err=91 ) visib
         visib = cmi2me(visib)
         goto 9999

91       ios = 1
         goto 9999
      end if

c*****!*****************************************************************

c     can only reach this section of subroutine if there is a slash
c     in the visibility number
c
c     verify that the length of the visibility string is equal to
c     the position of the slash plus one
c
c     in other words, there must only be one digit to the right of the
c     slash.  generate an error if this is not true.
c
c

      if ( lenvis .eq. (islash + 1 ) ) then
c        everything is OK
      else
c        trouble!  something is wrong
c        warn user and bail out

         call msg('vis:error-more than one digit to right of slash',0)
         call msgi('vis: length of visibility string = ',0,lenvis)
         call msgi('vis: slash located in byte number ',0,islash)
         call msg('vis: expected length = slash + 1',0)
         call msgc('vis: token = ',0,token,'a10')

         ios = 1
         goto 9999
      end if

c*****!*****************************************************************

c     at this point, we know there is a slash and only a single
c     digit follows the slash
c
c     read the bytes separately and do the required arithmetic
c     to compute the fraction of miles visibility

      read ( vs ( (islash-1):(islash-1) ), '(F1.0)' ) temp1
      read ( vs ( (islash+1):(islash+1) ), '(f1.0)' ) temp2
      temp3 = temp1 / temp2  !this is the fraction
      call msgr('vis: fractional mile computed as ',3,temp3,'f4.2')

c*****!*****************************************************************

c     now check for any whole number preceding the fraction
c     if one is found, read it and add it to the fractional amount
c
c     there can only be a maximum of one digit preceding the fraction
c
c     therefore if the position of the slash is in byte 3, then
c     there is an integer preceding the fraction
c
c     if the slash is in byte 2, there is no preceding integer
c
c     no other possibility exists at this point in the code

      if ( islash .eq. 3 ) then
         read ( vs (1:1), '(F1.0)', err=92 ) temp4
         visib = temp4 + temp3
         call msgr('vis: visibility in miles = ',3,visib,'F5.2')
         visib = cmi2me(visib)
         goto 9999

92       ios = 1
         goto 9999
      else
         visib = temp3
         call msgr('vis: visibility in miles = ',3,visib,'F5.2')
         visib = cmi2me(visib)
         goto 9999
      end if

c*****!*****************************************************************



9999  continue
c     write(lognum, '('' vis: Leaving vis now'')' )
c     exit the subroutine now
      return
      end
