      subroutine vipr(token, lentok, visib, ipw, ios)

c     vipr.f - parses visibility/present weather/obstructions to vision
c              from ASOS SAO observation and returns visibility in
c              meters and a present weather code flag value
c
c     written by Ronald A. Murdock
c
c     April 15, 1992
c
c     Input parameters:
c       token - string to be parsed
c       lentok - length of token

c
c    Returned parameters:
c       visib - real number visibility in meters
c
c       ipw   - integer code flag value for present weather
c       ios   - zero if everything OK, one if something wrong
c
c
c     LOG OF PROGRAM CHANGES
c
c     Creation date:  4/15/92
c
c     4/23/92 - commented out all messages being written to log file

c     4/30/92 - added common /BUGBLK/
c             - added calls to msg family of subroutines for debugging
c             - added code to handle tokens with missing visibility
c               such as MR-
c
c*****!*****************************************************************
c23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
c*****!*****************************************************************
c

      character*1  blank ! one-byte blank
      character*1  ob !returned value from subroutine blocker
c                          (F, H, or blank)

      character*10 token !the character string containing visibility
c                         present weather, and obstructions to vision
c                         (this is what must be parsed)

      integer lentok  !length in bytes of token
      integer ipw     !code flag value for present weather
      integer ios     !zero if everything OK, one if not OK
      integer lognum  !fortran unit number for log file
      integer zero    !the value zero

      real visib  !visibility in meters

      logical debug1, debug2, debug3, screen  !debug level variables

C*****!****************************************************************!

      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!


      blank = ' '
      zero  = 0
      ios = zero



c     write(lognum, '('' vipr: Entering vipr....'')' )
      call msg('vipr: entering vipr',3)



C*****!****************************************************************!
c     first check for a missing value
c     token length would have to be one byte
c     and token would be 'M'
      if ( lentok .eq. 1 .and. token(1:1) .eq. 'M' ) then
c        this is a missing value flag
c        set values accordingly and exit vipr
         visib = -999.99
         ipw = -999
         call msg('vipr:missing value for token',3)
         goto 9999
      else
      end if

C*****!****************************************************************!
c     next check for a partial missing value
c     token length would have to be greater than one byte
c     and token(1:1) would be 'M'
      if ( lentok .gt. 1 .and. token(1:1) .eq. 'M' ) then
c        this is a partial missing value flag
c        set visibility value accordingly
         call msg('vipr:partial missing value for token',3)
         visib = -999.99
      end if


c     now finish processing the partial missing flag
c     call blocker and pwea, but do not call vis


C*****!****************************************************************!
c     determine if there are any obstructions to vision
c     call blocker - this will return either 'F', 'H', or ' '
c     returned values are F=fog, H=haze, blank=no obstructions
c     ob is not returned to the calling program from vipr

      call msg('vipr:calling blocker',3)
      call blocker(token, lentok,ob,ios,lognum)

c     check return code from blocker
      if ( ios .ne. zero ) then
         call msg('vipr:bad return code from blocker',0)
         call msg('vipr:unable to parse obstructions to vision',0)
         call msg(token,0)
         goto 9999
      end if


c     now call pwea to get a code value for the present weather string
c     see code in pwea for code values 
c     this will return a value of 100 if no weather observed
c     (NOTE: this code value will be modified if no weather was observed
c      but fog or haze are reported)

      call msg('vipr:calling pwea',3)
      call pwea(token, lentok, ipw, ios)
      call msg('vipr:returned from pwea',3)

c     check return code from pwea
      if ( ios .ne. zero ) then
         call msg('vipr:bad return code from pwea',0)
         call msg('vipr:unable to parse present weather',0)
         goto 9999
      else
      end if



c     call vis to parse the visibility in miles and return
c     visibility in meters
c     (check that this is not a partially missing value for token)



      if ( lentok .gt. 1 .and. token(1:1) .eq. 'M' ) then
c        this is a partial missing value flag
c        do not call vis
         call msg('vipr:partial missing value for token - skip vis',3)
         goto 1000
      end if

      call msg('vipr:calling vis',3)

      call vis ( token, lentok, visib, ios)

      call msg('vipr:returned from vis',3)

c     check return code from vis
      if ( ios .ne. zero ) then
         call msg('vipr:bad return code from vis',0)
         call msg('vipr:unable to parse visibility',0)
         goto 9999
      else
      end if


1000  continue
c     if no significant weather was observed ( code = 100 ) then
c     check if either fog (F) or haze(H) was reported.
c     if so, adjust the present weather flag to report this.
c     Note that there is only one code for fog ( code = 130 ), but
c     haze has two possibilities.
c
c     Haze can either be 104 for visibility GE 1 km, or
c     haze is 105 for visibility less than 1 km.
c     
c     check visibility and choose accordingly

      if ( ipw .eq. 100 ) then
c        no significant weather reported
c        check for fog or haze

         if ( ob .eq. 'F' ) then
c           fog reported - set ipw to 130
            ipw = 130

         else if (ob .eq. 'H' ) then
c           haze reported - check visibility to determine code

            if ( visib .ge. 1000. ) then
c              visibility greater than or equal to one kilometer
c              set ipw to 104
               ipw = 104
            else if ( visib .ge. 0.0 ) then
c              visibility less than one kilometer
c              set ipw to 105
               ipw = 105
            else
c              set ipw to missing - no way to determine which code is right
c              normally would only reach this if the visibility was missing
               call msg('vipr:set ipw to missing value of -999',3)
               call msg('vipr:visibility must be missing',3)
               ipw = -999
            end if



         else if ( ob .eq. ' ' ) then
c             no fog or haze reported - leave ipw as is

         else
c             should never reach this step
              call msg('vipr:error in reported visibility',0)
              write(6, '('' vipr:ob = '',A1)' ) ob
              write(lognum, '('' vipr:ob = '',A1)' ) ob
              ios = 1
              goto 9999
         end if


      else
c        some weather reported
c        no adjustments needed
      end if





9999  continue
      call msg('vipr:leaving vipr now',3)
      return
      end
