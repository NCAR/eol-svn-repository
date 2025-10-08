      subroutine blocker(token, lentok, ob, ios, lognum)

c     blocker.f - parses visibility/present weather/obstructions to vision
c              token from ASOS SAO observation and returns obstructions as 
c              'F', 'H', or a single blank.
c
c     written by Ronald A. Murdock
c
c     April 15, 1992
c
c     Input parameters:
c       token - string to be parsed
c       lentok - length of token
c       lognum - fortran unit number for log file
c
c    Returned parameters:
c       ob - one byte flag indicating vision obstructions
c                'F' = fog
c                'H' = haze
c                ' ' = no obstructions reported
c
c       ios   - zero if everything OK, one if something wrong
c
c
c     LOG OF PROGRAM CHANGES
c
c     Creation date:  4/15/92
c
c     920417 - changed name from obv to blocker.f
c              changed returned parameter name from obsvis to ob
c
c     4/23/92 - commented out all messages to log file except errors
c
c
c*****!*****************************************************************
c23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
c*****!*****************************************************************
c

      character*1  blank ! one-byte blank
      character*1  ob !(F, H, or blank) obstructions to vision
c                          indicator

      character*10 token !the character string containing visibility
c                         present weather, and obstructions to vision
c                         (this is what must be parsed)

      integer lentok  !length in bytes of token
      integer ios     !zero if everything OK, one if not OK
      integer lognum  !fortran unit number for log file
      integer zero    !the value zero


      blank = ' '
      zero  = 0

      ios = zero

c*****!*****************************************************************

c     write(lognum, '('' blocker: Entering blocker....'')' )
c     write(lognum, '('' blocker: Parse obstructions to vision'')' )
c     write(lognum, '('' blocker: token = '',A10)' ) token
c     write(lognum, '('' blocker: lentok = '',i2)' ) lentok

c
c     set ob for obstructions to vision
c     set to blank if none found
c     if any obstructions are present, the last byte in token
c     will contain the flag

      if ( token(lentok:lentok) .eq. 'F' ) then
         ob = 'F'
      else if ( token(lentok:lentok) .eq. 'H' ) then
         ob = 'H'
      else
         ob = ' '
      end if


c     write(lognum, 
c    *'('' blocker: returning value for ob of '',A1)' ) ob


9999  continue
c     write(lognum, '('' blocker: Leaving blocker now'')' )
c     exit the subroutine now
      return
      end
