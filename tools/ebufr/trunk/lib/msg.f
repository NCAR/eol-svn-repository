      subroutine msg(text,level)
c
c     writes a message to the log file and screen if
c     the appropriate debug level is true and if the
c     user wanted output to screen


c     created by Ronald A. Murdock
c

c     Input to subroutine:
c        text - character string message
c        level - debug level for messages

c     Returned from subroutine:
c        No values returned

c     History of changes:
c
c        22may92 - retyped after delete



      character*(*) text
      integer level
      integer lognum
      logical debug1, debug2, debug3, screen
      common /bugblk/ debug1, debug2, debug3, screen, lognum


      if ( level .eq. 0 ) then
           write(lognum,'(1x,A)' ) text
           write(6, '(1x,A)' ) text  !level 0 always reaches screen
      end if

      if ( level .eq. 1 ) then
         if ( debug1 ) then
           write(lognum,'(1x,A)' ) text
           if ( screen )  write(6, '(1x,A)' ) text
         end if
      end if

      if ( level .eq. 2 ) then
         if ( debug2 ) then
           write(lognum,'(1x,A)' ) text
           if ( screen )  write(6, '(1x,A)' ) text
         end if
      end if

      if ( level .eq. 3 ) then
        if (debug3) then
           write(lognum,'(1x,A)' ) text
           if ( screen )  write(6, '(1x,A)' ) text
         end if
      end if

      return
      end
