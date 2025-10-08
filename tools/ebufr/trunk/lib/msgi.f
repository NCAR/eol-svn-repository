      subroutine msgi(text,level,int1)
c
c     writes a message to the log file and screen if
c     the appropriate debug level is true and if the
c     user wanted output to screen

c     this subroutine also prints out an integer value
      character*(*) text
      integer level, int1
      integer lognum
      logical debug1, debug2, debug3, screen
      common /bugblk/ debug1, debug2, debug3, screen, lognum


      if ( level .eq. 0 ) then
           write(lognum,'(1x,A,I5)') text,int1
           write(6, '(1x,A,I5)' ) text, int1
      end if

      if ( level .eq. 1 ) then
         if ( debug1 ) then
           write(lognum,'(1x,A,I5)') text, int1
           if ( screen )  write(6, '(1x,A,I5)' ) text, int1
         end if
      end if

      if ( level .eq. 2 ) then
         if ( debug2 ) then
           write(lognum,'(1x,A,I5)') text, int1
           if ( screen )  write(6, '(1x,A,I5)' ) text, int1
         end if
      end if

      if ( level .eq. 3 ) then
        if (debug3) then
           write(lognum,'(1x,A,I5)') text, int1
           if ( screen )  write(6, '(1x,A,I5)' ) text, int1
         end if
      end if

      return
      end
