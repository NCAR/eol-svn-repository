      subroutine msgr(text,level,real1,rfmt)
c
c     writes a message to the log file and screen if
c     the appropriate debug level is true and if the
c     user wanted output to screen

c     this subroutine also prints out a real value
c     following the text string

      character*(*) text
      character*(*) rfmt
      character*15  newfmt
      integer level
      integer lognum
      real real1
      logical debug1, debug2, debug3, screen
      common /bugblk/ debug1, debug2, debug3, screen, lognum

      newfmt = '(1X,A,'//rfmt//')'

      if ( level .eq. 0 ) then
           write(lognum,newfmt) text,real1
           write(6, newfmt ) text, real1
      end if

      if ( level .eq. 1 ) then
         if ( debug1 ) then
           write(lognum,newfmt) text, real1
           if ( screen )  write(6, newfmt ) text, real1
         end if
      end if

      if ( level .eq. 2 ) then
         if ( debug2 ) then
           write(lognum,newfmt) text, real1
           if ( screen )  write(6, newfmt ) text, real1
         end if
      end if

      if ( level .eq. 3 ) then
        if (debug3) then
           write(lognum,newfmt) text, real1
           if ( screen )  write(6, newfmt ) text, real1
         end if
      end if

      return
      end
