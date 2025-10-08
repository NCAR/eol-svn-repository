      subroutine msgc(text,level,char1,cfmt)
c
c     writes a message to the log file and screen if
c     the appropriate debug level is true and if the
c     user wanted output to screen

c     this subroutine also prints out a character value
c     following the text string using the specified format

c     created by Ronald A. Murdock
c
c     Creation date:  15May92

c     Input to subroutine:
c        text - character string message
c        level - debug level for messages
c        char1 - character variable whose value will be shown
c        cfmt  - character format to use for printing char1

c     Returned from subroutine:
c        No values returned

c     History of changes:
c
c        15may92 - creation



      character*(*) text
      character*(*) cfmt
      character*15  newfmt
      character*(*) char1
      integer level
      integer lognum
      logical debug1, debug2, debug3, screen
      common /bugblk/ debug1, debug2, debug3, screen, lognum

      newfmt = '(1X,A,'//cfmt//')'

      if ( level .eq. 0 ) then
           write(lognum,newfmt) text,char1
           write(6, newfmt ) text, char1
      end if

      if ( level .eq. 1 ) then
         if ( debug1 ) then
           write(lognum,newfmt) text, char1
           if ( screen )  write(6, newfmt ) text, char1
         end if
      end if

      if ( level .eq. 2 ) then
         if ( debug2 ) then
           write(lognum,newfmt) text, char1
           if ( screen )  write(6, newfmt ) text, char1
         end if
      end if

      if ( level .eq. 3 ) then
        if (debug3) then
           write(lognum,newfmt) text, char1
           if ( screen )  write(6, newfmt ) text, char1
         end if
      end if

      return
      end
