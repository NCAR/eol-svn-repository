      subroutine reject(text,buffer,ireject,rejnum)
c
c     writes a message to the log file, screen, and
c     reject file

c     also increments the rejected records counter

c     written by Ronald A. Murdock
c                NCAR/MMM/STORM Project Office
c
c     creation date: 22may92
c
c     History of changes:
c
c     22may92 - creation
c
c


      character*(*) text
      character*200 buffer
      integer ireject
      integer rejnum, lognum
      logical debug1, debug2, debug3, screen
      common /bugblk/ debug1, debug2, debug3, screen, lognum


c     write a blank line before the message
      write(lognum, '(/)' )
      write(rejnum, '(/)' )

c     write the message to the screen, log file, and rejects file
c     write(6, '(1x,A)' ) text  !level 0 always reaches screen
      write(lognum, '(1x,A)' ) text  !level 0 always reaches screen
      write(rejnum, '(1x,A)' ) text  !level 0 always reaches screen

c     write the buffer string to the screen, log file, and rejects file
c     write(6, '(a200)' ) buffer
      write(lognum, '(a200)' ) buffer
      write(rejnum, '(a200)' ) buffer

c     increment the rejected records counter
      ireject = ireject + 1

      return
      end
