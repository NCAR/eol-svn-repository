C
C $Id: gtfils.f,v 1.1 1992/06/22 17:03:35 wayne Exp $
C $Log: gtfils.f,v $
c Revision 1.1  1992/06/22  17:03:35  wayne
c Initial Version
c
C

      logical function gtfils (iunit,msunit,ifile,msfile)

c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Get filenames and open three files
c***********************************************************************
c***********************************************************************
c   Function parameters
c***********************************************************************
      integer iunit,msunit
      character*50 ifile,msfile
c***********************************************************************
c   Local variables
c***********************************************************************
c
      character*1 yn
      integer errnum

c
c Get filenames and open files
c
      iunit = 11
      gtfils = .false.
c
c Open file for error messages
c
      print *,' Send error messages to file (no => STDOUT) (y/n)?'
      read '(A)',yn
      if (yn.eq.'y'.or.yn.eq.'Y') then
          msunit = 13
          print *,' Name of message file?'
          read '(A)',msfile
          call opnfil(msunit,msfile,'UNK',errnum,6)
          if (errnum .ne. 0) then
              msunit = 6
              write(msunit,*)' Error opening message ',msfile
              gtfils = .true.
              goto 100
          end if
      else
          msunit = 6
      end if
c
c Open input file
c
      print *,' Name of High Plains File to Convert to QCF?'
      read '(A)',ifile
      write(msunit,*)' Opening ',ifile
      call opnfil(iunit,ifile,'OLD',errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*)' Error opening E-BUFR file ',ifile
          gtfils = .true.
          if (yn.eq.'y'.or.yn.eq.'Y') then
              call clsfil(msunit,6)
          end if
          goto 100
      end if

c
c All files open, so error = false
c
      gtfils = .FALSE.
c
 100  return
      END
