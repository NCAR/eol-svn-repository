C
C $Id$
C $Log$
C

      logical function gtfils (iunit,ounit,msunit,stnunit,punit,
     +                   ifile,ofile,msfile,stnfile,pfile,sfile)

c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Get 6 filenames and open 5 files
c***********************************************************************
c   Function parameters
c***********************************************************************
      integer iunit,ounit,msunit,stnunit,punit
      character*50 ifile,ofile,msfile,stnfile,pfile,sfile
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
      ounit = 12
      stnunit = 14
      punit = 15
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
c Open input E-BUFR file
c
      print *,' Name of E-BUFR File to Convert to QCF?'
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
c Open input station file
c
      print *,' Name of Station Name / Location file?'
      read '(A)',stnfile
      write(msunit,*)' Opening ',stnfile
      call opnfil(stnunit,stnfile,'OLD',errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*)' Error opening station file ',stnfile
          gtfils = .true.
          call clsfil(iunit,msunit)
          if (yn.eq.'y'.or.yn.eq.'Y') then
              call clsfil(msunit,6)
          end if
          goto 100
      end if

c
c Open output station file
c
      print *,' Name of Output station information file?'
      read '(A)',sfile

c
c Open output QCF file
c
      print *,' Name of QCF Output File?'
      read '(A)',ofile
      write(msunit,*)' Opening ',ofile
      call opnfil(ounit,ofile,'UNK',errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*)' Error opening QCF file ',ofile
          gtfils = .true.
          call clsfil(iunit,msunit)
          call clsfil(stnunit,msunit)
          if (yn.eq.'y'.or.yn.eq.'Y') then
              call clsfil(msunit,6)
          end if
          goto 100
      end if
c
c Open output daily precip file
c
      print *,' Name of Daily Precip Output File?'
      read '(A)',pfile
      write(msunit,*)' Opening ',pfile
      call opnfil(punit,pfile,'UNK',errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*)' Error opening Daily Precip file ',pfile
          gtfils = .true.
          call clsfil(iunit,msunit)
          call clsfil(stnunit,msunit)
          call clsfil(ounit,msunit)
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
