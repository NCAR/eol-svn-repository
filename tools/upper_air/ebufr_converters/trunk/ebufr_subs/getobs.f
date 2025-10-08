C
C $Id: getobs.f,v 2.6 1995/02/27 17:54:03 john Exp $
C
C getobs - get the next observation from EBUFR file
c
c*
c* Copyright (C) 1992 by UCAR
c*      University Corporation for Atmospheric Research
c*
c* Permission to use, copy, modify, and distribute this software and its
c* documentation for any purpose and without fee is hereby granted, provided
c* that the above copyright notice and this permission notice appear in all
c* copies and in all supporting documentation, and that the name of UCAR
c* not be used in advertising or publicity pertaining to distribution of
c* the software in source or compiled form, whether alone or as part of
c* another program, without specific, written prior permission.
c*
c* Any modifications that you make to this software must be explicitly
c* identified as such and include your name and the date of modification.
c*
c* In addition, UCAR gives you permission to link the compiled version of
c* this file with other programs, and to distribute those without any
c* restriction coming from the use of this file.
c*
c* Although this software is provided in the hope that it will be useful,
c* UCAR makes no representations about the suitability of this software
c* for any purpose. This software and any accompanying written materials
c* are provided "as is" without warranty of any kind. UCAR expressly
c* disclaims all warranties of any kind, either express or implied,
c* including but not limited to the implied warranties of merchantibility
c* and fitness for a particular purpose. UCAR does not indemnify any
c* infringement of copyright, patent, or trademark through use or
c* modification of this software.
c*
c* UCAR does not normally provide maintenance or updates for its software.
c

      subroutine getobs(iunit,msunit,datum,fmt,numdat,
     +                  xout,yout,ERR,EOF,FSTTIM,CHKALL)
c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Return the next observation from an E-BUFR file.  The
c              first time this routine is called, all the E-BUFR header
c              records are read, and appropriate data items to support
c              decoding the data are initialized.
c
c GETOBS - GET next OBServation
c
c Returns the data for the next observation from an EBUFR file. The first
c time it is called, all EBUFR header records are read and all control
c variables are initialized.
c
c In:
c  iunit - unit number of EBUFR input file
c  msunit - unit number of file to which to write error message
c  eof - end of file flag (must be set to FALSE on the first call)
c  fsttim - TRUE if this is the first time getobs() is being called for iunit
c  chkall - TRUE if you want to check the header records for errors and fill
c           the common blocks ebidn*;
c           FALSE if you want to completely ignore header records;
c           setting it to FALSE may make your program run slightly faster
c
c Out:
c  datum - array of strings containing data values
c  fmt - array of FORMAT strings with which to read data
c  numdat - number of data items (top index of datum,fmt,xout,yout arrays)
c  xout - array of X descriptors for data
c  yout - array of Y descriptors for data
c  err - error flag
c  eof - end of file flag
c  fsttim - set to .FALSE. if no errors occurred
c
c Commons changed:
c  ebidn* - info from EBUFR header records (types 0,1,3)
c  ebhdr* - info from header to EBUFR record, i.e. nominal date,time,etc
c You can also assume that GETOBS changes every COMMON block
c  defined in "ebufr_vars.inc", however these are internal variables for
c  the Read Routines and the average user does not need to access them.
c
c ebidn*,ebhdr* are defined in "ebufr_obs.inc" (and Chapter 4)
c MAXDAT is defined in "ebufr_parm.inc"
c
c Errors are fatal. You should stop processing the current input file.
c
c***********************************************************************
c  Includes for low-level E-BUFR routines
c***********************************************************************
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'
c***********************************************************************
c  Subroutine parameter definitions
c***********************************************************************
      integer iunit,msunit,numdat
      logical ERR,EOF,FSTTIM,CHKALL
c
c Definition of variables to contain E-BUFR observation
c
C Following include file helps to define parameters and does not
C   (at least at the time of this writing) contain any COMMON statements
      include 'ebufr_obs.inc'
c***********************************************************************
c  Local Variables 
c***********************************************************************
      integer reclen,rec(MAXSIZ),len3,len4,errnum
      real bd
      integer lat,lon
      integer*2 temp
      integer bfsec3,bfsec4,nxtbts
      integer index,bitnum
      integer numsub,obsdat,cmpdat
      logical DSCSCN
      DATA DSCSCN/.FALSE./
      SAVE numsub,obsdat,cmpdat,DSCSCN

C Function definitions
      logical rdhrec,ebread

c
c Initialize variables
c
      ERR = .false.
      len3=0
c
c If called with EOF = true, set error and return
c
      if (EOF) then
          ERR = .true.
          write(msunit,*) ' GETOBS called with EOF = true'
          goto 999
      end if
c
c If not first time in, read a data record, else
c read header records, get decoding info, and read first data record
c
      if (.not.FSTTIM) then
          ERR = ebread(iunit,msunit,rec,reclen,EOF)
          if (ERR) then
              write(msunit,*) ' Error encountered in EBREAD'
              goto 999
          end if
          if (EOF) then
              goto 999
          end if
      else
          ERR = rdhrec(iunit,msunit,rec,reclen,
     +                 numsub,obsdat,cmpdat,EOF,DSCSCN,CHKALL)
          if (ERR) then
              write(msunit,*) ' Error encountered in RDHREC'
              goto 999
          end if
          if (EOF) then
              write(msunit,*) ' EOF encountered in RDHREC'
              ERR = .true.
              goto 999
          end if
          FSTTIM = .false.
      end if
c
c Extract data from rec and load into common header variables, datum, and fmt
c
      temp = rec(2)*256+rec(3)
      write(clyear,'(I4)') temp
      temp = rec(4)
      write(clmon,'(I2.2)') temp
      temp = rec(5)
      write(clday,'(I2.2)') temp
      temp = rec(6)
      write(clhour,'(I2.2)') temp
      temp = rec(7)
      write(clmin,'(I2.2)') temp
      temp = rec(8)
      write(clsec,'(I2.2)') temp
      index=9
      bitnum=1
      lat=nxtbts(25,rec,reclen,0,index,bitnum)
      cllat = bd(lat,5,-9000000)
      index=13
      bitnum=1
      lon=nxtbts(26,rec,reclen,0,index,bitnum)
      cllon = bd(lon,5,-18000000)
      write(clibyt,'(I3.3)') rec(17)

c
c If E-BUFR type 2 record not seen in header records, then each
c data record contains an independent descriptor section, which
c must be decoded in order to unpack the record.
c
      if (.not.DSCSCN) then
         errnum = bfsec3(rec,reclen,HDRSIZ,MAXDES,
     +        len3,numsub,obsdat,cmpdat,numfxy,f,x,y)
         if (errnum.ne.0) then
            call bferr(errnum,msunit)
            ERR = .true.
            goto 999
         end if
      end if

c
c     Now decode data contained in rec
c
      errnum = bfsec4(rec,reclen,HDRSIZ+len3,len4,numsub,cmpdat,
     +             f,x,y,numfxy,xout,yout,datum,fmt,MAXDAT,numdat)
      if (errnum.ne.0) then
          call bferr(errnum,msunit)
          ERR = .true.
      end if

 999  RETURN
      END
