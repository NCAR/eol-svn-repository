C
C $Id: rdhrec.f,v 2.5 1992/11/20 20:48:43 john Exp $
C
C rdhrec - read and process the EBUFR header records
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

      logical function rdhrec(iunit,msunit,rec,reclen,
     +                 numsub,obsdat,cmpdat,EOF,DSCSCN,CHKALL)
c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Read E-BUFR header records, init decoder values, and
c              return first data record.
c***********************************************************************
c  Includes for low-level E-BUFR routines
c***********************************************************************
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'
c***********************************************************************
c  Subroutine parameter definitions
c***********************************************************************
      integer iunit,msunit,reclen,rec(MAXLEN)
      integer numsub,obsdat,cmpdat
      logical EOF,DSCSCN,CHKALL
      include 'ebufr_obs.inc'
c***********************************************************************
c  Local Variables 
c***********************************************************************
      logical ERR,lddesc,ebread,ldtbls
      integer len3,length,tmp1,tmp2,tmp3,tmp4
      integer floc(MAXDES),xloc(MAXDES),yloc(MAXDES)
      integer errnum
C function defintions
      integer bfsec0,bfsec1,bfsec3,bfsec4
      external bfsec0,bfsec1,bfsec3,bfsec4
c
c Initialize variables
c
      ERR = .false.
      len3=0
c
c Read first record from file
c
      ERR = ebread(iunit,msunit,rec,reclen,EOF)
      if (EOF) then
          write(msunit,*) ' EOF Encoutered on first read in RDHREC'
          ERR = .true.
          goto 999
      end if
      if (ERR) then
          write(msunit,*) ' ERR Encoutered on first read in RDHREC'
          goto 999
      end if
c
c Loop until first type 9 record seen, initializing as appropriate
c
 5    continue
      if (CHKALL) then
        go to (9,11,21,31,101,101,61,71,81,999) rec(1)+1
      else
        go to (101,101,21,101,101,101,101,71,81,999) rec(1)+1
      end if
      write (msunit,*) 'rdhrec(): bad EBUFR record type'
      go to 101
 9    errnum = bfsec0(rec,reclen,HDRSIZ,ebedn,dattyp,
     +  bgedn,mtedn)
      if (errnum .ne. 0) then
	call bferr(errnum,msunit)
	ERR = .true.
	write (msunit,*) 'rdhrec(): Error in type 0 record'
	go to 999
      end if
      go to 101
 11   errnum = bfsec1(rec,reclen,HDRSIZ,length,
     +  editn,center,update,tmp1,msgtyp,
     +  subtyp,ver,ebyear,ebmon,ebday,
     +  ebhour,ebmin)
      if (errnum .ne. 0) then
	call bferr(errnum,msunit)
	write (msunit,*) 'rdhrec(): Error in type 0 record'
	ERR = .true.
	go to 999
      end if
      if (tmp1 .eq. 1) then
	write (msunit,*)
     +   'rdhrec(): Error: optional section exists type 1 record.'
	ERR = .true.
	go to 999
      end if
      go to 101
 21   errnum = bfsec3(rec,reclen,HDRSIZ,MAXDES,
     +                length,numsub,obsdat,cmpdat,numfxy,f,x,y)
          if (errnum.ne.0) then
           call bferr(errnum,msunit)
           ERR = .true.
           write(msunit,*) 'rdhrec(): Error Processing type 2 record'
           goto 999
          end if
          DSCSCN = .true.
      go to 101
 31   errnum = bfsec3(rec,reclen,HDRSIZ,MAXDES,
     +         len3,tmp1,tmp2,tmp3,tmp4,floc,xloc,yloc)
          if (errnum.ne.0) then
           call bferr(errnum,msunit)
           ERR = .true.
           write(msunit,*) 'rdhrec(): Error Processing type 3 record'
           goto 999
          end if
      errnum = bfsec4(rec,reclen,HDRSIZ+len3,length,tmp1,tmp3,
     +         floc,xloc,yloc,tmp4,xout,yout,
     +         ebtext,fmt,MAXTXT,numtxt)
          if (errnum.ne.0) then
           call bferr(errnum,msunit)
           ERR = .true.
           write(msunit,*) 'rdhrec(): error processing type 3 bfsec4'
           goto 999
          end if
      go to 101
 61   errnum = bfsec3(rec,reclen,HDRSIZ,MAXDES,
     +         len3,tmp1,tmp2,tmp3,tmp4,floc,xloc,yloc)
          if (errnum.ne.0) then
           call bferr(errnum,msunit)
           ERR = .true.
           write(msunit,*) 'rdhrec(): Error Processing type 6 record'
           goto 999
          end if
      errnum = bfsec4(rec,reclen,HDRSIZ+len3,length,tmp1,tmp3,
     +         floc,xloc,yloc,tmp4,xout,yout,datum,fmt,MAXDAT,tmp2)
          if (errnum.ne.0) then
           call bferr(errnum,msunit)
           ERR = .true.
           write(msunit,*) 'rdhrec(): error processing type 6 bfsec4'
           goto 999
          end if
      go to 101
 71   ERR = lddesc(msunit,rec,reclen)
          if (ERR) then
           write(msunit,*) 'rdhrec(): Error Processing type 7 record'
           goto 999
          end if
      go to 101
 81   ERR = ldtbls(msunit,rec,reclen)
	if (ERR) then
          write(msunit,*) 'rdhrec(): Error Processing type 8 record'
          goto 999
        end if
      go to 101

 101  continue
c
c Read next record from file
c
      ERR = ebread(iunit,msunit,rec,reclen,EOF)
      if (EOF) then
          write(msunit,*)
     +           ' EOF Encoutered before first data record in RDHREC'
          ERR = .true.
          goto 999
      end if
      if (ERR) then
          write(msunit,*) ' ERR Encoutered on read in RDHREC'
          goto 999
      end if
c
c Loop back for processing of next record
c
      goto 5

 999  rdhrec = ERR
      RETURN
      END
