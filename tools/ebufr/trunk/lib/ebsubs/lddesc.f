C
C $Id: lddesc.f,v 2.4 1994/12/08 23:26:28 john Exp $
C
C lddesc - load the descriptor definitions (EBUFR record type 7)
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

      logical function lddesc(msunit,rec,reclen)
c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Load a descriptor definition from a type 7 record
c***********************************************************************
c  Includes for low-level E-BUFR routines
c***********************************************************************
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'
c***********************************************************************
c  Subroutine parameter definitions
c***********************************************************************
      integer msunit,reclen,rec(MAXLEN)
c***********************************************************************
c  Local Variables 
c***********************************************************************
      logical ERR
      integer errno
      integer len3,len4
      integer numdat,flocal(MAXDES),xlocal(MAXDES),ylocal(MAXDES)
      integer numsub,obsdat,cmpdat,numdes,bfsec3,bfsec4
c      integer nbits,scale,ref
      character*128 datum(MAXDAT)
      character*8 fmt(MAXDAT)
c      character*32 name1,name2
c      character*24 units
      integer i
      integer xout(MAXDAT),yout(MAXDAT)
c
c     Variables relevant to descriptor being defined:
c
      integer ftmp,xtmp,ytmp
      integer fdef,xdef,ydef
      integer scalsn,scldef,refsn,refdef,bitdef
      character*32 nm1def,nm2def
      character*24 untdef
c
c Initialize variables
c
      ERR = .false.
      len3=0
      ftmp=int(rec(2)/64)
      xtmp=mod(rec(2),64)
      ytmp=rec(3)
      if (ftmp.ne.int(rec(9)/64) .or. xtmp.ne.mod(rec(9),64) .or.
     +     ytmp.ne.rec(10)) then
         write(msunit,*)' F/X/Y mismatch in Time/CLI.'
         ERR = .true.
         goto 999
      end if
c
c     Decode descriptor section
c
      errno = bfsec3(rec,reclen,HDRSIZ,MAXDAT,
     +     len3,numsub,obsdat,cmpdat,numdes,flocal,xlocal,ylocal)
      if (errno.ne.0) then
         call bferr(errno,msunit)
         ERR = .true.
         goto 999
      end if
c
c     Now get data.
c
      errno = bfsec4(rec,reclen,HDRSIZ+len3,len4,numsub,cmpdat,
     +     flocal,xlocal,ylocal,numdes,xout,yout,datum,fmt,
     +     MAXDAT,numdat)
      if (errno.ne.0) then
         call bferr(errno,msunit)
         ERR = .true.
         goto 999
      end if
      scalsn=1
      refsn=1
      do 30 i=1,numdat
c     Interpret the data as defining element descriptors:
      if (xout(i).eq.0) then
            if (yout(i).eq.10) then
               read(datum(i),'(I1)')fdef
            else if (yout(i).eq.11) then
               read(datum(i),'(I2)')xdef
            else if (yout(i).eq.12) then
               read(datum(i),'(I3)')ydef
            else if (yout(i).eq.13) then
               read(datum(i),'(A32)')nm1def
            else if (yout(i).eq.14) then
               read(datum(i),'(A32)')nm2def
            else if (yout(i).eq.15) then
               read(datum(i),'(A24)')untdef
            else if (yout(i).eq.16) then
               if (datum(i)(1:1).eq.'-') scalsn=-1
            else if (yout(i).eq.17) then
               read(datum(i),'(I3)')scldef
            else if (yout(i).eq.18) then
               if (datum(i)(1:1).eq.'-') refsn=-1
            else if (yout(i).eq.19) then
               read(datum(i),'(I10)')refdef
            else if (yout(i).eq.20) then
               read(datum(i),'(I3)')bitdef
            end if
         end if
 30   continue

      if (ftmp.ne.fdef .or. xtmp.ne.xdef .or. ytmp.ne.ydef) then
         write(msunit,*)' Error in Parsing a desciptor definition ',
     +                  'in lddesc.'
         write(msunit,*)' Mismatch: Header says F=',ftmp,' X=',xtmp,
     +        ' Y=',ytmp,'; Body says F=',fdef,' X=',xdef,' Y=',ydef,'.'
      end if

c     Loop through known descriptors.  If descriptor already known, use
c     this definition; otherwise add it, if there's room.

c Skip irrelevent descriptors
      if (fdef.ne.0) then
         goto 999
      end if
      do 20 i=1,curdes
         if (xtab(i).eq.xdef .and. ytab(i).eq.ydef) then
            scale(i)=scalsn*scldef
            ref(i)=refsn*refdef
            nbits(i)=bitdef
            name1(i)=nm1def
            name2(i)=nm2def
            units(i)=untdef
            goto 999
         end if
 20   continue

c     It wasn't found, so add it.
      if (curdes.ge.MAXDES) then
         write(msunit,*)
     +     ' Error!  Out of room for adding descriptors in lddesc.'
         ERR = .true.
         goto 999
      end if
      curdes=curdes+1
      xtab(curdes)=xdef
      ytab(curdes)=ydef
      scale(curdes)=scalsn*scldef
      ref(curdes)=refsn*refdef
      nbits(curdes)=bitdef
      name1(curdes)=nm1def
      name2(curdes)=nm2def
      units(curdes)=untdef

 999  lddesc = ERR
      RETURN
      END
