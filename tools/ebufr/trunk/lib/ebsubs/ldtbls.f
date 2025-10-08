C
C $Id: ldtbls.f,v 2.5 1994/12/08 23:26:29 john Exp $
C
C ldtbls - load code/flag table definitions (EBUFR type 6 record)
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
C
C ldtbls - LoaD TaBLeS ; loads information for code/flag tables from
C          EBUFR type 8 records
C Written by: John J. Allison
C             NCAR/STORM Project Office, Boulder CO
C             05 Aug 1992
C
C Modified from "lddesc.f" by Wayne Brazille and "dsptbl.f" (part of
C   ebdump utility) by Mark Bradford, both at NCAR/STORM Project Office

      logical function ldtbls(msunit,rec,reclen)
c***********************************************************************
c  Includes for low-level E-BUFR routines
c***********************************************************************
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'
c***********************************************************************
c  Subroutine parameter definitions
c***********************************************************************
      integer msunit,reclen,rec(MAXLEN)
C
C external functions
      integer bfinfo
      external bfinfo
c***********************************************************************
c  Local Variables 
c***********************************************************************
      logical ERR
      integer errno
      integer len3,len4
      integer numdat,flocal(MAXDES),xlocal(MAXDES),ylocal(MAXDES)
      integer numsub,obsdat,cmpdat,numdes,bfsec3,bfsec4
      character*128 datum(MAXDAT)
      character*8 fmt(MAXDAT)
      integer i
      integer xout(MAXDAT),yout(MAXDAT)
c
c     Variables relevant to descriptor being defined:
c
      integer ftmp,xtmp,ytmp
      integer fdef,xdef,ydef
      integer lnbits,lscale,lref
      character*32 lname1,lname2
      character*24 lunits
      integer entry
      character*32 line1,line2,line3,line4,line5
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
      do 30 i=1,numdat
c     Interpret the data as defining element descriptors:
      if (xout(i).eq.0) then
            if (yout(i).eq.10) then
               read(datum(i),'(I1)')fdef
            else if (yout(i).eq.11) then
               read(datum(i),'(I2)')xdef
            else if (yout(i).eq.12) then
               read(datum(i),'(I3)')ydef
            else if (yout(i).eq.255) then
               read(datum(i),'(I3)')entry
            else if (yout(i).eq.254) then
               read(datum(i),'(A32)')line1
            else if (yout(i).eq.253) then
               read(datum(i),'(A32)')line2
            else if (yout(i).eq.252) then
               read(datum(i),'(A32)')line3
            else if (yout(i).eq.251) then
               read(datum(i),'(A32)')line4
            else if (yout(i).eq.250) then
               read(datum(i),'(A32)')line5
            end if
         end if
 30   continue

      if (ftmp.ne.fdef .or. xtmp.ne.xdef .or. ytmp.ne.ydef) then
	 write(msunit,*)'LDTBLS(): Error:'
         write(msunit,*)'  Mismatch: Header says F=',ftmp,' X=',xtmp,
     +     ' Y=',ytmp,'; Body says F=',fdef,' X=',xdef,' Y=',ydef,'.'
      end if

      if (fdef.ne.0) then
         goto 999
      end if
      if (entry.lt.0) then
	write(msunit,*) 'ldtbls(): Error: entry .lt. 0'
	ERR = .true.
	goto 999
      else if ((entry+1).gt.MAXENT) then
	write(msunit,*) 'ldtbls(): entry .gt. MAXENT'
	write(msunit,*) ' increase MAXENT in ebufr_parm.inc'
	ERR = .true.
	goto 999
      end if

c     Loop through known tables.  If table already known, use
c     this definition; otherwise add it, if there's room.

      do 20 i=1,numtbl
      if (xtbl(i).eq.xdef .and. ytbl(i).eq.ydef) then
      errno=bfinfo(xdef,ydef,lscale,lref,lnbits,lname1,lname2,lunits)
	  if (errno .ne. 0) then
	    call bferr(errno,msunit)
	    goto 999
	  end if
	  if (lunits(1:4) .eq. 'Flag') then
	    flag(i) = .true.
C Flag table values go from 1..nbits, so we don't want the +1 array index
C   correction needed below for possible code table values of 0
	    entry = entry - 1
	  else
	    flag(i) = .false.
          end if
C number of entries is max of current number and value of entry(index corrected)
C because entries could come out of order, e.g. entry 10 then entry 3
	  nument(i) = max(nument(i),entry+1)
	  code(1,entry+1,i) = line1
	  code(2,entry+1,i) = line2
	  code(3,entry+1,i) = line3
	  code(4,entry+1,i) = line4
	  code(5,entry+1,i) = line5
          goto 999
         end if
 20   continue

c Not already defined, so add it.
      if (numtbl .ge. MAXTBL) then
	write(msunit,*) 'ldtbls(): Number of tables exceeds max.'
	write(msunit,*) ' change MAXTBL in ebufr_parm.inc'
	ERR = .true.
	goto 999
      end if

      numtbl = numtbl + 1
      xtbl(numtbl) = xdef
      ytbl(numtbl) = ydef
      errno = bfinfo(xdef,ydef,lscale,lref,lnbits,lname1,lname2,lunits)
      if (errno .ne. 0) then
        call bferr(errno,msunit)
        goto 999
      end if
      if (lunits(1:4) .eq. 'Flag') then
	flag(numtbl) = .true.
C there is one entry for each bit
	nument(numtbl) = lnbits
C array index un-correction since flag table values go 1..max
	entry = entry - 1
      else
	flag(numtbl) = .false.
C there is one entry for each possible number lnbits wide
	nument(numtbl) = 2**lnbits
      end if
      code(1,entry+1,i) = line1
      code(2,entry+1,i) = line2
      code(3,entry+1,i) = line3
      code(4,entry+1,i) = line4
      code(5,entry+1,i) = line5

 999  ldtbls = ERR
      RETURN
      END
