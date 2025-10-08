C
C $Id: getrec.f,v 2.4 1992/08/17 18:03:06 john Exp $
C
C getrec - get an EBUFR record
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

************************************************************************
      subroutine getrec(iunit,rec,reclen,EOF)
c     Routine fetches record from unit IUNIT.
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     Pseudocode:
c     Call (machine-dependent) routine to get record into character buffer.
c     Decipher length of record from header.
c     Copy values from character buffer into integer buffer.
c     RETURN.

      integer HDRSIZ,MAXLEN
      parameter (HDRSIZ=22,MAXLEN=9999)
      integer iunit,rec(MAXLEN),reclen,errnum,i,digit
      logical EOF,ERR
      character cbuf(MAXLEN)

c     Routine RDREC may, perforce, contain machine/OS-dependent code.
      call rdrec(iunit,cbuf,errnum,ERR,EOF)
      if (ERR) goto 2000
      if (EOF) RETURN
      reclen=0
      do 10 i=HDRSIZ-3,HDRSIZ
         read(cbuf(i),'(I1)',err=2000,iostat=errnum)digit
         reclen=reclen+digit*10**(HDRSIZ-i)
 10   continue
      if (reclen.gt.(MAXLEN-HDRSIZ)) then
         print *,' Record length should not exceed ',MAXLEN-HDRSIZ,
     .        '.  Record had length ',reclen,'.'
         STOP
      end if
      do 20 i=1,reclen+HDRSIZ
         rec(i)=ichar(cbuf(i))
 20   continue
      reclen=reclen+HDRSIZ
      RETURN

 2000 print *,' Error ',errnum,' in GETREC!'
      STOP
      END
