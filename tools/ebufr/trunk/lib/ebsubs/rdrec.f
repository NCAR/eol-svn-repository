C
C $Id: rdrec.f,v 2.4 1992/08/17 18:03:18 john Exp $
C
C rdrec - read an EBUFR record
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
      subroutine rdrec(iunit,cbuf,errnum,ERR,EOF)

c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     THIS ROUTINE MAY BE MACHINE-SPECIFIC.

c     Routine reads one record of input file (which is already open
c     on iunit) into character buffer cbuf.  This version should work
c     on a Sun, and is tested under SunOS 4.1.1 with FORTRAN 1.3.1.
c     It should also work on a DEC machine under ULTRIX.

c     This routine uses Sun's fgetc routine to get input one byte at
c     a time, ignoring the 'record structure' as defined by \n characters.

      integer iunit,errnum,MAXLEN,i,digit,reclen,HDRSIZ
      integer fgetc
      parameter (MAXLEN=9999,HDRSIZ=22)
      character cbuf(MAXLEN)
      logical ERR,EOF

      do 50 i=1,HDRSIZ
         errnum=fgetc(iunit,cbuf(i))
         if (errnum.eq.-1) then
            EOF=.TRUE.
            RETURN
         else if (errnum.ne.0) then
            ERR=.TRUE.
            RETURN
         end if
 50   continue
      reclen=0
      do 10 i=HDRSIZ-3,HDRSIZ
         read(cbuf(i),'(I1)',err=200,iostat=errnum)digit
         reclen=reclen+digit*10**(HDRSIZ-i)
 10   continue
      do 60 i=1,reclen
         errnum=fgetc(iunit,cbuf(HDRSIZ+i))
         if (errnum.eq.-1) then
            EOF=.TRUE.
            RETURN
         else if (errnum.ne.0) then
            ERR=.TRUE.
            RETURN
         end if
 60   continue
      RETURN
 100  continue
      EOF=.TRUE.
      RETURN
 200  continue
      ERR=.TRUE.
      RETURN
      END
