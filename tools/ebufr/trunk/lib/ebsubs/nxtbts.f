C
C $Id: nxtbts.f,v 2.4 1992/08/17 18:03:14 john Exp $
C
C nxtbts - get the next set of bits from the record
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
      integer function nxtbts(nbits,rec,reclen,offset,index,bitnum)

c     Function returns an integer composed of the next NBITS bits
c     from REC starting at INDEX and BITNUM.
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     BITNUM is the BUFR bit number, i.e., most significant bit = 1
c     and least significant bit = 8.  (It's backwards, but it's part of
c     BUFR.)

c     THIS FUNCTION IS MACHINE-SPECIFIC.  This version is for VMS and
c     Sun FORTRAN.  It *should* translate to Unisys ASCII FORTRAN
c     simply by changing the name IBITS to BITS.  (Won't know 'til we
c     try, eh?)

c     ERRORS:
c     Returns -1 if data overrun end of record.
c     Returns -2 if nbits is greater than 32.  (Still skips nbits bits.)

      integer nbits,reclen,rec(reclen),offset,index,bitnum,temp,i

      temp=0
      do 10 i=1,nbits
         if ((offset+index).gt.reclen) then
c     Error!
            nxtbts=-1
            RETURN
         end if
         if (nbits.le.32)temp=temp*2 +
     .        ibits(rec(offset+index),8-bitnum,1)
         bitnum=bitnum+1
         if (bitnum.gt.8) then
            bitnum=1
            index=index+1
         end if
 10   continue
      nxtbts=temp
      if (nbits.gt.32) nxtbts=-2
      RETURN
      END
