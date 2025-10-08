C
C $Id: bfsec3.f,v 2.4 1992/08/17 18:02:39 john Exp $
C
C bfsec3 - decode a BUFR section 3
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
      integer function bfsec3(rec,reclen,offset,maxnum,
     .     length,numsub,obsdat,cmpdat,numdes,f,x,y)

c     This routine decodes a BUFR section type 3 (Data Description).
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     Note that NO checking is done here as to the validity of the
c     descriptor; this is left until the data is interpreted, in case
c     the descriptor is not defined until later in the EBUFR file.

c     On entry:
c     REC holds the BUFR data.
c     RECLEN is the maximum length of the REC array.
c     OFFSET is the beginning point of the BUFR section within REC.
c     MAXNUM is the dimension of the output arrays F, X, and Y.

c     On exit:
c     LENGTH is the number of octets in the section.
c     OBSDAT is 1 if data is observed, 0 otherwise.
c     CMPDAT is 1 if data is compressed, 0 otherwise.
c     NUMDES is the number of descriptors currently in F, X, and Y.
c     F, X, and Y hold the descriptors themselves.

c     Returns:
c     0 if no error.
c     31 if OFFSET greater than RECLEN.
c     32 if F/X/Y arrays not large enough to hold all descriptors.
c     33 if a descriptor is malformed in some odd way.

c     NOTE: If an integer is only 16 bits, LENGTH may have to be treated
c     as a real value rather than an integer, necessitating modifications.

      integer reclen,rec(reclen),offset,maxnum
      integer length,numsub,obsdat,cmpdat,numdes
      integer f(maxnum),x(maxnum),y(maxnum)
      integer i

      bfsec3=0
      if (offset.ge.reclen) then
         bfsec3=31
         RETURN
      end if
      length=rec(offset+1)*65536+rec(offset+2)*256+rec(offset+3)
      numsub=rec(offset+5)*256+rec(offset+6)
      obsdat=0
      if (rec(offset+7).ge.128) obsdat=1
      cmpdat=0
      if (mod(rec(offset+7),128).ge.64) cmpdat=1

      numdes=0
      do 10 i=offset+8,offset+length-1,2
         if (numdes .ge. maxnum) then
            bfsec3=32
            RETURN
         end if
         numdes=numdes+1
         f(numdes)=int(rec(i)/64)
         x(numdes)=mod(rec(i),64)
         y(numdes)=rec(i+1)
c         print *,' I: ',numdes,' F: ',f(numdes),' X: ',x(numdes),
c     .        ' Y: ',y(numdes)
         if (f(numdes).gt.3 .or. f(numdes).lt.0 .or.
     .       x(numdes).gt.63 .or. x(numdes).lt.0 .or.
     .       y(numdes).gt.255 .or. y(numdes).lt.0) then
            bfsec3=33
            RETURN
         end if
 10   continue
      RETURN
      END
