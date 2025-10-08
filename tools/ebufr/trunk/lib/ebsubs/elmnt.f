C
C $Id: elmnt.f,v 2.4 1992/08/17 18:02:55 john Exp $
C
C elmnt - extract a single element from a BUFR section 4
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
      integer function elmnt(rec,reclen,offset,index,bitnum,
     .        numsub,cmpdat,xarg,yarg,datum,fmt)

c     Function extracts a single element (F=0) from a BUFR section 4.
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     On entry:
c     REC holds the BUFR data.
c     RECLEN is the maximum length of the REC array.
c     OFFSET is the beginning point of the BUFR section within REC.
c     INDEX is the number of the current octet.
c     BITNUM is the (BUFR-style) number of the bit in the current octet.
c     NUMSUB is the number of data subsets.
c     CMPDAT is 1 if the data are compressed, 0 otherwise.
c     XARG, and YARG indicate the appropriate descriptor.

c     On exit:
c     INDEX and BITNUM are correct.
c     DATUM is an internal file containing the integer or ASCII
c          representation of the datum.
c     FMT is the description of the format used to read/write DATUM.

c     Returns:
c     0 if no error.
c     1 if descriptor has been seen, but never described.
c     2 if descriptor has never been seen.
c     3 if data requested overrun record.
c     4 if data are compressed.  (Not yet understood by program.)

c     BUGS:
c     Assumes integers are big enough to handle any requested
c     number of bits.  (In practice, this means at least 32-bit integers.)
c     Doesn't handle compressed data.  (Yet!)
c     Determining ASCII vs. integer by the units name is ugly and prone
c     to breakage.
c     Doesn't handle missing data; this is left to the calling routine,
c     except in the case of character data -- missing characters are
c     replaced by '$' characters.

      integer reclen,rec(reclen),offset,xarg,yarg,i
      integer j,temp,nxtbts,index,bitnum
      integer numsub,cmpdat,nchar
      character*(*) datum
      character*8 fmt

c
c Includes for low level E-BUFR routines
c
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'

      elmnt=0
      if (cmpdat.ne.0) then
         elmnt=4
         RETURN
      end if

      do 10 i=1,curdes
         if (xtab(i).eq.xarg .and. ytab(i).eq.yarg) then
            if (nbits(i).gt.0) then
               if (units(i)(1:9).eq.'CCITT IA5' .or.
     .              units(i)(1:9).eq.'ccitt ia5' .or.
     .              units(i)(1:3).eq.'IA5' .or.
     .              units(i)(1:3).eq.'ia5' .or.
     .              units(i)(1:8).eq.'CCITTIA5' .or.
     .              units(i)(1:8).eq.'CCITT5' .or.
     .              units(i)(1:8).eq.'ccitt5' .or.
     .              units(i)(1:8).eq.'ccittia5') 
     .              then
                  nchar=nbits(i)/8
                  write(fmt,'(''(A'',i3,'')'')')nchar
                  do 20 j=1,nchar
                     temp=nxtbts(8,rec,reclen,offset,index,bitnum)
                     if (temp.eq.255) then
c     Missing datum!
                        datum(j:j)='$'
                     else
                        datum(j:j)=char(temp)
                     end if
 20               continue
                  RETURN
               else
                  temp=nxtbts(nbits(i),rec,reclen,offset,index,bitnum)
                  if (temp.eq.-1) then
                     elmnt=3
                     RETURN
                  end if
                  write(fmt,'(''(I'',i3,'')'')')len(datum)
                  write(datum,fmt)temp
                  RETURN
               end if
            else
               elmnt=1
               RETURN
            end if
         end if
 10   continue
      elmnt=2
      RETURN
      END
