C
C $Id: bfsec1.f,v 2.4 1992/08/17 18:02:37 john Exp $
C
C bfsec1 - decode a BUFR section 1
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
      integer function bfsec1(rec,reclen,offset,length,
     .     editn,center,update,optsec,msgtyp,subtyp,ver,
     .     year,month,day,hour,minute)

c     This routine decodes a BUFR section type 1 (Identification).
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     On entry:
c     REC holds the BUFR data.
c     RECLEN is the maximum length of the REC array.
c     OFFSET is the beginning point of the BUFR section within REC.

c     On exit:
c     EDITN is the edition number of the BUFR specification used.
c     CENTER is the originating center (not yet defined).
c     UPDATE is the update sequence number.
c     OPTSEC is 1 if an optional section exists, 0 otherwise.
c     MSGTYP is the type of the BUFR message.
c     SUBTYP is the (locally defined) BUFR message subtype.
c     VER is the version number of local tables (0 for BUFR standard).
c     YEAR, MONTH, DAY, HOUR, and MINUTE hold appropriate values.

c     Returns:
c     0 if no error.
c     21 if OFFSET is greater than RECLEN.

c     NOTE: If an integer is only 16 bits, LENGTH may have to be treated
c     as a real value rather than an integer, necessitating modifications.

      integer reclen,rec(reclen),offset
      integer length,editn,center,update,optsec,msgtyp,subtyp,ver
      integer year,month,day,hour,minute

      bfsec1=0
      if (offset.ge.reclen) then
         bfsec1=21
         RETURN
      end if
      length=rec(offset+1)*65536+rec(offset+2)*256+rec(offset+3)
      editn=rec(offset+4)
      center=rec(offset+5)*256+rec(offset+6)
      update=rec(offset+7)
      optsec=0
      if (rec(offset+8).ge.128) optsec=1
      msgtyp=rec(offset+9)
      subtyp=rec(offset+10)
      ver=rec(offset+11)*256+rec(offset+12)
      year=rec(offset+13)
      month=rec(offset+14)
      day=rec(offset+15)
      hour=rec(offset+16)
      minute=rec(offset+17)
      RETURN
      END
