C
C $Id: bfinfo.f,v 2.4 1992/08/17 18:02:33 john Exp $
C
C bfinfo - return information about a BUFR descriptor
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
      integer function bfinfo(xarg,yarg,oscale,oref,onbits,
     .     oname1,oname2,ounits)

c     Function provides scale, reference value, number of bits, name,
c     and units for a known BUFR element.
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     Returns 0 for normal exit, 91 if element unknown.

c
c includes for low-level E-BUFR routines
c
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'
c
c local variables
c
      integer xarg,yarg,i,oscale,oref,onbits
      character*32 oname1,oname2
      character*24 ounits


      do 10 i=1,curdes
         if (xtab(i).eq.xarg .and. ytab(i).eq.yarg) then
            oscale=scale(i)
            oref=ref(i)
            onbits=nbits(i)
            oname1=name1(i)
            oname2=name2(i)
            ounits=units(i)
            bfinfo=0
            RETURN
         end if
 10   continue
      bfinfo=91
      RETURN
      END
