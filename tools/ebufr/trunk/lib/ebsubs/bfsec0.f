C
C $Id: bfsec0.f,v 2.4 1992/08/17 18:02:35 john Exp $
C
C bfsec0 - decode EBUFR record 0
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
      integer function bfsec0(rec,reclen,offset,ebedn,dattyp,bgedn,
     .     mtedn)

c     This routine decodes an EBUFR section type 0 (Indicator).
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     On entry:
c     REC holds the BUFR data.
c     RECLEN is the maximum length of the REC array.
c     OFFSET is the beginning point of the EBUFR section within REC.

c     On exit:
c     EBEDN is the EBUFR edition number.
c     DATTYP is 0 if the encoding is BUFR, 1 if GRIB.
c     BGEDN is the BUFR/GRIB edition number.
c     MTEDN is the edition number of the BUFR/GRIB master tables.

c     Returns:
c     0 if no error.
c     11 if OFFSET is greater than RECLEN.

      integer reclen,rec(reclen),offset
      integer ebedn,dattyp,bgedn,mtedn

      bfsec0=0
      if (offset.ge.reclen) then
         bfsec0=11
         RETURN
      end if
      ebedn=rec(offset+1)
      dattyp=rec(offset+2)
      bgedn=rec(offset+3)
      mtedn=rec(offset+4)
      RETURN
      END
