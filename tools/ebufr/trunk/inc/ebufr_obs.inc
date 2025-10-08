c
c $Id: ebufr_obs.inc,v 2.2 1994/12/08 23:26:26 john Exp $
c
c ebufr_obs.inc - include file with variables for the current observation
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
c******************************************************************
c Include file "ebufr_obs.inc"
c   Variables that hold data for the current observation.
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.
c******************************************************************
C NEVER make these COMMON. This file is included by getobs() as
C   a way of defining its parameters. If these become COMMON, then
C   the parameters to getobs() will have to be removed or changed.
C
      character*128 datum(MAXDAT)
      character*8 fmt(MAXDAT)
      integer xout(MAXDAT),yout(MAXDAT)

c      structure /ebhdr/
         character*4 clyear
         character*2 clmon
         character*2 clday
         character*2 clhour
         character*2 clmin
         character*2 clsec
         real cllat
         real cllon
         character*3 clibyt
c      end structure
c      record /ebhdr/ hdr
       common /ebhdr1/ clyear,clmon,clday,clhour,clmin,clsec
       common /ebhdr2/ cllat,cllon,clibyt

c      structure /ebidnt/
	 integer ebedn,dattyp,bgedn,mtedn
	 integer editn,center,update,msgtyp,subtyp,ver
	 integer ebyear,ebmon,ebday,ebhour,ebmin
	 integer numtxt
	 character*80 ebtext(MAXTXT)
c      end structure
c      record /ebidnt/ ident
       common /ebidn0/ ebedn,dattyp,bgedn,mtedn
       common /ebidn1/ editn,center,update,msgtyp,subtyp,ver
       common /ebidn2/ ebyear,ebmon,ebday,ebhour,ebmin
       common /ebidn3/ numtxt,ebtext

c******************************************************************
c End of include file "ebufr_obs.inc"
c******************************************************************
