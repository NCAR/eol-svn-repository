c
c $Id: ebufr_data.inc,v 2.2 1992/11/20 20:48:37 john Exp $
c 
c ebufr_data.inc - include file with DATA statements
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
c
c include file "ebufr_data.inc"
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.
c
c***********************************************************************
c  This file contains the data definitions needed by
c  the E-BUFR read routines.  This file should be included in all
c  main programs that use the E-BUFR library.
c
c  This include file should be included EXACTLY ONCE in the main
c  program after all variable declarations and before any local
c  data statements.
c
c***********************************************************************
c    Initialize descriptors that must be known by an E-BUFR program
c***********************************************************************
      integer ebi
      data numfxy/0/
      data numtbl/0/
      data curdes/19/
      data (xtab(ebi),ebi=1,19)/17*0,5,6/
      data (ytab(ebi),ebi=1,19)/010,011,012,013,014,015,016,017,
     +   018,019,020,255,254,253,252,251,250,1,1/
      data (scale(ebi),ebi=1,19)/17*0,5,5/
      data (ref(ebi),ebi=1,19)/17*0,-9000000,-18000000/
      data (name1(ebi),ebi=1,19)/
     +     'F of descriptor','X of descriptor','Y of descriptor',
     +     'Element name line 1','Element name line 2',
     +     'Units name','Units scale sign','Units scale',
     +     'Units reference sign','Units reference value',
     +     'Element data width',
     +     'Code figure/flag','Code/flag desc. 1',
     +     'Code/flag desc. 2','Code/flag desc. 3',
     +     'Code/flag desc. 4','Code/flag desc. 5',
     +     'Latitude (high accuracy)','Longitude (high accuracy)'/
      data (name2(ebi),ebi=1,19)/19*' '/
      data (nbits(ebi),ebi=1,19)/8,16,24,256,256,192,8,24,8,80,24,
     +     24,256,256,256,256,256,25,26/
      data (units(ebi),ebi=1,19)/17*'CCITT IA5',2*'Degrees'/
c***********************************************************************
c   End of Include file "ebufr_data.inc 
c***********************************************************************
c
