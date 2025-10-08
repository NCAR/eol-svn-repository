c
c $Id: ebufr_vars.inc,v 2.1 1992/11/18 17:26:10 john Exp $
c
c ebufr_vars.inc - include file with internal variables for read routines
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
c include file "ebufr_vars.inc"
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.
c
c***********************************************************************
c  This file contains the variable and data definitions needed by
c  the E-BUFR read routines.  This file should be included in all
c  main programs that use the E-BUFR library.
c
c  The include file "ebufr-parms.inc" should be included in the main
c  program before this file is included.
c
c***********************************************************************
c  The following variables are arrays for storing the numeric designations
c  of the E-BUFR descriptors
c***********************************************************************
      integer f(MAXDES),x(MAXDES),y(MAXDES),numfxy
      common /ebufr/ f,x,y,numfxy
c***********************************************************************
c  The following variables contain the definitions of the 
c  set of element descriptors (F=0) that the program has obtained from the
c  E-BUFR header records.  
c
c  Note that some descriptor definitions will be loaded from data statements
c  in "ebufr_data.inc".
c
c  Note that these quantities are irrelevant for F=1, F=2, and
c  F=3, so there is no need to store the value of F -- it will
c  always be 0.
c***********************************************************************
      integer curdes,xtab(MAXDES),ytab(MAXDES),
     +     scale(MAXDES),ref(MAXDES),nbits(MAXDES)
      character*32 name1(MAXDES),name2(MAXDES)
      character*24 units(MAXDES)
      common /desc1/curdes,xtab,ytab,scale,ref,nbits
      common /desc2/name1,name2,units
c***********************************************************************
c  The following variables contain the code table definitions the
c  program will obtain from the E-BUFR header records.  This is pretty
c  ugly in FORTRAN, requiring statically-allocated parallel arrays.
c
c  Note that since code tables are only relevant to F=0 descriptors,
c  it is unnecessary to remember the F value for each table.
c***********************************************************************
      character*32 code(5,MAXENT,MAXTBL)
      integer numtbl,nument(MAXTBL),xtbl(MAXTBL),ytbl(MAXTBL)
      logical flag(MAXTBL)
      common /code1/code
      common /code2/numtbl,nument,xtbl,ytbl,flag
c***********************************************************************
c  The following variables contain the currently understood set of
c  sequence (F=3) descriptors.  This too is ugly.
c
c  Note that since sequences always have F=3, it is unnecessary to
c  keep track of the F value for each sequence descriptor.
c***********************************************************************
C sequence stuff is leftover from EBDUMP and is currently unused - jja
c      character*6 seqent(MAXSQL,MAXSEQ)
c      integer seqlen(MAXSEQ),xseq(MAXSEQ),yseq(MAXSEQ)
c      integer numseq
c      common /seq1/seqent
c      common /seq2/xseq,yseq,seqlen
c      common /seq3/numseq
c***********************************************************************
c   End of Include file "ebufr_vars.inc 
c***********************************************************************
c
