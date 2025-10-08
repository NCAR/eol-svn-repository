c
c $Id: ebufr_parm.inc,v 2.4 1995/03/10 17:43:42 john Exp $
c
c ebufr_parm.inc - include file defining the parameter values
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
c***********************************************************************
c  Include file "ebufr_parm.inc"
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.
c***********************************************************************
c
c***********************************************************************
c  The following parameters control array sizes:
c     MAXLEN is the maximum length of an EBUFR record.
c     MAXREC is the maximum number of records allowed to make up an entity.
c     HDRSIZ is the size of the EBUFR header on each record.
c     MAXDES is the maximum number of BUFR descriptors the code can manage
c            at one time.
c     MAXSIZ is the size of the entity-holding array.
c     MAXTBL is the maximum number of code tables the code can handle.
C     MAXENT is the maximum number of entries a single code table may 
c            have.
c     MAXSEQ is the maximum number of sequence descriptors the code can
c            handle.
c     MAXSQL is the maximum number of descriptors that can be in the
c            definition of a single sequence descriptor. 
c     MAXDAT is the maximum number of data values in a single observation
C     MAXTXT is the maximum number of textual description lines from an
C            EBUFR type 3 record that can be stored.
c***********************************************************************
      integer MAXREC,MAXLEN,HDRSIZ,MAXDES,MAXTBL
      integer MAXSEQ,MAXSQL,MAXDAT,MAXSIZ,MAXENT
      integer MAXTXT
      parameter (MAXREC=10,MAXLEN=9999,HDRSIZ=22,MAXDES=512)
      parameter (MAXSIZ=MAXREC*MAXLEN)
      parameter (MAXTBL=64,MAXENT=512)
      parameter (MAXSEQ=50,MAXSQL=10)
      parameter (MAXDAT=64000)
      parameter (MAXTXT=10)
c***********************************************************************
c End of include file "ebufr_parm.inc"
c***********************************************************************
