C
C $Id: bferr.f,v 2.4 1992/08/17 18:02:31 john Exp $
C
C bferr - write error messages for BUFR read routines
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
      subroutine bferr(err,ounit)

c     Routine writes message corresponding to error ERR on OUNIT.
c
c Written at by Mark Bradford NCAR/Office of Field Project Support.

      integer err,ounit
      integer NUMERR
      parameter (NUMERR=22)
      integer ecode(NUMERR),i
      character*60 msg(NUMERR)

      data ecode/1,2,3,4,11,21,31,32,33,41,42,43,44,45,46,47,48,
     .     51,52,53,54,91/

      data (msg(i),i=1,10)/
     .     'Data descriptor seen, but never defined (in ELMNT).',
     .     'Data descriptor never seen (in ELMNT).',
     .     'Data overrun record size (in ELMNT).',
     .     'Compressed data (not yet supported) (in ELMNT).',
     .     'Offset greater than record length in BFSEC0.',
     .     'Offset greater than record length in BFSEC1.',
     .     'Offset greater than record length in BFSEC3.',
     .     'Arrays for F, X, and Y too small in BFSEC3.',
     .     'Malformed descriptor found in BFSEC3.',
     .     'Offset greater than record length in BFSEC4.'/
      data (msg(i),i=11,20)/
     .     'Arrays too small to hold decoded data in BFSEC4.',
     .     'Inappropriate replication factor specified (F not 0).',
     .     'Text field (F=2 X=05) too large for output array.',
     .     'Attempt to replicate non-element (F not 0) descriptor.',
     .     'Operator (F=2) descriptor not yet implemented.',
     .     'Sequence descriptors (not yet supported).',
     .     'F out of range (less than 0 or greater than 3).',
     .     'Data descriptor not defined.',
     .     'Data descriptor not recognized.',
     .     'Data overrun record.'/
      data (msg(i),i=21,NUMERR)/
     .     'Compressed data (not yet supported).',
     .     'Unrecognized descriptor in BFINFO.'/

      do 10 i=1,NUMERR
         if (err.eq.ecode(i)) then
            write(ounit,*)' Error ',err,': ',msg(i)
            RETURN
         end if
 10   continue
      write(ounit,*)' Unrecognized error number ',err,'.'
      RETURN
      END
