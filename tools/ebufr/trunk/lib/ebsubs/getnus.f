C
C $Id: getnus.f,v 2.5 1992/11/20 20:48:41 john Exp $
C
C getnus - get name and units strings
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

         SUBROUTINE GETNUS (X,Y,NAMSTR,UNISTR,MSUNIT,ERR)
C
C GETNUS - GET Name and Unit Strings
C
C Written by: John J. Allison
C             NCAR/OFPS
C
C Calls BFINFO to set NAMSTR and UNISTR
C User could just as easily call BFINFO themselves; this function
C   is here so user doesn't get confused by nbits,scale,ref
C
c Returns strings containing the Name and Units for a given BUFR parameter.
c
c In:
c  x - X descriptor for info desired
c  y - Y descriptor for info desired
c  (F descriptor assumed 0)
c  msunit - unit number of file to which to write error message
c
c Out:
c  namstr - character string containing Name of X Y descriptor
c  unistr - character string containing Units of X Y descriptor
c  err - error flag
c
c An error indicates that the descriptor specified by X Y is unknown.
c

C Parameter definitions
         INTEGER X,Y,MSUNIT
         CHARACTER*64 NAMSTR
         CHARACTER*24 UNISTR
         LOGICAL ERR

C External function
         INTEGER BFINFO
         EXTERNAL BFINFO

C Local vars
         CHARACTER*32 TMPNM1,TMPNM2
         INTEGER TMPSCL,TMPREF,TMPBIT

         IF ( BFINFO(X,Y,TMPSCL,TMPREF,TMPBIT,TMPNM1,TMPNM2,UNISTR)
     .        .EQ. 0) THEN
           NAMSTR = TMPNM1 // TMPNM2
           ERR = .FALSE.
         ELSE
           ERR = .TRUE.
           WRITE (MSUNIT,'(A,I2,A3,I3,A)')
     .       'getnus(): Element X=',X,' Y=',Y,' uknown.'
         END IF
         RETURN
         END
