C
C $Id: abort.f,v 1.2 1992/08/18 15:15:26 john Exp $
C $Log: abort.f,v $
C Revision 1.2  1992/08/18 15:15:26  john
C Added (more) copyright information.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C
c
c Copyright Notice for the E-BUFR Generic Encoder Routines
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

C Routine Long Name: EBUFR-error-end
C  6 Character Name: ebend
C           Purpose: to end an erroneous generic EBUFR encoder
C                    program
C Import Parameters: (none)
C Export Parameters: (none)...doesn't return!

      SUBROUTINE EBEND()
      STOP 2
C     not reached!
      END
