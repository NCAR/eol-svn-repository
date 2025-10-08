C
C $Id: close-unit.f,v 1.2 1992/08/18 15:15:27 john Exp $
C $Log: close-unit.f,v $
C Revision 1.2  1992/08/18 15:15:27  john
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

C The following is a sample version of how to code the
C routine  CLSUNI passed to the WRTHDR routine.  See the
C user's guide for more information on what this routine
C does. 
C --------------------------------------------------
C Routine Long Name: close-unit
C  6 Character Name: CLSUNI
C           Purpose: To close a file opened for accessing the code/flag
C                    table associated with the (F,X,Y)-triple
C                    specified.
C Import Parameters:
C    LUNIT   --  unit opened for code/flag table.
C    FF      --  }
C    XX      --  }   Specify the (F,X,Y) triple of the parameter.
C    YY      --  }
C    ISCTAB  --  True  if the triple corresponds to a code table;
C                False if the triple corresponds to a flag table.
C Export Parameters: (none)
C     Prerequisites: ???
C Commons directly accessed: (none)
      SUBROUTINE CLSUNI(LUNIT, FF,XX,YY, ISCTAB)
      INTEGER LUNIT
      INTEGER FF, XX, YY
      LOGICAL ISCTAB


      CLOSE(UNIT=LUNIT, ERR=999)
      RETURN

C     Come here on closing error
 999  PRINT 8999, LUNIT, FF,XX,YY
      CALL ENCERR(42)

 8999 FORMAT (1X, 'Could not close unit ',I2,
     $        ' for (F,X,Y) = (', I2, ', ', I2, ', ', I3,
     $                       ') flag/code file.'/
     $        1X, 'Aborting program.')
      END
