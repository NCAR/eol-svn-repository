C
C $Id: bare.f,v 2.7 1994/12/08 23:26:24 john Exp $
C
C BARE - Bare Bones Version of using the EBRR
C
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
C Written by: John J. Allison
C University Corporation for Atmospheric Research
C Office of Field Project Support
C

         PROGRAM BARE
C
C This program is meant to illustrate the minimal usage of the
C   EBUFR Read Routines (EBRR).
C See the "Readme.ebrr" and "ebrr.doc" files for more info.
C

C EBRR functions used
         EXTERNAL GETVAL
         EXTERNAL GETOBS

C *** EBRR include files MUST BE IN THIS ORDER
         INCLUDE 'ebufr_parm.inc'
         INCLUDE 'ebufr_obs.inc'
         INCLUDE 'ebufr_vars.inc'

C Local variable declarations go here
         INTEGER IUNIT,OUTUNI,MSUNIT,J,RECCT
         CHARACTER*256 INFIL,OUTFIL
C following local variables needed for calling read routines
         INTEGER NUMDAT
         REAL RVAL
         LOGICAL FSTTIM,ERR,EOF
         CHARACTER*1 VALTYP
         CHARACTER*128 CVAL

C *** EBRR include file MUST GO HERE
         INCLUDE 'ebufr_data.inc'

C Initializations
         IUNIT = 11
         OUTUNI = 12
C MSUNIT - error message unit; 0 is stderr on Sun
         MSUNIT = 0
         RECCT = 0
C FSTTIM,EOF must be (re)set before each input file
         FSTTIM = .TRUE.
         EOF = .FALSE.

C Get filenames, open files
         WRITE (*,*) 'Enter EBUFR filename to read:'
         READ (*,'(A)') INFIL
         WRITE (*,'(A,A50)') 'Opening ',INFIL
         OPEN (FILE=INFIL, UNIT=IUNIT, STATUS='OLD', ERR=9000)
         WRITE (*,*) 'Output to a file (y/n)?'
         READ (*,'(A)') OUTFIL
         IF ((OUTFIL(1:1) .EQ. 'Y') .OR. (OUTFIL(1:1) .EQ. 'y')) THEN
           WRITE (*,*) 'Enter output filename:'
           READ (*,'(A)') OUTFIL
           OPEN (FILE=OUTFIL, UNIT=OUTUNI, STATUS='NEW', ERR=9000)
         ELSE
C Standard output
           OUTUNI = 6
         END IF

C the main loop
100      CONTINUE

C GETOBS - get data for the next observation (the next EBUFR record)
         CALL GETOBS (IUNIT,MSUNIT,DATUM,FMT,NUMDAT,
     .     XOUT,YOUT,ERR,EOF,FSTTIM,.FALSE.)
         IF (ERR) THEN
           WRITE (MSUNIT,*) 'Error occurred in GETOBS.'
           GO TO 300
         ELSE IF (EOF) THEN
           GO TO 300
         ELSE
           RECCT = RECCT + 1
           WRITE (OUTUNI,'(/,A,I1)') 'Record ',RECCT
           WRITE (OUTUNI,110) 'Nominal Date, Time: ',
     .       CLYEAR(3:4),CLMON,CLDAY,
     .       CLHOUR,CLMIN,CLSEC
110      FORMAT (A,A2,'/',A2,'/',A2,X,A2,':',A2,':',A2)
           WRITE (OUTUNI,'(A,F10.5,3X,A,F11.5,3X,A,A3)')
     .       'Latitude = ',CLLAT,' Longitude = ',CLLON,
     .       ' Unique ID = ',CLIBYT
         END IF

         WRITE (OUTUNI,'(A12)') 'XX YYY Value'
         WRITE (OUTUNI,'(A13)') '== === ======'

C For each piece of data, call GETVAL
         DO 200, J=1,NUMDAT,1

C GETVAL - get decoded value
         CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
     .     RVAL,CVAL,VALTYP,MSUNIT,ERR)
         IF (ERR) THEN
           WRITE (MSUNIT,*) 'Error occurred in GETVAL.'
           GO TO 300
         END IF
         IF (VALTYP .EQ. 'M') THEN
           WRITE (OUTUNI,8000) XOUT(J),YOUT(J),'Missing value'
         ELSE
           IF (VALTYP .EQ. 'R') THEN
             WRITE (OUTUNI,8001) XOUT(J),YOUT(J),RVAL
           ELSE IF (VALTYP .EQ. 'C') THEN
             WRITE (OUTUNI,8000) XOUT(J),YOUT(J),CVAL
           ELSE
             WRITE (MSUNIT,*) 'Unknown VALTYP returned from GETVAL.'
           END IF
         END IF
200      CONTINUE

         GO TO 100

300      CONTINUE

         CLOSE (IUNIT)
C Don't want to (try to) close the standard output!
         IF (OUTUNI .NE. 6) THEN
           CLOSE (OUTUNI)
         END IF

C skip over the WRITE statements below
         GO TO 9999

C FORMAT statements for general use
8000     FORMAT (I2,X,I3,X,A)
8001     FORMAT (I2,X,I3,X,F)

C WRITE statements for general use, e.g. for errors

9000     WRITE (*,*) 'Error opening file.'
         GO TO 9999

C End of program

9999     CONTINUE

C Fix for -fnonstd flag in -fast option on Sun's compiler
C Comment the next line for other systems
         CALL STANDARD_ARITHMETIC()

         STOP
         END
