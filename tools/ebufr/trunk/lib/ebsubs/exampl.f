C
C $Id: exampl.f,v 2.14 1994/12/08 23:26:26 john Exp $
C
C EXAMPL - Example program to use EBUFR Read Routines (EBRR)
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
C See the "Readme.ebrr" and "ebrr.doc" files for specific information
C on functions and subroutines called in this program.
C
C This program is intended to be an example and can be modified to output
C information in a particular format. Use this program as a guideline
C for structure and call sequence. If converting to another format, the
C GETNUS() and GETCFS() functions should probably be dropped. They return
C (sometimes long) character strings. See also "bare.f", a stripped-down
C version of exampl.
C
C This program is written in mostly ANSI Standard FORTRAN 77.
C The only exception that I know of is INCLUDE statements.
C I do not follow standard variable naming conventions, but instead declare
C all my variables. (I compile with "f77 -u" on the Sun, which says nothing
C is IMPLICIT.)
C After line 9999, I call STANDARD_ARITHMETIC as a fix for optimized Sun code.
C
C Functions and Subroutines:
C  EBRR GETOBS GETVAL GETNUS GETCFS
C  SUN STANDARD_ARITHMETIC
C
C23456789012345678901234567890123456789012345678901234567890123456789012

         PROGRAM EXAMPL
C
C EXAMPL - Example program to use EBUFR Read Routines (EBRR)
C

C External function declarations
         EXTERNAL GETVAL
         EXTERNAL GETOBS
         EXTERNAL GETNUS
         EXTERNAL GETCFS

C The INCLUDE command has the effect of inserting the named file at that point.
C PARAMETER statements for EBUFR read routines
         INCLUDE 'ebufr_parm.inc'

C Local PARAMTER statements go here


C Variables holding data for the current observation, and header record data
C These variables are accessed in the main program, and sent to
C  GETOBS, GETVAL, etc. as parameters
         INCLUDE 'ebufr_obs.inc'
C Variables used internally by EBUFR read routines
C (accessed internally via COMMON blocks)
         INCLUDE 'ebufr_vars.inc'

C Local variable declarations go here
         INTEGER IUNIT,OUTUNI,MSUNIT,J,RECCT,K
         CHARACTER*256 INFIL,OUTFIL
         CHARACTER*64 NAMSTR
         CHARACTER*24 UNISTR
         LOGICAL STDOUT

C following local variables needed for calling read routines
         INTEGER NUMDAT,CT
         REAL RVAL
         LOGICAL FSTTIM,ERR,EOF,CHKALL,PRNIDN
         CHARACTER*1 VALTYP
         CHARACTER*128 CVAL
         CHARACTER*160 CFSTR(MAXENT)

C Local COMMON statements go here


C DATA statements to initialize variables for EBUFR read routines
         INCLUDE 'ebufr_data.inc'

C Local DATA statements go here


C Actual code begins now
         WRITE (*,*) 'Welcome to exampl!'

C Here is where the top of a loop to process several input files would go
C50       CONTINUE

C Initializations
         IUNIT = 11
         OUTUNI = 12
C MSUNIT - error message unit; 0 is stderr on Sun
         MSUNIT = 0
         RECCT = 0
C FSTTIM,EOF,PRNIDN must be (re)set before each input file
         FSTTIM = .TRUE.
         EOF = .FALSE.
C PRNIDN - flag whether I've printed the Identification structure yet or not
C (can't use FSTTIM since GETOBS will change it before I can reference it)
         PRNIDN = .FALSE.
C CHKALL - do you want all record types checked for errors (TRUE), or
C     do you want to ignore records unimportant to the data (FALSE)
         CHKALL = .TRUE.

C Get filenames, open files
         WRITE (*,*) 'Enter EBUFR filename to read:'
         READ (*,'(A)') INFIL
C next 3 lines for the possible several input file loop
C         IF (INFIL(1:4) .EQ. 'stop') THEN
C           GO TO 301
C         END IF
         WRITE (*,'(A,A50)') 'Opening ',INFIL
         OPEN (FILE=INFIL, UNIT=IUNIT, STATUS='OLD', ERR=9000)
         WRITE (*,*) 'Output to a file (y/n)?'
         READ (*,'(A)') OUTFIL
         IF ((OUTFIL(1:1) .EQ. 'Y') .OR. (OUTFIL(1:1) .EQ. 'y')) THEN
           WRITE (*,*) 'Enter output filename:'
           READ (*,'(A)') OUTFIL
           WRITE (*,'(A,A50)') 'Opening ',OUTFIL
           OPEN (FILE=OUTFIL, UNIT=OUTUNI, STATUS='NEW', ERR=9000)
           STDOUT = .FALSE.
         ELSE
C Standard output
           OUTUNI = 6
           STDOUT = .TRUE.
         END IF
         WRITE (*,'(A,/)') 'Working...'

C Main code block
C  The basic idea is to get the data for an observation, then
C  decode the data. We place this in a main loop in order to get
C  data for each observation in turn. The current main loop
C  structure is an infinite loop using a goto statement. The
C  loop is exited when an error occurs, or when the end of file
C  is reached.
C
C  Decoding the data also requires a loop, since we must call
C  GETVAL for each data item separately.
C
C Pseduocode:
c while not end of file
c   call getobs
c   if err then quit processing this file
c   if this is the first time then write file identification info
c   write nominal date/time, location info for this observation
c   for each data value in this observation
c      call getval
c      if err then quit processing this file
c      write decoded data value
c   end for
c end while
c  
100      CONTINUE

C GETOBS - get data for the next observation (the next EBUFR record)
         CALL GETOBS (IUNIT,MSUNIT,DATUM,FMT,NUMDAT,
     .     XOUT,YOUT,ERR,EOF,FSTTIM,CHKALL)
         IF (ERR) THEN
           WRITE (MSUNIT,*) 'Error occurred in GETOBS.'
           GO TO 300
         ELSE IF (EOF) THEN
           WRITE (OUTUNI,*) 'End of input.'
           GO TO 300
         ELSE
           IF ((CHKALL) .AND. (.NOT. PRNIDN)) THEN
C Print out all the identification stuff from the EBUFR header records
C (note that the info won't be read in unless CHKALL is .TRUE.)
C This information occurs once per file, so we only want to print it
C out just before the first group of data. Therefore, we have to use
C the PRNIDN variable to tell us when to print this stuff. We can't
C use FSTTIM since it will be set to FALSE during the first call to GETOBS.
             WRITE (OUTUNI,'(A,/)') 'EBUFR File Header Info:'
             WRITE (OUTUNI,*) 'EBUFR Edition Number: ',EBEDN
             IF (DATTYP .EQ. 0) THEN
               WRITE (OUTUNI,*) 'Encoded data is in BUFR.'
             ELSE
               WRITE (OUTUNI,*) 'Encoded data is in GRIB.'
             END IF
             WRITE (OUTUNI,*) 'Encoding Edition Number: ',BGEDN
             WRITE (OUTUNI,*) 'Master Table Edition Number: ',MTEDN
             WRITE (OUTUNI,*) 'BUFR Specification Edition: ',EDITN
             WRITE (OUTUNI,*) 'Originating Center: ',CENTER
             WRITE (OUTUNI,*) 'Update sequence number: ',UPDATE
             WRITE (OUTUNI,*) 'BUFR message type: ',MSGTYP
             WRITE (OUTUNI,*) 'BUFR message subtype: ',SUBTYP
             WRITE (OUTUNI,*) 'Version Number of Local tables: ',VER
             WRITE (OUTUNI,*) 'EBUFR type 1 record Date, Time:'
             WRITE (OUTUNI,101) EBYEAR,EBMON,EBDAY,EBHOUR,EBMIN
101          FORMAT (X,I2.2,'/',I2.2,'/',I2.2,X,I2.2,':',I2.2)
             WRITE (OUTUNI,*) 'Descriptive Text (EBUFR type 3 records):'
             DO 105, K=1,NUMTXT
               WRITE (OUTUNI,'(X,A80)') EBTEXT(K)
105          CONTINUE
             PRNIDN = .TRUE.
             IF (STDOUT) THEN
               WRITE (OUTUNI,'(/,A)') '*** Press return to continue'
               READ (*,'(A)')
             END IF
           END IF
           RECCT = RECCT + 1
           WRITE (OUTUNI,'(/,A,I3)') 'Record ',RECCT
C Each data record has some "non-data" information that is important.
C This is the Nominal Date and Time, the Lat and Lon, and an ID number.
C (This is the info used to sort, merge, and select E-BUFR data.)
           WRITE (OUTUNI,110) ' Nominal Date, Time: ',
     .       CLYEAR(3:4),CLMON,CLDAY,
     .       CLHOUR,CLMIN,CLSEC
110        FORMAT (A,A2,'/',A2,'/',A2,X,A2,':',A2,':',A2)
           WRITE (OUTUNI,'(A,F11.5)') ' Latitude = ',CLLAT
           WRITE (OUTUNI,'(A,F11.5)') ' Longitude = ',CLLON
           WRITE (OUTUNI,'(A,A3)') ' Unique ID = ',CLIBYT
         END IF

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
C If VALTYP is 'M', then this is a missing value
C  (note that for some data, missing is a real data
C  item and shouldn't be ignored; e.g. the missing value
C  for the STORM QC Flag (0 33 255) means that the data to which
C  the flag corresponds is missing, not that the flag itself is missing)
           WRITE (OUTUNI,'(A,I2,A3,I3,A1)')
     .       'Data for BUFR Descriptor X=',XOUT(J),' Y=',YOUT(J),':'
           WRITE (OUTUNI,*) '  Missing value.'
         ELSE
           WRITE (OUTUNI,'(A,I2,A3,I3,A1)')
     .       'Data for BUFR Descriptor X=',XOUT(J),' Y=',YOUT(J),':'

C Get the Name and Units Strings (optional)
           CALL GETNUS(XOUT(J),YOUT(J),NAMSTR,UNISTR,MSUNIT,ERR)
           IF (.NOT. ERR) THEN
             WRITE (OUTUNI,'(A,A64)') ' Parameter Name=',NAMSTR
             WRITE (OUTUNI,'(A,A24)') ' Parameter Units = ',UNISTR
           END IF

C If we have a number in RVAL
           IF (VALTYP .EQ. 'R') THEN
             WRITE (OUTUNI,'(A10,F)') '  Value = ',RVAL

C Look up RVAL in the Code or Flag Table
C  Return description in CFSTR (optional)
             IF ((UNISTR(1:4) .EQ. 'Code') .OR.
     .          (UNISTR(1:4) .EQ. 'Flag')) THEN
               CALL GETCFS(XOUT(J),YOUT(J),RVAL,CFSTR,CT,MSUNIT,ERR)
               IF (.NOT. ERR) THEN
C Write each applicable description (could be >1 for Flag tables)
                  WRITE (OUTUNI,*)
     .              '  Decoded Text (truncated to 73 chars):'
                  DO 190, K=1,CT
                    WRITE (OUTUNI,'(6X,A73)') CFSTR(K)
190               CONTINUE
               END IF
             END IF

C Else if we have a Character string in cval
           ELSE IF (VALTYP .EQ. 'C') THEN
             WRITE (OUTUNI,'(A10,A32)') '  Value = ',CVAL
           ELSE
             WRITE (MSUNIT,*) 'Unknown Value Type returned from GETVAL'
           END IF
         END IF

C Continue statement for DO loop
200      CONTINUE

         IF (STDOUT) THEN
           WRITE (OUTUNI,'(/,A)') '*** Press return to continue'
           READ (*,'(A)')
         END IF
C Go back to GETOBS to get the next observation
C (this is the "end while" pseudocode statement for "while not end of file")
         GO TO 100

C Exit place for errors or end of file reading the current input file
300      CONTINUE

         WRITE (*,*) 'Closing files...'
         CLOSE (IUNIT)
C Don't want to (try to) close the standard output!
         IF (OUTUNI .NE. 6) THEN
           CLOSE (OUTUNI)
         END IF

C Here is where you would put the bottom of a loop to process
C  more than one input file
C         GO TO 50

C Exit place for the several input file loop
301      CONTINUE

C skip over the WRITE statements below
         GO TO 9999

C FORMAT statements for general use
c8000

C WRITE statements for general use, e.g. for errors

9000     WRITE (*,*) 'Error opening file.'
         GO TO 9999

C End of program

9999     CONTINUE
         WRITE (OUTUNI,*) 'End of exampl.'
         IF (OUTUNI .NE. 6) THEN
           WRITE (*,*) 'End of exampl.'
         END IF

C Fix for -fnonstd flag in -fast option on Sun's compiler
C Comment the next line for other systems
         CALL STANDARD_ARITHMETIC()

         STOP
         END
