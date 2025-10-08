C
C $Id: ebscf.f,v 1.17 1999/08/26 21:20:16 gallant Exp $
C
C EBSCF - EBUFR TO SCF CONVERSION PROGRAM
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

	PROGRAM EBSCF
C
C EBSCF - EBUFR TO SCF CONVERSION PROGRAM
C
C Written by:
C  John J. Allison / john@lightning.ofps.ucar.edu
C  UCAR/OFPS
C  15 Dec 1992 and 5 Feb 1993
C
C Modification History:
C  Darren R. Gallant / gallant@ucar.edu
C  UCAR/JOSS
C  04 Aug 1997
C  change header line 4 format statement 8003 to write F7.1 instead of F7.2
C  to accomodate altitudes greater than 9999.9
C 
C  changed section recalculating dewpts: If rh is zero, set dewpt to missing
C  this will avoid illegal operation LOG(0)
C
C  18 Aug 1997
C
C  changed how program calculates non decimal lat/lon positions
C  code now uses signs of CLLON and CLLAT when determining whether
C  E or W and N or S shoud be written
C
C  23 Jan 1998
C 
C  changed QC flag procedure for ascension rate
C  if dz is missing Qdz is set to missing, i.e 9.0
C  else Qdz is set to unchecked, i.e 99.0
C
C  21 May 1999
C  Fields 13 and 14 no longer missing by default. Elevation and Azimuth angle
C  used in NWS wind processing will be default. However other data can be 
C  placed in these fields as well. For Example Mixing Ratio and Virtual temp.
C  Data must be contained in this range [-99.9,998.9].
C
C  25 May 1999
C  Changed headers for data fields. If Mixing Ratio present in OPTSTR array
C  header for field 13 becomes MixRat where units are g/kg. If Virtual is
C  present in OPTSTR array, field 13 header becomes VirT in deg k. If neither
C  is present the header remains Elev with units deg/deg.
C
C  12 August 1999
C  Added section to determine whether to print profile or release type header
C  If NETSTR contains Profile or MOLTS or if OPTSTR(K) contains FSL, MAPS or
C  not actual rawinsonde then profile type header printed else release type
C  is printed. The value of logical variable PROFILE determines which header
C  used; only when PROFILE is set to true is profile type header printed 
C
C Synopsis:
C  ebscf [ infile outfile ]
C
C If no files are given, ebqcf converts stdin to stdout.
C Otherwise, exactly two filenames must be given: the input (EBUFR) file
C and the output (SCF) file. Only one file is converted per invocation.
C
C Examples:
C  The following two examples are equivalent:
C  % ebscf < abc.ebufr > def.cls
C  % ebscf abc.ebufr def.cls
C
C This program was modified from the example program included in
C the E-BUFR Read Routines (EBRR) library, and uses library calls
C to the EBRR. The EBRR is distributed by JOSS. This program is
C not ANSI FORTRAN 77. I have used Sun's command line argument
C routines. I have used the TAB indentation method. I have used
C include files.
C
C Functions and Subroutines:
C  EBRR GETOBS GETVAL
C  SUN STANDARD_ARITHMETIC IARGC GETARG INDEX
C
C23456789012345678901234567890123456789012345678901234567890123456789012

C External function declarations
	EXTERNAL GETOBS
	EXTERNAL GETVAL
	INTEGER IARGC
C	EXTERNAL IARGC
C	EXTERNAL GETARG
C	INTEGER INDEX
C	EXTERNAL INDEX
	REAL CALHUM
	REAL CALDEW
	REAL CALASC
	INTEGER GETLEN

	INCLUDE 'ebufr_parm.inc'
	INCLUDE 'ebufr_obs.inc'
	INCLUDE 'ebufr_vars.inc'

C Local variable declarations go here
	INTEGER IUNIT,OUTUNI,MSUNI,J,RECCT,ARGCT,K,L
	CHARACTER*256 INFIL,OUTFIL,BASNAM
	INTEGER NUMDAT,NUMOHD
	INTEGER QCTAB(0:15)
	REAL RVAL,OLDTIM,OLDALT
	LOGICAL FSTTIM,ERR,EOF,CHKALL,FILSEN,HDRWRI,MIXRAT,VIRTUAL
	LOGICAL FSTSND,FSTDAT,NOHUM,NODEW,LNGSTE,PROFILE
	CHARACTER*1 VALTYP
	CHARACTER*95 CVAL
	REAL SDATA(21),LATMIN,LONMIN,ALT
	CHARACTER*10 NETSTR
	CHARACTER*35 SITEID
	CHARACTER*6 FMTST6
	CHARACTER*9 FMTST9
	CHARACTER*35 TMPSTR,NONAME
	CHARACTER*130 OPTSTR(6)
	INTEGER TIME(6),NOMTIM(6),TMPI,TMPJ
	CHARACTER LATCH,LONCH

	INCLUDE 'ebufr_data.inc'


	IUNIT = 11
	OUTUNI = 12
	MSUNI = 0
	FILSEN = .FALSE.
	NOHUM = .FALSE.
	NODEW = .FALSE.
        PROFILE = .FALSE.
        MIXRAT = .FALSE.
        VIRTUAL = .FALSE.
	BASNAM(1:1) = '*'
	NONAME = 'Noname Field:                      '
	QCTAB(0) = 99.0
	QCTAB(1) = 1.0
	QCTAB(2) = 2.0
	QCTAB(3) = 3.0
	QCTAB(8) = 4.0
	QCTAB(15) = 9.0
	QCTAB(4) = 55.0
	QCTAB(5) = 55.0
	QCTAB(6) = 55.0
	QCTAB(7) = 55.0
	QCTAB(9) = 55.0
	QCTAB(10) = 55.0
	QCTAB(11) = 55.0
	QCTAB(12) = 55.0
	QCTAB(13) = 55.0
	QCTAB(14) = 55.0
	FMTST6 = '(A130)'
	FMTST9 = '(A35,A80)'
	EBTEXT(1) = '                                        ' //
&	  '                                        '

	ARGCT = IARGC()
20	CONTINUE
	IF (ARGCT .EQ. 0) THEN
	  INFIL = 'stdin'
	  IUNIT = 5
	  OUTFIL = 'stdout'
	  OUTUNI = 6
	  GO TO 50
	ELSE IF (ARGCT .EQ. 2) THEN
	  CALL GETARG(1,INFIL)
	  CALL GETARG(2,OUTFIL)
	ELSE
	  WRITE (0,*) 'Usage: ebscf [ infile outfile ]'
	  GO TO 9999
	END IF

	OPEN (FILE=INFIL,UNIT=IUNIT,STATUS='OLD',ERR=9000)
49	CONTINUE
	OPEN (FILE=OUTFIL,UNIT=OUTUNI,STATUS='NEW',ERR=9000)
50	CONTINUE
	FILSEN = .TRUE.
	RECCT = 0
	FSTTIM = .TRUE.
	EOF = .FALSE.
C CHKALL says whether to check all EBUFR records, or just
C the important ones (2,9,?)
C - set to TRUE to get EBUFR Type 3 stuff out and into
C   the Project ID: header line
C	CHKALL = .FALSE.
	CHKALL = .TRUE.
	HDRWRI = .FALSE.

C is this the first sounding seen?
	FSTSND = .TRUE.

C This next block of comments remains from the EBRR example
C  program; it may no longer apply exactly. - jja
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

C *** ***** NOTE ****
C One ENTIRE sounding is put into one single EBUFR record
C That is, one call to GETOBS will return one entire sounding
C   which can be read through calls to GETVAL

100	CONTINUE

C GETOBS - get data for the next observation
	CALL GETOBS (IUNIT,MSUNI,DATUM,FMT,NUMDAT,
&	  XOUT,YOUT,ERR,EOF,FSTTIM,CHKALL)
	IF (ERR) THEN
	  WRITE (MSUNI,*) 'Error occurred in GETOBS.'
	  WRITE (MSUNI,*) 'RECCT = ',RECCT
	  GO TO 300
	ELSE IF (EOF) THEN
D	  WRITE (MSUNI,*) 'End of input.'
	  GO TO 300
	ELSE
	  RECCT = RECCT + 1
	END IF

C is this the first data seen in this sounding?
	FSTDAT = .TRUE.

C
C *** IMPORTANT, re: ascension rate
C   set OLDTIM to 9998.0 if you want the first ascension rate to be 0.0
C      with unchecked (99.0) QC flag
C   set OLDTIM to 9999.0 if you want the first asc.rate to be 999.0 (missing)
C      with missing (9.0) QC flag
C

C	OLDTIM = 9998.0
	OLDTIM = 9999.0

	OLDALT = 99999.0
	NUMOHD = 0
	LNGSTE = .FALSE.

	READ (CLYEAR,'(I4)') NOMTIM(1)
	READ (CLMON,'(I2)') NOMTIM(2)
	READ (CLDAY,'(I2)') NOMTIM(3)
	READ (CLHOUR,'(I2)') NOMTIM(4)
	READ (CLMIN,'(I2)') NOMTIM(5)
	READ (CLSEC,'(I2)') NOMTIM(6)

C For each piece of data, call GETVAL
C	DO 200, J=1,NUMDAT,1
	J = 1
101	CONTINUE
	IF (J .GT. NUMDAT) THEN
	  GO TO 200
	END IF

C GETVAL - get decoded value
	CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	IF (ERR) THEN
	  WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	  WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	  GO TO 300
	END IF

C This section added by Darren R. Gallant August 12th, 1999
C The goal here is to use NETSTR or OPTSTR to determine whether
C sounding is a release or profile type and print the appropriate header
C using LOGICAL PROFILE.
	IF(.NOT.PROFILE) THEN
	   IF(INDEX(NETSTR,"Profile").OR.INDEX(NETSTR,"MOLTS")) THEN
	      PROFILE = .TRUE.
           ELSE
              k = 1
	      DO WHILE(.NOT. PROFILE .AND. K .LE. NUMOHD)
		 IF(INDEX(OPTSTR(K),"MAPS").OR.INDEX(OPTSTR(K),"FSL")) THEN
		    PROFILE = .TRUE.
                 ELSE IF(INDEX(OPTSTR(K),"not actual rawinsonde")) THEN
                    PROFILE = .TRUE.
                 END IF
                 K = K+1
	      END DO
           ENDIF
        END IF


C here is where you put ebufr data into sdata

C this means the start of a new level in the sounding !!!
C so, if this is the first level then print out header lines
C     else print out the previous level (now in sdata)
C and set sdata(1) = rval
	IF ((XOUT(J) .EQ. 4) .AND. (YOUT(J) .EQ. 247)) THEN

	  IF (FSTDAT) THEN
C hdr line 1
	    WRITE (OUTUNI,'(A35,A10)')
&	      'Data Type:                         ',
&	      NETSTR
C hdr line 2
	    IF (NUMTXT .GE. 1) THEN
C   use EBUFR Type 3 if available
	      WRITE (FMTST9,'(A6,I2.2,A1)') '(A35,A', GETLEN(EBTEXT(1)), ')'
C '(A35,A)'
	      WRITE (OUTUNI,FMTST9)
&	        'Project ID:                        ',
&	        EBTEXT(1)
	    ELSE
	      WRITE (OUTUNI,'(A35,A1)')
&	        'Project ID:                        ',
&	        '0'
	    END IF
C hdr line 3
	    IF(PROFILE) THEN
	       IF (LNGSTE) THEN
		  WRITE (OUTUNI,'(A35,A35)')
&	      'Profile Site Type/Site ID:         ',
&	      SITEID(1:35)
	      ELSE
	      WRITE (OUTUNI,'(A35,A10)')
&	      'Profile Site Type/Site ID:         ',
&	      SITEID(1:10)
	      END IF
	    ELSE
	       IF (LNGSTE) THEN
	      WRITE (OUTUNI,'(A35,A35)')
&	      'Release Site Type/Site ID:         ',
&	      SITEID(1:35)
	    ELSE
	      WRITE (OUTUNI,'(A35,A10)')
&	      'Release Site Type/Site ID:         ',
&	      SITEID(1:10)
	    END IF
	    END IF

C  compute minutes
            TMPI = ABS(INT(CLLAT))
            TMPJ = ABS(INT(CLLON))
C            write(*,*)'TMPJ ',TMPJ
	    LATMIN = (ABS(CLLAT) - TMPI)*60.0
	    LONMIN = (ABS(CLLON) - TMPJ)*60.0
C            write(*,*)'LATMIN ',LATMIN
C            write(*,*)'LONMIN ',LONMIN
	    IF (CLLAT .LT. 0.0) THEN
	      LATCH = 'S'
	    ELSE
	      LATCH = 'N'
	    END IF

	    IF (CLLON .LT. 0.0) THEN
	      LONCH = 'W'
	    ELSE
	      LONCH = 'E'
	    END IF
	   
C hdr line 4
	    IF(PROFILE) THEN
	       WRITE (OUTUNI,8003)
&	    'Profile Location (lon,lat,alt):    ',
&	    TMPJ,LONMIN,LONCH,TMPI,LATMIN,LATCH,CLLON,CLLAT,ALT
            ELSE
	       WRITE (OUTUNI,8003)
&	    'Release Location (lon,lat,alt):    ',
&	    TMPJ,LONMIN,LONCH,TMPI,LATMIN,LATCH,CLLON,CLLAT,ALT
	    END IF
C hdr line 5
            IF(PROFILE) THEN
	       WRITE (OUTUNI,8004)
&	      'UTC Profile Time (y,m,d,h,m,s):    ',TIME
            ELSE
	       WRITE (OUTUNI,8004)
&	      'UTC Release Time (y,m,d,h,m,s):    ',TIME
            END IF

C write header lines 6-11
C  note that we actually want the line to contain the character '/'
C  (e.g. in C style, each line is "/\n")
C	    WRITE (OUTUNI,'(A1,/,A1,/,A1,/,A1,/,A1,/,A1)')
C&	    '/','/','/','/','/','/'

C write header lines 6-11
C - if any of the optional header fields existed in EBUFR, use them
C   else write the '/' character on each line
	    DO 110, K=1,NUMOHD,1
	      WRITE (FMTST6,'(A2,I3.3,A1)') '(A', GETLEN(OPTSTR(K)), ')'
C '(A130)'
	      WRITE (OUTUNI, FMTST6) OPTSTR(K)
              IF(.NOT. VIRTUAL) THEN
		 IF(INDEX(OPTSTR(K),"Virtual")) THEN
		    VIRTUAL = .TRUE.
                 ELSE IF(INDEX(OPTSTR(K),"virtual")) THEN
                    VIRTUAL = .TRUE.
                 END IF
              END IF
	      IF(.NOT.MIXRAT) THEN
		 IF(INDEX(OPTSTR(K),"Mixing Ratio")) THEN
		    MIXRAT = .TRUE.
                 ELSE IF(INDEX(OPTSTR(K),"mixing ratio")) THEN
                    MIXRAT = .TRUE.
                 END IF
              END IF
110	    CONTINUE
	    DO 111, L=K,6,1
	      WRITE (OUTUNI, '(A1)') '/'
111	    CONTINUE

C write header line 12 (nominal launch time)
	    IF(PROFILE) THEN
	    WRITE (OUTUNI,8004)
&	      'Nominal Profile Time (y,m,d,h,m,s):',NOMTIM
	    ELSE
	    WRITE (OUTUNI,8004)
&	      'Nominal Release Time (y,m,d,h,m,s):',NOMTIM
	    END IF
c write header lines 13,14,15
	    CALL SCFHDR(OUTUNI,VIRTUAL,MIXRAT)
	    FSTDAT = .FALSE.
	  ELSE
C do some calculations
C humidity - calc from dry bulb, dew point
	    IF (NOHUM) THEN
	      SDATA(5) = CALHUM(SDATA(3),SDATA(4))
	    END IF
C dew point - calc from dry bulb, humidity
	    IF (NODEW) THEN
	      SDATA(4) = CALDEW(SDATA(3),SDATA(5))
	    END IF
C u,v winds - calc from wind speed,direction
	    CALL CALWND(SDATA(6),SDATA(7),SDATA(8),SDATA(9))
C ascension rate - calc from altitude,time
	    SDATA(10) = CALASC(SDATA(1),SDATA(15),OLDTIM,OLDALT)
	    OLDTIM = SDATA(1)
	    OLDALT = SDATA(15)
C uv/dZ flag is either missing or unchecked
            IF(SDATA(10) .EQ. 999.0) THEN
              SDATA(21) = 9.0
            ELSE
	      SDATA(21) = 99.0
            ENDIF
C 13,14 are no longer missing
C	    SDATA(13) = 999.0
C	    SDATA(14) = 999.0
C write data
C	    CALL WRTSCF(OUTUNI,IDATA,1,IOS)
	    WRITE (OUTUNI,8002) SDATA
	  END IF
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(1) = RVAL
	  ELSE
	    SDATA(1) = 9999.0
	  END IF
	END IF

C station id
	IF (XOUT(J) .EQ. 1) THEN
	 IF (YOUT(J) .EQ. 253) THEN
	  IF (VALTYP .EQ. 'C') THEN
	    SITEID(1:10) = CVAL(1:10)
	  ELSE
	    SITEID(1:10) = 'UNKNOWN   '
	  END IF
	 ELSE IF (YOUT(J) .EQ. 248) THEN
	  IF (VALTYP .EQ. 'C') THEN
	    SITEID(1:35) = CVAL(1:35)
	    LNGSTE = .TRUE.
	  ELSE
	    SITEID(1:10) = 'UNKNOWN   '
	  END IF
	 ELSE IF (YOUT(J) .EQ. 254) THEN
	  IF (VALTYP .EQ. 'C') THEN
	    NETSTR(1:10) = CVAL(1:10)
	  ELSE
	    NETSTR(1:10) = 'UNKNOWN   '
	  END IF
	 ELSE IF (YOUT(J) .EQ. 249) THEN
C save field name until we see a value
	  IF (VALTYP .EQ. 'C') THEN
	    TMPSTR = CVAL(1:35)
	  ELSE
	    TMPSTR = NONAME
	  END IF
	 ELSE IF (YOUT(J) .EQ. 250) THEN
C now we have both name and value, so store into OPTSTR
	  NUMOHD = NUMOHD + 1
	  IF (VALTYP .EQ. 'C') THEN
	    OPTSTR(NUMOHD) = TMPSTR // CVAL
	  ELSE
	    OPTSTR(NUMOHD) = '/'
	  END IF
	  TMPSTR = NONAME
	 END IF
	 GO TO 199
	END IF

c launch altitude
	IF ((XOUT(J) .EQ. 7) .AND. (YOUT(J) .EQ. 1)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    ALT = RVAL
	  ELSE
	    ALT = 99999.0
	  END IF
	 GO TO 199
	END IF

c time
	IF ((XOUT(J) .EQ. 4) .AND. (YOUT(J) .LE. 6)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    TIME(YOUT(J)) = INT(RVAL)
	  ELSE
	    TIME(YOUT(J)) = 99
	  END IF
	 GO TO 199
	END IF

c pressure and QC flag
	IF ((XOUT(J) .EQ. 10) .AND. (YOUT(J) .EQ. 4)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(2) = RVAL / 100.0
	    J = J + 1
	    CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	    IF (ERR) THEN
	      WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	      WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	      GO TO 300
	    END IF
	    IF (VALTYP .EQ. 'R') THEN
	      SDATA(16) = QCTAB(INT(RVAL))
C	      GO TO 112
	    ELSE
	      SDATA(2) = 9999.0
	      SDATA(16) = 9.0
	    END IF
	  ELSE
	    J = J + 1
	    SDATA(2) = 9999.0
	    SDATA(16) = 9.0
	  END IF
	 GO TO 199
	END IF
112	CONTINUE

C height or altitude
	IF ((XOUT(J) .EQ. 7) .AND. (YOUT(J) .EQ. 253)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(15) = RVAL
	  ELSE
	    SDATA(15) = 99999.0
	  END IF
	 GO TO 199
	END IF

C
C Temp/dewpt conversions:
C *** Use same number as c2e, i.e. 273.1 (since CLASS only has 1 decimal place)
C

C dry bulb temperature and QC flag
	IF ((XOUT(J) .EQ. 12) .AND. (YOUT(J) .EQ. 1)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(3) = RVAL - 273.1
	    J = J + 1
	    CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	    IF (ERR) THEN
	      WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	      WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	      GO TO 300
	    END IF
	    IF (VALTYP .EQ. 'R') THEN
	      SDATA(17) = QCTAB(INT(RVAL))
	    ELSE
	      SDATA(3) = 999.0
	      SDATA(17) = 9.0
	    END IF
	  ELSE
	    J = J + 1
	    SDATA(3) = 999.0
	    SDATA(17) = 9.0
	  END IF
	 GO TO 199
	END IF

C dew point, humidity, and QC flag
	IF ((XOUT(J) .EQ. 12) .AND. (YOUT(J) .EQ. 3)) THEN
	  SDATA(4) = 999.0
	  SDATA(5) = 999.0
	  NODEW = .FALSE.
	  NOHUM = .FALSE.
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(4) = RVAL - 273.1
	  ELSE
	    SDATA(4) = 999.0
	  END IF
C look for humidity and/or qc flag
	  J = J + 1
	  CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	         RVAL,CVAL,VALTYP,MSUNI,ERR)
	  IF (ERR) THEN
	    WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	    WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	    GO TO 300
	  END IF
	  IF ((XOUT(J) .EQ. 13) .AND. (YOUT(J) .EQ. 254)) THEN
C we have a humidity!
	    IF (VALTYP .EQ. 'R') THEN
	      SDATA(5) = RVAL
	    ELSE
	      SDATA(5) = 999.0
	    END IF
	    J = J + 1
	    CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	    IF (ERR) THEN
	      WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	      WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	      GO TO 300
	    END IF
	  END IF
C assume we have a QC flag
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(18) = QCTAB(INT(RVAL))
	  ELSE
C QC flag is missing so force dewpt, humid to be missing
	    SDATA(4) = 999.0
	    SDATA(5) = 999.0
	    SDATA(18) = 9.0
	  END IF
C if both are missing then set QC flag to missing
	  IF ((SDATA(4) .EQ. 999.0) .AND. (SDATA(5) .EQ. 999.0)) SDATA(18) = 9.0
C if only one is missing, then calculate it
	  IF (SDATA(4) .EQ. 999.0) NODEW = .TRUE.
	  IF (SDATA(5) .EQ. 999.0) NOHUM = .TRUE.
	 GO TO 199
	END IF

C wind speed, direction, qc flag
	IF ((XOUT(J) .EQ. 11) .AND. (YOUT(J) .EQ. 2)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(8) = RVAL
	    J = J + 1
	    CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	    IF (ERR) THEN
	      WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	      WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	      GO TO 300
	    END IF
	    IF (VALTYP .EQ. 'R') THEN
	      SDATA(9) = RVAL
	    ELSE
	      SDATA(9) = 999.0
	    END IF
	    J = J + 1
	    CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	    IF (ERR) THEN
	      WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	      WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	      GO TO 300
	    END IF
	    IF (VALTYP .EQ. 'R') THEN
	      SDATA(19) = QCTAB(INT(RVAL))
	      SDATA(20) = SDATA(19)
	    ELSE
	      SDATA(19) = 9.0
	      SDATA(20) = 9.0
	    END IF
	  ELSE
	    J = J + 2
	    SDATA(8) = 999.0
	    SDATA(9) = 999.0
	    SDATA(19) = 9.0
	    SDATA(20) = 9.0
	  END IF
	 GO TO 199
	END IF

C longitude
	IF ((XOUT(J) .EQ. 6) .AND. (YOUT(J) .EQ. 1)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(11) = RVAL
	  ELSE
	    SDATA(11) = 9999.0
	  END IF
	END IF
C latitude
	IF ((XOUT(J) .EQ. 5) .AND. (YOUT(J) .EQ. 1)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(12) = RVAL
	  ELSE
	    SDATA(12) = 999.0
	  END IF
	END IF

C Elevation Angle (NWS only) or Mixing Ratio or Virtual temp
	IF ((XOUT(J) .EQ. 5) .AND. (YOUT(J) .EQ. 254)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(13) = RVAL
	  ELSE
	    SDATA(13) = 999.0
	  END IF
	END IF

C Azimuth Angle
	IF ((XOUT(J) .EQ. 5) .AND. (YOUT(J) .EQ. 255)) THEN
	  IF (VALTYP .EQ. 'R') THEN
	    SDATA(14) = RVAL
	  ELSE
	    SDATA(14) = 999.0
	  END IF
	END IF
C  A check for ebufred soundings before SDATA(13) and SDATA(14) saved by c2e
C  If both fields are zero then both fields set to missing
	IF(SDATA(13) .EQ. 0 .AND. SDATA(14) .EQ. 0) THEN
	   SDATA(13) = 999.0
	   SDATA(14) = 999.0
        END IF

C Continue statement for DO loop
199	CONTINUE
	J = J + 1
	GO TO 101
200	CONTINUE

C Write last level of sounding
	    IF (NOHUM) THEN
	      SDATA(5) = CALHUM(SDATA(3),SDATA(4))
	    END IF
	    IF (NODEW) THEN
	      SDATA(4) = CALDEW(SDATA(3),SDATA(5))
	    END IF
	    CALL CALWND(SDATA(6),SDATA(7),SDATA(8),SDATA(9))
	    SDATA(10) = CALASC(SDATA(1),SDATA(15),OLDTIM,OLDALT)
	    OLDTIM = SDATA(1)
	    OLDALT = SDATA(15)
	    SDATA(21) = 99.0

C	    SDATA(13) = 999.0
C	    SDATA(14) = 999.0
	    WRITE (OUTUNI,8002) SDATA

C Go back to GETOBS to get the next observation
C (this is the "end while" pseudocode statement for "while not end of file")
	GO TO 100

C Exit place for errors or end of file reading the current input file
300	CONTINUE

C	WRITE (MSUNI,'(A20,A)') INFIL,': EOF'
C	WRITE (MSUNI,*) RECCT,' records seen.'
C Don't want to (try to) close the standard input and output!
	IF (IUNIT .NE. 5) THEN
	  CLOSE (IUNIT)
	  CLOSE (OUTUNI)
	END IF

C Exit place for the several input file loop
301	CONTINUE

350	CONTINUE

C skip over the WRITE statements below
	GO TO 9999

C FORMAT statements for general use
8000	FORMAT ('Bad VALTYP returned from GETVAL.',/
&		'RECCT = ',I,X,'J = ',I)
8001	FORMAT (A2,'/',A2,'/',A2,X,A2,':',A2,':',A2,
&	    X,F10.5,X,F11.5,X,A3)
8002	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)
8003	FORMAT (A35,I3.3,X,F5.2,'''',A1,',',X,
&	  I2.2,X,F5.2,'''',A1,',',X,F7.2,',',X,F6.2,',',X,F7.1)
8004	FORMAT (A35,I4,',',X,I2.2,',',X,I2.2,',',X,
&	  I2.2,':',I2.2,':',I2.2)

C WRITE statements for general use, e.g. for errors

9000	WRITE (MSUNI,*) 'Error opening file.'
	GO TO 9999

C End of program

9999	CONTINUE

C Fix for -fnonstd flag in -fast option on Sun's compiler
C Comment the next line for other systems
	CALL STANDARD_ARITHMETIC()

	STOP
	END



	SUBROUTINE WRTSCF (FILENO,SDATA,IOS)
	INTEGER FILENO,IOS
	REAL SDATA(21)
C
C Write SDATA(*) onto FILENO
C Returns IOS from WRITE
C
	WRITE (FILENO,1717,IOSTAT=IOS)
&	  SDATA(1),SDATA(2),SDATA(3),SDATA(4),
&	  SDATA(5),SDATA(6),SDATA(7),SDATA(8),
&	  SDATA(9),SDATA(10),SDATA(11),
&	  SDATA(12),SDATA(13),SDATA(14),
&	  SDATA(15),SDATA(16),SDATA(17),
&	  SDATA(18),SDATA(19),SDATA(20),
&	  SDATA(21)
	RETURN

1717	FORMAT (2(F6.1,X),3(F5.1,X),2(F6.1,X),3(F5.1,X),F8.3,X,
&	  F7.3,X,2(F5.1,X),F7.1,X,5(F4.1,X),F4.1)

	END



	REAL FUNCTION CALHUM(TEMP,DEWPT)
	REAL TEMP,DEWPT
C
C CALHUM - Calculate Humidity from dry bulb and dew point temperatures
C
C Using the GEMPAK3 equation from Smithsonian Table No. 89:
C
C HUM = VAPR / VAPS * 100.0
C VAPR = 6.1121 * EXP((17.67 * DEWPT)/(DEWPT + 243.5))
C VAPS = 6.1121 * EXP((17.67 * TEMP) /(TEMP  + 243.5))
C
C where
C  DEWPT is the dewpoint temperature in Celsius
C  TEMP is the dry bulb temperature in Celsius
C  HUM is the relative humidity
C  VAPR is the vapor pressure in millibars
C  VAPS is the saturation vapor pressure in mb
C
C Reference:
C  Bolton, David. "The Computation of Equivalent Potential
C   Temperature." Month. Weather Rev.: Vol. 108 (July 1980), page 1046.
C
	REAL VAPR,VAPS

	IF ((TEMP .EQ. 999.0) .OR. (DEWPT .EQ. 999.0)) THEN
C Missing
	  CALHUM = 999.0
	  RETURN
	END IF
	VAPR = 6.1121 * EXP((17.67 * DEWPT)/(DEWPT + 243.5))
	VAPS = 6.1121 * EXP((17.67 * TEMP) /(TEMP  + 243.5))
	CALHUM = VAPR / VAPS * 100.0
C range checks to fit in the output field width
	IF (CALHUM .GT. 100.0) CALHUM = 999.0
	IF (CALHUM .LT. 0.0) CALHUM = 999.0
	RETURN
	END

	REAL FUNCTION CALDEW(TEMP,HUM)
	REAL TEMP,HUM
C
C CALDEW - Calculate Dew Point temperature from dry bulb temp and relative humidity
C
C Reference:
C  Bolton, David. "The Computation of Equivalent Potential
C   Temperature." Month. Weather Rev.: Vol. 108 (July 1980), page 1046.
C
C SVP = Saturation Vapor Pressure at 0 degrees Celsius
	REAL SVP
	PARAMETER (SVP=6.1121)
C WVP = Water Vapor Pressure
	REAL WVP
C DP = Dew Point
	REAL DP
 
	IF ((TEMP .EQ. 999.0) .OR. (HUM .EQ. 999.0)) THEN
C Missing
	  CALDEW = 999.0
	  RETURN
        ELSE IF (HUM .EQ. 0.0) THEN
          CALDEW = 999.0
          RETURN
	END IF
	WVP = SVP*(HUM/100.0)*EXP(17.67*TEMP/(TEMP+243.5))
	DP = (243.5*LOG(WVP)-440.8)/(19.48-LOG(WVP))
	CALDEW = DP
C range checks to fit in the output field width
	IF (CALDEW .GE. 998.95) CALDEW = 999.0
	IF (CALDEW .LT. -99.9) CALDEW = 999.0
	RETURN
	END



	REAL FUNCTION CALASC(TIM,ALT,OLDTIM,OLDALT)
	REAL TIM,ALT,OLDTIM,OLDALT
C
C CALASC - Calculate Ascension Rate
C
	IF (OLDTIM .EQ. 9998.0) THEN
C special first level flag --- set to 0.0 instead of missing
	  CALASC = 0.0
	  RETURN
	END IF
	IF ((OLDTIM .NE. 9999.0) .AND. (OLDALT .NE. 99999.0)
&	    .AND. (TIM .NE. 9999.0) .AND. (ALT .NE. 99999.0)) THEN
	  CALASC = (ALT - OLDALT) / (TIM - OLDTIM)
	ELSE
	  CALASC = 999.0
	END IF
	IF (CALASC .GE. 998.95) CALASC = 999.0
	IF (CALASC .LT. -99.9) CALASC = 999.0
	RETURN
	END



	SUBROUTINE CALWND(U,V,SPD,DIR)
	REAL U,V,SPD,DIR
C
C CALWND - Calculate u,v from speed,direction
C
	REAL A
	REAL SIN,COS,PI
	PARAMETER (PI=3.1415927)

	IF ((SPD .EQ. 999.0) .OR. (DIR .EQ. 999.0)) THEN
C missing
	  U = 9999.0
	  V = 9999.0
	  RETURN
	END IF
C check for the axis, to avoid underflows/overflows
	IF ((DIR .EQ. 0.0) .OR. (DIR .EQ. 360.0)) THEN
	  U = 0.0
	  IF (SPD .EQ. 0.0) THEN
	    V = 0.0
	  ELSE
C don't want V = -0.0 !
	    V = -1.0 * ABS(SPD)
	  END IF
	  RETURN
	ELSE IF (DIR .EQ. 90.0) THEN
	  U = -1.0 * ABS(SPD)
	  V = 0.0
	  RETURN
	ELSE IF (DIR .EQ. 180.0) THEN
	  U = 0.0
	  V = ABS(SPD)
	  RETURN
	ELSE IF (DIR .EQ. 270.0) THEN
	  U = ABS(SPD)
	  V = 0.0
	  RETURN
	END IF
C set the signs how we want; convert the angle from the funny
C   degrees system to radians
	IF (DIR .LT. 90.0) THEN
	  A = 90.0 - DIR
	  U = -1.0
	  V = -1.0
	ELSE IF (DIR .LT. 180.0) THEN
	  A = 450.0 - DIR
	  U = -1.0
	  V = 1.0
	ELSE IF (DIR .LT. 270.0) THEN
	  A = 450.0 - DIR
	  U = 1.0
	  V = 1.0
	ELSE
	  A = 450.0 - DIR
	  U = 1.0
	  V = -1.0
	END IF
	A = A * PI / 180.0
	U = U * ABS(SPD) * ABS(COS(A))
	V = V * ABS(SPD) * ABS(SIN(A))
	RETURN
	END



	SUBROUTINE SCFHDR(UNITNO,VIRTUAL,MIXRAT)
        LOGICAL VIRTUAL,MIXRAT
	INTEGER UNITNO
C
C SCFHDR - writes header lines 13,14,15

C Use BUFFER since lines are so long
	CHARACTER*130 BUFFER
	IF(.NOT. VIRTUAL .AND. .NOT. MIXRAT) THEN
	BUFFER(1:40) = ' Time  Press  Temp  Dewpt  RH    Uwind  '
	BUFFER(41:80) = 'Vwind  Wspd  Dir   dZ      Lon     Lat  '
	BUFFER(81:130) =
&	' Elev  Azim    Alt    Qp   Qt   Qh   Qu   Qv   Qdz'
	WRITE (UNITNO,'(A130)') BUFFER

	BUFFER(1:40) = '  sec    mb     C     C     %     m/s   '
	BUFFER(41:80) = ' m/s   m/s   deg   m/s     deg     deg  '
	BUFFER(81:130) =
&	'  deg   deg     m    code code code code code code'
	WRITE (UNITNO,'(A130)') BUFFER

	BUFFER(1:40) = '------ ------ ----- ----- ----- ------ -'
	BUFFER(41:80) = '----- ----- ----- ----- -------- -------'
	BUFFER(81:130) =
&	' ----- ----- ------- ---- ---- ---- ---- ---- ----'
	WRITE (UNITNO,'(A130)') BUFFER
        ELSE IF(MIXRAT) THEN
	BUFFER(1:40) = ' Time  Press  Temp  Dewpt  RH    Uwind  '
	BUFFER(41:80) = 'Vwind  Wspd  Dir   dZ      Lon     Lat  '
	BUFFER(81:130) =
&	'MixRat Azim    Alt    Qp   Qt   Qh   Qu   Qv   Qdz'
	WRITE (UNITNO,'(A130)') BUFFER

	BUFFER(1:40) = '  sec    mb     C     C     %     m/s   '
	BUFFER(41:80) = ' m/s   m/s   deg   m/s     deg     deg  '
	BUFFER(81:130) =
&	' g/kg   deg     m    code code code code code code'
	WRITE (UNITNO,'(A130)') BUFFER

	BUFFER(1:40) = '------ ------ ----- ----- ----- ------ -'
	BUFFER(41:80) = '----- ----- ----- ----- -------- -------'
	BUFFER(81:130) =
&	' ----- ----- ------- ---- ---- ---- ---- ---- ----'
	WRITE (UNITNO,'(A130)') BUFFER
        ELSE IF(VIRTUAL) THEN
	BUFFER(1:40) = ' Time  Press  Temp  Dewpt  RH    Uwind  '
	BUFFER(41:80) = 'Vwind  Wspd  Dir   dZ      Lon     Lat  '
	BUFFER(81:130) =
&	'VirT   Azim    Alt    Qp   Qt   Qh   Qu   Qv   Qdz'
	WRITE (UNITNO,'(A130)') BUFFER

	BUFFER(1:40) = '  sec    mb     C     C     %     m/s   '
	BUFFER(41:80) = ' m/s   m/s   deg   m/s     deg     deg  '
	BUFFER(81:130) =
&	'  K     deg     m    code code code code code code'
	WRITE (UNITNO,'(A130)') BUFFER

	BUFFER(1:40) = '------ ------ ----- ----- ----- ------ -'
	BUFFER(41:80) = '----- ----- ----- ----- -------- -------'
	BUFFER(81:130) =
&	' ----- ----- ------- ---- ---- ---- ---- ---- ----'
	WRITE (UNITNO,'(A130)') BUFFER
        END IF
	RETURN
	END

	INTEGER FUNCTION GETLEN(STR)
	CHARACTER*(*) STR
	INTEGER I

	DO 1818, I=LEN(STR),1,-1
	  IF (STR(I:I) .NE. ' ') THEN
	    GO TO 1819
	  END IF
1818	CONTINUE
1819	CONTINUE
	GETLEN = I
	RETURN
	END


