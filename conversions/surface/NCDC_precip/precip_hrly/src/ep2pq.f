C
C $Id: ep2pq.f,v 1.9 1995/07/10 17:07:24 john Exp $
C
C EP2PQ - E-BUFR PRECIP TO PRECIP-QCF CONVERSION PROGRAM
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

	PROGRAM EP2PQ
C
C EP2PQ - E-BUFR PRECIP TO PRECIP-QCF CONVERSION PROGRAM
C
C Written by:
C  John J. Allison / john@lightning.ofps.ucar.edu
C  NCAR/OFPS
C  09 Sep 1992
C
C Modification History:
C  Oct/Nov 1992 by John J. Allison
C    added support for OFPS version of EBUFR Precip
C
C This program converts an E-BUFR Precip file to a Precip version
C of the Quality Control Format (QCF) used by OFPS. The E-BUFR Precip
C file is assumed to have the following form:
C
C	0 04 255	time increment (local GMT offset)
C	1 03 000	repeat 3 descriptors A times
C	0 31 001	A
C	0 08 022	number of time interval replications
C	0 13 001	precip value
C	0 08 255	precip qualifier
C
C The above is the original NCDC version of EBUFR Precip. ep2pq will also
C handle the OFPS version (or possibly some intermixing of the two):
C
C	0 01 254	network identifier
C	0 01 255	station identifier
C	1 04 000	repeat 4 descriptors A times
C	0 31 001	A
C	0 08 022	number of time interval replications
C	0 13 001	precip value
C	0 08 255	precip qualifier
C	0 33 255	QC flag
C
C Note that the (1 04 000) and (0 31 001) don't actually appear in the
C EBUFR data, rather the 4 descriptors appear A times. Those two descriptors
C are shown so that you know how the encoding was performed.
C
C ep2pq will also handle the OFPS Dailly Precip format, which is the same as
C the regular format except for three things. First, there are 31 time intervals
C per record, rather than 24 (hourly) or 96 (15-minute). Months with <31 days
C have missing values for the extra days. Second, an extra parameter has been
C added: observation hour (BUFR 0 04 04). This is the hour which marks either
C the end of the 24 hour period for which data was collected. Third, the
C station identifier is 15 characters. To be clear:
C
C
C	0 01 254	network identifier
C	0 01 251	station identifier
C	1 05 000	repeat 5 descriptors A times
C	0 31 001	A
C	0 08 022	number of time interval replications
C	0 13 001	precip value
C	0 08 255	precip qualifier
C	0 33 255	QC flag
C	0 04 004	observation hour
C
C
C The algorithm for unpacking the data is:
C	GETOBS()
C	print nominal date,time,lat,lon,clibyt
C	GETVAL()
C	if (x=4 and y=255) then print value
C	do j=2 to numdat step 3
C	  GETVAL(J)
C	  ctr=int(rval)
C	  GETVAL(J+1)
C	  precip=rval
C	  GETVAL(J+2)
C	  qual=rval
C	  do k=1 to ctr
C	    precip_array(k)=precip
C	    qual_array(k)=qual
C	    if (precip=missing) then qc_flag(k)='M'
C	      else qc_flag(k)='U'  ; (unchecked)
C	  end do k
C	end do j
C	max=24 for hourly data
C	max=96 for 15-min data
C	do i=1 to max
C	  print precip_array(i),qual_array(i),qc_flag(i)
C	end do i
C The actual implementation of the above algorithm is slightly
C different, but the above is how I think about it. Of special
C note may be the fiddling I had to do with the FORMAT and WRITE
C statements to get things to print without starting newlines.
C This code is not the greatest, and I can see many improvements
C that could be accomplished via a rewrite, some of which are
C sprinkled throughout the code.
C
C This program was modified from the example program included in
C the E-BUFR Read Routines (EBRR) library, and uses library calls
C to the EBRR. The EBRR is distributed by OFPS. This program is
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

	INCLUDE 'ebufr_parm.inc'
	INCLUDE 'ebufr_obs.inc'
	INCLUDE 'ebufr_vars.inc'

C Default values for network and station strings
C  (in case they are not present in the EBUFR data)
	CHARACTER*10 DEFNET
	PARAMETER (DEFNET='NCDC      ')
	CHARACTER*15 DEFSTN
	PARAMETER (DEFSTN='               ')
C Values to use in case network or station string is marked as Missing
	CHARACTER*10 MISNET
	PARAMETER (MISNET='          ')
	CHARACTER*15 MISSTN
	PARAMETER (MISSTN='               ')

C Local variable declarations go here
	INTEGER IUNIT,OUTUNI,MSUNI,J,RECCT,K,I,ARGCT
	CHARACTER*256 INFIL,OUTFIL,ARGV
	CHARACTER CH,QCF,QCFLG(96)
	INTEGER CURPRE,CURQAL,CURQCF,QUAL(96),OBSHR(96),CUROBS
	INTEGER TMPCLI,NUMREP,MAXTIM,HROFF
	REAL PRECIP(96)
	INTEGER NUMDAT,BADCT,MAXBAD,FMTNUM
C	INTEGER TMPMON,TMPDAY,TMPYR
C	INTEGER LOC
	REAL RVAL
	LOGICAL FSTTIM,ERR,EOF,CHKALL,FORCHR,FILSEN,ISDATA
	LOGICAL DLYSEN,FORDLY,PRNDLY,LNGSTN
	CHARACTER*1 VALTYP
	CHARACTER*32 CVAL
	CHARACTER QCTAB(0:15)
	CHARACTER*10 NETSTR
	CHARACTER*15 STNSTR

	INCLUDE 'ebufr_data.inc'

	IUNIT = 11
	OUTUNI = 12
	MSUNI = 0
	MAXTIM = 24
	MAXBAD = 10
	FMTNUM=211
	FORCHR = .FALSE.
	FILSEN = .FALSE.
	PRNDLY = .FALSE.
	DLYSEN = .FALSE.
	FORDLY = .FALSE.
	LNGSTN = .TRUE.

	QCTAB(0) = 'U'
	QCTAB(1) = 'G'
	QCTAB(2) = 'B'
	QCTAB(3) = 'D'
	QCTAB(4) = 'N'
	QCTAB(5) = 'X'
	QCTAB(6) = 'E'
	QCTAB(7) = 'C'
	QCTAB(8) = 'T'
	QCTAB(9) = 'I'
	QCTAB(10) = '?'
	QCTAB(11) = '?'
	QCTAB(12) = '?'
	QCTAB(13) = '?'
	QCTAB(14) = '?'
	QCTAB(15) = 'M'

	I = 1
	ARGCT = IARGC()
20	CONTINUE
	IF (I .GT. ARGCT) THEN
	  WRITE (MSUNI,*) 'Usage: ep2pq [options] <infile> <outfile>'
	  GO TO 9999
C	  INFIL = 'stdin'
C	  IUNIT = 5
C	  OUTFIL = 'stdout'
C	  OUTUNI = 6
C	  GO TO 50
	END IF

	DO WHILE (I .LE. ARGCT)

	CALL GETARG(I,ARGV)
	IF (ARGV(1:1) .EQ. '-') THEN
	  CH = ARGV(2:2)
	  IF (CH .EQ. 'f') THEN
	    MAXTIM = 96
	  ELSE IF (CH .EQ. 'F') THEN
	    MAXTIM = 96
	    FORCHR = .FALSE.
	    PRNDLY = .FALSE.
	  ELSE IF (CH .EQ. 'h') THEN
	    MAXTIM = 24
	  ELSE IF (CH .EQ. 'H') THEN
	    MAXTIM = 24
	    FORCHR = .TRUE.
	  ELSE IF (CH .EQ. 'd') THEN
	    MAXTIM = 31
	    PRNDLY = .TRUE.
	    FORDLY = .FALSE.
	  ELSE IF (CH .EQ. 'D') THEN
	    MAXTIM = 31
	    PRNDLY = .TRUE.
	    FORCHR = .TRUE.
	    FORDLY = .TRUE.
	  ELSE IF (CH .EQ. 'b') THEN
	    READ (ARGV(3:),'(I)') MAXBAD
	    IF (MAXBAD .EQ. 0) THEN
	      MAXBAD = 2147483647
	    END IF
	  END IF
	  GO TO 301
	END IF

C get input, output filenames
	INFIL = ARGV
	I = I + 1
	IF (I .GT. ARGCT) THEN
	  WRITE (MSUNI,*) 'No output file given.'
	  GO TO 9999
	END IF
	CALL GETARG(I,ARGV)
	OUTFIL = ARGV
C force a quit after one file is processed
	I = ARGCT + 1

C next part builds an outfilename from an infilename
C skip over it since we now want outfilename on cmd line
C	J = 0
45	CONTINUE
C	LOC = J
C	J = J + INDEX(ARGV(J+1:),'.')
C	IF (J .NE. LOC) THEN
C	  GO TO 45
C	END IF
C	IF (LOC .EQ. 0) THEN
C	  LOC = INDEX(ARGV,' ')
C	END IF
C	OUTFIL = INFIL(1:LOC-1) // '.0pqc'

	OPEN (FILE=INFIL,UNIT=IUNIT,STATUS='OLD',ERR=9000)
	OPEN (FILE=OUTFIL,UNIT=OUTUNI,STATUS='NEW',ERR=9000)
50	CONTINUE
	FILSEN = .TRUE.
	RECCT = 0
	FSTTIM = .TRUE.
	EOF = .FALSE.
	CHKALL = .TRUE.
	BADCT = 0

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
100	CONTINUE

C GETOBS - get data for the next observation
	CALL GETOBS (IUNIT,MSUNI,DATUM,FMT,NUMDAT,
&	  XOUT,YOUT,ERR,EOF,FSTTIM,CHKALL)
	IF (ERR) THEN
	  WRITE (MSUNI,*) 'Error occurred in GETOBS.'
	  WRITE (MSUNI,*) 'RECCT = ',RECCT
	  GO TO 300
	ELSE IF (EOF) THEN
	  WRITE (MSUNI,*) 'End of input.'
	  GO TO 300
	ELSE
	  RECCT = RECCT + 1
	END IF

C Don't print data past Mar 16
C	READ (CLYEAR,'(I4)') TMPYR
C	READ (CLMON,'(I2)') TMPMON
C	READ (CLDAY,'(I2)') TMPDAY
C	IF (TMPYR .EQ. 1992) THEN
C	  IF ((TMPMON .GT. 3) .OR.
C&	      ((TMPMON .EQ. 3) .AND. (TMPDAY .GT. 16))) THEN
C	    GO TO 100
C	  END IF
C	END IF

	CURPRE = 1
	CURQAL = 1
	CURQCF = 1
	CUROBS = 1
	NETSTR = DEFNET
	STNSTR = DEFSTN
C For each piece of data, call GETVAL
	DO 200, J=1,NUMDAT,1

C GETVAL - get decoded value
	CALL GETVAL(DATUM(J),XOUT(J),YOUT(J),FMT(J),
&	           RVAL,CVAL,VALTYP,MSUNI,ERR)
	IF (ERR) THEN
	  WRITE (MSUNI,*) 'Error occurred in GETVAL.'
	  WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	  GO TO 300
	END IF
D	WRITE (MSUNI,*) 'J=',J,' DATUM(J)=',DATUM(J)
D	WRITE (MSUNI,*) '  RVAL=',RVAL,' VALTYP=',VALTYP
D	WRITE (MSUNI,*) '  XOUT=',XOUT(J),' YOUT=',YOUT(J)
	IF ((XOUT(J) .EQ. 4) .AND. (YOUT(J) .EQ. 255)) THEN
	  IF (VALTYP .EQ. 'M') THEN
	    HROFF = -99
	  ELSE IF (VALTYP .EQ. 'R') THEN
	    HROFF = INT(RVAL)
	  ELSE
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad GMT offset (0 04 255).'
	    HROFF = -99
	  END IF
	ELSE IF ((XOUT(J) .EQ. 1) .AND. (YOUT(J) .EQ. 254)) THEN
	  IF (VALTYP .EQ. 'M') THEN
	    NETSTR = MISNET
	  ELSE IF (VALTYP .EQ. 'C') THEN
	    NETSTR = CVAL(1:10)
	  ELSE
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad network identifier (0 01 254).'
	  END IF
	ELSE IF ((XOUT(J) .EQ. 1) .AND. (YOUT(J) .EQ. 253)) THEN
	  IF (VALTYP .EQ. 'M') THEN
	    STNSTR = MISSTN
	  ELSE IF (VALTYP .EQ. 'C') THEN
C 1:15 for daily format, 11:15 are ignored for others
	    STNSTR(1:10) = CVAL(1:10)
	    STNSTR(11:15) = '     '
	    LNGSTN = .FALSE.
	  ELSE
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad station identifier (0 01 253).'
	  END IF
	ELSE IF ((XOUT(J) .EQ. 1) .AND. (YOUT(J) .EQ. 251)) THEN
	  IF (VALTYP .EQ. 'M') THEN
	    STNSTR = MISSTN
	  ELSE IF (VALTYP .EQ. 'C') THEN
C 1:15 for daily format
	    STNSTR = CVAL(1:15)
	    LNGSTN = .TRUE.
	  ELSE
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad station identifier (0 01 251).'
	  END IF
	ELSE IF ((XOUT(J) .EQ. 8) .AND. (YOUT(J) .EQ. 22)) THEN
	  IF (VALTYP .NE. 'R') THEN
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad time interval reps (0 08 022).'
	  ELSE
	    NUMREP = INT(RVAL)
D	    WRITE (MSUNI,*) ' NUMREP=',NUMREP,'RVAL=',RVAL,' record ',RECCT
	  END IF
	ELSE IF ((XOUT(J) .EQ. 13) .AND. (YOUT(J) .EQ. 11)) THEN
	  IF (VALTYP .EQ. 'M') THEN
	    RVAL = -999.99
	  ELSE IF (VALTYP .NE. 'R') THEN
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad precip value (0 13 011).'
	  END IF
	  CURPRE = CURPRE+NUMREP
	  IF (CURPRE-1 .LE. 96) THEN
	    DO 150, K=CURPRE-NUMREP,CURPRE-1,1
	      PRECIP(K) = RVAL
150	    CONTINUE
	  END IF
	ELSE IF ((XOUT(J) .EQ. 8) .AND. (YOUT(J) .EQ. 255)) THEN
	  IF (VALTYP .EQ. 'M') THEN
	    RVAL = 7.0
	  ELSE IF (VALTYP .NE. 'R') THEN
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad precip qualifier (0 08 255).'
	  END IF
C Set QCF QC flag; it is assumed that the data matches the qualifier,
C  and so the data is not checked. If the data is missing and the
C  qualifier is not 7 (missing) or 3 (deleted), then a later QC
C  program should decide what to do.
	  IF ((RVAL .EQ. 7.0) .OR. (RVAL .EQ. 3.0)) THEN
	    QCF = 'M'
	  ELSE
	    QCF = 'U'
	  END IF
	  CURQAL = CURQAL+NUMREP
	  IF (CURQAL-1 .LE. 96) THEN
	    DO 160, K=CURQAL-NUMREP,CURQAL-1,1
	      QUAL(K) = INT(RVAL)
	      QCFLG(K) = QCF
160	    CONTINUE
	  END IF
	ELSE IF ((XOUT(J) .EQ. 33) .AND. (YOUT(J) .EQ. 255)) THEN
C Ah-ha! We have found a QC flag, so ignore the QCFLG set above.
	  IF (VALTYP .EQ. 'M') THEN
	    RVAL = 15.0
	  ELSE IF (VALTYP .NE. 'R') THEN
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad QC flag (0 33 255).'
	  END IF
	  CURQCF = CURQCF+NUMREP
	  IF (CURQCF-1 .LE. 96) THEN
	    DO 170, K=CURQCF-NUMREP,CURQCF-1,1
	      QCFLG(K) = QCTAB(INT(RVAL))
170	    CONTINUE
	  END IF
	ELSE IF ((XOUT(J) .EQ. 4) .AND. (YOUT(J) .EQ. 4)) THEN
	  DLYSEN = .TRUE.
	  MAXTIM = 31
	  IF (VALTYP .EQ. 'M') THEN
	    RVAL = 99.0
	  ELSE IF (VALTYP .NE. 'R') THEN
	    WRITE (MSUNI,8000) RECCT,J
	    WRITE (MSUNI,*) ' Bad obs hour (0 04 004).'
	  END IF
	  CUROBS = CUROBS+NUMREP
	  IF (CUROBS-1 .LE. 96) THEN
	    DO 180, K=CUROBS-NUMREP,CUROBS-1,1
	      OBSHR(K) = INT(RVAL)
180	    CONTINUE
	  END IF
	ELSE
	  WRITE (MSUNI,*) 'Bad data descriptor.'
	  WRITE (MSUNI,*) 'RECCT = ',RECCT,' J = ',J
	END IF

C Continue statement for DO loop
200	CONTINUE

	IF (CURPRE .NE. CURQAL) THEN
	  WRITE (MSUNI,*) 'Number of precip values does',
&	    ' not equal the number of qualifiers.'
	END IF
C if there are no QC flags in EBUFR data then CURQCF == 1 so don't error
	IF ((CURPRE .NE. CURQCF) .AND. (CURQCF .GT. 1)) THEN
	  WRITE (MSUNI,*) 'Number of precip values does',
&	    ' not equal the number of qc flags.'
	END IF
C if there are no QC flags in EBUFR data then CURQCF == 1 so don't error
	IF ((CURQCF .NE. CURQAL) .AND. (CURQCF .GT. 1)) THEN
	  WRITE (MSUNI,*) 'Number of qc flags does',
&	    ' not equal the number of qualifiers.'
	END IF
	IF (CURPRE-1 .LT. MAXTIM) THEN
	  IF (BADCT .LE. MAXBAD) THEN
	  WRITE (MSUNI,'(A,I,A,I2,A)')
&	   'Number of precip values = ',CURPRE-1,
&	    '; expected ',MAXTIM
	  WRITE (MSUNI,*) ' Appending missing values for:'
	  WRITE (MSUNI,8001)
&	    CLYEAR(3:4),CLMON,CLDAY,
&	    CLHOUR,CLMIN,CLSEC,
&	    CLLAT,CLLON,CLIBYT
	  END IF
	  BADCT = BADCT + 1
	END IF
	DO 210, K=CURPRE,MAXTIM,1
	  PRECIP(K) = -999.99
	  QUAL(K) = 7
	  QCFLG(K) = 'M'
	  OBSHR(K) = 99
210	CONTINUE
	IF (CURPRE-1 .GT. MAXTIM) THEN
	  IF (BADCT .LE. MAXBAD) THEN
	    WRITE (MSUNI,'(A,I,A,I2)')
&	     'Number of precip values = ',CURPRE-1,
&	      '; expected ',MAXTIM
	  END IF
	  BADCT = BADCT + 1
C we know MAXTIM <= 96 from above
	  IF (FORCHR) THEN
	    IF (BADCT .LE. MAXBAD) THEN
	      WRITE (MSUNI,*) 'Truncating record for:'
	    END IF
	  ELSE
	    MAXTIM = CURPRE-1
	    IF (BADCT .LE. MAXBAD) THEN
	      WRITE (MSUNI,*) 'Expanding record for:'
	    END IF
	  END IF
	  IF (BADCT .LE. MAXBAD) THEN
	  WRITE (MSUNI,8001)
&	    CLYEAR(3:4),CLMON,CLDAY,
&	    CLHOUR,CLMIN,CLSEC,
&	    CLLAT,CLLON,CLIBYT
	  END IF
	END IF

C fix for accumulations
C should really change the way I get the data originally
C instead of getting precip and expanding it, then getting qualifier and
C  expanding it, I should get both precip and qualifier and then expand
C  them together, taking into account accumulations
	ISDATA = .FALSE.
	DO 215, K=1,MAXTIM,1
	  IF (QUAL(K) .EQ. 2) THEN
	    IF (QUAL(K+1) .EQ. 2) THEN
	      PRECIP(K) = 0.0
	      QUAL(K) = 1
	    END IF
	  END IF
	  IF ((QUAL(K) .NE. 3) .AND. (QUAL(K) .NE. 7)) THEN
	    ISDATA = .TRUE.
	  END IF
215	CONTINUE

	IF ((.NOT. DLYSEN) .AND. FORDLY) THEN
	  GO TO 9001
	END IF

C Don't write all missing or all deleted records
	IF (ISDATA) THEN

	READ (CLIBYT,'(I3)') TMPCLI

	IF (FORDLY .OR. (PRNDLY .AND. DLYSEN)) THEN
	  WRITE (OUTUNI,212)
&	    CLYEAR(1:4),CLMON,
&	    NETSTR,STNSTR,
&	    CLLAT,CLLON,TMPCLI,
&	    (PRECIP(K), QUAL(K), QCFLG(K), OBSHR(K), K=1,MAXTIM)
	ELSE
	  IF (LNGSTN) THEN
	   WRITE (OUTUNI,213)
&	    CLYEAR(3:4),CLMON,CLDAY,
&	    CLHOUR,CLMIN,CLSEC,
&	    NETSTR,STNSTR(1:15),
&	    CLLAT,CLLON,TMPCLI,
&	    (PRECIP(K), QUAL(K), QCFLG(K), K=1,MAXTIM)
	  ELSE
	   WRITE (OUTUNI,211)
&	    CLYEAR(3:4),CLMON,CLDAY,
&	    CLHOUR,CLMIN,CLSEC,
&	    NETSTR,STNSTR(1:10),
&	    CLLAT,CLLON,TMPCLI,
&	    (PRECIP(K), QUAL(K), QCFLG(K), K=1,MAXTIM)
	  END IF
	END IF

211	FORMAT (A2,'/',A2,'/',A2,X,A2,':',A2,':',A2,
&	    X,A10,X,A10,
&	    X,F10.5,X,F11.5,X,I3,96(: X,F7.2,X,I1,X,A1))
212	FORMAT (A4,'/',A2,
&	    X,A10,X,A15,
&	    X,F10.5,X,F11.5,X,I3,96(: X,F7.2,X,I1,X,A1,X,I2))
213	FORMAT (A2,'/',A2,'/',A2,X,A2,':',A2,':',A2,
&	    X,A10,X,A15,
&	    X,F10.5,X,F11.5,X,I3,96(: X,F7.2,X,I1,X,A1))

	END IF

C Go back to GETOBS to get the next observation
C (this is the "end while" pseudocode statement for "while not end of file")
	GO TO 100

C Exit place for errors or end of file reading the current input file
300	CONTINUE

	WRITE (MSUNI,'(A50,A)') INFIL,': EOF'
	WRITE (MSUNI,*) RECCT,' records seen.'
	WRITE (MSUNI,*) BADCT,' bad(?) records seen.'
C Don't want to (try to) close the standard input and output!
	IF (IUNIT .NE. 5) THEN
	  CLOSE (IUNIT)
	  CLOSE (OUTUNI)
	END IF

C Exit place for the several input file loop
301	CONTINUE

	I = I + 1
C Loop for next cmd line arg
	END DO
350	CONTINUE
	IF (.NOT. FILSEN) THEN
	  WRITE (MSUNI,*) 'No filenames given.'
	END IF

C skip over the WRITE statements below
	GO TO 9999

C FORMAT statements for general use
8000	FORMAT ('Bad VALTYP returned from GETVAL.',/
&		'RECCT = ',I,X,'J = ',I)
8001	FORMAT (A2,'/',A2,'/',A2,X,A2,':',A2,':',A2,
&	    X,F10.5,X,F11.5,X,A3)

C WRITE statements for general use, e.g. for errors

9000	WRITE (MSUNI,*) 'Error opening file.'
	GO TO 9999
9001	WRITE (MSUNI,*) 'Error: observation hour not present.'
	GO TO 9999

C End of program

9999	CONTINUE

C Fix for -fnonstd flag in -fast option on Sun's compiler
C Comment the next line for other systems
	CALL STANDARD_ARITHMETIC()

	STOP
	END
