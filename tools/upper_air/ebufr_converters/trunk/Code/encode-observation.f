C
C $Id: encode-observation.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: encode-observation.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Modifications:
C    9 Dec 1991 David Casperson:
C        Fixed an error in ENCPR2.
C
C Routine Long Name: Encode-observation
C  6 Character Name: ENCOBS
C           Purpose: To encode one complete observation as an EBUFR
C                    type 9 record and write the resulting record to an
C                    open EBUFR file.
C            Method:
C Import Parameters:
C    UNIT    --  Fortran unit number of file opened for writing on
C                 which the current record will be written.
C    OCODE   --  Code identifying the observation to be written.
C    TIME    --  Array of six integers identifying the time of the
C                 observation.
C    LATLON  --  Array of two reals identifying the latitude and
C                 longitude of the observation.  (Should be accurate to
C                 1E-5 degrees.)
C    CLIBYT  --  Uniquifying byte for distinguishing between stations
C                 that have almost identical latitudes and longitudes.
C                 Should be a positive integer between 0 and 255.
C    DFLAG   --  True if this routine should write a BUFR section 3.
C                 Must be true if more than observation type is being
C                 written to the current file.
C    IDATA   --  }  These three arrays contain the data to be encoded.
C    RDATA   --  }  Exactly how this information is encoded is
C    CDATA   --  }  described ...
C Export Parameters: (none)
C     Prerequisites:  UNIT must be opened for writing in a manner
C                     suitable for EBUFR files and the appropriate
C                     EBUFR header records must have already been
C                     written to the file.  UNIT must be positioned to
C                     the beginning of a record.
C Commons directly accessed: (none).
      SUBROUTINE ENCOBS(UNIT, OCODE, TIME, LATLON, CLIBYT,
     $     DFLAG, IDATA, RDATA, CDATA)
      INTEGER UNIT, OCODE, CLIBYT, TIME(6)
      REAL LATLON(2)
      LOGICAL DFLAG
      INTEGER IDATA(*)
      REAL RDATA(*)
      CHARACTER *(*) CDATA
C ====================================================================
C   Local variables
      INTEGER PSTART, PEND, PCUR, PNEXT
C ====================================================================
      CALL STRTOB
      CALL EBFSTU(UNIT)
      CALL EBFCLR

      CALL WRTR9H(TIME, LATLON, CLIBYT)
      IF (DFLAG) THEN
         CALL WRDESC(OCODE)
      ENDIF

      CALL STRTSC(4)
        CALL EBPTBY(0)
        CALL GPRNGE(OCODE, PSTART, PEND)
        PCUR = PSTART
C     while PCUR <= PEND do
 10     IF (PCUR .LE. PEND) THEN
           CALL ENCPR0(PEND, PCUR, PNEXT, IDATA, RDATA, CDATA)
           PCUR = PNEXT
C     endwhile
           GO TO 10
        ENDIF
      CALL ENDSC(4)
      CALL EBFLSH
      RETURN
      END

C Routine Long Name: start-new-observation
C  6 Character Name: STRTOB
C           Purpose: To initialize data local to the observation
C                    encoding routine.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none) but must be called before calling any of
C                    get_next_ival, get_next_rval, or get_next_cval.
C Commons directly
C          accessed: ECOLOC
      SUBROUTINE STRTOB()
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/encode-observation-local.f'

      IDTIDX = 0
      RDTIDX = 0
      CDTIDX = 0
      RETURN
      END

C Routine Long Name: write-descriptor-for-observation
C  6 Character Name: WRDESC
C           Purpose: To write  BUFR section 3 information for an
C                    observation.
C Import Parameters:
C    OCODE   --  Code describing the observation.
C Export Parameters: (none)
C     Prerequisites:
C Commons directly
C          accessed: (none)
        SUBROUTINE WRDESC(OCODE)
        INTEGER OCODE
C ====================================================================
C   Local variables
        INTEGER PSTART, PEND, I
        INTEGER FF, XX, YY
C ====================================================================
        CALL GPRNGE(OCODE, PSTART, PEND)
        CALL STRTSC(3)
          CALL EBPTBY(0)
C       Number of data sets is one
          CALL EBPTIN(1, 16)
C       Data is observed, but not compressed.
          CALL EBPT1B(1)
          CALL EBPT0B(1)
          CALL EBALGN(8)
          DO 10 I=PSTART, PEND, 1
             CALL GETFXY(I, FF, XX, YY)
             CALL EBPTIN(FF, 2)
             CALL EBPTIN(XX, 6)
             CALL EBPTIN(YY, 8)
 10       CONTINUE
        CALL ENDSC(3)
        RETURN
        END

C Routine Long Name: Encode-parameter-or-replication
C  6 Character Name: ENCPR0
C           Purpose: To encode the parameter or replication described by
C                    PCUR.
C            Method: Checks the F value associated with PCUR and then
C                    encodes the appropriate information.  Normally
C                    PNEXT is set to the first parameter after PCUR, but
C                    in the case of a replication it is set to the next
C                    parameter not encoded by this call.
C Import Parameters:
C    PEND    --  Index of the last parameter for this observation.  Used
C                to check the sanity of replicator X values.
C    PCUR    --  Index of the current parameter.
C    IDATA   --  }  These three arrays contain the data to be encoded.
C    RDATA   --  }  Exactly how this information is encoded is
C    CDATA   --  }  described ...
C Export Parameters:
C    PNEXT   --  Index of the next parameter to be encoded after this
C                call.
C     Prerequisites:
C Commons directly
C          accessed:
      SUBROUTINE ENCPR0(PEND, PCUR, PNEXT, IDATA, RDATA, CDATA)
      INTEGER PEND, PCUR, PNEXT
      INTEGER IDATA(*)
      REAL RDATA(*)
      CHARACTER *(*) CDATA

      EXTERNAL ENCPAR
      INTEGER  ENCPAR
C ====================================================================
C   Local variables
      INTEGER F, X, Y, DUMMY
C ====================================================================

      CALL GETFXY(PCUR, F, X, Y)
      IF (F .EQ. 0) THEN
         DUMMY = ENCPAR(PCUR, IDATA, RDATA, CDATA)
         PNEXT = PCUR+1
      ELSEIF (F .EQ. 1) THEN
         CALL ENCPR1(PEND, PCUR, PNEXT, IDATA, RDATA, CDATA)
      ELSEIF (F .EQ. 2) THEN
         CALL ENCERR(18)
      ELSEIF (F .EQ. 3) THEN
         CALL ENCERR(19)
      ELSE
         CALL ENCERR(20)
      ENDIF

      RETURN
      END

C Routine Long Name: Encode-replication
C  6 Character Name: ENCPR1
C           Purpose: To encode replication described by the parameter
C                    PCUR.
C            Method: The X and Y values of the current parameter are
C                    examined to determine the extent and number of
C                    replications.  If Y=0 then the dynamic replication
C                    routine is called. PNEXT is set to the next
C                    parameter not encoded by this call.
C Import Parameters:
C    PEND    --  Index of the last parameter for this observation.  Used
C                to check the sanity of replicator X values.
C    PCUR    --  Index of the current parameter.
C    IDATA   --  }  These three arrays contain the data to be encoded.
C    RDATA   --  }  Exactly how this information is encoded is
C    CDATA   --  }  described ...
C Export Parameters:
C    PNEXT   --  Index of the next parameter to be encoded after this
C                call.
C     Prerequisites:
C Commons directly
C          accessed:
      SUBROUTINE ENCPR1(PEND, PCUR, PNEXT, IDATA, RDATA, CDATA)
      INTEGER PEND, PCUR, PNEXT
      INTEGER IDATA(*)
      REAL RDATA(*)
      CHARACTER *(*) CDATA

      EXTERNAL ENCPAR
      INTEGER  ENCPAR
C ====================================================================
C   Local variables
      INTEGER F, X, Y, DUMMY, REPS, SUBPAR
C ====================================================================

      CALL GETFXY(PCUR, F, X, Y)
      IF (F .NE. 1) THEN
         CALL ENCERR(21)
      ELSEIF (PCUR+X .GT. PEND) THEN
         CALL ENCERR(22)
      ELSEIF (Y .EQ. 0) THEN
         CALL ENCPR2(PEND, PCUR, PNEXT, IDATA, RDATA, CDATA)
      ELSE
         PNEXT = PCUR + X + 1
         DO 10 REPS = 1, Y, 1
            DO 20 SUBPAR = PCUR+1, PNEXT-1, 1
               DUMMY = ENCPAR(SUBPAR, IDATA, RDATA, CDATA)
 20         CONTINUE
 10      CONTINUE
      ENDIF

      RETURN
      END

C Routine Long Name: Encode-dynamic-replication
C  6 Character Name: ENCPR2
C           Purpose: To encode the dynamic replication described by the
C                    parameter PCUR.
C            Method: The X value of the current parameter is
C                    examined to determine the extent of the
C                    replications.  Then the next parameter is
C                    extracted from the user data and encoded.  Its
C                    value is used as the number of replications. PNEXT
C                    is set to the next parameter not encoded by this
C                    call.
C Import Parameters:
C    PEND    --  Index of the last parameter for this observation.  Used
C                to check the sanity of replicator X values.
C    PCUR    --  Index of the current parameter.
C    IDATA   --  }  These three arrays contain the data to be encoded.
C    RDATA   --  }  Exactly how this information is encoded is
C    CDATA   --  }  described ...
C Export Parameters:
C    PNEXT   --  Index of the next parameter to be encoded after this
C                call.
C     Prerequisites:
C Commons directly accessed:
C Modifications: 
C    9 Dec 1991 David Casperson:  Fixed an error in the value of PNEXT.
      SUBROUTINE ENCPR2(PEND, PCUR, PNEXT, IDATA, RDATA, CDATA)
      INTEGER PEND, PCUR, PNEXT
      INTEGER IDATA(*)
      REAL RDATA(*)
      CHARACTER *(*) CDATA

      EXTERNAL ENCPAR
      INTEGER  ENCPAR

C ====================================================================
C   Local variables
      INTEGER F, X, Y, DUMMY, REPS, SUBPAR
C ====================================================================

      CALL GETFXY(PCUR, F, X, Y)
      IF ((F .NE. 1) .OR. (Y .NE. 0)) THEN
         CALL ENCERR(28)
      ELSEIF (PCUR+X .GT. PEND) THEN
         CALL ENCERR(22)
      ELSE
         PNEXT = PCUR + X + 2
         Y = ENCPAR(PCUR+1, IDATA, RDATA, CDATA)
         IF (Y .LT. 0) THEN
            CALL ENCERR(31)
         ENDIF
         DO 10 REPS = 1, Y, 1
            DO 20 SUBPAR = PCUR+2, PNEXT-1, 1
               DUMMY = ENCPAR(SUBPAR, IDATA, RDATA, CDATA)
 20         CONTINUE
 10      CONTINUE
      ENDIF

      RETURN
      END

C Routine Long Name: Encode-parameter
C  6 Character Name: ENCPAR
C           Purpose: To encode one actual parameter (Not replicator or
C                    sequence, etc.)
C Import Parameters:
C    PP      --  Index of the actual parameter to encode.
C    IDATA   --  }  These three arrays contain the data to be encoded.
C    RDATA   --  }  Exactly how this information is encoded is
C    CDATA   --  }  described ...
C Export Parameters:
C    -- returns the value of the parameter if it is an integer;
C       otherwise -1.  (This is needed for dynamic replicators.)
C     Prerequisites: STRTOB must be called before this routine is called
C                    for the first time for an observation.
C Commons directly
C          accessed:
      INTEGER FUNCTION ENCPAR (PP, IDATA, RDATA, CDATA)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/encode-observation-local.f'
      INTEGER PP
      INTEGER IDATA(*)
      REAL RDATA(*)
      CHARACTER *(*) CDATA

C ====================================================================
C   Local variables
      INTEGER VTYPE
      INTEGER IVAL
      REAL RVAL
      CHARACTER *300 CVAL
      INTEGER CLEN
      LOGICAL BAD, MSSING
C ====================================================================
      CALL GETRAW(PP, IDATA, RDATA, CDATA, VTYPE,
     $            IVAL, RVAL, CVAL, CLEN, BAD, MSSING)
      IF (BAD) THEN
         CALL ENCBAD(PP)
         ENCPAR = -1
      ELSEIF (MSSING) THEN
         CALL ENCMSS(PP)
         ENCPAR = -1
      ELSEIF (VTYPE .EQ. CDATAT) THEN
         CALL ENCCHR(PP, CVAL, CLEN)
         ENCPAR = -1
      ELSEIF ((VTYPE .EQ. IDATAT) .OR. (VTYPE .EQ. RDATAT)) THEN
         CALL ENCNUM(PP, VTYPE, IVAL, RVAL, IREP)
         ENCPAR = IREP
      ELSE
         CALL ENCERR(25)
      ENDIF
      RETURN
      END

C Routine Long Name: get-raw-parameter-value
C  6 Character Name: GETRAW
C           Purpose: To get a raw parameter value from (IDATA, RDATA, CDATA)
C Import Parameters:
C    PP      --  number of the parameter in parameter table.
C    IDATA  }
C    RDATA  }--  data arrays containing parameters.
C    CDATA  }
C Export Parameters:
C    VTYPE   --  type of raw parameter value returned.
C    IVAL   }
C    RVAL   }--  actual value returned in one of these parameters.
C    CVAL   }
C    CLEN    --  length of CVAL when VTYPE is character.
C    BAD     --  true if problem getting parameter value
C    MSSING  --  true if raw value indicates that is value is missing.
C     Prerequisites: 
C Commons directly
C          accessed: none
      SUBROUTINE GETRAW(PP, IDATA, RDATA, CDATA, VTYPE,
     $            IVAL, RVAL, CVAL, CLEN, BAD, MSSING)
      INTEGER PP
      INTEGER IDATA(*)
      REAL RDATA(*)
      CHARACTER*(*) CDATA
      INTEGER VTYPE, IVAL
      REAL RVAL
      CHARACTER*(*) CVAL
      INTEGER CLEN
      LOGICAL BAD, MSSING

      INCLUDE '../Commons/parameters.f'

      EXTERNAL GNIVAL
      INTEGER  GNIVAL
      
      EXTERNAL GNRVAL
      REAL     GNRVAL
C ====================================================================
C   Local variables
      INTEGER HOW, WIDTH
      CHARACTER *20 FORMT

      INTEGER IRVAL
      REAL RIVAL
      EQUIVALENCE (IRVAL, RIVAL)

      INTEGER MVAL
      PARAMETER (MVAL=2147483647)
C ====================================================================
      BAD = .FALSE.
      MSSING = .FALSE.
      CALL GETFMT(PP, HOW, FORMT, WIDTH, VTYPE)
      
      IF (HOW .EQ. IDATAT) THEN
         IF (VTYPE .NE. IDATAT) THEN
            CALL ENCERR(23)
         ELSE
            IVAL = GNIVAL(IDATA)
         ENDIF

      ELSEIF (HOW .EQ. RDATAT) THEN
         IF (VTYPE .NE. RDATAT) THEN
            CALL ENCERR(24)
         ELSE
            RVAL = GNRVAL(RDATA)
         ENDIF


      ELSEIF (HOW .EQ. CDATAT) THEN
         CALL GNCVAL(FORMT, WIDTH, VTYPE, CDATA,
     $        IVAL, RVAL, CVAL, CLEN, BAD)

      ELSE
         CALL ENCERR(25)
      ENDIF

      IF (.NOT. BAD) THEN
         MSSING = ((VTYPE .EQ. IDATAT) .AND. (IVAL .EQ. MVAL))
         IF ((.NOT. MSSING) .AND. (VTYPE .EQ. RDATAT)) THEN
            RIVAL = RVAL
            MSSING = (IRVAL .EQ. MVAL)
         ENDIF
      ENDIF 
      END

C Routine Long Name: get-next-ival
C  6 Character Name: GNIVAL
C           Purpose: To return the next unused integer value from the
C                    array IDATA.
C Import Parameters:
C    IDATA   --  array of integer values from user routines
C Export Parameters:
C    -- returns next value
C     Prerequisites:  STRTOB must be called before this routine is
C                     first called.
C Commons directly
C          accessed: ECOLOC
      INTEGER FUNCTION GNIVAL(IDATA)
      INTEGER IDATA(*)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/encode-observation-local.f'

      IDTIDX = IDTIDX + 1
      GNIVAL = IDATA(IDTIDX)
      RETURN
      END

C Routine Long Name: get-next-rval
C  6 Character Name: GNRVAL
C           Purpose: To return the next unused real value from the
C                    array RDATA.
C Import Parameters:
C    RDATA   --  array of real values from user routines
C Export Parameters:
C    -- returns next real value.
C     Prerequisites:  STRTOB must be called before this routine is
C                     first called.
C Commons directly
C          accessed: ECOLOC
      REAL FUNCTION GNRVAL(RDATA)
      REAL RDATA(*)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/encode-observation-local.f'

      RDTIDX = RDTIDX + 1
      GNRVAL = RDATA(RDTIDX)
      RETURN
      END

C Routine Long Name: get-next-cval
C  6 Character Name: GNCVAL
C           Purpose: To get the next data value from the character
C                    array CDATA using FORMT.  Result is returned in
C                    one of IVAL, RVAL, or CVAL.  The character data
C                    cursor is advanced by WIDTH.
C Import Parameters:
C    FORMT   --  format to use in reading the value.
C    WIDTH   --  Total number of characters to use in reading the value.
C    VTYPE   --  Type of value to read (integer code).
C    CDATA   --  Character string containing all of the character
C                encoded user data.
C Export Parameters:
C    IVAL    --  }
C    RVAL    --  }  One of these will contain the actual value read.
C    CVAL    --  }
C    CLEN    --  Length of the actual CVAL read, if read.
C    BAD     --  True if there is a problem reading the format      
C     Prerequisites:
C Commons directly
C          accessed: ECOLOC

      SUBROUTINE GNCVAL(FORMT, WIDTH, VTYPE, CDATA,
     $                  IVAL, RVAL, CVAL, CLEN, BAD)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/encode-observation-local.f'

      INTEGER WIDTH, VTYPE, IVAL, CLEN
      REAL RVAL
C  The *(msform) is needed for ANSI conformity below.
C  If your compiler can handle character*(*)'s in concatenations
C  you might want to change this back to character*(*).
      CHARACTER  FORMT*(MSFORM)
      CHARACTER *(*)  CDATA, CVAL
      LOGICAL BAD

      EXTERNAL GETLEN
      INTEGER GETLEN

C ====================================================================
C   Local variables
      INTEGER I1,I2
C ====================================================================
      I1 = CDTIDX+1
      I2 = CDTIDX + WIDTH

      IF (VTYPE .EQ. IDATAT) THEN
         READ(UNIT=CDATA(I1:I2),
     $        FMT=('('//FORMT//')'),
     $        ERR=988)                     IVAL
      ELSEIF (VTYPE .EQ. RDATAT) THEN
         READ(UNIT=CDATA(I1:I2),
     $        FMT=('('//FORMT//')'),
     $        ERR=988)                     RVAL
      ELSEIF (VTYPE .EQ. CDATAT) THEN
         READ(UNIT=CDATA(I1:I2),
     $        FMT=('('//FORMT//')'),
     $        ERR=988)                     CVAL
         CLEN = GETLEN(CVAL)
      ELSE
         CALL ENCERR(25)
      ENDIF

      CDTIDX = CDTIDX + WIDTH
      BAD = .FALSE.
      RETURN

 988  CONTINUE
      BAD = .TRUE.
      RETURN
      END

C Routine Long Name: encode-bad-value
C  6 Character Name: ENCBAD
C           Purpose: to handle a bad parameter value
C Import Parameters:
C    PP      --  index of parameter in paramter table.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly accessed: (none)
      SUBROUTINE ENCBAD(PP)
      INTEGER PP
      
C ====================================================================
C   Local variables
      INTEGER FF, XX, YY
      INTEGER NBAD, NBMAX
      PARAMETER (NBMAX=6)
      SAVE NBAD
      DATA NBAD/0/
C ====================================================================

      NBAD = NBAD + 1
      IF (NBAD .LE. NBMAX) THEN
         CALL GETFXY(PP, FF, XX, YY)
         PRINT 6001, PP, FF, XX, YY
         IF (NBAD .EQ. NBMAX) THEN
            PRINT 6002
         ENDIF
      ENDIF
      CALL ENCMSS(PP)
      RETURN
 6001 FORMAT (' Bad value for parameter ', I2,
     $         '(FF = ',I1, 'XX = ',I2.2, 'YY = ',I3.3, ')',
     $        / ' Missing substituted ')
 6002 FORMAT (' Further warnings suppressed.')
      END

C Routine Long Name: encode-missing-value
C  6 Character Name: ENCMSS
C           Purpose: to encode a missing parameter value
C Import Parameters: 
C    PP      --  index of missing parameter in parameter table
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly accessed: (none)

      SUBROUTINE ENCMSS(PP)
      INTEGER PP

      EXTERNAL GETBTW
      INTEGER GETBTW

      CALL EBPT1B(GETBTW(PP))
      RETURN
      END

C Routine Long Name: encode-character-data
C  6 Character Name: ENCCHR
C           Purpose: To encode a character valued parameter.
C Import Parameters:
C    PP      --  Index of parameter in the parameter table.
C    CVAL    --  Character string to encode.
C    CLEN    --  Length of the character string
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly accessed: (none)
      SUBROUTINE ENCCHR(PP,CVAL,CLEN)
      INTEGER PP
      CHARACTER*(*) CVAL
      INTEGER CLEN

      EXTERNAL GETBTW
      INTEGER GETBTW
C ====================================================================
C   Local variables
      INTEGER B
C ====================================================================
      B = GETBTW(PP)
      IF (0 .NE. MOD(B,8)) THEN
         CALL ENCERR(26)
C        no return
      ELSEIF (8*CLEN .GT. B) THEN
         PRINT 6001, PP
      ENDIF

      CALL EBPCHP(CVAL, CLEN, B/8)
      RETURN
 6001 FORMAT (' Warning: character parameter truncated (PP = ', I3,')')
      END

C Routine Long Name: encode-numeric-value
C  6 Character Name: ENCNUM
C           Purpose: to encode a numeric value (but not MISSING).
C Import Parameters:
C    PP      --  index of parameter to encode in parameter table.
C    VTYPE   --  type of parameter.
C    ival    --  value of parameter if VTYPE == IDATAT.
C    rval    --  value of parameter if VTYPE == RDATAT.
C Export Parameters:
C    irep    --  value written to EBUFR record.
C     Prerequisites:
C Commons directly
C          accessed:
      SUBROUTINE ENCNUM(PP, VTYPE, IVAL, RVAL, IREP)
      INCLUDE '../Commons/parameters.f'
      
      INTEGER PP, VTYPE
      INTEGER IVAL
      REAL RVAL
      INTEGER IREP
C ====================================================================
C   Local variables
      INTEGER MAXINT
      PARAMETER (MAXINT=2147483647)
      LOGICAL BAD
      INTEGER SCALE, REFVAL
      REAL R1
      INTEGER INEW
C ====================================================================
      CALL GETSCL(PP, SCALE, REFVAL)
      IF (VTYPE .EQ. IDATAT) THEN
         R1 = REAL(IVAL)* (10.0 ** SCALE) - REAL(REFVAL)
         BAD =  (ABS(R1) .GT. 2147483647)
         IF (.NOT. BAD) THEN
            IF (SCALE .LT. 0) THEN
               IP10 = 10 ** (-SCALE)
               INEW = (IVAL +IP10/2)/IP10 - REFVAL
            ELSE
               INEW = IVAL*(10**SCALE) - REFVAL
            ENDIF
         ENDIF
      ELSEIF (VTYPE .EQ. RDATAT) THEN
         BAD = ((SCALE+LOG10(MAX(1.0,ABS(RVAL)))) .GT. 11)
         IF (.NOT. BAD) THEN
            R1 = RVAL * (10.0 ** SCALE) - REAL(REFVAL)
            BAD =  (ABS(R1) .GT. 2147483647)
         ENDIF 
         IF (.NOT. BAD) THEN
            INEW = NINT(R1)
         ENDIF
      ELSE
         CALL ENCERR(27)
      ENDIF

      IF (BAD) THEN
         CALL ENCBAD(PP)
         IREP = -1
      ELSE
         CALL ENCSCL(PP, INEW, IREP)
      ENDIF
      RETURN
      END

C Routine Long Name: encode-scaled-value
C  6 Character Name: ENCSCL
C           Purpose: To encode a scaled parameter value.
C Import Parameters:
C    PP      --  index of paramter in the parameter table.
C    ival    --  scaled parameter value.
C Export Parameters:
C    irep    --  value actually written to EBUFR file.
C     Prerequisites: (none)
C Commons directly accessed: (none)
      SUBROUTINE ENCSCL(PP, IVAL, IREP)
      INTEGER PP, IVAL, IREP

      EXTERNAL IENCOD
      INTEGER IENCOD

      EXTERNAL GETBTW
      INTEGER GETBTW

C ====================================================================
C   Local variables
      INTEGER ICODE, B
C ====================================================================
      ICODE = IENCOD(PP, IVAL)
      B = GETBTW(PP)

      IF ((ICODE .LT. 0) .OR.
     $     ((B .LT. 31) .AND. (ICODE .GT. (2**B)-1)) .OR.
     $     (B .EQ. 31) .AND. (ICODE .EQ. 2147483647)) THEN
         CALL ENCBAD(PP)
         IREP = -1
      ELSE
         CALL EBPTIN(ICODE, B)
         IREP = ICODE
      ENDIF

      RETURN
      END

C Routine Long Name: internal-encode
C  6 Character Name: IENCOD
C           Purpose: To internally encode a user value before storing it
C                    in an EBUFR record
C Import Parameters:
C    IVAL    --  Value from user data to be encoded possibly.
C    PP      --  Index of paramter in parameter tables.
C Export Parameters:
C    MATCH   --  Set true if an explicit code match was found, false
C                otherwise.
C    -- Returns the encoded value.  If there is no explicit match
C       returns IVAL.
C     Prerequisites:  The code tables must have been previously stored.
C Commons directly
C          accessed: (none)
      INTEGER FUNCTION IENCOD(PP, IVAL)
      INTEGER IVAL, PP

      EXTERNAL ENCODE
      INTEGER ENCODE
C ====================================================================
C   Local variables
      INTEGER IDX1, IDX2
      INTEGER FF, XX, YY
      LOGICAL MATCH
C ====================================================================
      CALL GETCDI(PP, IDX1, IDX2)
      IENCOD = ENCODE(IDX1, IDX2, IVAL, MATCH)
      IF ((IDX1 .LE. IDX2) .AND. .NOT. MATCH) THEN
         PRINT 6001, PP, FF, XX, YY, IVAL
      ENDIF 
      RETURN
 6001 FORMAT (' Warning: parameter ',I2,
     $        '(F = ',I1,', X = ',I2.2, ', Y = ', I3.3, ')',
     $        ' has code pairs but none matched value: ',I10/
     $        '    Raw value used.')
      END
