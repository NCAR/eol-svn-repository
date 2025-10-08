C
C $Id: write-EBUFR-header-records.f,v 1.2 1994/03/18 19:06:57 john Exp $
C $Log: write-EBUFR-header-records.f,v $
C Revision 1.2  1994/03/18 19:06:57  john
C Fixed EBUFR type 0 and 1 writing.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C Modifications:  13 Dec 1991 David Casperson
C    Fixed an error in DOREC8 that caused the flag F,X,Y not be
C    computed before calling GTFXYU.
C----
C Routine Long Name: Write-header-records
C  6 Character Name: WRTHDR
C           Purpose: To write the header records for one EBUFR file.
C Import Parameters:
C    EUNIT   --  Unit on which to write header records.
C    CUNIT   --  Unit from which to read class descriptions.
C    PUNIT   --  Unit from which to read parameter descriptions.
C    GTFXYU  --  Routine to open flag/code table for input.
C    CLSUNI  --  Routine to close flag/table unit
C    TIME    --  An array of six integers formatted as follows:
C       TIME(1) - year   (only mod 100 value is important here.)
C       TIME(2) - month
C       TIME(3) - day
C       TIME(4) - hour
C       TIME(5) - minute
C       TIME(6) - second. (ignored by this routine.)
C    FILENO  --   file described in the ctl-file for which to write 
C                 header records.
C Export Parameters: (none)
C     Prerequisites: EUNIT, CUNIT, PUNIT must be opened appropriately.
C                    Information describing the file to be written must
C                    have been previously read and stored in the 
C                    appropriate commons.
C Commons directly accessed: (none)

      SUBROUTINE WRTHDR(EUNIT, CUNIT, PUNIT,
     $                  GTFXYU, CLSUNI, TIME, FILENO)
      EXTERNAL GTFXYU, CLSUNI
      INTEGER EUNIT, CUNIT, PUNIT, TIME(6), GTFXYU, FILENO
      INCLUDE '../Commons/parameters.f'

C ====================================================================
C   Local variables
      INTEGER TYPE, SBTYPE, TEXTLN, OBSLST(MOBSPF), NUMOBS
      INTEGER EBFVER, BUFEDN, BFMTVS, ORGCEN, SEQNO, BUFRMT
      CHARACTER*(MTXTLN) TEXT
C ====================================================================

      CALL EBFSTU(EUNIT)
      CALL GETFIN(FILENO, TYPE, SBTYPE, TEXT, TEXTLN, OBSLST, NUMOBS)
      CALL GETSTC(EBFVER, BUFEDN, BUFRMT, ORGCEN, SEQNO, BFMTVS)

      CALL WRTRC0(EBFVER, 0, BUFEDN, BFMTVS)
      CALL WRTRC1(BUFRMT, ORGCEN, SEQNO, TYPE, SBTYPE, BFMTVS, TIME)
      IF (NUMOBS .EQ. 1) THEN
         CALL WRORC2(OBSLST(1))
      ENDIF

      CALL WRTRC3(TEXT,TEXTLN)
      CALL DOREC6(OBSLST, NUMOBS, CUNIT)
      CALL DOREC7(OBSLST, NUMOBS, PUNIT)
      CALL DOREC8(OBSLST, NUMOBS, GTFXYU, CLSUNI)

      RETURN
      END

C Routine Long Name: Write-record-2-for-observation
C  6 Character Name: WRORC2
C           Purpose: To write an EBUFR type 2 record describing 
C                    the data to be contained in this EBUFR file.
C Import Parameters:
C    OCODE   --  Code for the observation type to be described in the
C                type 2 record.
C Export Parameters: (none)
C     Prerequisites: ???
C Commons directly accessed: (none)

      SUBROUTINE WRORC2(OCODE)
      INTEGER OCODE

      CALL EBFCLR
        CALL EBPTBY(2)
        CALL EBPTZB(6)
        CALL EBPTBY(1)
        CALL EBPTZB(8)
        CALL EBPTBY(1)
      CALL EBSTVP
        CALL WRDESC(OCODE)
      CALL EBFLSH

      RETURN
      END

C Routine Long Name: Do-type-6-records
C  6 Character Name: DOREC6
C           Purpose: To write all of the type 6 records needed for 
C                    this file.
C Import Parameters:
C    OBSLST  --  List of observation codes to be written to this file.
C    NUMOBS  --  Number of elements in OBSLST.
C    CUNIT   --  Unit from which to read the class descriptions.      
C Export Parameters: (none)
C     Prerequisites: ???
C Commons directly accessed: (none)

      SUBROUTINE DOREC6(OBSLST, NUMOBS, CUNIT)
      INTEGER OBSLST(*), NUMOBS, CUNIT
      INCLUDE '../Commons/parameters.f'
      
C ====================================================================
C   Local variables
      LOGICAL XUSED
      DIMENSION XUSED(0:63)
      LOGICAL EOFFLG
      CHARACTER*(XXSIZE) DESCR
      INTEGER I, J, PSTART, PEND, FF, XX, YY, LEN
C ====================================================================
      DO 10 I=0,63,1
         XUSED(I) = .FALSE.
 10   CONTINUE 

      DO 20 I=1,NUMOBS,1
         CALL GPRNGE(OBSLST(I), PSTART, PEND)
         DO 30 J=PSTART,PEND,1
            CALL GETFXY(J, FF,XX,YY)
            IF (FF .EQ. 0) THEN
C       We'll have to think about FF=3
C       when we add sequences.
               XUSED(XX) = .TRUE.
            ENDIF
 30      CONTINUE
 20   CONTINUE
      XUSED(0) = .TRUE.

      EOFFLG = .FALSE.
      REWIND(CUNIT)
C   while not (EOFFLG) do
 40   IF (.NOT. EOFFLG) THEN
         CALL RDCLR(CUNIT, XX, DESCR, LEN, EOFFLG)
         IF (EOFFLG) GO TO 40
         IF (.NOT. XUSED(XX)) GO TO 40
         CALL WRTRC6(XX, DESCR, LEN)
         XUSED(XX) = .FALSE.
C   endwhile
         GO TO 40
      ENDIF

      CALL CHKUDC(XUSED)
      RETURN
      END

C Routine Long Name: check-code-and-flag-table-name
C  6 Character Name: CHKCFT
C           Purpose: To determine whether a parameter uses a code
C                    or flag table.
C Import Parameters:
C    UNITS   --  name of the units for this parameter.
C Export Parameters:
C    ISCODE  --  Set true if this parameter's value is a code table.
C    ISFLAG  --  Set true if this parameter's value is a flag table.
C     Prerequisites: (none)
C Commons directly
C          accessed: (none)
      SUBROUTINE CHKCFT(UNITS, ISCODE, ISFLAG)
      CHARACTER *(*) UNITS
      LOGICAL ISCODE, ISFLAG

      ISCODE = INDEX(UNITS, 'Code table') .GT. 0
      ISFLAG = INDEX(UNITS, 'Flag table') .GT. 0
      RETURN
      END

C Routine Long Name: Do-type-7-records
C  6 Character Name: DOREC7
C           Purpose: To write all of the type 7 records needed for 
C                    this file.
C Import Parameters:
C    OBSLST  --  List of observation codes to be written to this file.
C    NUMOBS  --  Number of elements in OBSLST.
C    PUNIT   --  Unit from which to read the parameter descriptions.      
C Export Parameters: (none)
C     Prerequisites: ???
C Commons directly accessed: (none)

      SUBROUTINE DOREC7(OBSLST, NUMOBS, PUNIT)
      INTEGER OBSLST(*), NUMOBS, PUNIT

      EXTERNAL FDFXYI, MARKED
      LOGICAL  FDFXYI, MARKED
      INCLUDE '../Commons/parameters.f'
C ====================================================================
C   Local variables
      LOGICAL EOFFLG
      LOGICAL ISCODE, ISFLAG
      CHARACTER  NAME*(NMSIZE), UNITS*(UNSIZE)
      INTEGER I, FF, XX, YY, SCALE, REFVAL, BTWDTH
C ====================================================================

      CALL MKLIST(OBSLST, NUMOBS)
      
      EOFFLG = .FALSE.
      REWIND(PUNIT)

C   while not (EOFFLG) do
 40   IF (.NOT. EOFFLG) THEN
         CALL RDPRC(PUNIT, FF, XX, YY, NAME, UNITS,
     $               SCALE, REFVAL, BTWDTH,
     $               EOFFLG)
         IF (EOFFLG) GO TO 40
         IF (.NOT. FDFXYI(FF, XX, YY, I)) GO TO 40
         IF (.NOT. MARKED(I)) GO TO 40
C      We get here if have found a record describing a marked
C      FXY triple.         
         CALL STRSCL(I, SCALE, REFVAL, BTWDTH)
         CALL CHKCFT(UNITS, ISCODE, ISFLAG)
         CALL STRCFL(I, ISCODE, ISFLAG)
         CALL WRTRC7(FF, XX, YY, NAME, UNITS, SCALE, REFVAL, BTWDTH)
         CALL UMKFXY(I)
C   endwhile
         GO TO 40
      ENDIF

      CALL CHKMRK
      RETURN
      END

C Routine Long Name: Do-type-8-records
C  6 Character Name: DOREC8
C           Purpose: To write all of the type 8 records needed for 
C                    this file.
C Import Parameters:
C    OBSLST  --  List of observation codes to be written to this file.
C    NUMOBS  --  Number of elements in OBSLST.
C    GTFXYU  --  Routine to open flag/code table for input.
C    CLSUNI  --  Routine to close flag/table unit
C Export Parameters: (none)
C     Prerequisites: DOREC7 must be called first in order that this 
C                    routine know which parameters have code/flag tables
C                    as their values.
C Commons directly accessed: (none)
C Modifications: 13 Dec 1991 David Casperson
C    Now calls GT1FXY to get the correct f,x,y values before calling
C    GTFXYU for flag tables.

      SUBROUTINE DOREC8(OBSLST, NUMOBS, GTFXYU, CLSUNI)
      EXTERNAL GTFXYU, CLSUNI
      INTEGER OBSLST(*), NUMOBS, GTFXYU

      EXTERNAL FDFXYI, MARKED, FLAGGD, CODED
      LOGICAL  FDFXYI, MARKED, FLAGGD, CODED

      INCLUDE '../Commons/parameters.f'
C ====================================================================
C   Local variables
      LOGICAL EOFFLG
      LOGICAL LDUMMY
      CHARACTER DESCR*(CXSIZE)
      INTEGER I, MXFXY, CFUNIT, ICODE,
     $        FF, XX, YY, LEN
C ====================================================================
      CALL MKLIST(OBSLST, NUMOBS)
      LDUMMY = FDFXYI(4,0,0,MXFXY)

      DO 10 I=1,MXFXY-1,1
         IF (MARKED(I)) THEN
            IF (CODED(I)) THEN
               CALL GT1FXY(I,FF,XX,YY)
               CFUNIT = GTFXYU(FF, XX, YY, .TRUE.)
               EOFFLG = .FALSE.
C            While not EOF do
 20            IF (.NOT. EOFFLG) THEN
                  CALL RDCTR(CFUNIT, ICODE, DESCR, LEN, EOFFLG)
                  IF (EOFFLG) GO TO 20
                  CALL WRTRC8(XX, YY, ICODE, DESCR, LEN)
C            endwhile
                  GO TO 20
               ENDIF
            ELSEIF (FLAGGD(I)) THEN
               CALL GT1FXY(I,FF,XX,YY)
               CFUNIT = GTFXYU(FF, XX, YY, .FALSE.)
               EOFFLG = .FALSE.
C            While not EOF do
 22            IF (.NOT. EOFFLG) THEN
                  CALL RDFTR(CFUNIT, ICODE, DESCR, LEN, EOFFLG)
                  IF (EOFFLG) GO TO 22
                  CALL WRTRC8(XX, YY, ICODE, DESCR, LEN)
C            endwhile
                  GO TO 22
               ENDIF
            ENDIF
         ENDIF 
 10   CONTINUE

      CALL CLRMKS
         
      RETURN
      END

C Routine Long Name: Mark-observation-list
C  6 Character Name: MKLIST
C           Purpose: To mark all of the entries in the FXY table that 
C                    correspond to parameters in Obs_List and clear
C                    all other marks.
C Import Parameters:
C    OBSLST  --  List of observation codes to be written to this file.
C    NUMOBS  --  Number of elements in OBSLST.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly accessed: (none)

      SUBROUTINE MKLIST(OBSLST, NUMOBS)
      INTEGER OBSLST(*), NUMOBS

      EXTERNAL FDFXY
      INTEGER  FDFXY
C ====================================================================
C   Local variables
      INTEGER I, J, PSTART, PEND
      INTEGER FF,XX,YY
C ====================================================================
      CALL CLRMKS
      DO 10 I=1,NUMOBS,1
         CALL GPRNGE(OBSLST(I), PSTART, PEND)
         DO 20 J=PSTART,PEND,1
            CALL GETFXY(J,FF,XX,YY)
            IF ((FF .EQ. 0) .OR. (FF .EQ. 3)) THEN
C                                N.B.  FF=3 not yet supported.
               CALL MRKPAR(J)
            ENDIF 
 20      CONTINUE
 10   CONTINUE

      DO 30 I=10,20,1
         J = FDFXY(0,0,I)
         CALL MKFXY(J)
 30   CONTINUE

      DO 40 I=250,255,1
         J = FDFXY(0,0,I)
         CALL MKFXY(J)
 40   CONTINUE 

      RETURN
      END

C Routine Long Name: check-for-undescribed-classes
C  6 Character Name: CHKUDC
C           Purpose: To check for any undescribed classes, writing error
C                    messages if found.
C Import Parameters: CLIST
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly accessed: (none)

      SUBROUTINE CHKUDC(CLIST)
      LOGICAL CLIST(0:63)
C ====================================================================
C   Local variables
      LOGICAL MSSING
      INTEGER I
C ====================================================================
      MSSING = .FALSE.
      DO 10 I=0,63,1
         IF (CLIST(I)) THEN
            IF (.NOT. MSSING) THEN
               MSSING = .TRUE.
               PRINT 9997
            ENDIF
            PRINT 9996, I
         ENDIF
 10   CONTINUE

      IF (MSSING) THEN
         PRINT 9995
C The following call to ENCERR removed 19 Sep 1991.
C The philosophy here is to try to write an EBUFR file if possible,
C even if it may be missing some records required to create a
C correctly formatted EBUFR file.
C-       CALL ENCERR(43)
      ENDIF

      RETURN
C   Format for initial error message
 9997 FORMAT (1X,
     $        'After reading the class description unit the following'/
     $        '  classes were still undescribed')
C   Format for each missing class
 9996 FORMAT (T10, I3)
C   Format for good-bye message
 9995 FORMAT (1X,
     $        'Continuing in face of missing class descriptions: '/
     $        '  The resulting EBUFR file may be missing '//
     $        'important header records.')
      END

C Routine Long Name: check-for-marks
C  6 Character Name: CHKMRK
C           Purpose: To check to see if any FXY tuples are still marked.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly accessed: (none)
      
      SUBROUTINE CHKMRK()

      EXTERNAL FDFXYI, MARKED
      LOGICAL FDFXYI, MARKED
C ====================================================================
C   Local variables
      LOGICAL MSSING
      INTEGER MXFXY, I, FF, XX, YY
C ====================================================================
      MSSING = FDFXYI(4,0,0,MXFXY)
      DO 10 I=1,MXFXY-1,1
         IF (MARKED(I)) THEN
            IF (.NOT. MSSING) THEN
               MSSING = .TRUE.
               PRINT 9997
            ENDIF
            CALL GT1FXY(I, FF, XX, YY)
            PRINT 9996,  FF,XX,YY,I
         ENDIF
 10   CONTINUE

      IF (MSSING) THEN
         PRINT 9995
C The following call to ENCERR removed 19 Sep 1991.
C The philosophy here is to try to write an EBUFR file if possible,
C even if it may be missing some records required to create a
C correctly formatted EBUFR file.
C-       CALL ENCERR(44)
      ENDIF

      RETURN
C   Format for initial error message
 9997 FORMAT (1X,
     $        'After reading the parameter description unit'/
     $        '   the following classes were still undescribed: ')
C   Format for each missing class
 9996 FORMAT (T10, 'FF = ',I1, 2X, 'XX = ',I2, 2X, 'YY = ',I3, 2X,
     $              '(entry ',I5,' in the FXY table)')
C   Format for good-bye message
 9995 FORMAT (1X,
     $   'Continuing in face of missing parameter descriptions: '/
     $        '  The resulting EBUFR file may be missing '//
     $        'important header records.')
      END
