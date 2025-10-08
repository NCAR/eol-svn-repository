C
C $Id: read-one-file-description.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-one-file-description.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-one-file-descriptions
C  6 Character Name: RD1FLD
C           Purpose: To read the descriptions of one file from the 
C                    control file.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: The control file must have been opened for reading
C                    and positioned correctly.
C Commons directly
C          accessed: CTLFIL
      SUBROUTINE RD1FLD()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ctl_file.f'

C ====================================================================
C   Local variables
      INTEGER OBSLST(MOBSPF), NUMOBS
      INTEGER TYPE, SBTYPE, TEXTLN
      CHARACTER TEXT*(MTXTLN)
      INTEGER L1, L2
C ====================================================================

      IF (LINCLS .NE. FILECL) THEN
         CALL ENCERR(36)
      ENDIF

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(37)
      ENDIF
      READ(UNIT=LINE,FMT=9998,ERR=998) TYPE

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(37)
      ENDIF
      READ(UNIT=LINE,FMT=9997,ERR=998) SBTYPE

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(37)
      ENDIF
      READ(UNIT=LINE,FMT=9996,ERR=998) TEXTLN
      CALL RDCLIN

      L1=TEXTLN
      L2=0
 10   IF (L1 .GT. 0) THEN
         IF (L1 .GE. 80) THEN
            READ (UNIT=LINE,FMT=9995,ERR=998) TEXT(L2+1:L2+80)
            L2 = L2 + 80
            L1 = L1 - 80
         ELSE
            READ (UNIT=LINE,FMT=9994,ERR=998) TEXT(L2+1:TEXTLN)
            L2 = TEXTLN
            L1 = 0
         ENDIF
         CALL RDCLIN
         GO TO 10
      ENDIF

      CALL RDOBSL(NUMOBS, OBSLST)
      CALL STNFIN(TYPE, SBTYPE, TEXT, TEXTLN, OBSLST, NUMOBS)
      IF (LINCLS .NE. EFILCL) THEN
         CALL ENCERR(38)
      ELSE
         CALL RDCLIN
      ENDIF
      RETURN

 998  CALL ENCERR(41)
C     Never reached
      RETURN 

 9998 FORMAT (I4)
 9997 FORMAT (I4)
 9996 FORMAT (I4)
 9995 FORMAT (A80)
 9994 FORMAT (A)

      END
