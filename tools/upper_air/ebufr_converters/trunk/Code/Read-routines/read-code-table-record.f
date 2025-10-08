C
C $Id: read-code-table-record.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-code-table-record.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-code-table-record
C  6 Character Name: RDCTR
C           Purpose: To read the next valid description of a code value
C                    from a file containing code table information.
C Import Parameters:
C    UNIT    --  unit from which to read information.
C Export Parameters:
C    CODEVL  --  Code value described.
C    DESCR   --  text describing the class.
C    LENGTH  --  length of the descriptive text.
C    EOF     --  set true if an EOF is detected on UNIT.
C     Prerequisites:
C              UNIT must be opened for reading and must access a
C              file describing the code values for a particular
C              F X Y triple.
C Commons directly accessed: none.
      SUBROUTINE RDCTR(UNIT,CODEVL,DESCR,LENGTH,EOF)
      INTEGER UNIT, CODEVL, LENGTH
      CHARACTER*(*) DESCR
      LOGICAL EOF

      EXTERNAL GETLEN
      INTEGER GETLEN
C ====================================================================
C   Local variables
      CHARACTER LINE*400, COMMNT
      PARAMETER (COMMNT='!')
C ====================================================================

 10   READ(UNIT=UNIT,FMT=5005,END=999) LINE
      IF ((LINE .EQ. ' ') .OR. (LINE(1:1) .EQ. COMMNT) ) GO TO 10

      EOF = .FALSE.
      READ(UNIT=LINE,FMT=5006) CODEVL, DESCR
      LENGTH = INDEX(DESCR, COMMNT) - 1
      IF (LENGTH .EQ. -1) THEN
         LENGTH = GETLEN(DESCR)
      ENDIF 
      RETURN

 999  EOF = .TRUE.
      CODEVL = -1
      DESCR = ' '
      LENGTH = 0
      RETURN

 5005 FORMAT (A)
 5006 FORMAT (BN, I4, 4X, A)
      END
