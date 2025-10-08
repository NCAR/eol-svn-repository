C
C $Id: read-class-unit-record.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-class-unit-record.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-class-unit-record
C  6 Character Name: RDCLR
C           Purpose: To get the next valid class description from a
C                    file containing information like BUFR table B.
C Import Parameters:
C    UNIT    --  unit from which to read information.
C Export Parameters:
C    X       --  class number to be described.
C    DESCR   --  text describing the class
C    LENGTH  --  length of the descriptive text.
C    EOF     --  set true if an EOF is detected on UNIT.
C     Prerequisites:
C              UNIT must be opened for reading and must access an
C              appropriately formatted file.
C Commons directly accessed: none.
      SUBROUTINE RDCLR(UNIT,X,DESCR,LENGTH,EOF)
      INTEGER UNIT, X, LENGTH
      CHARACTER*(*) DESCR
      LOGICAL EOF

      EXTERNAL GETLEN
      INTEGER GETLEN
C ====================================================================
C   Local variables
      CHARACTER LINE*400, COMMNT
      PARAMETER (COMMNT='!')
C ====================================================================

 10   READ(UNIT=UNIT,FMT=5001,END=999) LINE
      IF ((LINE .EQ. ' ') .OR. (LINE(1:1) .EQ. COMMNT) ) GO TO 10

      EOF = .FALSE.
      READ(UNIT=LINE,FMT=5002) X, DESCR
      LENGTH = INDEX(DESCR, COMMNT) - 1
      IF (LENGTH .EQ. -1) THEN
         LENGTH = GETLEN(DESCR)
      ENDIF 
      RETURN

 999  EOF = .TRUE.
      X = -1
      DESCR = ' '
      LENGTH = 0
      RETURN

 5001 FORMAT (A)
 5002 FORMAT (I2, 6X, A)
      END
