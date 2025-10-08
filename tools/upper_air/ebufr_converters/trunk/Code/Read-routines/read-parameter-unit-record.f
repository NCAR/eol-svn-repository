C
C $Id: read-parameter-unit-record.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-parameter-unit-record.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C  Revisions:
C    2 Dec 1991 David Casperson  -- modified format statemment used by 
C                                  rdprc.
C Routine Long Name: read-parameter-unit-record
C  6 Character Name: RDPRC
C           Purpose: To get the next valid class description from a
C                    file containing parameter descriptions.
C Import Parameters:
C    UNIT    --  unit from which to read information.
C Export Parameters:
C    F       -- }
C    X       -- } Define the parameter being described.
C    Y       -- }
C    NAME    -- Name of the parameter (text).
C    UNITS   -- Units the parameter is measured in (text).
C                   Must be CODE TABLE for parameters that have an
C                   associated code table and FLAG TABLE for
C                   parameters that have an associated flag table.
C    SCALE   -- Scale factor (signed integer).
C    REFVAL  -- Reference value (signed integer).
C    BTWDTH  -- Bit width for encoded value in EBUFR record.
C    EOF     -- Set true if END-OF-FILE detected on UNIT before a valid
C                 description was found.
C     Prerequisites:
C              UNIT must be opened for reading and must access an
C              appropriately formatted file.
C Commons directly accessed:  (none).
C Modification:  2 Dec 1991 David Casperson --
C    This routine modified to use updated format that Wayne Brazille's 
C    software produces.  The format suggested in any user documentation
C    written before this date is incorrect.
      SUBROUTINE RDPRC(UNIT,F,X,Y,NAME,UNITS,SCALE,REFVAL,BTWDTH,EOF)
      INTEGER UNIT, F, X, Y, SCALE, REFVAL, BTWDTH
      CHARACTER*(*) NAME, UNITS
      LOGICAL EOF
C ====================================================================
C   Local variables
      CHARACTER LINE*160, COMMNT
      PARAMETER (COMMNT='!')
C ====================================================================

 10   READ(UNIT=UNIT,FMT=5003,END=999) LINE
      IF ((LINE .EQ. ' ') .OR. (LINE(1:1) .EQ. COMMNT) ) GO TO 10

      EOF = .FALSE.
      READ(UNIT=LINE,FMT=5004) F,X,Y,
     $     UNITS, SCALE, REFVAL, BTWDTH, NAME
      RETURN

 999  EOF = .TRUE.
      RETURN

 5003 FORMAT (A)
C-----------------------------
C                 F,X,Y,
 5004 FORMAT (T57, I1, 1X, I2, 1X, I3, 1X,
C                 UNITS
     $     A24
C                 Scale factor, reference value, bit width
     $     T94, I4, 1X, I11, 1X, I4,
C                 Name
     $     T121, A64)
      END
