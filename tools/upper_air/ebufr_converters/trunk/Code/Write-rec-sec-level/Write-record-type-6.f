C
C $Id: Write-record-type-6.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-record-type-6.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-record-type-6
C  6 Character Name: WRTRC6
C           Purpose: to create and{write one type 6 EBUFR record
C Import Parameters:
C    CLASS    -- Number of class (X value) this record describes.
C    TEXT     -- Characters giving an ASCII description of the class.
C    TEXTLN   -- Length of the signifigant text in TEXT
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC6(CLASS, TEXT, TEXTLN)
      CHARACTER*(*) TEXT
      INTEGER CLASS, TEXTLN
C ====================================================================
C Here is an index of the routines used for directly accessing 
C  the EBUFR-buffer:
C      Short name   Long name          Parameters
C      ----- ----   ---- ----          ----------
C      ebfclr       Clear-EBUFR          none
C      strtsc       start-BUFR-section   section-number
C      endsc        end-EBUFR-section    section-number
C      ebstvp       start-EBUFR-variable-part    none
C      ebptzb       put-zero-bytes       number-of-bytes
C      ebptby       put-byte             byte-value
C      ebpchr       put-characters       characters, number of chars
C      ebpchp       put-chars-padded     characters, size-of-src,
C                                          size-of-dest
C      ebpt0b       put-zero-bits        number-of-bits
C      ebpt1b       put-one-bits         number-of-bits
C      ebalgn       EBUFR-align          alignment
C      ebptin       put-integer          integer_val, bit_width
C      ebptia       put-integer-ascii    integer_val, num_chars
C ====================================================================
C Local variables
      INTEGER FF(4), XX(4), YY(4)
      DATA    FF/  0,  0,  0,  0/
     $        XX/  0,  0,  0,  0/
     $        YY/ 10, 11, 13, 14/  
C ====================================================================
      CALL EBFCLR
        CALL EBPTBY(6)
        CALL EBPTBY(CLASS)
        CALL EBPTZB(6)
        CALL EBPTBY(CLASS)
        CALL EBPTZB(8)
      CALL EBSTVP

        CALL WRTSC3(1, .FALSE., .FALSE., FF, XX, YY, 4)

        CALL STRTSC(4)
          CALL EBPTZB(1)
          CALL EBPTIA(0,1)
          CALL EBPTIA(CLASS,2)
          CALL EBPCHP(TEXT, TEXTLN, 64)
        CALL ENDSC(4)
      CALL EBFLSH
      RETURN
      END
