C
C $Id: Write-record-type-3.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-record-type-3.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-record-type-3
C  6 Character Name: WRTRC3
C           Purpose: to create and{write one type 3 EBUFR record
C Import Parameters:
C    TEXT     -- Characters giving an ASCII description of the data.
C    TEXTLN   -- Length of the signifigant text in TEXT
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC3(TEXT, TEXTLN)
      CHARACTER*(*) TEXT
      INTEGER TEXTLN
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
      INTEGER FF(1), XX(1), YY(1)
C ====================================================================
      CALL EBFCLR
        CALL EBPTBY(3)
        CALL EBPTZB(6)
        CALL EBPTBY(1)
        CALL EBPTZB(8)
        CALL EBPTBY(1)
      CALL EBSTVP

        FF(1) = 2
        XX(1) = 5
        YY(1) = TEXTLN
        CALL WRTSC3(1, .FALSE., .FALSE., FF, XX, YY, 1)

        CALL STRTSC(4)
          CALL EBPTZB(1)
          CALL EBPCHR(TEXT, TEXTLN)
        CALL ENDSC(4)
      CALL EBFLSH
      RETURN
      END
