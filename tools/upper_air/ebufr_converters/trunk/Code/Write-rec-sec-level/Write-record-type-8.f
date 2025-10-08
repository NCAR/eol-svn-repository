C
C $Id: Write-record-type-8.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-record-type-8.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-record-type-8
C  6 Character Name: WRTRC8
C           Purpose: to create and{write one type 8 EBUFR record
C Import Parameters:
C    X        -- X value } These define the parameter to be described.
C    Y        -- Y value }
C    CODE     -- Code value to be described for this X,Y pair.
C    TEXT     -- Characters giving an ASCII description of this code
C                 value.
C    TEXTLN   -- Length of the signifigant text in TEXT
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC8(X, Y, CODE, TEXT, TEXTLN)
      CHARACTER*(*) TEXT
      INTEGER X, Y, CODE, TEXTLN
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
      INTEGER FF(9), XX(9), YY(9)
      DATA    FF/   0,   0,   0,   0,   0,   0,   0,   0,   0/
     $        XX/   0,   0,   0,   0,   0,   0,   0,   0,   0/
     $        YY/  10,  11,  12, 255, 254, 253, 252, 251, 250/
C ====================================================================
      CALL EBFCLR
        CALL EBPTBY(8)
        CALL EBPTBY(X)
        CALL EBPTBY(Y)
        CALL EBPTIN(CODE, 32)
        CALL EBPTZB(1)

        CALL EBPTBY(X)
        CALL EBPTBY(Y)
        CALL EBPTIN(CODE, 32)
        CALL EBPTZB(3)
      CALL EBSTVP

        CALL WRTSC3(1, .FALSE., .FALSE., FF, XX, YY, 9)

        CALL STRTSC(4)
          CALL EBPTZB(1)
        
          CALL EBPTIA(F,1)
          CALL EBPTIA(X,2)
          CALL EBPTIA(Y,3)
          CALL EBPTIA(CODE, 3)
          CALL EBPCHP(TEXT, TEXTLN, 160)
        CALL ENDSC(4)
      CALL EBFLSH
      RETURN
      END
