C
C $Id: Write-record-type-2.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-record-type-2.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-record-type-2
C  6 Character Name: WRTRC2
C           Purpose: to create and{write one type 2 EBUFR record
C Import Parameters:
C    F        -- Array of F values }
C    X        -- Array of X values } These define the observation
C    Y        -- Array of Y values }
C    NFXY     -- Number of F,X,Y values
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC2(F, X, Y, NFXY)
      INTEGER NFXY, F(NFXY), X(NFXY), Y(NFXY)
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
      CALL EBFCLR
        CALL EBPTBY(2)
        CALL EBPTZB(6)
        CALL EBPTBY(1)
        CALL EBPTZB(8)
        CALL EBPTBY(1)
      CALL EBSTVP
        CALL WRTSC3(1, .TRUE., .FALSE., F, X, Y, NFXY)
      CALL EBFLSH
      RETURN
      END
