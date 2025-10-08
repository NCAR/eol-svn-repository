C
C $Id: Write-record-type-7.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-record-type-7.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-record-type-7
C  6 Character Name: WRTRC7
C           Purpose: to create and{write one type 7 EBUFR record
C Import Parameters:
C    F        -- F value }
C    X        -- X value } These define the parameter to be described.
C    Y        -- Y value }
C    ENAME    -- Element name for this parameter.
C    UNITS    -- Units that this parameter is measured in.
C    SCALE    -- Scale factor for this element.
C    REFVAL   -- Reference value for encoding this parameter
C    BTWDTH   -- Number of bits this parameter takes in EBUFR record.
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC7(F, X, Y, ENAME, UNITS, SCALE, REFVAL, BTWDTH)
      CHARACTER*(*) ENAME, UNITS
      INTEGER F, X, Y, SCALE, REFVAL, BTWDTH
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
      INTEGER FF(11), XX(11), YY(11)
      CHARACTER SFSIGN, RVSIGN
      DATA    FF/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
     $        XX/  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
     $        YY/ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20/
C ====================================================================
      IF (SCALE .GE. 0) THEN
         SFSIGN = '+'
      ELSE
         SFSIGN = '-'
      ENDIF

      IF (REFVAL .GE. 0) THEN
         RVSIGN = '+'
      ELSE
         RVSIGN = '-'
      ENDIF

      CALL EBFCLR
        CALL EBPTBY(7)
        CALL EBPTBY(F*64+X)
        CALL EBPTBY(Y)
        CALL EBPTZB(5)
        CALL EBPTBY(F*64+X)
        CALL EBPTBY(Y)
        CALL EBPTZB(7)
      CALL EBSTVP

        CALL WRTSC3(1, .FALSE., .FALSE., FF, XX, YY, 11)

        CALL STRTSC(4)
          CALL EBPTZB(1)
        
          CALL EBPTIA(F,1)
          CALL EBPTIA(X,2)
          CALL EBPTIA(Y,3)
          CALL EBPCHR(ENAME(1:32), 32)
          CALL EBPCHR(ENAME(33:64), 32)
          CALL EBPCHR(UNITS, 24)
          CALL EBPCHR(SFSIGN, 1)
          CALL EBPTIA(ABS(SCALE), 3)
          CALL EBPCHR(RVSIGN, 1)
          CALL EBPTIA(ABS(REFVAL), 10)
          CALL EBPTIA(BTWDTH, 3)
        CALL ENDSC(4)
      CALL EBFLSH
      RETURN
      END
