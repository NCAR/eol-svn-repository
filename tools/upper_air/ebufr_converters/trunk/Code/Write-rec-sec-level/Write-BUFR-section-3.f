C
C $Id: Write-BUFR-section-3.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-BUFR-section-3.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-BUFR-section-3
C  6 Character Name: WRTSC3
C           Purpose: To write BUFR section 3 to the current EBUFR record.
C Import Parameters:
C    NDSS     -- Number of data subsets
C    OBSVD    -- True if this is observed data, false otherwise.
C    CMPRSD   -- True if compressed data, false otherwise.      
C    F        -- Array of F values }
C    X        -- Array of X values } These define the observation
C    Y        -- Array of Y values }
C    NFXY     -- Number of F,X,Y values
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTSC3(NDSS, OBSVD, CMPRSD, F,X,Y,NFXY)
      INTEGER NDSS, NFXY
      LOGICAL OBSVD, CMPRSD
      INTEGER F(NFXY), X(NFXY), Y(NFXY)
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
      INTEGER I
C ====================================================================      
      CALL STRTSC(3)
      CALL EBPTZB(1)
      CALL EBPTIN(NDSS, 16)
      IF (OBSVD) THEN
         CALL EBPTIN(1, 1)
      ELSE
         CALL EBPTIN(0 ,1)
      ENDIF 

      IF (CMPRSD) THEN
         CALL EBPTIN(1, 1)
      ELSE
         CALL EBPTIN(0 ,1)
      ENDIF

      CALL EBPT0B(6)

      DO 10 I = 1,NFXY,1
         CALL EBPTBY(64*F(I)+X(I))
         CALL EBPTBY(Y(I))
 10   CONTINUE
      CALL ENDSC(3)
      RETURN
      END
      
