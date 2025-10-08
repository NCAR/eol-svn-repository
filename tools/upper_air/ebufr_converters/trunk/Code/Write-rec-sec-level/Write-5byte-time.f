C
C $Id: Write-5byte-time.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-5byte-time.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-5byte-time
C  6 Character Name: WRT5BT
C           Purpose: To write the 5 byte time part of a BUFR section 1.
C Import Parameters:
C    TIME  -- is a six element integer array as follows
C       TIME(1) - year   (only mod 100 value is important here.)
C       TIME(2) - month
C       TIME(3) - day
C       TIME(4) - hour
C       TIME(5) - minute
C       TIME(6) - second. (ignored by this routine.)
C Export Parameters: none
C     Prerequisites: ebfclr must be call before this routine, and the
C                    output buffer must be positioned correctly.
C Commons directly
C          accessed: none
      SUBROUTINE WRT5BT(TIME)
      INTEGER TIME(6)
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
      CALL EBPTBY(MOD(TIME(1),100))
      CALL EBPTBY(TIME(2))
      CALL EBPTBY(TIME(3))
      CALL EBPTBY(TIME(4))
      CALL EBPTBY(TIME(5))
      RETURN
      END
