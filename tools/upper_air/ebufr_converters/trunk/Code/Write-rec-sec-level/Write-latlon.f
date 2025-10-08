C
C $Id: Write-latlon.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-latlon.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-latlon
C  6 Character Name: WRTLL
C           Purpose: To write the latitude and longitude fixed part of
C                    record 9 type EBUFR record.
C Import Parameters:
C    LATLON  -- is a two element real array as follows:
C       LATLON(1) - latitude  in degrees.
C       LATLON(2) - longitude in degrees.
C Export Parameters: (none)
C     Prerequisites: ebfclr must be call before this routine, and the
C                    buffer must be positioned at the correct byte.
C         
C Commons directly accessed: (none)

      SUBROUTINE WRTLL(LATLON)
      REAL LATLON(2)
      INTEGER RSCALE

      RSCALE(A,IS,IREF) = NINT(A*(10.0**IS)-IREF)
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
      CALL EBPTIN(RSCALE(LATLON(1),5, -9000000),25)
      CALL EBALGN(32)
      CALL EBPTIN(RSCALE(LATLON(2),5,-18000000),26)
      CALL EBALGN(32)
      RETURN
      END
