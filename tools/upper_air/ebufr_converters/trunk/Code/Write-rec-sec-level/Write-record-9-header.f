C
C $Id: Write-record-9-header.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: Write-record-9-header.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: Write-record-9-header
C  6 Character Name: WRTR9H
C           Purpose: To write the fixed part of record 9 type EBUFR
C                    record.
C Import Parameters:
C    TIME  -- is a six element integer array as follows
C       TIME(1) - year
C       TIME(2) - month
C       TIME(3) - day
C       TIME(4) - hour
C       TIME(5) - minute
C       TIME(6) - second.
C    LATLON  -- is a two element real array as follows
C       LATLON(1) - latitude  in decimal degrees.
C       LATLON(2) - longitude in decimal degrees.
C    CLIBYT  --  Uniquifying byte for distinguishing between stations
C                 that have almost identical latitudes and longitudes.
C                 Should be a positive integer between 0 and 255.
C Export Parameters:
C    none
C     Prerequisites: ebfclr must be call before this routine.
C         
C Commons directly
C          accessed: none
      SUBROUTINE WRTR9H(TIME, LATLON, CLIBYT)
      INTEGER TIME(6), CLIBYT
      REAL LATLON(2)
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
      CALL EBPTBY(9)
      CALL WRT7BT(TIME)
      CALL WRTLL(LATLON)
      CALL EBPTBY(CLIBYT)
      CALL EBSTVP
      RETURN
      END
