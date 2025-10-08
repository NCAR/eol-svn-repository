C
C $Id: Write-record-type-0.f,v 1.2 1994/03/18 19:07:11 john Exp $
C $Log: Write-record-type-0.f,v $
C Revision 1.2  1994/03/18 19:07:11  john
C Fixed EBUFR type 0 and 1 writing.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C Routine Long Name: Write-record-type-0
C  6 Character Name: WRTRC0
C           Purpose: to create and write one type 0 EBUFR record.
C Import Parameters: 
C     EBUFRV  --  EBUFR version
C     BUFRF   --  BUFR/GRIB flag. Should be 0 for BUFR; 1 for GRIB
C     BUFRED  --  BUFR edition
C     MTVERS  --  BUFR master table *version* number
C Export Parameters: 
C     none
C     Prerequisites:  the unit on which output should occur must have
C        been previously set.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC0(EBUFRV, BUFRF, BUFRED, MTVERS)
      INTEGER EBUFRV, BUFRF, BUFRED, MTVERS
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
        CALL EBPTBY(0)

        CALL EBPTZB(6)
        CALL EBPTBY(1)
        CALL EBPTZB(8)
        CALL EBPTBY(1)

      CALL EBSTVP
        CALL EBPTBY(EBUFRV)
        CALL EBPTBY(BUFRF)
        CALL EBPTBY(BUFRED)
        CALL EBPTBY(MTVERS)
      CALL EBFLSH
      RETURN
      END
