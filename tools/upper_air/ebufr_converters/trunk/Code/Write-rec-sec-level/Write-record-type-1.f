C
C $Id: Write-record-type-1.f,v 1.2 1994/03/18 19:07:12 john Exp $
C $Log: Write-record-type-1.f,v $
C Revision 1.2  1994/03/18 19:07:12  john
C Fixed EBUFR type 0 and 1 writing.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C Routine Long Name: Write-record-type-1
C  6 Character Name: WRTRC1
C           Purpose: to create and write one type 1 EBUFR record
C Import Parameters:
C    BUFMT    -- BUFR master table (0 if standard BUFR tables used)
C    TYPE     -- Message type as specified in Table A of BUFR specs.
C    ORGCEN   -- Number of the originating centre.
C    SEQNO    -- Sequence number of the message.
C    SBTYPE   -- Message sub type as defined locally.
C    MTVERS   -- Version num of master table used (now 2 for BUFR)
C    TIME     -- Array of 6 integers as follows:
C       TIME(1) - year -   (only mod 100 value used by this routine)
C       TIME(2) - month
C       TIME(3) - day
C       TIME(4) - hour
C       TIME(5) - minute
C       TIME(6) - second    (not used by this routine)
C Export Parameters: none
C     Prerequisites: unit to write to must have been previously stored.
C Commons directly
C          accessed: none
      SUBROUTINE WRTRC1(BUFRMT, ORGCEN, SEQNO, TYPE, SBTYPE,
     .                  MTVERS, TIME)
      INTEGER BUFRMT, ORGCEN, SEQNO, TYPE, SBTYPE, MTVERS, TIME
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
        CALL EBPTBY(1)

        CALL EBPTZB(6)
        CALL EBPTBY(1)
        CALL EBPTZB(8)
        CALL EBPTBY(1)
      CALL EBSTVP
        CALL STRTSC(1)
          CALL EBPTBY(BUFRMT)
          CALL EBPTIN(ORGCEN, 16)
          CALL EBPTBY(SEQNO)
          CALL EBPTBY(0)
          CALL EBPTBY(TYPE)
          CALL EBPTBY(SBTYPE)
          CALL EBPTBY(MTVERS)
C next: version number of local tables used to augment the master table
          CALL EBPTBY(0)
          CALL WRT5BT(TIME)
        CALL ENDSC(1)
      CALL EBFLSH
      RETURN
      END
