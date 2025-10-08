C
C $Id: buffer.f,v 1.5 1994/10/11 20:39:42 john Exp $
C $Log: buffer.f,v $
C Revision 1.5  1994/10/11 20:39:42  john
C Fixed stuff for EBUFR continuation records.
C
c Revision 1.4  1994/03/18  20:28:54  john
c *** empty log message ***
c
c Revision 1.3  1992/08/17  16:31:00  murdock
c *** empty log message ***
c
c Revision 1.2  1992/08/17  16:25:24  murdock
c
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C re: Log above - murdock was just playing around, learning cvs
C     (no changes were made)
C
C$$$ Modifcations:  11 Dec 1991 David Casperson
C$$$   made a fix to ebptin to correct a problem that occurred
C$$$   when the buffer pointer was not on a byte boundary, and
C$$$   the field being written didn't fill the next field.
C$$$EBUFR-store-unit(UNIT)  -- store the unit on which this record will be
C$$$    written.  Must be called before the first call to
C$$$    EBUFR-flush-buffer, and before each change of output file.
C$$$
C$$$EBUFR-clear-buffer -- prepare to create a new EBUFR record.  Does
C$$$    various sanity checks to verify calling program logic.
C$$$    Should be called before any of the other EBUFR-buffer
C$$$    routines.
C$$$
C$$$EBUFR-flush-buffer -- does various sanity checks to verify
C$$$    calling program logic.  Writes current EBUFR-buffer contents
C$$$    as one EBUFR record on appropriate output unit.  (Set by
C$$$    EBUFR-clear-buffer?)
C$$$
C$$$BUFR-start-section (k) -- does byte alignment to 16 bit boundary.
C$$$    Records where section k starts, and writes 4 0-bytes.  Must be
C$$$    followed by a BUFR-end-section(k).
C$$$begin
C$$$    if cur_section <> -1 or last_section >= k then
C$$$        ERROR
C$$$    else
C$$$        cur_section =  k
C$$$    endif
C$$$
C$$$    align(16)
C$$$    section_start_loc = nbytes
C$$$    put-0-bytes(4)
C$$$end
C$$$
C$$$BUFR-end-section-(k) -- does byte alignment to 16 bit boundary.
C$$$    Writes length of section k in appropriate location.  Must be
C$$$    preceded by BUFR-start-section(k).
C$$$begin
C$$$    if cur_section <> k then
C$$$        ERROR
C$$$    else
C$$$        cur_section =  -1
C$$$        last_section = k
C$$$    endif
C$$$
C$$$    align(16)
C$$$    len = nbytes - section_start_loc
C$$$    B[section_start_loc+1] = len/256^2
C$$$    B[section_start_loc+2] = mod(len/256, 256)
C$$$    B[section_start_loc+3] = mod(len, 256)
C$$$end
C$$$
C$$$start-variable-part -- fills the remainder of the fixed part with 0's.
C$$$begin
C$$$    if    cur_section <> -1
C$$$       or last_section <> -1
C$$$       or nbytes*8+nbits>22*8 then
C$$$        ERROR
C$$$    endif
C$$$
C$$$    nbytes = 22
C$$$    nbits  = 0
C$$$end
C$$$
C$$$put-0-bytes (N) -- writes N 0 bytes to EBUFR-buffer.  Buffer must
C$$$    be byte aligned before this routine is called.
C$$$begin
C$$$    check-space(8*N)
C$$$    nbytes = nbytes + N
C$$$end
C$$$
C$$$put-byte -- writes BYTEVAL to EBUFR-buffer.  Buffer must be byte
C$$$    aligned before this routine is called.
C$$$begin
C$$$    check-space(8)
C$$$    nbytes = nbytes + 1
C$$$    B[nbytes] = BYTEVAL
C$$$end
C$$$
C$$$ ???????????????????????????????????????????????????
C$$$ ? Should I be worrying about non-ascii underlying ?
C$$$ ? character sets here?                            ?
C$$$ ???????????????????????????????????????????????????
C$$$put-chars(CVAL, NCHARS) -- writes NCHARS characters from CVAL to
C$$$    EBUFR-buffer.  Buffer does NOT need to be byte aligned for
C$$$    this to work.
C$$$begin
C$$$    check-space(8*NCHARS)
C$$$
C$$$    if nbits = 0 then
C$$$        B[nbytes + 1 : nbytes + NCHARS +1] = CVAL[1 : NCHARS]
C$$$        nbytes = nbytes + NCHARS
C$$$    else
C$$$        for i from 1 to NCHARS  do
C$$$            put-int(CVAL[i:i], 8)
C$$$        endfor
C$$$    endif
C$$$end
C$$$
C$$$put-chars-padded -- (CVAL, NSRC, NDEST) writes min(NSRC,NDEST)
C$$$    characters from CVAL to EBUFR-buffer.  pads with min(NDEST -
C$$$    NSRC,0) space characters.  Buffer does NOT need to be byte
C$$$    aligned for this to work.
C$$$begin
C$$$    (* check-space(8*NDEST) if you want to be sure to barf *
C$$$     * before writing characters on overflow.*)
C$$$    padding = '                                '
C$$$    nchars = min(NSRC, NDEST)
C$$$    put-chars(CVAL[1:nchars], nchars)
C$$$    NDEST = NDEST - nchars
C$$$    while NDEST > 0 do
C$$$        nchars = min( len(padding), NDEST )
C$$$        put-chars(padding(1:nchars), nchars)
C$$$        NDEST = NDEST - nchars
C$$$    endwhile
C$$$end
C$$$
C$$$put-0-bits  -- writes N 0 bits to EBUFR-buffer.
C$$$begin
C$$$    check-space(N)
C$$$    M = nbits + N + 8 * nbytes
C$$$    nbytes = M / 8
C$$$    nbits  = M - 8 * nbytes
C$$$end
C$$$
C$$$put-1-bits  -- writes N 1 bits to EBUFR-buffer.
C$$$begin
C$$$    check-space(N)
C$$$
C$$$    Q = B[nbytes+1] + rsh(x'FF', nbits)  (* Q is always the value *
C$$$                                          * of the next byte to   *
C$$$                                          * store                 *)
C$$$    while N  > 0 do
C$$$        k = 8 - nbits             (* k is the numbers to store in *
C$$$                                   * the next byte                *)
C$$$        if N >= k then
C$$$            nbytes = nbytes + 1
C$$$            B[nbytes] = Q
C$$$            N = N - k
C$$$            nbits = 0
C$$$            Q = x'FF'
C$$$        else
C$$$            Q = lsh(rsh(Q,k-N), k-N)      (*Q is an N bit value shifted  *
C$$$                                           * left k-N bits               *)
C$$$            B[nbytes+1] = Q
C$$$            nbits = nbits + N
C$$$            N = 0
C$$$        endif
C$$$    endwhile
C$$$end
C$$$align -- align to N bit boundary by writing 0 bits.
C$$$begin
C$$$    M = nbits + 8 * nbytes
C$$$    put-0-bits( mod(M, N) )
C$$$end
C$$$
C$$$put-bit  -- write BIT to EBUFR-buffer.
C$$$begin
C$$$    if BIT = 0 then
C$$$        put-0-bits(1)
C$$$    else if BIT = 1 then
C$$$        put-1-bits(1)
C$$$    else
C$$$        ERROR
C$$$    endif
C$$$end
C$$$
C$$$put-int -- arguments IVAL, NN.  Checks to see that IVAL is an
C$$$    integer in the range 0...2^NN-2, that NN is in the range
C$$$    1...31. Coded to be fast, but endian independent.
C$$$begin
C$$$    if IVAL < 0
C$$$        or IVAL > 0 and NN = 0
C$$$        or IVAL > 2 * ( 2 ^ (NN - 1 ))
C$$$            (* Be careful to avoid overflow *)
C$$$        or nbits + NN + 8 * nbytes > 8 * LEN(EBUFR-buffer)
C$$$    then ERROR
C$$$    endif
C$$$
C$$$    if IVAL = 0 then
C$$$        put-0-bits(NN)
C$$$        return
C$$$    endif
C$$$
C$$$    while NN > 0 do
C$$$        k = 8 - nbits
C$$$        m = NN - k
C$$$        if m >=0 then
C$$$            q = IVAL / (2^m)
C$$$            IVAL = IVAL - q * (2^m)
C$$$            nbytes = nbytes + 1
C$$$            B[nbytes] = B[nbytes] + q
C$$$            NN = NN - k
C$$$            nbits = 0
C$$$        else
C$$$            B[nbytes + 1] = B[nbytes + 1] + IVAL * (2^(-m))
C$$$            nbits = nbits + NN
C$$$            NN = 0
C$$$        endif
C$$$    endwhile
C$$$end
C$$$
C$$$put-int-ascii(IVAL, NN) --   Checks to see that IVAL is
C$$$    in the range 0...10^(NN)- 1.  Formats IVAL into NN ASCII
C$$$    string with leading zeroes.  Result is put in buffer with
C$$$    put-chars.
C$$$begin
C$$$    check_overflow(8 * NN)
C$$$    if IVAL >= 10^NN then
C$$$        ERROR
C$$$    endif
C$$$    STR = IVAL formatted to NN chars with leading zeros.
C$$$    put-chars(STR, NN)
C$$$end
      SUBROUTINE EBFSTU(UNIT)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      INTEGER UNIT

      EBUNIT = UNIT
      RETURN
      END

C =====================================================================
      SUBROUTINE SB(WHERE, WHAT)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      
      INTEGER WHERE, WHAT
      EBFREC(WHERE:WHERE) = CHAR(WHAT)
      RETURN
      END

C =====================================================================      
      INTEGER FUNCTION BB(WHERE)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      INTEGER WHERE
      BB = ICHAR(EBFREC(WHERE:WHERE))
      RETURN
      END
C =====================================================================      
      INTEGER FUNCTION RSH(IVAL, NN)
      INTEGER IVAL, NN

      EXTERNAL LSH
      INTEGER LSH

      RSH = LSH(IVAL,-NN)
      RETURN
      END
C =====================================================================      
      INTEGER FUNCTION LSH(IVAL, NN)
      INTEGER IVAL, NN
      INTEGER PWR2(31)
      DATA PWR2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
     $     16384, 32768, 65536, 131072, 262144, 524288, 1048576 ,
     $     2097152, 4194304, 8388608, 16777216, 33554432, 67108864,
     $     134217728, 268435456, 536870912, 1073741824/

      IF (NN .GE. 0) THEN
         LSH = IVAL * PWR2(1+NN)
      ELSE
         LSH = IVAL / PWR2(1-NN)
      ENDIF
      RETURN
      END
C =====================================================================
      SUBROUTINE EBLCHK(NBTS)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      
      INTEGER NBTS
      IF ((8*NBYTES+NBITS+NBTS) .GT. 8*BUFRS) THEN
         CALL ENCERR(3)
      ENDIF
      RETURN
      END

C =====================================================================      
      SUBROUTINE EBFCLR
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      
      INTEGER I

      DO 10 I = 1, BUFRS, 1
         CALL SB(I,0)
 10   CONTINUE

      NBYTES = 0
      NBITS  = 0
      CURSEC = -1
      PRVSEC = -1
      SCSTRT = 0
      
      RETURN
      END

C =====================================================================      
      SUBROUTINE EBFLSH
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      EXTERNAL RSH, LSH, BB
      INTEGER  RSH, LSH, BB
      
      INTEGER HDRLOC
      PARAMETER (HDRLOC=22)
      INTEGER I, KONT, MAXBUF, NUMREC, RSTRT, REND
C
C max len of *variable*section* of EBUFR Type 9 record is 9973 bytes!
C Note: HDRLOC=22 i.e. the length of the EBUFR record header stuff
C So:
C  1. MAXBUF-HDRLOC = total length of data
C  2. Ceiling( (MAXBUF-22) / 9973 ) = Number of Records required
C  3. We don't have a Ceiling function, so add 9972 to the numerator
C     and let integer division take care of the rest
C  4. Thus: Number of Records = (MAXBUF-22+9972)/9973
C
C This is kind of confusing; so much so that the original
C GER got it wrong! Original GER line:
C     NUMREC = (MAXBUF-HDRLOC+9998)/(9999-HDRLOC)
C But Max len of variable part is 9973, thus max record size
C is 9995 = 9973+22. This produces:
C     NUMREC = (MAXBUF-HDRLOC+9994)/(9995-HDRLOC)
C Which is:
C     NUMREC = (MAXBUF-HDRLOC+9994)/(9973)
C Ughh! This leaves a 21 byte gap where a record looks like it requires
C two EBUFR records when it really could fit into one.
C
C -jja
C
      CALL EBALGN(16)
      MAXBUF = NBYTES
      NUMREC = (MAXBUF-HDRLOC+9972)/9973
      IF (NUMREC .GE. 2) THEN
C first Continutation byte indicates that this is the first of N+1 records,
C  so KONT must be NUMREC-1 so that N+1 = KONT+1 = NUMREC
C -jja
         KONT = NUMREC - 1
      ELSE
         KONT = 0
      ENDIF
      DO 10 I = 1,NUMREC,1
C 9973 is the max len of variable section, so (on records 2+),
C we need to skip that many bytes in the EBFREC array
C -jja
         RSTRT = HDRLOC+1+(I-1)*(9973)
         REND = MIN(MAXBUF, RSTRT+(9973)-1)
C        Point to byte 18!
         NBYTES = 17
         CALL EBPTBY(KONT)
         CALL EBPTIA(REND-RSTRT+1,4)
         CALL WRTEBB(EBUNIT, (REND-RSTRT+1)+HDRLOC,
     $               EBFREC(1:HDRLOC)//EBFREC(RSTRT:REND))
         IF (KONT .LT. 128) THEN
            KONT = 128
         ELSE 
            KONT = KONT + 1
         ENDIF 
 10   CONTINUE 
      RETURN
      END

C =====================================================================      
      SUBROUTINE STRTSC(K)
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER K

      IF ((CURSEC .NE. -1) .OR. (PRVSEC .GE. K)) THEN
         CALL ENCERR(1)
      ENDIF

      CURSEC = K
      CALL EBALGN(BALIGN)
      SCSTRT = NBYTES
      CALL EBPTZB(3)
      RETURN
      END

C =====================================================================      
      SUBROUTINE ENDSC(K)

      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      INTEGER K

      INTEGER LEN, NNBYTE, NNBITS
      
      IF (CURSEC .NE. K) THEN
         CALL ENCERR(2)
      ELSE
         CURSEC = -1
         PRVSEC = K
      ENDIF

      CALL EBALGN(BALIGN)
      LEN = NBYTES - SCSTRT

      NNBYTE = NBYTES
      NBYTES = SCSTRT
      NNBITS = NBITS
      NBITS = 0

      CALL EBPTIN(LEN,24)
      NBYTES = NNBYTE
      NBITS = NNBITS
      
      RETURN
      END
         
C =====================================================================      
C                              start-variable-part
      SUBROUTINE EBSTVP()        

      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      IF ((CURSEC .NE. -1) .OR.
     $     (PRVSEC .NE. -1) .OR.
     $     (NBYTES*8+NBITS .GT. 22*8)) THEN
         CALL ENCERR(4)
      ENDIF

      NBYTES = 22
      NBITS = 0
      RETURN
      END

C =====================================================================      
C                             put-0-bytes      
      SUBROUTINE EBPTZB(N)      
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER N

      IF (NBITS .NE. 0) CALL ENCERR(5)
      CALL EBLCHK(8*N)

      NBYTES = NBYTES + N
      RETURN
      END

C =====================================================================      
C                                ! put-byte
      SUBROUTINE EBPTBY(N)      
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER N

      IF (NBITS .NE. 0) CALL ENCERR(5)
      CALL EBLCHK(8)

      NBYTES = NBYTES + 1
      CALL SB(NBYTES, N)
      RETURN
      END

C =====================================================================      
C    ???????????????????????????????????????????????????
C    ? Should I be worrying about non-ascii underlying ?
C    ? character sets here?                            ?
C    ???????????????????????????????????????????????????
C                                     ! put-chars
      SUBROUTINE EBPCHR(CVAL, NCHRS) 
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER NCHRS
      CHARACTER CVAL*(*)

      INTEGER I
      INTEGER ICV

      ICV(I) = ICHAR(CVAL(I:I))

      CALL EBLCHK(8*NCHRS)

      IF (NBITS .EQ. 0) THEN
         DO 10 I = 1,NCHRS,1
            CALL SB(NBYTES+I,   ICV(I) )
 10      CONTINUE
         NBYTES = NBYTES + NCHRS
      ELSE
         DO 20 I = 1, NCHRS, 1
            CALL EBPTIN(ICV(I), 8)
 20      CONTINUE
      ENDIF
      RETURN
      END

C =====================================================================      
C                                           ! put-chars-padded
      SUBROUTINE EBPCHP(CVAL, NSRC, NDEST) 
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER NCHRS, NDEST, NSRC, NDEST1
      CHARACTER CVAL*(*)
      CHARACTER*32 PADDNG
      PARAMETER (PADDNG='                                ')

      NCHRS = MIN(NSRC, NDEST)
      CALL EBPCHR(CVAL(1:NCHRS), NCHRS)
      NDEST1 = NDEST - NCHRS
C     DO WHILE NDEST>0
 10   IF (NDEST1 .GT. 0) THEN
         NCHRS = MIN(NDEST1, LEN(PADDNG))
         CALL EBPCHR(PADDNG, NCHRS)
         NDEST1 = NDEST1 - NCHRS
C     END WHILE
         GO TO 10
      ENDIF
      RETURN
      END

C =====================================================================      
C                                ! put-0-bits
      SUBROUTINE EBPT0B(N)      
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER N
      INTEGER M
      
      CALL EBLCHK(N)
      M = NBYTES*8+NBITS+N
      NBYTES = M/8
      NBITS = M - 8 * NBYTES
      RETURN
      END
      
C     =====================================================================      
C                                ! put-1-bits
      SUBROUTINE EBPT1B(N)      
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER N

      EXTERNAL RSH, LSH, BB
      INTEGER  RSH, LSH, BB

      INTEGER N1, QQ, K

      CALL EBLCHK(N)
      N1 = N
      QQ = BB(NBYTES+1)+RSH(255,NBITS)

C   do while n1 > 0
 5    IF (N1 .GT. 0) THEN

         K = 8 - NBITS
         IF (N1 .GE. K) THEN
            NBYTES = NBYTES + 1
            NBITS = 0
            CALL SB(NBYTES, QQ)
            QQ = 255
            N1 = N1 - K
         ELSE
            CALL SB(NBYTES+1, LSH(RSH(QQ,K-N1),K-N1))
            NBITS = NBITS + N1
            N1 = 0
         ENDIF

C     endwhile
         GO TO 5
      ENDIF

      RETURN
      END
C =====================================================================
C A more mathematically standard version of mmod.!
      INTEGER FUNCTION MMOD(A,B)
      INTEGER A,B
      MMOD = MOD(A,B)
      IF (MMOD*B .LT. 0) MMOD = MMOD+B
      RETURN
      END
C =====================================================================      
C                                ! align
      SUBROUTINE EBALGN(N)      

      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'
      INTEGER N
      INTEGER M

      M = NBYTES*8+NBITS
      CALL EBPT0B(MMOD(-M, N))
      RETURN
      END
C =====================================================================      
C                                  ! put-int
      SUBROUTINE EBPTIN(IVAL, NN) 
      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER IVAL, NN
      EXTERNAL RSH, LSH, BB
      INTEGER  RSH, LSH, BB
      
      INTEGER NN1, IVAL1
      INTEGER Q,M,K,CBV

      INTEGER WRNCNT
      SAVE WRNCNT

      IF ((NN .LT. 0) .OR. (NN .GT. 32)) THEN
C                                  ^^^^
C Should really be 31 but 32 is used in writing EBUFR type 8 records.!
C
         CALL ENCERR(45)
      ENDIF 
      CALL EBLCHK(NN)
      IF ((IVAL.LT.0) .OR.
     $     ((NN .EQ. 0) .AND. (IVAL.GT.0)) .OR.
     $     (IVAL .GT. RSH(1073741823, MAX(0,30-NN))) ) THEN
         WRNCNT = WRNCNT + 1
         IF (WRNCNT .LE. 5) THEN
            PRINT 5001
            IF (WRNCNT .EQ. 5) THEN
               PRINT 5002
            ENDIF
         CALL EBPT1B(NN)
         ENDIF
 5001    FORMAT (1X, 'EBPTIN:  Warning integer did not fit in bit'
     $           ' field.  Replaced with all ones.')
 5002    FORMAT (1X, 'EBPTIN:  Future warnings suppressed.')
      ENDIF

      IF (IVAL .EQ. 0) THEN
         CALL EBPT0B(NN)
         RETURN
      ENDIF

      IVAL1 = IVAL
      NN1 = NN
      CBV = BB(NBYTES+1)

C     do while nn1 > 0
 10   IF (NN1 .GT. 0) THEN
C      Loop assertions:  After writing IVAL1 in NN1 bits the buffer 
C                        will be correct.  CBV is equal to BB(NBYTES+1).
         K = 8 - NBITS
         M = NN1 - K
         Q = RSH(IVAL1, M)
         IVAL1 = IVAL1 - LSH(Q, M)
         CBV = CBV + Q
         CALL SB(NBYTES+1,CBV)
         IF (M .GE. 0) THEN
            NBYTES = NBYTES + 1
            NBITS = 0
            CBV = 0
            NN1 = NN1 - K
         ELSE
            NBITS = NBITS + NN1
            NN1 = 0
         ENDIF 
C     end while
         GO TO 10
      ENDIF


      RETURN
      END

C =====================================================================      
C                                  !  put-int-ascii
      SUBROUTINE EBPTIA(IVAL, NN) 

      INCLUDE '../Commons/parameters.f'
      INCLUDE '../Commons/buffer.f'

      INTEGER IVAL, NN

      CHARACTER FMTSTR*12, CHRSTR*12

      WRITE (UNIT=FMTSTR,FMT=9997) NN,NN
      WRITE (UNIT=CHRSTR,FMT=FMTSTR) IVAL
      CALL EBPCHR(CHRSTR, NN)
      RETURN 




 9997 FORMAT ('(I',I2.2,'.',I2.2,')')
      END


















