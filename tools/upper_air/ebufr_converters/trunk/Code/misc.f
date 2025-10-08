C
C $Id: misc.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: misc.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: EBUFR-encode-error
C  6 Character Name: ENCERR
C           Purpose: To handle internal logic errors detected by 
C                    the EBUFR encoder.
C Import Parameters:
C    ERRNO   --  Number of the error observerved.
C Export Parameters: (none) . . .routine should not exit.

      SUBROUTINE ENCERR(ERRNO)
      INTEGER ERRNO

      INTEGER NERMGS, NPRFXS, MSGLEN, PRELEN
      INTEGER I
      PARAMETER (NERMGS=45,NPRFXS=5,MSGLEN=80, PRELEN=30)
      INTEGER PRENO(NERMGS)
      CHARACTER*(PRELEN) PREFIX(NPRFXS)
      CHARACTER*(MSGLEN) MESAGE(NERMGS)

C                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
      DATA PRENO/  3, 3, 4, 3, 3, 5, 5, 5, 5, 4, 4, 3, 5, 4, 4,
C
C                 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
     $             5, 4, 2, 2, 3, 3, 5, 5, 5, 3, 2, 3, 3, 4, 5,
C
C                 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45
     $             5, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 5, 5, 5, 5/

      DATA PREFIX/'Control file format error:  ',
     $            'Implementation restriction:  ',
     $            'Internal logic error:  ',
     $            'Storage exceeded:  ',
     $            ''/

      DATA (MESAGE(I),I=1,15)/
     $     'beginning of BUFR section out of order.',
     $     'attempted to end BUFR section not currently started.',
     $     'EBUFR buffer overflow.',
     $     'attempted to start variable section twice.',
     $     'EBPTZB called while not aligned on byte boundary.',
     $     'bit width too small for integer.',
     $     'Parameter code does not correspond to a known parameter.',
     $     'Duplicate use of observation code.',
     $     'Attempt to use out of range file index.',
     $     'attempt to create too many output files.',
     $     'attempt to store too many observations for one file.',
     $     'bad value contained in FINFO(4,?).',
     $     'Attempt to use out of range code table index.',
     $     'too many observation types.',
     $     'too many code pairs.'/

      DATA (MESAGE(I),I=16,30)/
     $     'Attempt to use out of range parameter table index.',
     $     'too many parameters.',
     $     'operators (F=2) not currently available.',
     $     'sequences (F=3) not currently available.',
     $     'F value in FXY table not in range 0---3.',
     $     'ENCPR1 called when F <> 1.',
     $     'X replication range extends beyond end of current observatio
     &n.',
     $     'HOW value (HOW=IDATAT) inconsistent with VTYPE value (VTYPE 
     &<> IDATAT).',
     $     'HOW value (HOW=RDATAT) inconsistent with VTYPE value (VTYPE 
     &<> RDATAT).',
     $     'HOW not one of IDATAT, RDATAT, or CDATAT.',
     $     'Character data bit width not a multiple of 8.',
     $     'VTYPE not one of IDATAT, RDATAT, or CDATAT.',
     $     'either F<>1 or Y<>0 in ENCPR.',
     $     'Attempt to create too many FXY triple.',
     $     'Attempt to index FXY table with invalid index.'/

      DATA (MESAGE(I),I=31,45)/
     $     'Bad parameter value for number of replications in a dynamic 
     &replication.',
     $     'STATIC not found where expected.',
     $     'Unexpect line in STATIC section.',
     $     'Format error in STATIC section.',
     $     'No files described.',
     $     'RD1FLD called while not looking at FILE line.',
     $     'Missing information from file section.',
     $     'Missing END FILE line.',
     $     'Missing END FORMAT line.',
     $     'Unexpected information after EOF.',
     $     'Missing or badly formatted file information.',
     $     'Program aborted because of I/O problem.',
     $     ' ',
     $     ' ',
     $     'Bad bit width specification detected in EBPTIN.'/

      PRINT 9998, ERRNO, PREFIX(PRENO(ERRNO)), MESAGE(ERRNO)
      CALL EBEND()
C   Never reached!
      RETURN

 9998 FORMAT (1X, 'Serious error ', I2, ' detected.'/
     $        A/A)
      END

C Routine Long Name: get-length
C  6 Character Name: GETLEN
C           Purpose: to determine the length of the non-blank portion
C                    of a string.
C Import Parameters:
C    STR     --  string to find length of.
C Export Parameters:
C    --  returns length of string without trailing blanks.


      INTEGER FUNCTION GETLEN(STR)
      CHARACTER*(*) STR

C ====================================================================
C   Local variables
      INTEGER I
C ====================================================================
      DO 10 I=LEN(STR),1,-1
         IF (STR(I:I) .NE. ' ') THEN
            GO TO 11
         ENDIF
 10   CONTINUE
C     exit loop
 11   CONTINUE

      GETLEN = I
      RETURN
      END
