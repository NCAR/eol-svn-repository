C
C $Id: encode.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: encode.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: encode
C  6 Character Name: ENCODE
C           Purpose: To return a code value for a user supplied integer
C                    value.
C Import Parameters:
C    INDEX1  --  First index into the array of code pairs.
C    INDEX2  --  Last  index into the array of code pairs.
C    IVAL    --  Unencoded value (to be supplied by the user).
C Export Parameters:
C    MATCH   --  Set true if an explicit match is found.
C    --  Returns encoded value (to be stored in EBUFR record). (If no
C        explicit match is found, it returns IVAL.)
C     Prerequisites: The code pairs must be stored before this routine
C                    is called
C Commons directly
C          accessed: CDTABL
      INTEGER FUNCTION ENCODE(INDEX1, INDEX2, IVAL, MATCH)
      INTEGER INDEX1, INDEX2, IVAL
      LOGICAL MATCH
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/code_table.f'
C ====================================================================
C   Local variables
      INTEGER I
C ====================================================================
      IF (INDEX2 .GE. INDEX1) THEN
C        if INDEX2 is less than INDEX1 then this parameter has no code
C        pairs, and either could conceivably not be a legitimate index
C        into the code table. 29 Aug 1991. David Casperson.
         CALL CHKCTI(INDEX1) 
         CALL CHKCTI(INDEX2)
      ENDIF 

      ENCODE = IVAL
      MATCH = .FALSE.
      DO 10 I = INDEX1,INDEX2,1
         IF (IVAL .EQ. CODTBL(1,I)) THEN
            ENCODE = CODTBL(2,I)
            MATCH = .TRUE.
            GO TO 11
         ENDIF
 10   CONTINUE
C     loop exit
 11   CONTINUE 
      
      RETURN
      END
