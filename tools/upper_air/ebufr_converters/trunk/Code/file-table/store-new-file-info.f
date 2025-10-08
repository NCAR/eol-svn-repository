C
C $Id: store-new-file-info.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: store-new-file-info.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: store-new-file-info
C  6 Character Name: STNFIN
C           Purpose: To store information describing a previously 
C                    undescribed EBUFR output file for future use.
C Import Parameters:
C    TYPE    --  Message type as defined in BUFR Table A.
C    SBTYPE  --  Message sub-type as locally defined.
C    TEXT    --  Text to describe this file 
C                (to be put in EBUFR record 3).
C    TEXTLN  --  Length of text.
C    OBSLST  --  An array containing the codes of the observation types
C                to be contained in this file.
C    NUMOBS  --  The number of observation codes in OBSLST.
C Export Parameters: (none)
C     Prerequisites: clear-file-tables must be called before the first
C                    call to this routine.
C Commons directly
C          accessed: FINFO
      SUBROUTINE STNFIN(TYPE, SBTYPE, TEXT, TEXTLN, OBSLST, NUMOBS)
      INTEGER TYPE, SBTYPE, TEXTLN, NUMOBS
      CHARACTER*(*) TEXT
      INTEGER OBSLST(NUMOBS)
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/file_info.f'

      FILTOP = FILTOP + 1
      IF (FILTOP .GT. MXFILS)  THEN
         CALL ENCERR(10)
      ENDIF

      FINFO(1, FILTOP) = TYPE
      FINFO(2, FILTOP) = SBTYPE 
      FINFO(3, FILTOP) = TEXTLN
      FINFO(4, FILTOP) = NUMOBS

      IF (NUMOBS .GE. MOBSPF) THEN
         CALL ENCERR(11)
      ENDIF 
      
      DO 10 I = 1, NUMOBS, 1
         FOBSL(I, FILTOP) = OBSLST(I)
 10   CONTINUE 

      FTEXT(FILTOP)    = TEXT(1:TEXTLN)
      RETURN
      END
