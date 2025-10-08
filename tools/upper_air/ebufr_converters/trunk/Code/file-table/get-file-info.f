C
C $Id: get-file-info.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-file-info.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-file-info
C  6 Character Name: GETFIN
C           Purpose: To get information about an EBUFR output file 
C                    from the FINFO common.
C Import Parameters:
C    FILENO  --  Number of the file about which information is wanted.
C Export Parameters: 
C    TYPE    --  Message type as defined in BUFR Table A.
C    SBTYPE  --  Message sub-type as locally defined.
C    TEXT    --  Text to describe this file 
C                (to be put in EBUFR record 3).
C    TEXTLN  --  Length of text.
C    OBSLST  --  An array containing the codes of the observation types
C                to be contained in this file.
C    NUMOBS  --  The number of observation codes in OBSLST.
C     Prerequisites: Information about this file must have previously
C                    been stored.
C Commons directly
C          accessed: FINFO
      SUBROUTINE GETFIN(FILENO, TYPE, SBTYPE,
     $     TEXT, TEXTLN, OBSLST, NUMOBS)
      INTEGER FILENO, TYPE, SBTYPE, TEXTLN, NUMOBS
      CHARACTER*(*) TEXT
      INTEGER OBSLST(*)
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/file_info.f'

      CALL CHKFTI(FILENO)


      TYPE    = FINFO(1, FILENO)
      SBTYPE  = FINFO(2, FILENO)
      TEXTLN  = FINFO(3, FILENO)
      NUMOBS  = FINFO(4, FILENO)

      IF (NUMOBS .GE. MOBSPF) THEN
         CALL ENCERR(12)
      ENDIF
      
      DO 10 I = 1, NUMOBS, 1
         OBSLST(I) = FOBSL(I, FILENO)
 10   CONTINUE 

      TEXT(1:TEXTLN) = FTEXT(FILENO)(1:TEXTLN)
      RETURN
      END
