C
C $Id: get-formatting-info.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-formatting-info.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-formmatting-info
C  6 Character Name: GETFMT
C           Purpose: To find format information describing how to 
C                    retrieve a parameter from user data.
C Import Parameters:
C    INDEX   --  Index into the parameter tables.
C Export Parameters: 
C    HOW     --  Code value describing how this parameter is to be
C                retrieved.
C    FORMT   --  Text giving the format of this data item for the case
C                where HOW is from CDATA.
C    WIDTH   --  Total width of the format item in FORMT (Number of
C                characters to advance the pointer into CDATA).
C    TYPE    --  Code value describing the type of data to be read.
C                Must be the same as HOW except when HOW is from CDATA.
C     Prerequisites:  The information must have previously been stored.
C Commons directly
C          accessed: PTAB1, PTAB2
      SUBROUTINE GETFMT(INDEX, HOW, FORMT, WIDTH, TYPE)
      INTEGER INDEX, HOW, WIDTH, TYPE
      CHARACTER *(*) FORMT
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      CALL CHKPTI(INDEX)

      FXYIDX = PTABLE(1, INDEX)
      HOW = PTABLE(2, INDEX)
      WIDTH = PTABLE(3, INDEX)
      TYPE = PTABLE(4, INDEX)
      CDSTRT = PTABLE(5, INDEX)
      CDEND = PTABLE(6, INDEX)

      FORMT = PFORM(INDEX)
      RETURN
      END
