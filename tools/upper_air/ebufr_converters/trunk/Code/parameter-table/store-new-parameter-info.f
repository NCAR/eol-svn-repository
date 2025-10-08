C
C $Id: store-new-parameter-info.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: store-new-parameter-info.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: store-new-parameter-info
C  6 Character Name: STNPAR
C           Purpose: To store information describing a previously
C                    undescribed parameter for future use.
C Import Parameters:
C    FXYIDX  --  Index of the FXY table associated with this parameter.
C    HOW     --  Code value describing how this parameter is to be
C                retrieved.
C    FORMT   --  Text giving the format of this data item for the case
C                where HOW is from CDATA.
C    WIDTH   --  Total width of the format item in FORMT (Number of
C                characters to advance the pointer into CDATA).
C    TYPE    --  Code value describing the type of data to be read.
C                Must be the same as HOW except when HOW is from CDATA.
C    CDSTRT  --  Index of the first code pair for this parameter.
C    CDEND   --  Index of the last  code pair for this parameter.
C Export Parameters: (none)
C     Prerequisites: clear-parameter-tables must be called before the
C                    first call to this routine.
C Commons directly
C          accessed: PTAB1, PTAB2
      SUBROUTINE STNPAR(FXYIDX, HOW, FORMT, WIDTH, TYPE, CDSTRT, CDEND)
      INTEGER FXYIDX, HOW, WIDTH, TYPE, CDSTRT, CDEND
      CHARACTER *(*) FORMT
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      PTOP = PTOP + 1
      IF (PTOP .GT. MXPRMS)  THEN
         CALL ENCERR(17)
      ENDIF

      PTABLE(1, PTOP) = FXYIDX
      PTABLE(2, PTOP) = HOW
      PTABLE(3, PTOP) = WIDTH
      PTABLE(4, PTOP) = TYPE
      PTABLE(5, PTOP) = CDSTRT
      PTABLE(6, PTOP) = CDEND

      PFORM(PTOP)    = FORMT
      RETURN
      END
