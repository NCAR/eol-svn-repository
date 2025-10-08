C
C $Id: end-code-pairs.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: end-code-pairs.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: end-code-pairs
C  6 Character Name: ENCDPR
C           Purpose: To return the indices for the range of code pairs
C                    for the current parameter.
C Import Parameters: (none)
C Export Parameters:
C    STARTI  --  starting index for the range of parameters.      
C    STOPI   --  stopping index for the range of parameters.      
C     Prerequisites: start-code-pairs must be called before beginning to
C                    store code pairs.
C Commons directly
C          accessed: CDTABL
      SUBROUTINE ENCDPR(STARTI, STOPI)
      INTEGER STARTI, STOPI
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/code_table.f'

      STOPI = CDTTOP
      STARTI = CPSTRT
      RETURN
      END
