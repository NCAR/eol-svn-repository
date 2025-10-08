C
C $Id: end-parameters.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: end-parameters.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: end-parameters
C  6 Character Name: ENPRMS
C           Purpose: To return the indices for the range of parameters
C                    for the current observation.
C Import Parameters: (none)
C Export Parameters:
C    STARTI  --  starting index for the range of parameters.      
C    STOPI   --  stopping index for the range of parameters.      
C     Prerequisites: start-parameters must be called before beginning to
C                    store parameters.
C Commons directly
C          accessed: PTAB1
      SUBROUTINE ENPRMS(STARTI, STOPI)
      INTEGER STARTI, STOPI
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      STOPI = PTOP
      STARTI = PNEXT
      RETURN
      END
