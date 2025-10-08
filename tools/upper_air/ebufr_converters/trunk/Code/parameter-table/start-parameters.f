C
C $Id: start-parameters.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: start-parameters.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: start-parameters
C  6 Character Name: STPRMS
C           Purpose: To remember where the parameters for a particular
C                    observation start.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: clear-parameter-tables must be called before the 
C                    first call to this routine.
C Commons directly
C          accessed: PTAB1
      SUBROUTINE STPRMS()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      PNEXT = PTOP + 1
      RETURN
      END
