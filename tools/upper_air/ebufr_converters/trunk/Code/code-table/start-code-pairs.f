C
C $Id: start-code-pairs.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: start-code-pairs.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: start-code-pairs
C  6 Character Name: STCDPR
C           Purpose: To remember where the code pairs for a particular
C                    parameter start.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: clear-code-tables must be called before the first
C                    call to this routine.
C Commons directly
C          accessed: CDTABL
      SUBROUTINE STCDPR()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/code_table.f'

      CPSTRT = CDTTOP + 1
      RETURN
      END
