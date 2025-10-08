C
C $Id: clear-observation-table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: clear-observation-table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: clear-observation-table
C  6 Character Name: CLROBS
C           Purpose: To reset the observation table to an initial no
C                    data state.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: OBSTBL
      SUBROUTINE CLROBS()
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/observation_table.f'

      OBSTOP = 0
      RETURN
      END
