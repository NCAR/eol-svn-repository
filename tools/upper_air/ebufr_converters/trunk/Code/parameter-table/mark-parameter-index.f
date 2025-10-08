C
C $Id: mark-parameter-index.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: mark-parameter-index.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: mark-parameter-index
C  6 Character Name: MRKPAR
C           Purpose: To set a mark for a given parameter.
C Import Parameters: 
C    INDEX   --  Index into to the parameters table to mark.
C Export Parameters: (none)
C Commons directly
C          accessed: PTAB1
      SUBROUTINE MRKPAR(INDEX)
      INTEGER INDEX
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      CALL CHKPTI(INDEX)
      CALL MKFXY(PTABLE(1,INDEX))
      RETURN
      END
