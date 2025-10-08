C
C $Id: get-code-indices-from-parameter-table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: get-code-indices-from-parameter-table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: get-code-indices-from-parameter-table
C  6 Character Name: GETCDI
C           Purpose: To get the indices into the code table for this 
C                    parameter.
C Import Parameters:
C    PP      --  Index into the parameter table.
C Export Parameters:
C    CODBGN  --  Index of first code table pair.
C    CODEND  --  Index of last  code table pair.
C     Prerequisites:  The data must have been stored first.
C Commons directly
C          accessed: PTAB1
      SUBROUTINE GETCDI(PP, CODBGN, CODEND)
      INTEGER PP, CODBGN, CODEND
      INCLUDE '../../Commons/parameters.f'
      INCLUDE '../../Commons/ptable.f'

      CALL CHKPTI(PP)
      CODBGN = PTABLE(5, PP)
      CODEND = PTABLE(6, PP)
      RETURN
      END
