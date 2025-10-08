C
C $Id: read-observation-list.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-observation-list.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-observation-list
C  6 Character Name: RDOBSL
C           Purpose: To read the list of observations for one file
C                    from the control file.
C Import Parameters:
C Export Parameters:
C    NUMOBS  --  Number of observations in the list.
C    OBSLST  --  List of observations for this file.
C     Prerequisites: The control file must have been opened for reading
C                    and positioned correctly.
C Commons directly
C          accessed: CTLFIL
      SUBROUTINE RDOBSL(NUMOBS, OBSLST)
      INCLUDE '../../Commons/ctl_file.f'

      INTEGER NUMOBS
      INTEGER OBSLST(*)

      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(37)
      ENDIF

      L1 = INDEX(LINE, '(')
      L2 = INDEX(LINE, ')')
      NUMOBS = (L2-L1+1)/4
      READ(UNIT=LINE(L1+1:L2-1), FMT='(2000(I2, 2X))')
     $     (OBSLST(I), I=1,NUMOBS)
      CALL RDCLIN
      RETURN
      END
      
