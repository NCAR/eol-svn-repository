C
C $Id: initialize-control-data.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: initialize-control-data.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: initialize-control-data
C  6 Character Name: INITCD
C           Purpose: To set internal data areas to their initial, no 
C                    data state.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: (none)
      SUBROUTINE INITCD()

      CALL CLRFIL
      CALL CLRFXY
      CALL CLROBS
      CALL CLRPAR

C     ...and prime the reading pump
      CALL RDCLIN
      RETURN 

      END
