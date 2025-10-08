C
C $Id: ctl_file_access.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: ctl_file_access.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-control-file-line
C  6 Character Name: RDCLIN
C           Purpose: To read and classify one line from the control 
C                    file.
C Import Parameters: (from the common CTLFIL)
C    CTLUNI  --  unit from which to read.
C Export Parameters:
C    LINCLS (in common CTLFIL)
C            --  type of line read.
C    LINE   (in common CTLFL1)
C            --  actual line read (after skipping comments.)
C     Prerequisites:  CTLUNI must contain the unit number of a unit 
C                     opened for reading the control file.
C Commons directly
C          accessed: CTLFIL, CTLFL1


      SUBROUTINE RDCLIN
      INCLUDE '../../Commons/ctl_file.f'

 10   CONTINUE
      READ(UNIT=CTLUNI, FMT='(A80)', END=999) LINE

      IF (LINE .EQ. ' ' .OR. LINE(1:1) .EQ. COMMNT) THEN
         LINCLS=COMCL

      ELSEIF (INDEX(LINE,STATST) .GT. 0) THEN
         LINCLS=STATCL

      ELSEIF (INDEX(LINE,EFILST) .GT. 0) THEN
         LINCLS=EFILCL

      ELSEIF (INDEX(LINE,FILEST) .GT. 0) THEN
         LINCLS=FILECL

      ELSEIF (INDEX(LINE,EOBSST) .GT. 0) THEN
         LINCLS=EOBSCL

      ELSEIF (INDEX(LINE,OBSST) .GT. 0) THEN
         LINCLS=OBSCL

      ELSEIF (INDEX(LINE,CODEST) .GT. 0) THEN
         LINCLS=CODECL

      ELSE
         LINCLS = OTHECL
      ENDIF

      IF (LINCLS .EQ. COMCL) THEN
         GO TO 10
      ENDIF 
      RETURN

 999  LINCLS = EOFCL
      LINE = ' '
      RETURN

      END
