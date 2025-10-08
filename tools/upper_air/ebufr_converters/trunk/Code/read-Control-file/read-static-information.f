C
C $Id: read-static-information.f,v 1.3 1994/06/24 16:17:51 john Exp $
C $Log: read-static-information.f,v $
C Revision 1.3  1994/06/24 16:17:51  john
C Fixed a bug I introduced when fixing master table number stuff.
C
c Revision 1.2  1994/03/18  19:07:40  john
c Added a line in the control file for Master Table Version.
c
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C Routine Long Name: read-static-information
C  6 Character Name: RDSTAT
C           Purpose: To read static information from the control file
C                    and store it in the static common area.
C Import Parameters: (none)
C Export Parameters: (none)
C     Prerequisites: The control file must have been opened for reading
C                    and the first line read by RDCLIN
C Commons directly
C          accessed: CTLFIL
      SUBROUTINE RDSTAT
      INCLUDE '../../Commons/ctl_file.f'

C ====================================================================
C   Local variables
      INTEGER EBUFV, BUFRE, BUFMTN, ORGCEN, SEQNO
C ====================================================================
      IF (LINCLS .NE. STATCL) THEN
         CALL ENCERR(32)
      ENDIF

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(33)
      ENDIF
      READ (UNIT=LINE,FMT=9997,ERR=998) EBUFV

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(33)
      ENDIF
      READ (UNIT=LINE,FMT=9996,ERR=998) BUFRE

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(33)
      ENDIF
      READ (UNIT=LINE,FMT=9995,ERR=998) BUFMTN

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(33)
      ENDIF
      READ (UNIT=LINE,FMT=9995,ERR=998) BFMTVS

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
         CALL ENCERR(33)
      ENDIF
      READ (UNIT=LINE,FMT=9995,ERR=998) ORGCEN

      CALL RDCLIN
      IF (LINCLS .NE. OTHECL) THEN
C         CALL ENCERR(33)
C assume we have an old-style control file (w/o bfmtvs)
C  - so adjust things accordingly
          SEQNO = ORGCEN
          ORGCEN = BFMTVS
C Master Table Version number
C it's probably 2 --- check current WMO Manual on Codes
          BFMTVS = 2
      ELSE
          READ (UNIT=LINE,FMT=9994,ERR=998) SEQNO
          CALL RDCLIN
      ENDIF

      CALL STRSTC(EBUFV, BUFRE, BUFMTN, ORGCEN, SEQNO, BFMTVS)
      RETURN

 998  CALL ENCERR(34)
C     Never reached!
      RETURN

 9998 FORMAT (I6)
 9997 FORMAT (I6)
 9996 FORMAT (I6)
 9995 FORMAT (I6)
 9994 FORMAT (I6)
      END
