C
C $Id: store-static-info.f,v 1.2 1994/03/18 19:07:57 john Exp $
C $Log: store-static-info.f,v $
C Revision 1.2  1994/03/18 19:07:57  john
C Added data for Master Table Version.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C Routine Long Name: store-static-info
C  6 Character Name: STRSTC
C           Purpose: To load information into the static table.
C Import Parameters:
C    EBFVER  --  EBUFR version.
C    BUFEDN  --  BUFR edition.
C    BFMTNO  --  BUFR master table number.
C    ORGCEN  --  Originating centre.
C    SEQNO   --  Sequence number.
C    MTVERS  --  Master Table *Version* number.
C Export Parameters: (none)
C     Prerequisites: (none)
C Commons directly
C          accessed: STATIC

      SUBROUTINE STRSTC(EXFVER,BXFEDN,BXMTNO,OXGCEN,SXQNO,MXVERS)
      INTEGER EXFVER,BXFEDN,BXMTNO,OXGCEN,SXQNO,MXVERS
      INCLUDE '../../Commons/static_info.f'

       EBFVER = EXFVER
       BUFEDN = BXFEDN
       BFMTNO = BXMTNO
       ORGCEN = OXGCEN
       SEQNO  = SXQNO 
       MTVERS = MXVERS

       RETURN
       END
