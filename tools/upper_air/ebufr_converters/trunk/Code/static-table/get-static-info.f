C
C $Id: get-static-info.f,v 1.2 1994/03/18 19:07:55 john Exp $
C $Log: get-static-info.f,v $
C Revision 1.2  1994/03/18 19:07:55  john
C Added data for Master Table Version.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C Routine Long Name: get-static-info
C  6 Character Name: GETSTC
C           Purpose: To read information from the static table.
C Import Parameters: (none)
C Export Parameters:
C    EBFVER  --  EBUFR version.
C    BUFEDN  --  BUFR edition.
C    BFMTNO  --  BUFR master table number.
C    ORGCEN  --  Originating centre.
C    SEQNO   --  Sequence number.
C    MTVERS  --  Master Table *Version* number.
C     Prerequisites:  This information must have been stored.
C Commons directly
C          accessed: STATIC

      SUBROUTINE GETSTC(EXFVER,BXFEDN,BXMTNO,OXGCEN,SXQNO,MXVERS)
      INTEGER EXFVER,BXFEDN,BXMTNO,OXGCEN,SXQNO,MXVERS
      INCLUDE '../../Commons/static_info.f'

       EXFVER = EBFVER
       BXFEDN = BUFEDN
       BXMTNO = BFMTNO
       OXGCEN = ORGCEN
       SXQNO  = SEQNO
       MXVERS = MTVERS

       RETURN
       END
