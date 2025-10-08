C
C $Id: static_info.f,v 1.2 1994/03/18 20:29:25 john Exp $
C $Log: static_info.f,v $
C Revision 1.2  1994/03/18 20:29:25  john
C Added a variable for master table VERSION number.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C  This file defines the common STATIC which contains information
C  about the EBUFR files that is relatively likely not to change.  
C  This information is read from the STATIC section of the control file.

C-    integer ebfver,           ! EBUFR version number
C-   $     bufedn,              ! BUFR edition
C-   $     bfmtno,              ! BUFR master table number
C-   $     mtvers               ! BUFR master table Version number
C-   $     orgcen,              ! orginating centre number
C-   $     seqno                ! seqence number

      INTEGER EBFVER, BUFEDN, BFMTNO, ORGCEN, SEQNO, MTVERS
      COMMON /STATIC/ EBFVER, BUFEDN, BFMTNO, ORGCEN, SEQNO, MTVERS
      SAVE   /STATIC/
