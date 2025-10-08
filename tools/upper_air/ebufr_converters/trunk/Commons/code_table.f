C
C $Id: code_table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: code_table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C**********************************************************************
C
C Code table information.  Information for encoding special values into
C ebufr code values.  (Has nothing to do with BUFR code tables.)
C
C (Comments ANSIsized 17 Sep 1991, David Casperson)

C------------------------------------------------------------------
C-    integer codtbl(2,mcdprs)
C             codtbl             !contains hetergeneous data as follows:
C             codtbl(1,___)      ! Value to encode.
C             codtbl(2,___)      ! Encoded value.
C
C------
C-    integer cdttop,            !  Total number of code pairs
C-   &        cpstrt             !  temp. used to create code table
C-                               !    pairs for parameter.
C------------------------------------------------------------------

      INTEGER CODTBL(2,MCDPRS)
      INTEGER CDTTOP, CPSTRT
      COMMON /CDTABL/ CODTBL, CDTTOP, CPSTRT
      SAVE   /CDTABL/
