C
C $Id: ptable.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: ptable.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C**********************************************************************
C
C  Parameter information other than from BUFR tables
C
C-    integer ptable(6,mxprms)
C             ptable             !contains hetergeneous data as follows:
C             ptable(1,___)      ! Index into fxy table
C             ptable(2,___)      ! Code for parameter input location
C             ptable(3,___)      ! Number of chars for data in CDATA
C             ptable(4,___)      ! Type of data
C             ptable(5,___)      ! Index of first code value in code tbl
C             ptable(6,___)      ! Index of last code value in code tbl
C------
C-    character*(msform) pform(mxprms) 
C-                               ! Format for data to be read from CDATA
C-    integer ptop               !  Total number of parameters so far
C-    integer pnext              !  For use by start/end parameters.
C-    common /ptab1/  ptable, ptop, pnext
C-    common /ptab2/ pform

      INTEGER PTABLE(6,MXPRMS)
      CHARACTER*(MSFORM) PFORM(MXPRMS) 
      INTEGER PTOP
      INTEGER PNEXT
      COMMON /PTAB1/  PTABLE, PTOP, PNEXT
      COMMON /PTAB2/ PFORM
      SAVE   /PTAB1/,
     $       /PTAB2/
