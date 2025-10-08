C
C $Id: observation_table.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: observation_table.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C**********************************************************************
C
C Per observation information
C

C-    integer obstbl(3,mxnobs)
C             obstbl             !contains hetergeneous data as follows:
C             obstbl(1,___)      ! Observation type code.
C             obstbl(2,___)      ! Index to first obs parameter in ptab1
C             obstbl(3,___)      ! Index to last  obs parameter in ptab1
C-    integer obstop             ! Top of the observation table

      INTEGER OBSTBL(3,MXNOBS)
      INTEGER OBSTOP
      COMMON /OBSTBL/ OBSTBL, OBSTOP
      SAVE   /OBSTBL/
