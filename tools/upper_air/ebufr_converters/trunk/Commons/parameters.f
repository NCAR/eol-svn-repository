C
C $Id: parameters.f,v 1.2 1994/10/11 20:39:54 john Exp $
C $Log: parameters.f,v $
C Revision 1.2  1994/10/11 20:39:54  john
C Fixed stuff for EBUFR continuation records.
C
c Revision 1.1  1992/08/17  14:52:32  john
c Initial
c
C

C  comments ANSIsized 17 Sep 1991 by David Casperson.

C-    parameter (mcdprs=1000,   ! Maximum code pairs
C-   .     mxfils=9,            ! Maximum number of EBUFR output files
C-   .     mtxtln=100,          ! Maximum text length for EBUFR type
C-   .                          ! 3 record
C-   .     mobspf=10,           ! Maximum num of obsvtn types per file
C-   .     maxfxy=1000,         ! Maximum num of distinct fxy triples
C-   .     mxnobs=30,           ! Maximum total number of obs types
C-   .     mxprms=1000)         ! Maximum total number of params

      INTEGER MCDPRS, MXFILS, MTXTLN, MOBSPF, MAXFXY, MXNOBS, MXPRMS
      PARAMETER (MCDPRS=1000,
     .     MXFILS=9,
     .     MTXTLN=100,
     .
     .     MOBSPF=10,
     .     MAXFXY=1000,
     .     MXNOBS=30,
     .     MXPRMS=1000)


C  EBUFR buffer related parameters:
C-    parameter (bufrs=50000)   ! Maximum size of an EBUFR record.
C-    parameter (strtva=22)     ! location of start of variable part.
C-    parameter (balign=16)     ! Alignment of BUFR sections

C-    Aug 19 ,1997 Modified by Darren R. Gallant UCAR/JOSS
C-    increased bufrs to 200000 to allow for large input files

C-    Oct 14 ,1999 Modified by Darren R. Gallant UCAR/JOSS
C-    increased bufrs to 300000 to allow for larger input files
      INTEGER BUFRS, STRTVA, BALIGN
      PARAMETER (BUFRS=300000)
      PARAMETER (STRTVA=22)
      PARAMETER (BALIGN=16)

C-----------
C     The following are used by the encode-observation routines:
C     (Whose says ANSI fortran can't have enum statements?)      
      INTEGER IDATAT
      INTEGER RDATAT
      INTEGER CDATAT

      PARAMETER (IDATAT=1,RDATAT=2,CDATAT=3)
C-----------

C-    integer msform
C-    parameter (msform=20)     ! Maximum length of a format for reading
C-                              ! a parameter from CDATA.

C-    integer xxsize
C-    parameter (xxsize=64)     ! Maximum length of a description of a
C-                              ! class (in characters).
C-    integer nmsize
C-    parameter (nmsize=64)     ! Size of the name field for type 7 records
C-                              ! (in characters).
C-    integer unsize
C-    parameter (unsize=24)     ! Size of the units field for type 7 records
C-                              ! (in characters).
C-    integer cxsize
C-    parameter (cxsize=160)    ! Size of the descriptive text for type-8
C-                              ! records (in characters).

      INTEGER CXSIZE, MSFORM, NMSIZE, UNSIZE, XXSIZE
      PARAMETER (CXSIZE=160, MSFORM=20,
     $           NMSIZE=64, UNSIZE=24, XXSIZE=64)











