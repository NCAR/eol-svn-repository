C
C $Id: file_info.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: file_info.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C$$$  This file defines the FINFO1 and FINFO2 commons.  This
C$$$  information comes from the file section of the control file,
C$$$  and describes the EBUFR files to create.

C$$$  This comment written 17 Sep 1991 by David Casperson.

C-    integer finfo(4,mxfils),  ! Heterogeneous per file information as
C-                              ! follows
C             finfo(1,____)          Type code for this file
C             finfo(2,____)          Sub-type code for this file
C             finfo(3,____)          Text length for text in ftext
C             finfo(4,____)          Number of observations in this file
C-   $        fobsl(mobspf,mxfils), ! List of observation codes for
C-                                  ! this file fobsl(i,j) is the code
C-                                  ! for i-th observation of file j.
C-   $        filtop            ! Total number of files described so
C-                              ! far.
C-
C-    character*(mtxtln) ftext(mxfils) 
C-                               ! Descriptive text for record 3 of
C-                               ! this file.

      INTEGER FINFO(4,MXFILS),
     $        FOBSL(MOBSPF,MXFILS),
     $        FILTOP

      CHARACTER*(MTXTLN) FTEXT(MXFILS) 

      COMMON /FINFO1/ FINFO, FOBSL, FILTOP
      COMMON /FINFO2/ FTEXT
      SAVE   /FINFO1/,
     $       /FINFO2/
