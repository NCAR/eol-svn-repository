C
C $Id: buffer.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: buffer.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C$$$ This file defines the common which represents the EBUFR-buffer
C$$$ where each record of an EBUFR file is built.  To maintain some
C$$$ sort of data abstraction, the information in this common is
C$$$ accessed through the EBUFR-buffer-routines, and no other.
C$$$ 
C$$$ ANSI Fortran 77 does not support a LOGICAL*1 type, nor (needless
C$$$ to say) any form of direct bit access.  Since an EBUFR record is
C$$$ essentially a bit string this presents a problem: how to represent
C$$$ an EBUFR record in a (Fortran 77) implementation dependent way.
C$$$ 
C$$$ I don't know of an entirely satisfactory solution to this problem.
C$$$ The one adopted here is to define the EBUFR-buffer as a character
C$$$ array.  This assumes that a CHAR variable takes a full byte of
C$$$ storage and can represent all 2^8 values.  If this isn't true of
C$$$ your version of Fortran 77 you will need to use a different
C$$$ version of EBUFR-buffer routines.
C$$$ 
C$$$ Fortran 77 doesn't allow CHARACTER and other data to be stored in
C$$$ the same COMMON, so two common names are used below.

C+    integer cursec            ! Current buffer section (def. -1)
C+    integer scstrt            ! Number of bytes before start of
C+                              ! current section.
C+    integer prvsec            ! Previous buffer section (def. -1)
C   The following two values act as the "cursor" into the current
C   record.  The total number of bits so far written to the
C   current record is given by 8*nbytes+nbits. 0 <= nbits < 8.
C+    integer nbits
C+    integer nbytes
C+    integer ebunit            ! unit on which to write the record
C+    character ebfrec*(bufrs)  ! The record itself
C@--This isn't ansii
C@      double precision grx5wq   ! This is a hack to force as much
C@                                ! alignment as possible.  Not at all
C@                                ! necessary, but might conceivably make
C@                                ! buffer routines (especially
C@                                ! local modifications) work faster.
C@      equivalence (ebfrec,grx5wq)

C
C  -- ANSIsized the comments 17 Sep 1991, David Casperson.
C

      INTEGER CURSEC
      INTEGER SCSTRT
      INTEGER PRVSEC
      INTEGER NBITS
      INTEGER NBYTES
      INTEGER EBUNIT
      CHARACTER EBFREC*(BUFRS)
      COMMON /EBUFRB/ CURSEC, SCSTRT, PRVSEC, NBITS, NBYTES, EBUNIT
      COMMON /EBUFRC/ EBFREC

      SAVE /EBUFRB/,/EBUFRC/
