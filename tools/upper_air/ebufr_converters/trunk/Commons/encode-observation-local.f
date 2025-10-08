C
C $Id: encode-observation-local.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: encode-observation-local.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C$$$  This file defines the common ECOLOC which is local to the
C$$$  observation encoding routines.  It contains the indices into
C$$$  the user passed data.  The actual definition of the
C$$$  parameters etc. is stored in another common.
C$$$  
C$$$  This comment written 17 Sep 1991 by David Casperson.

      INTEGER IDTIDX
      INTEGER RDTIDX
      INTEGER CDTIDX
      COMMON /ECOLOC/ IDTIDX, RDTIDX, CDTIDX
      SAVE   /ECOLOC/
