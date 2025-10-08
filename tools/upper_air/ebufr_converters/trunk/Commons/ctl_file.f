C
C $Id: ctl_file.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: ctl_file.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C$$$  This file defines the common information used by the
C$$$  routines that read the control file.  This information is
C$$$  strictly local to the control file reading routines, and
C$$$  does not contain the actual information read by the control
C$$$  routines.
C$$$  
C$$$  The above comment written 17 Sep 1991 by David Casperson.
C$$$  Comments ANSIsized at the same time.

      INTEGER COMCL, STATCL, FILECL, EFILCL, OBSCL, EOBSCL,
     $        CODECL, OTHECL, EOFCL

      PARAMETER (COMCL=0, STATCL=1, FILECL=2, EFILCL=3, OBSCL=4,
     $           EOBSCL=5, CODECL=6, OTHECL=7, EOFCL=8)

      CHARACTER STATST*6, FILEST*4, EFILST*8, OBSST*11, EOBSST*15,
     $          CODEST*6, COMMNT*1

      PARAMETER (COMMNT='!', STATST='STATIC',FILEST='FILE',
     $           EFILST='END FILE', OBSST='OBSERVATION',
     $           EOBSST='END OBSERVATION', CODEST='CODE: ')

C-    integer ctluni            ! unit from which to read the control
C-                              ! file.
C-
C-    integer lincls            ! class of the last line read.
C-    character*(80) line       ! last line read

      INTEGER CTLUNI, LINCLS
      CHARACTER*(80) LINE

      COMMON /CTLFIL/ CTLUNI, LINCLS
      COMMON /CTLFL1/ LINE
