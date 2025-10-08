C
C $Id: read-control-file.f,v 1.1 1992/08/17 14:52:32 john Exp $
C $Log: read-control-file.f,v $
C Revision 1.1  1992/08/17 14:52:32  john
C Initial
C
C

C Routine Long Name: read-control-file
C  6 Character Name: RDCONF
C           Purpose: To read all information from the control file into
C                    the appropriate common areas.
C Import Parameters: 
C    UNITNO  --  Number of the unit from which to read control file 
C                info.
C Export Parameters: (none)
C     Prerequisites: control file must be opened for reading and 
C                    rewound.
C Commons directly
C          accessed: CTLFIL


      SUBROUTINE RDCONF(UNITNO)
      INTEGER UNITNO
      INCLUDE '../../Commons/ctl_file.f'

      CTLUNI = UNITNO
      CALL INITCD
      CALL RDSTAT
      CALL RDFILD
      CALL RDOBSD
      RETURN
      END
      
