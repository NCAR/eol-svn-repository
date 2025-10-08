C******************************************************************************
C
C stations record type
C
C 25 Aug 94 lec
C   Update station string from 10 to 15 chars.
C******************************************************************************

      integer*2  NSTNPARMS
      parameter (NSTNPARMS=8)		!num of parms values in stn record
	 structure /stnrec/
         character*10 network	!list of network/stations
         character*15 station	!list of network/stations
         real lat			!station latitude
         real lon			!station longitude
         real elev			!station elevation (km)
         integer x			!x distance (m) from nearest SW MAPS grid pt
         integer y			!y distance (m) from nearest SW MAPS grid pt
         real val(NSTNPARMS)		!last parameter values for this stn
         integer dt(NSTNPARMS)	!last parameter dates for this stn
      end structure
	 record /stnrec/ stns(MAXSTNS)
      integer  nstns		!num of stns() elements used
      integer stndx    ! x offset (m) from nearest SW grid point of qc point
      integer stndy    ! y offset (m) from nearest SW grid point of qc point
	 common /stncom/ nstns,stns,stndx,stndy
