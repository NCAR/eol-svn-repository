C These are the MAPS grid information for the in memory array
      common /gridcom/ grid
      integer GRIDX,GRIDY,GRIDP,GRIDT,GRIDSIZE
	 parameter (GRIDX=81,		! No. of grid points in x direction
     +           GRIDY=62,		! No. of grid points in y direction
     +           GRIDP=7,		! No. of parameters in the grid
     +           GRIDT=2,		! No. of time increments in the grid
     +           GRIDSIZE=60)		! Spacing of grid points (km)
C
C     If following are used in future, then ensure correct values are used
C     for GRIDLAT and GRIDLON. For the NetCDF (GIST) data use:
C           GRIDLAT = 22.8756 and GRIDLON = -120.4911.
C
C     real GRIDLAT,GRIDLON
C    +           GRIDLAT=22.83730717,	! Lat of grid point (1,1) (deg N)
C    +           GRIDLON=-120.49050192,	! Long of grid point (1,1) (deg W)
      real grid(GRIDX,GRIDY,GRIDP,GRIDT) ! Analyses grid
