c------------------------------------------------------------
c
c domain parameters
c
c define model domain size
c
      integer ix,           ! number of model grid points in east/west direction
     +        jx            ! number of model grid points in north/south direction
c
c ********************************************************
c
c need to change ix and jx to match model domain size
c
      parameter (ix=70, jx=55)
c
c ********************************************************
c
c define SHEBA column output size
c
      integer ic,           ! number of model grid points in east/west direction
     +        jc            ! number of model grid points in north/south direction
      parameter (ic=3,jc=3) ! THESE VALUES SHOULD NOT BE CHANGED
c
c define pressure level information
c
      integer nprs
      parameter (nprs=12)
      real pres(nprs)
      data pres /300., 500., 600., 700., 800., 850.,
     &            875., 900., 925., 950., 975., 1000./
c
c define number of output periods per day
c
      integer nt            ! number of output periods per day (8 3h intervals per day)
      parameter (nt=8)      ! THIS VALUE SHOULD NOT BE CHANGED
c------------------------------------------------------------
