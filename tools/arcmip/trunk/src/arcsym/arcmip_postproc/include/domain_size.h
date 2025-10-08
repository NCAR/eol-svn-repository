c------------------------------------------------------------
c define atmospheric model domain size
      integer ix,jx,kx
      parameter (ix=70, jx=55, kx=23)
c define ice model domain size
      integer ixi,iyi,ncat
      parameter (ixi=ix+1, iyi=jx+1, ncat=10)
c define land surface model domain size
      integer ixl,jxl
      parameter(ixl=ix-1,jxl=jx-1)
      integer msl
      parameter (msl=6)
c define radiation domain size
      integer ixr,jxr,kxr
      parameter(ixr=ix-1,jxr=jx-1,kxr=kx)
c define SHEBA column size
      integer ic,jc
      parameter (ic=3,jc=3)
c define half sigma levels
      real sigmah(kx+1)
      data sigmah /.00, .05, .10, .15, .20, .25,
     +             .30, .35, .40, .45, .50, .55,
     +             .60, .65, .70, .75, .80, .85,
     +             .89, .93, .96, .98, .99, 1.0/
c define other misc. domain parameters
      real ptop, clon, clat,dx
      parameter (ptop=10.)   ! pressure at model top (mb)
      parameter (clon=-153.) ! domain center longitude (deg)
      parameter (clat=72.)   ! domain center latitude (deg)
      parameter (dx=50000.)  ! horizontal grid spacing (m)
      integer mt
      parameter (mt=8) ! maximum number of output times
                       ! per day that can be processed
c define pressure level information
      integer nprs
      parameter (nprs=12)
      real prs(nprs)
      data prs /300., 500., 600., 700., 800., 850.,
     &            875., 900., 925., 950., 975., 1000./
c------------------------------------------------------------
