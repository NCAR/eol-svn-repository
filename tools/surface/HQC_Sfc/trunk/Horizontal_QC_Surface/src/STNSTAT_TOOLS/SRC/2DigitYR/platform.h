C platform variables
      integer*2  np					!num of platform elements used
      character*10 platform(MAXPLATFORM)	!list of platforms

C platform common block
      common /pltfrmcom/ np,platform
