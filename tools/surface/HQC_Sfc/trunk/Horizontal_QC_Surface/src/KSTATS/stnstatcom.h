C arrays for the statistics reports
      integer*2 NPRTPARMS
      integer*2 NUMDAYS
      parameter (NPRTPARMS=9)					!no. of parms to print
      parameter (NUMDAYS=45) !GIST/VORTEX (183) period

C Was: parameter (NUMDAYS=100)

      integer stats(NQCPARMS,NQCFLAGS)
      integer p(MAXPLATFORM,NPRTPARMS,NQCFLAGS)	!summary stats by platform
      integer d(NUMDAYS,NPRTPARMS,NQCFLAGS)	!summary stats by day (was 45 days)
      integer h(24,NPRTPARMS,NQCFLAGS)		!summary stats by hour
      integer s(1500,NPRTPARMS,NQCFLAGS)	!summary stats by station (was 1000)

C global common block
      common /global/ stats,d,h,p,s
