C
C $Id$
C
      structure /dqcf/
         character*4 year
         character*2 date(2)    !UTC date of observation
         character*2 qtime(2)   !UTC time of observation
         character*10 qnet      !network identifier (platform abbrev.)
         character*15 statn     !station identifier (was 10 chars)
         real ll(2)             !latitude,longitude of station
         integer occur          !station occurence
         real staelv            !station elevation in meters
         real mxtmp             !Maximum temperature in C at 2 meters
         integer mxflg1         !qc flag 1 for Maximum Temperature
         integer mxflg2         !qc flag 2 for Maximum Temperature
         real mntmp             !Minimum temperature in C at 2 meters
         integer mnflg1         !qc flag 1 for Minimum Temperature
         integer mnflg2         !qc flag 2 for Minimum Temperature
         real sndep             !Total Snow Depth in meters
         integer sdflg1         !qc flag 1 for Snow Depth
         integer sdflg2         !qc flag 2 for Snow Depth
         real precip            !total precipitation in mm
         integer prcflg1        !qc flag 1 for total precipitation
         integer prcflg2        !qc flag 2 for total precipitation
         real snfall            !Daily snow fall in meters
         integer sfflg1         !qc flag 1 for daily snow fall
         integer sfflg2         !qc flag 2 for daily snow fall
      end structure
