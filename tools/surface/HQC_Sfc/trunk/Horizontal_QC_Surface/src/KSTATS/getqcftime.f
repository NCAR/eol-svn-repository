C******************************************************************************
C
C subroutine: getqcftime
C description: Convert qcf time to a string of date and time
C
C              format of string is:  YYDDDHHMM
C
C                        where YY : year			(YY= 0-99)
C                             DDD : julian day of year (DDD= 1-366)
C                              HH : hour			(HH= 0-23)
C                              MM : minute			(MM= 0-59)
C returns:  the date/time in the above format
C************************************************************************

      character*9 function getqcftime()

      implicit none

      include 'qcfcom.h'

      integer qy,qm,qd,qh,qmn
      character*9 getbtime

C convert qcf date to day of year
      read(qcf.yr,1002) qy
      read(qcf.mo,1002) qm
      read(qcf.day,1002) qd
      read(qcf.hr,1002) qh
      read(qcf.min,1002) qmn
      getqcftime=getbtime(qy,qm,qd,qh,qmn)
      return
1002  format(i2)
      end
