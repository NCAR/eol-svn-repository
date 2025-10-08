C******************************************************************************
C
C subroutine: getqcftime_4DYR
C description: Convert qcf time to a string of date and time
C
C              format of string is:  YYYYDDDHHMM
C
C                        where YYYY : year			(YYYY= 0-9999)
C                             DDD : julian day of year (DDD= 1-366)
C                              HH : hour			(HH= 0-23)
C                              MM : minute			(MM= 0-59)
C returns:  the date/time in the above format
C
C 9 Sept 2001 lec
C   Updated to handle 4 digit year.
C************************************************************************

      character*9 function getqcftime_4DYR()

      implicit none

      include 'qcfcom_4DYR.h'

      integer qy,qm,qd,qh,qmn
      character*11 getbtime_4DYR

C convert qcf date to day of year
      read(qcf.yr,1004) qy
      read(qcf.mo,1002) qm
      read(qcf.day,1002) qd
      read(qcf.hr,1002) qh
      read(qcf.min,1002) qmn
      getqcftime_4DYR=getbtime_4DYR(qy,qm,qd,qh,qmn)
      return
1004  format(i4)
1002  format(i2)
      end
