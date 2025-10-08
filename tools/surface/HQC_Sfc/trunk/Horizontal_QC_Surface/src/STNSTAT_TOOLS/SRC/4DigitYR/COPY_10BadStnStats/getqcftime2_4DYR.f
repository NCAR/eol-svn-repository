C******************************************************************************
C
C subroutine: getqcftime2_4DYR
C description: Convert qcf time to a string of date and time
C
C              format of string is:  YYYYDDDHHMM
C
C                        where YYYY : year			(YYYY= 0-9999)
C                              DDD : julian day of year         (DDD= 1-366)
C                              HH : hour			(HH= 0-23)
C                              MM : minute			(MM= 0-59)
C returns:  the date/time in the above format
C************************************************************************

      character*11 function getqcftime2_4DYR()

      implicit none

      include 'qcfcom_4DYR.h'
      include 'debug.h'

      integer qy,qm,qd,qh,qmn
      character*11 getbtime_4DYR

C convert qcf date to day of year
      read(qcf.nomyr,1004) qy
      read(qcf.nommo,1002) qm
      read(qcf.nomday,1002) qd
      read(qcf.nomhr,1002) qh
      read(qcf.nommin,1002) qmn

      if (debug) write(0,*) 'getqcftime2_4DYR: qy, qm, qd, qh, qmn::',
     +            qy, qm, qd, qh, qmn
      if (debug) write(0,*) 'Call getbtime_4DYR()'

      getqcftime2_4DYR=getbtime_4DYR(qy,qm,qd,qh,qmn)

      return
1004  format(i4)
1002  format(i2)
      end
