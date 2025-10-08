C***********************************************************************
C
C function: getplatform()
C description: Use qcf.network to lookup platform code.  If the platform
C              does not exist in the platform array then add it.
C 
C returns    rc=0  for failure
C            rc>0  for success, where rc is the index in the platform 
C                  array of the qcf.network
C
C************************************************************************

      integer*2 function getplatform()

      implicit none

      include 'qcglobal.h'
      include 'platform.h'
      include 'qcfcom_4DYR.h'
      integer i

C look up the platform code
      if(np .eq. 0) then 
        getplatform=1
        platform(1) = qcf.network
        np=1
        goto 10
      end if
      getplatform=0
      do i=1,np
         if(qcf.network .eq. platform(i)) then
           getplatform=i
           goto 10
         end if
      end do
      np = np+1
      if(np .gt. MAXPLATFORM) then
          write(0,*) 'Overflow on # platform for ',qcf.network,
     +               '   Record skipped.'
          return
      end if
      platform(np) = qcf.network
      getplatform=np
10    continue

      return
      end
