C***********************************************************************
C
C function: getdays()
C description: calculate the difference in days between the qcf date
C              and the starting date (day1).
C
C************************************************************************

      integer function getdays()

      implicit none

      include 'qcglobal.h'
      include 'qcfcom.h'
      include 'configcom.h'
      include 'debug.h'
      integer  yr,mo,day,bday,cday
      character*9 getbtime,bdate

      if(debug) then
         write(*,*) 'in getdays(): day1=',day1
      end if
	 getdays=0
C convert begin date to julian
      read(day1,1002) mo,day,yr
C     write(*,*) 'in getdays(): day1 mo,day,yr=',mo,day,yr
      bdate=getbtime(yr,mo,day,0,0)
      read(bdate,1003) bday
C     write(*,*) 'in getdays(): bday=',bday
C convert qcf date to julian
      read(qcf.yr,1005) yr
      read(qcf.mo,1005) mo
      read(qcf.day,1005) day
C     write(*,*) 'in getdays(): qcf mo,day,yr=',mo,day,yr
      bdate=getbtime(yr,mo,day,0,0)
      read(bdate,1003) cday
C     write(*,*) 'in getdays(): cday=',cday
      getdays=cday-bday+1
      return
1002  format(i2,'/',i2,'/',i2)
1003  format(2x,i3)
1005  format(i2)
      end
