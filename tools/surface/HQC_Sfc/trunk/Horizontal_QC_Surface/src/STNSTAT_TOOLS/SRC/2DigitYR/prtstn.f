C******************************************************************************
C
C program: prtstn
C author:  K. Scully
C date:    05/27/92
C description: Print the station master file
C
C usage:   prtstn2
C        
C******************************************************************************

      program prtstn2

      implicit none

      include 'qcglobal.h'
      include 'stncom.h'
      include 'ioerr.h'

      integer tm,time
      character*26 ctime
      character*10 network
      character*15 station
      integer stncode
      real    lat,lon,elev
      integer i

C************************** end variable definitions *********************

C     write(*,*)  'Running prtstn program...'

C open the qc_stns file
      open(unit_stns,name=qc_stns,status='old',iostat=ios,err=901)

      tm=time()
      write(*,1009) ctime(tm)
C loop through the records.
      do while (.true.)
          read(unit_stns,1001,iostat=ios,err=901,end=999) stncode,
     +         network,station,lat,lon,elev
          write(*,1010) stncode,network,station,lat,lon,elev
      end do
      goto 999
901   errstr=gerror(errstr)
      write(*,*) 'prtstn(): I/O Error: ',ios,' ',errstr
999   close(unit_stns)
      write(*,*) "In prtstns.f"
      call standard_arithmetic()
      stop
1001  format(i3,a10,a15,f10.5,f11.5,f7.2)
1009  format(/,'Network/Stations:',45x,a26,//,
     +         'Code   Network     Station',8x,'Lat',10x,'Lon',7x,
     +         'Elev',/)
1010  format(i4,2x,a10,2x,a15,2x,f10.5,2x,f11.5,2x,f7.2)
      end
