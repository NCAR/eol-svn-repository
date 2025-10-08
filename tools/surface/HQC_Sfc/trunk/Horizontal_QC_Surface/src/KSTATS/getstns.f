C************************************************************************
C
C function: getstns()
C description: load the qc station master file
C
C 25 Aug 94 lec
C   Updated length of station ID from 10 chars to 15 chars
C************************************************************************

      subroutine getstns()

      implicit none

      include 'qcglobal.h'
      include 'stncom.h'
      include 'debug.h'
      include 'ioerr.h'

      character*10 network
      character*15 station
      integer stncode,dx,dy,dt(NSTNPARMS)
      real    lat,lon,elev,val(NSTNPARMS)
      integer i,j

C open the qc_stns file
      open(unit_stns,name=qc_stns,status='old',iostat=ios,err=901)
      if(debug) then
         write(0,*) "Reading in qc_stns file:"
         write(0,*)
      end if

C loop through the records.
      do while (.true.)
          read(unit_stns,1001,iostat=ios,err=901,end=900) stncode,
     +         network,station,lat,lon,elev,dx,dy,
     +         (val(i),i=1,NSTNPARMS),(dt(i),i=1,NSTNPARMS)
          stns(stncode).network=network
          stns(stncode).station=station
          stns(stncode).lat=lat
          stns(stncode).lon=lon
          stns(stncode).elev=elev
          stns(stncode).x=dx
          stns(stncode).y=dy
          do i=1,NSTNPARMS
             stns(stncode).val(i)=val(i)
             stns(stncode).dt(i)=dt(i)
          end do
		nstns=nstns+1
      end do

900   if(debug) then
	    write(0,*) ' Display the stations?'
         accept 1004, YN
         if(yn .eq. 'N' .or. yn .eq. 'n') goto 999
         write(0,*)
         write(0,*) 'num stations=',nstns
         do i=1,nstns
            write(0,1002) i,stns(i).network,stns(i).station,
     +        stns(i).lat,stns(i).lon,stns(i).elev,
     +        stns(i).x,stns(i).y,
     +        (stns(i).val(j),j=1,NSTNPARMS),
     +        (stns(i).dt(j),j=1,NSTNPARMS)
         end do
	    write(0,*) ' Continue?'
         accept 1004, YN
         if(yn .eq. 'N' .or. yn .eq. 'n') stop
      end if
      goto 999
901   errstr=gerror(errstr)
      write(0,*) 'getstns(): I/O Error: ',ios,' ',errstr
999   close(unit_stns)
      return
1001  format(i3,a10,a15,f10.5,f11.5,f7.2,i5,i5,8(f7.2),8(i9))
1002  format(i3,2x,a10,2x,a15,2x,f10.5,2x,f11.5,2x,f7.2,2x,
     +       i5,2x,i5,8(2x,f7.2),8(2x,i9))
1004  format(a1)
      end
