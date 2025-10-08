C******************************************************************************
C
C program: stnbuddy
C author:  K. Scully
C date:    06/19/92
C description: Print the station master file and list its closest 5 stations
C
C usage:   stnbuddy
C
C 25 Aug 94 lec
C   Updated s/w to handle 15 char station ID instead of 10 chars.        
C******************************************************************************

      program stnbuddy

      implicit none

      include 'qcglobal.h'
      include 'stncom.h'
      include 'debug.h'
      include 'ioerr.h'


      integer*2  NBUDDYS
      parameter (NBUDDYS=5)		!num of buddy stations to store
      structure /buddyrec/
         integer stn	!station index code of the buddy station
         real km		!distance in km of the buddy station
      end structure
      record /buddyrec/ buddy(MAXSTNS,NBUDDYS)
      real xkm			!x distance in km of the buddy station
      real ykm			!y distance in km of the buddy station
      real zkm			!z (elev) distance in km of the buddy station
      real km			!vector distance in km of the buddy station
      integer tm,time
      character*26 ctime
      integer i,j,k,m

C************************** end variable definitions *********************

C     write(*,*)  'Running stnbuddy program...'

C open the qc_stns file
      open(unit_stns,name=qc_stns,status='old',iostat=ios,err=901)
C open the output file
      open(20,name='qcfstns',status='new',iostat=ios,err=901)

      tm=time()
      write(*,1009) ctime(tm)
C get all the stations
      call getstns()
C loop through the stations and find the NBUDDYS closest stations.
      do i=1,nstns
         do j=1,nstns
            if(i .eq. j) goto 100
            if(stns(i).network .eq. 'ASOSH' .and. 
     +         stns(j).network .eq. 'ASOS5') goto 100
            if(stns(j).network .eq. 'ASOSH' .and. 
     +         stns(i).network .eq. 'ASOS5') goto 100
            call ll2xy(stns(j).lat,stns(j).lon,xkm,ykm,
     +                 stns(i).lat,stns(i).lon)
            km=sqrt(xkm**2+ykm**2)
            do k=1,NBUDDYS
C     write(*,*) 'i=',i,'; j=',j,'; k=',k,'; km=',km,
C    +           '; buddy(i,k)=',buddy(i,k).stn,';',buddy(i,k).km
               if(buddy(i,k).stn .eq. 0 .or. km .lt. buddy(i,k).km)
     +            then
                  do m=NBUDDYS,k+1,-1
                     buddy(i,m).stn=buddy(i,m-1).stn
                     buddy(i,m).km =buddy(i,m-1).km
                  end do		!m
		        buddy(i,k).stn=j
		   	   buddy(i,k).km =km
			   goto 100
               end if
            end do		 	!k
100      continue
         end do			!j
      end do				!i
      do i=1,nstns
         write(*,1010) i,stns(i).network,stns(i).station,
     +      stns(i).lat,stns(i).lon,stns(i).elev,stns(i).x,stns(i).y
         write(*,1011)
         do k=1,NBUDDYS
	       j=buddy(i,k).stn
	       km=buddy(i,k).km
            write(*,1012) km,j,stns(j).network,stns(j).station,
     +         stns(j).lat,stns(j).lon,stns(j).elev
         end do
         write(20,1013,iostat=ios,err=901)
     +      i,stns(i).network,stns(i).station,
     +      stns(i).lat,stns(i).lon,stns(i).elev,stns(i).x,stns(i).y,
     +      (buddy(i,k).stn,buddy(i,k).km,k=1,NBUDDYS)
      end do
      goto 999
901   errstr=gerror(errstr)
      write(*,*) 'stnbuddy(): I/O Error: ',ios,' ',errstr
999   close(unit_stns)
      close(20)
      write(*,*)' In stnbuddy()'
      call standard_arithmetic()
      stop
1009  format(/,'Network/Station codes:',70x,a26,//,
     +         'Code   Network     Station',8x,'Lat',10x,'Lon',7x,
     +         'Elev     x      y',/)
1010  format(i4,2x,a10,2x,a15,2x,f10.5,2x,f11.5,2x,f7.2,2x,i5,2x,i5)
1011  format(45x,'Buddies: ','Dist (km) Code   Network     Station',
     +    8x,'Lat',10x,'Lon',7x,'Elev')
1012  format(55x,f8.3,2x,i4,2x,a10,2x,a15,2x,f10.5,2x,f11.5,2x,f7.2)
1013  format(i4,a10,a15,f10.5,f11.5,f7.2,i5,i5,5(i4,f8.3))
      end
