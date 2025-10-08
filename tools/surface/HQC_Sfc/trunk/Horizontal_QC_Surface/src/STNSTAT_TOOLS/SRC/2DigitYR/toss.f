C******************************************************************************
C
C program: toss
C author:  K. Scully
C date:    05/13/92
C description: Print toss reports
C
C usage:   toss [-h | -5m  | -20m] [-b | -d | -bd] [-debug] -sXXXX
C        
C              where -debug     option will print variable info as processing
C                               is done.
C                    -h         print hourly report
C                    -5m        print 5 minute report
C                    -20m       print 5 minute report
C                    -b         use only "bad" qc flags
C                    -d         use only "doubtful" qc flags
C                    -bd        use both "bad" and "doubtful" qc flags
C                    -sXXXX     print report for station id = XXXX
C
C 25 Aug 94 lec
C   Update station ID size from 10 to 15 chars.
C 14 Sep 94 lec
C   Made mods so hrly report only printed when hrly data is checked.
C   Five minute data will only have five minute report. Did some cleanup.
C 16 Nov 94 lec
C   Update s/w to handle 20 min data.
C******************************************************************************

      program toss

      implicit none

      include 'qcglobal.h'
      include 'tosscom.h'
      include 'stncom.h'
      include 'debug.h'
      include 'ioerr.h'

C misc. variables
      integer iargc,narg
      integer*2 rct,rcv,gettoss,getvar
      character*6 arg1,arg2,arg3,arg4

C print flags
	 logical prtb		!flag=true to print "B" qc flag info
	 logical prtd		!flag=true to print "D" qc flag info
	 logical prth		!flag=true to print hourly report
	 logical prt5m		!flag=true to print 5 minute report
         logical prt20m         !flag=true to print 20 minute report
	 integer*2 stnid	!stn id if printing single station only

C************************** end variable definitions *********************

      prth   = .true.   ! Default is hourly data
      prt5m  = .false.  
      prt20m = .false.
      prtb   = .true.   ! Default is print values flagged as bad.
      prtd   = .true.   ! Default is print values flagged as dubious.
      debug  = .false.
      stnid  = 0        ! Default is to print all stations.

C get command line arguments
      narg = iargc()
      if(narg .eq.1) then
         call getarg(1,arg1)
	    call setparms(arg1,stnid,prth,prt5m,prt20m,prtb,prtd)
      else if(narg .eq.2) then
         call getarg(1,arg1)
         call getarg(2,arg2)
	    call setparms(arg1,stnid,prth,prt5m,prt20m,prtb,prtd)
	    call setparms(arg2,stnid,prth,prt5m,prt20m,prtb,prtd)
      else if(narg .eq.3) then
         call getarg(1,arg1)
         call getarg(2,arg2)
         call getarg(3,arg3)
	    call setparms(arg1,stnid,prth,prt5m,prt20m,prtb,prtd)
	    call setparms(arg2,stnid,prth,prt5m,prt20m,prtb,prtd)
	    call setparms(arg3,stnid,prth,prt5m,prt20m,prtb,prtd)
      else if(narg .eq.4) then
         call getarg(1,arg1)
         call getarg(2,arg2)
         call getarg(3,arg3)
         call getarg(4,arg4)
	    call setparms(arg1,stnid,prth,prt5m,prt20m,prtb,prtd)
	    call setparms(arg2,stnid,prth,prt5m,prt20m,prtb,prtd)
	    call setparms(arg3,stnid,prth,prt5m,prt20m,prtb,prtd)
	    call setparms(arg4,stnid,prth,prt5m,prt20m,prtb,prtd)
      else
         write(*,*) 'Invalid number of arguments'
      end if

      if(debug) then
         write(*,*)  'Running in DEBUG mode'
         if(prth) write(*,*) 'Printing hourly report'
         if(prt5m) write(*,*) 'Printing 5-minute report'
         if(prt20m) write(*,*) 'Printing 20-minute report'
         if(prtb) write(*,*) 'Printing report for bad flags'
         if(prtb) write(*,*) 'Printing report for doubtful flags'
         if(stnid.gt.0) write(*,*) 'Printing report for station: ',stnid
      end if
      nstns=0

C open the files
      open(unit_var,name='prttoss.var',status='old',
     +     iostat=ios,err=901)
      open(unit_toss,name='prttoss.toss',status='old',
     +     iostat=ios,err=901)
      open(unit_pres,name=qc_pres,
     +     form='print',status='new',iostat=ios,err=901)
      open(unit_temp,name=qc_temp,
     +     form='print',status='new',iostat=ios,err=901)
      open(unit_wind,name=qc_wind,
     +     form='print',status='new',iostat=ios,err=901)
      open(unit_prcp,name=qc_prcp,
     +     form='print',status='new',iostat=ios,err=901)

C get station from station master file
      call getstns()

C get variances limits from config file
      call getconfig()

C loop through the file of input records
      tossstn=0
      do while (rcv.eq.0 .and. rct.eq.0)
         rct=gettoss(stnid,prtb,prtd,prt5m,prt20m) !get next toss stn & fill in toss qc codes

         rcv=getvar(stnid,prt5m,prt20m)	!get variance records for current toss stn

         call prttoss(prt5m,prt20m,prtb,prtd)	!print the toss info for this stn,day
      end do

900   continue
      close(unit_toss)
      close(unit_var)
      close(unit_pres)
      close(unit_temp)
      close(unit_wind)
      close(unit_prcp)
      goto 999
901	 errstr=gerror(errstr)
      write(*,*) 'toss(): I/O Error: ',ios,' ',errstr
999   write(*,*) 'In Toss'
      call standard_arithmetic()
      stop
      end
C************************************************************************
C
C function: setparms(arg)
C description: set command line parameters
C
C************************************************************************

      subroutine setparms(arg,stnid,prth,prt5m,prt20m,prtb,prtd)

      implicit none

      include 'debug.h'

      character*6 arg
C print flags
	 logical prtb		!flag=true to print "B" qc flag info
	 logical prtd		!flag=true to print "D" qc flag info
	 logical prth		!flag=true to print hourly report
	 logical prt5m		!flag=true to print 5 minute report
	 logical prt20m		!flag=true to print 20 minute report
	 integer*2 stnid	!stn id if printing single station only

C      if(debug) then
         write(*,*) "Enter setparms(): arg=",arg
C      end if
      if(arg .eq. '-debug' .or. arg .eq. -DEBUG) then
         debug = .true.
         write(*,*) "setparms(): arg=",arg
      else if(arg .eq. '-b') then
         prtb = .true.
         prtd = .false.
      else if(arg .eq. '-d') then
         prtb = .false.
         prtd = .true.
      else if(arg .eq. '-bd' .or. arg .eq. '-db') then
         prtb = .true.
         prtd = .true.
      else if(arg .eq. '-h') then
         prth = .true.
         prt5m = .false.
         prt20m = .false.
      else if(arg .eq. '-5m') then
         prth = .false.

         write(*,*)'setparms: we have 5minute data!!!'

         prt5m = .true.
         prt20m = .false.
      else if(arg .eq. '-20m') then
         prth = .false.
         prt5m = .false.
         prt20m = .true.
         write (*,*) 'We have 20minute data'

      else if(arg(1:2) .eq. '-s') then
         read(arg(3:6),1000) stnid
         write (*,*) 'Process only infor for station:', stnid
      end if
      return
1000  format(i4)
      end
C************************************************************************
C
C function: gettoss()
C description: get the toss records
C
C************************************************************************

      integer function gettoss(stnid,prtb,prtd,prt5m,prt20m)

      implicit none

      include 'qcglobal.h'
      include 'tosscom.h'
      include 'configcom.h'
      include 'debug.h'
      include 'ioerr.h'

      logical prtb,prtd,prt5m,prt20m

      integer*2 stnid
      integer*2 stn
      integer*2 code

      character*1 qc

      integer bmo,bday,byr
      integer i
      integer ii,jj
      integer yr,day,hr,min,h,m

      data stn /0/
      data code /0/
      data qc /' '/
      data yr /0/
      data day /0/
      data hr /0/
      data min /0/

      if(debug) then
         write(*,*) 'gettoss():'
      end if

      read(day1,1000) bmo,bday,byr

C initialize the print arrays

	 do jj=1,8
	    do ii=1,24
	       vh(ii,jj).maps = 0.0
	       vh(ii,jj).val = 0.0
	       vh(ii,jj).code = 0
	       vh(ii,jj).qc = ' '
         end do
	    do ii=1,289       ! was 288
	       vm(ii,jj).maps = 0.0
	       vm(ii,jj).val = 0.0
	       vm(ii,jj).code = 0
	       vm(ii,jj).qc = ' '
         end do
      end do

      i = 0         ! init after using above
C
C get the first toss record
C 

      if(tossstn .eq. 0) then

10       read(unit_toss,1001,iostat=ios,err=901,end=900)
     +       stn,day,hr,min,code,qc

         if(stnid .gt. 0 .and. stnid .ne. stn) goto 10
         if(.not. prtb .and. qc .eq.'B') goto 10
         if(.not. prtd .and. qc .eq.'D') goto 10

      end if

C make the new toss record the current one
	 tossstn=stn
	 tossday=day
C
C     loop through the data records
C
      do while(.true.)
         if(debug) then
            write(*,1002) stn,byr,day,hr,min,code,qc
	       type *,' Continue?'
            accept 1004, yn
            if(yn .eq. 'N' .or. yn .eq. 'n') stop
         end if
C
C       if same station, update toss code in the variance array
C
	if(stn .eq. tossstn) then

           if(code.le.8) i=code
           if(code.eq.13) i=8

           if(code.gt.8 .and. mod(code,NQCPARMS).le.8) then
              i=mod(code,NQCPARMS)
           end if

           if(i.eq.0) i=17
           if(code.eq.35) i=5

           h=hr+1
	   m=hr*12+min/5+1

	   vm(m,i).code=code
	   vm(m,i).qc=qc

	   if(.not.prt5m) then
	      vh(h,i).code=code
	      vh(h,i).qc=qc
	   end if

	   if(prt5m .and. min.eq.55) then
	      vh(h,i).code=code
	      vh(h,i).qc=qc
	   end if

         else
	    goto 100			!this is the next toss station
	 end if

20       continue

         read(unit_toss,1001,iostat=ios,err=901,end=900) stn,
     +      day,hr,min,code,qc

         if(stnid .gt. 0 .and. stnid .ne. stn) goto 20
         if(.not. prtb .and. qc .eq.'B') goto 20
         if(.not. prtd .and. qc .eq.'D') goto 20

      end do

100   continue
      gettoss=0
      if(debug) then
         write(*,*) 'tossstn=',tossstn,'; tossday=',tossday
	 end if
      goto 999

900   gettoss=-2
      if(debug) then
         write(*,*) '*** end of file ***  for toss file'
         write(*,*) 'tossstn=',tossstn,'; tossday=',tossday
	 end if
      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'gettoss(): I/O Error: ',ios,' ',errstr
      write(*,*)'Last read:stn,day,hr,min,code,qc:', 
     + stn,day,hr,min,code,qc
      gettoss=ios

999   return
1000  format(i2,'/',i2,'/',i2)
1001  format(i4,i3,i2,i2,i2,a1)
1002  format('Station: ',i4,'   date/time: ',i2,'.',i3,' ',i2,':',i2,
     +      ' code:',i2,' flg: ',a1)
1004  format(a1)
      end
C***********************************************************************
C
C function: getvar()
C description: input the variance records
C
C************************************************************************

      integer*2 function getvar(stnid,prt5m,prt20m)

      implicit none

      include 'qcglobal.h'
      include 'tosscom.h'
      include 'configcom.h'
      include 'debug.h'
      include 'ioerr.h'

      integer*2 stnid
      logical prt5m
      logical prt20m
      integer i
      integer*2 stn
	 real maps(NANALPARMS),var(NANALPARMS)
      integer day,hr,min,h,m
      integer i
	 logical found
	 data stn /0/
	 data day /0/
	 data hr /0/
	 data min /0/
	 data maps /0.,0.,0.,0.,0.,0.,0./
	 data var  /0.,0.,0.,0.,0.,0.,0./

      if(debug) then
         write(*,*) 'getvar():'
      end if

C get the first variance record
	 if(stn .eq. 0) then
10       read(unit_var,1004,iostat=ios,err=901,end=900)
     +     stn,day,hr,min,(maps(i),var(i),i=1,NANALPARMS)
         if(stnid .gt. 0 .and. stnid .ne. stn) goto 10
      end if

C read the variance record
      found=.false.
      do while(.true.)
         if(stn .eq. tossstn) then
            found=.true.
		  h=hr+1
		  m=hr*12+min/5+1
            if(debug) then
		     write(*,*) 'add as: h=',h,'; m=',m
	       end if
            do i=1,NANALPARMS
		if(maps(i) .eq. -999.99) then
                  vm(m,i).maps = maps(i)
		else
                  vm(m,i).maps = maps(i)+varo(i)
	        end if

		if(var(i) .eq. -999.99 .or. maps(i) .eq. -999.99) then
                  vm(m,i).val = var(i)
		else
                  vm(m,i).val = var(i)+vm(m,i).maps

                  if(i.eq.7 .and. vm(m,i).val .lt. 0 .and.
     +               vm(m,i).val .ne. -999.99 )       !WIND Dir lec added 21 Nov 94
     +                 vm(m,i).val = vm(m,i).val + 360.0

	        end if

		if(.not.prt5m) then
                   vh(h,i).maps = vm(m,i).maps
                   vh(h,i).val = vm(m,i).val
		end if

		if(prt5m .and. min.eq.55) then
                   vh(h,i).maps = vm(m,i).maps
                   vh(h,i).val = vm(m,i).val
		end if

            end do

         else if(found) then		!this is the next station
            goto 100
         end if

20       read(unit_var,1004,iostat=ios,err=901,end=900)
     +     stn,day,hr,min,(maps(i),var(i),i=1,NANALPARMS)

         if(stnid .gt. 0 .and. stnid .ne. stn) goto 20

         if(debug) then
            write(*,1003) stn,day,hr,min
            write(*,1007) (i,maps(i),var(i),i=1,NANALPARMS)
	       type *,' Continue?'
            accept 1006, yn
            if(yn .eq. 'N' .or. yn .eq. 'n') stop
            if(yn .eq. 'q') debug = .false.
         end if

      end do
100   getvar=0
      goto 999
900   getvar=-2
      if(debug) then
         write(*,*) '*** end of file ***  for var file'
	 end if
      goto 999
901   errstr=gerror(errstr)
      write(*,*) 'getvar(): I/O Error: ',ios,' ',errstr
      getvar=ios
999   return
1002  format(a2,'/',a2,'/',a2)
1003  format('Station: ',i4,'   date/time: ',i3,' ',i2,':',i2,/,
     +      '       MAPS   QC VAL')
1007  format(7(i2,2x,f7.2,2x,f7.2,/))
1004  format(i4,i3,i2,i2,14(f7.2))
1005  format(i2)
1006  format(a1)
      end

C***********************************************************************
C
C function: prttoss()
C description: print the toss info saved in the print bucket arrays
C
C************************************************************************

      integer function prttoss(prt5m,prt20m,prtb,prtd)

      implicit none

      include 'qcglobal.h'
      include 'tosscom.h'
      include 'configcom.h'
      include 'stncom.h'
      include 'debug.h'
      include 'ioerr.h'

	 integer h,m,i,j
	 integer p(4,2)
	 real dif(3)
	 real chg(3)

C print flags
	 logical prtb		!flag=true to print "B" qc flag info
	 logical prtd		!flag=true to print "D" qc flag info
	 logical prt5m		!flag=true to print 5 minute report
	 logical prt20m		!flag=true to print 20 minute report
	 logical prtpres,prttemp,prtwind,prtprcp

      if(debug) then
         write(*,*) 'prttoss():'
      end if

      prtpres=.false.
      prttemp=.false.
      prtwind=.false.
      prtprcp=.false.

C initialize 5-min print start,stop array elements
	 do i=1,4
	   do j=1,2
		p(i,j) = 0
        end do
      end do

C see if station has any B or D flags and set first and last index 
C numbers for printing
	 do i=1,288
	    if(prtb) then
	      if(vm(i,1).qc .eq. 'B' .or. vm(i,2).qc .eq. 'B'
     +      .or. vm(i,3).qc .eq. 'B') then
              prtpres = .true.
              if(p(1,1) .eq. 0) p(1,1) = i
              p(1,2) = i
           end if
	      if(vm(i,4).qc .eq. 'B' .or. vm(i,5).qc .eq. 'B') then
              prttemp = .true.
              if(p(2,1) .eq. 0) p(2,1) = i
              p(2,2) = i
           end if
	      if(vm(i,6).qc .eq. 'B' .or. vm(i,7).qc .eq. 'B') then
              prtwind = .true.
              if(p(3,1) .eq. 0) p(3,1) = i
              p(3,2) = i
           end if
	      if(vm(i,8).qc .eq. 'B') then
              prtprcp = .true.
              if(p(4,1) .eq. 0) p(4,1) = i
              p(4,2) = i
           end if
         end if
	    if(prtd) then
	      if(vm(i,1).qc .eq. 'D' .or. vm(i,2).qc .eq. 'D'
     +      .or. vm(i,3).qc .eq. 'D') then
              prtpres = .true.
              if(p(1,1) .eq. 0) p(1,1) = i
              p(1,2) = i
           end if
	      if(vm(i,4).qc .eq. 'D' .or. vm(i,5).qc .eq. 'D') then
              prttemp = .true.
              if(p(2,1) .eq. 0) p(2,1) = i
              p(2,2) = i
           end if
	      if(vm(i,6).qc .eq. 'D' .or. vm(i,7).qc .eq. 'D') then
              prtwind = .true.
              if(p(3,1) .eq. 0) p(3,1) = i
              p(3,2) = i
           end if
	      if(vm(i,8).qc .eq. 'D') then
              prtprcp = .true.
              if(p(4,1) .eq. 0) p(4,1) = i
              p(4,2) = i
           end if
         end if
      end do

C adjust 5-min start/stop print elements to print 6 before and after
	 do i=1,4
         p(i,1) = p(i,1)-6
         p(i,2) = p(i,2)+6
         if(p(i,1) .lt. 1) p(i,1)=1
         if(p(i,2) .gt. 288) p(i,2)=288
      end do

C     tm=time()
C     ctm=ctime(tm)
C
C print pressure info

      if(prtpres) then
         write(unit_pres,1021) tossstn,stns(tossstn).network,
     +      stns(tossstn).station,stns(tossstn).lat,stns(tossstn).lon,
     +      stns(tossstn).elev,stns(tossstn).x,stns(tossstn).y
	    write(unit_pres,1041)

       if (.not.prt5m) then    ! Don't print hrly if data is 5min

         do i=1,24
            dif(1) = vh(i,1).val-vh(i,1).maps
            dif(2) = vh(i,2).val-vh(i,2).maps
            dif(3) = vh(i,3).val-vh(i,3).maps

 	    if(i.eq.1) then
               chg(1) = 0.
               chg(2) = 0.
               chg(3) = 0.
	    else
               chg(1) = vh(i,1).val-vh(i-1,1).val
               chg(2) = vh(i,2).val-vh(i-1,2).val
               chg(3) = vh(i,3).val-vh(i-1,3).val
	    end if

	    if(vh(i,1).maps .eq.-999.99 .or. vh(i,1).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

	    if(vh(i,2).maps .eq.-999.99 .or. vh(i,2).val .eq.-999.99)
     +      then
               dif(2)=0.
               chg(2)=0.
            end if

	    if(vh(i,3).maps .eq.-999.99 .or. vh(i,3).val .eq.-999.99)
     +      then
               dif(3)=0.
               chg(3)=0.
            end if

            write(unit_pres,1001,iostat=ios,err=901) tossday,i-1,
     +         vh(i,1).maps,vh(i,1).val,dif(1),chg(1),
     +         vh(i,1).code,vh(i,1).qc,
     +         vh(i,2).maps,vh(i,2).val,dif(2),chg(2),
     +         vh(i,2).code,vh(i,2).qc,
     +         vh(i,3).maps,vh(i,3).val,dif(3),chg(3),
     +         vh(i,3).code,vh(i,3).qc
	    end do ! 1 to 24
         end if !hrly data

	    if(prt5m) then    !5 min data

	     write(unit_pres,1031)

	     do i=p(1,1),p(1,2)
C was:		  h=i/12   ! orig fails at 55 mins after hr
		  h=(i-1)/12
		  m=mod(i-1,12)*5

                  dif(1) = vm(i,1).val-vm(i,1).maps
                  dif(2) = vm(i,2).val-vm(i,2).maps
                  dif(3) = vm(i,3).val-vm(i,3).maps

	       if(i.eq.1) then
                  chg(1) = 0.
                  chg(2) = 0.
                  chg(3) = 0.
	       else
                  chg(1) = vm(i,1).val-vm(i-1,1).val
                  chg(2) = vm(i,2).val-vm(i-1,2).val
                  chg(3) = vm(i,3).val-vm(i-1,3).val
	       end if

	    if(vm(i,1).maps .eq.-999.99 .or. vm(i,1).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

	    if(vm(i,2).maps .eq.-999.99 .or. vm(i,2).val .eq.-999.99)
     +      then
               dif(2)=0.
               chg(2)=0.
            end if

	    if(vm(i,3).maps .eq.-999.99 .or. vm(i,3).val .eq.-999.99)
     +      then
               dif(3)=0.
               chg(3)=0.
            end if

            write(unit_pres,1011,iostat=ios,err=901) tossday,h,m,
     +         vm(i,1).maps,vm(i,1).val,dif(1),chg(1),
     +         vm(i,1).code,vm(i,1).qc,
     +         vm(i,2).maps,vm(i,2).val,dif(2),chg(2),
     +         vm(i,2).code,vm(i,2).qc,
     +         vm(i,3).maps,vm(i,3).val,dif(3),chg(3),
     +         vm(i,3).code,vm(i,3).qc
	     end do !5 min
	    end if ! prt5m
	 end if  ! if prt pres

C print temperature info
	 if(prttemp) then

	    write(unit_temp,1021) tossstn,stns(tossstn).network,
     +      stns(tossstn).station,stns(tossstn).lat,stns(tossstn).lon,
     +      stns(tossstn).elev,stns(tossstn).x,stns(tossstn).y

	    write(unit_temp,1042)

          if (.not.prt5m) then    !Print hours for Hrly data not 5 min. data

	    do i=1,24

            dif(1) = vh(i,4).val-vh(i,4).maps
            dif(2) = vh(i,5).val-vh(i,5).maps

	    if(i.eq.1) then
               chg(1) = 0.
               chg(2) = 0.
	    else
               chg(1) = vh(i,4).val-vh(i-1,4).val
               chg(2) = vh(i,5).val-vh(i-1,5).val
	    end if

	    if(vh(i,4).maps .eq.-999.99 .or. vh(i,4).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

	    if(vh(i,5).maps .eq.-999.99 .or. vh(i,5).val .eq.-999.99)
     +      then
               dif(2)=0.
               chg(2)=0.
            end if

            write(unit_temp,1002,iostat=ios,err=901) tossday,i-1,
     +         vh(i,4).maps,vh(i,4).val,dif(1),chg(1),
     +         vh(i,4).code,vh(i,4).qc,
     +         vh(i,5).maps,vh(i,5).val,dif(2),chg(2),
     +         vh(i,5).code,vh(i,5).qc

	    end do  !1 to 24

          end if ! hrly data

	  if(prt5m) then

	     write(unit_temp,1032)

	     do i=p(2,1),p(2,2)
		  h=(i-1)/12
		  m=mod(i-1,12)*5
                  dif(1) = vm(i,4).val-vm(i,4).maps
                  dif(2) = vm(i,5).val-vm(i,5).maps

	    if(i.eq.1) then
               chg(1) = 0.
               chg(2) = 0.
	    else
               chg(1) = vm(i,4).val-vm(i-1,4).val
               chg(2) = vm(i,5).val-vm(i-1,5).val
	    end if

	    if(vm(i,4).maps .eq.-999.99 .or. vm(i,4).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

	    if(vm(i,5).maps .eq.-999.99 .or. vm(i,5).val .eq.-999.99)
     +      then
               dif(2)=0.
               chg(2)=0.
            end if

            write(unit_temp,1012,iostat=ios,err=901) tossday,h,m,
     +         vm(i,4).maps,vm(i,4).val,dif(1),chg(1),
     +         vm(i,4).code,vm(i,4).qc,
     +         vm(i,5).maps,vm(i,5).val,dif(2),chg(2),
     +         vm(i,5).code,vm(i,5).qc

          end do !5 min
         end if  !5 min

       end if  ! if prt temp

C print wind info

	 if(prtwind) then

	    write(unit_wind,1021) tossstn,stns(tossstn).network,
     +      stns(tossstn).station,stns(tossstn).lat,stns(tossstn).lon,
     +      stns(tossstn).elev,stns(tossstn).x,stns(tossstn).y

	    write(unit_wind,1043)
 
            if (.not.prt5m) then

	    do i=1,24

            if (vh(i,7).val .gt.  360.0)
     +         vh(i,7).val = vh(i,7).val-360.0                          ! lec - added 21 Nov 94
 
            if (vh(i,7).val .lt. -360.0 .and. vh(i,7).val .ne. -999.99)
     +         vh(i,7).val = vh(i,7).val+360.0                          ! lec - added 21 Nov 94 
       
            dif(1) = vh(i,6).val-vh(i,6).maps
            dif(2) = vh(i,7).val-vh(i,7).maps

            if(dif(2) .gt.  180.0) dif(2) = dif(2)-360.0
            if(dif(2) .lt. -180.0) dif(2) = dif(2)+360.0

	    if(i.eq.1) then
               chg(1) = 0.
               chg(2) = 0.
	    else
               chg(1) = vh(i,6).val-vh(i-1,6).val
               chg(2) = vh(i,7).val-vh(i-1,7).val
	    end if

	    if(vh(i,6).maps .eq.-999.99 .or. vh(i,6).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

  	    if(vh(i,7).maps .eq.-999.99 .or. vh(i,7).val .eq.-999.99)
     +      then
               dif(2)=0.
               chg(2)=0.
            end if

            write(unit_wind,1002,iostat=ios,err=901) tossday,i-1,
     +         vh(i,6).maps,vh(i,6).val,dif(1),chg(1),
     +         vh(i,6).code,vh(i,6).qc,
     +         vh(i,7).maps,vh(i,7).val,dif(2),chg(2),
     +         vh(i,7).code,vh(i,7).qc

	    end do ! 1 to 24
         end if ! hrly data

	    if(prt5m) then

	     write(unit_wind,1032)

	     do i=p(3,1),p(3,2)
		  h=(i-1)/12
		  m=mod(i-1,12)*5

               if (vm(i,7).val .gt.  360.0) then
                   vm(i,7).val = vm(i,7).val-360.0                          ! lec - added 21 Nov 94
               end if

               if (vm(i,7).val .lt. -360.0 .and. 
     +           vm(i,7).val .ne. -999.99)
     +             vm(i,7).val = vm(i,7).val+360.0                          ! lec - added 21 Nov 94

                  dif(1) = vm(i,6).val-vm(i,6).maps
                  dif(2) = vm(i,7).val-vm(i,7).maps

	       if(i.eq.1) then
                  chg(1) = 0.
                  chg(2) = 0.
	       else
                  chg(1) = vm(i,6).val-vm(i-1,6).val
                  chg(2) = vm(i,7).val-vm(i-1,7).val
	       end if

	    if(vm(i,6).maps .eq.-999.99 .or. vm(i,6).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1) = 0.
            end if

	    if(vm(i,7).maps .eq.-999.99 .or. vm(i,7).val .eq.-999.99)
     +      then
               dif(2)=0.
               chg(2)=0.
               vm(i,7).val = -999.99  !just set it -999.99 - possibly misleading
            end if

            write(unit_wind,1012,iostat=ios,err=901) tossday,h,m,
     +         vm(i,6).maps,vm(i,6).val,dif(1),chg(1),
     +         vm(i,6).code,vm(i,6).qc,
     +         vm(i,7).maps,vm(i,7).val,dif(2),chg(2),
     +         vm(i,7).code,vm(i,7).qc
	     end do ! 5 min 
	    end if  ! prt5m
	 end if  ! prt wind

C print precipitation info

	 if(prtprcp) then

	    write(unit_prcp,1021) tossstn,stns(tossstn).network,
     +      stns(tossstn).station,stns(tossstn).lat,stns(tossstn).lon,
     +      stns(tossstn).elev,stns(tossstn).x,stns(tossstn).y

            write(unit_prcp,1045)
	    write(unit_prcp,1044)

            if (.not.prt5m) then

	    do i=1,24
               dif(1) = vh(i,8).val-vh(i,8).maps

	       if(i.eq.1) then
                  chg(1) = 0.
	       else
                  chg(1) = vh(i,8).val-vh(i-1,8).val
	       end if

	    if(vh(i,8).maps .eq.-999.99 .or. vh(i,8).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

            write(unit_prcp,1004,iostat=ios,err=901) tossday,i-1,
     +         vh(i,8).maps,vh(i,8).val,dif(1),chg(1),
     +         vh(i,8).code,vh(i,8).qc

	    end do ! 1 to 24
         end if !hrly data


	    if(prt5m) then

	     write(unit_prcp,1044)

	     do i=p(4,1),p(4,2)
		  h=(i-1)/12
		  m=mod(i-1,12)*5
                  dif(1) = vm(i,8).val-vm(i,8).maps

		  if(i.eq.1) then
                     chg(1) = 0.
		  else
                     chg(1) = vm(i,8).val-vm(i-1,8).val
		  end if

 	    if(vm(i,8).maps .eq.-999.99 .or. vm(i,8).val .eq.-999.99)
     +      then
               dif(1)=0.
               chg(1)=0.
            end if

            write(unit_prcp,1014,iostat=ios,err=901) tossday,h,m,
     +         vm(i,8).maps,vm(i,8).val,dif(1),chg(1),
     +         vm(i,8).code,vm(i,8).qc

            end do ! 5 min data
	    end if ! prt5m

	 end if ! prt prcp

      prttoss=0

      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'prttoss(): I/O Error: ',ios,' ',errstr
      prttoss=ios

999   return

C
C     All formats were 7.2 instead of 8.2. Increased length
C     to handle large diffs, changes, etc. Also increased
C     all 6.2 to 7.2.
C
1001  format(' ',i4,x,i2,3(3x,2(2x,f8.2),x,2(f8.2),x,i2,x,a1))
1002  format(' ',i4,x,i2,2(3x,2(2x,f8.2),x,2(f8.2),x,i2,x,a1))
1004  format(' ',i4,x,i2,  3x,2(2x,f8.2),x,2(f8.2),x,i2,x,a1)
1011  format(' ',i4,x,i2,':',i2,x,
     +       3(5x,f8.2,2x,f8.2,2x,f7.2,x,f7.2,x,i2,x,a1))
1012  format(' ',i4,x,i2,':',i2,x,
     +       2(5x,f8.2,2x,f8.2,2x,f7.2,x,f7.2,x,i2,x,a1))
1014  format(' ',i4,x,i2,':',i2,x,
     +         5x,f8.2,2x,f8.2,2x,f7.2,x,f7.2,x,i2,x,a1)
1021  format('1','id=',i4,'  Network=',a10,'  Station=',a15,
     +    '    lat=',f10.5,' lon=',f11.5,' elev=',f7.2,
     +    ' dx=',i5,' dy=',i5,/)
1031  format(' ',/,' day hr:min ',
     +       3('        maps      qcf       var     chg   cd  qc'))
1032  format(' ',/,' day hr:min ',
     +       2('        maps      qcf       var     chg   cd  qc'))
1034  format(' ',/,' day hr:min ',
     +         '        maps      qcf       var     chg   cd  qc')
1041  format(' ',8x,'          ********** Station Pressure *********',
     +          '      ********* Sea Level Pressure ********',
     +          '      ********** Calc SL Pressure *********',/,
     +'  day hr ',3('           maps      qcf     var    chg  cd  qc'))
1042  format(' ',8x,'          ************* Temperature ***********',
     +          '      ************** Dew Point ************',/,
     +'  day hr ',2('           maps      qcf     var    chg  cd  qc'))
1043  format(' ',8x,'          ************* Wind Speed ************',
     +          '      *********** Wind Direction **********',/,
     +'  day hr ',2('           maps      qcf     var    chg  cd  qc'))
1044  format(' ',8x,
     +'*********** Precipitation (Not Compared to MAPS) ***********',/,
     +'  day hr ',  '           maps      qcf     var    chg  cd  qc')
C
C1044  format(' ',8x,'          *********** Precipitation ***********',/,
C     +'  day hr ',  '           maps      qcf     var    chg  cd  qc')
C
1045  format(' ',8x,'Note that Precip is NOT compared to MAPS. ',/,
     +' ',8x,'Gross limits are applied instead. ',/,
     +' ',8x,'Following indicates where to look in data, only.')

      end
