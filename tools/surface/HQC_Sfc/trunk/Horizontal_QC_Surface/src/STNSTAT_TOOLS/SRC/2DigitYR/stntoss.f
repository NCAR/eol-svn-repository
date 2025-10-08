C******************************************************************************
C
C program: stntoss
C author:  K. Scully
C date:    05/28/92
C description: Print variance reports
C
C usage:   stntoss [-debug] variance file
C        
C              where -debug     option will print variable info as processing
C                               is done.
C                    input_file is the name of the input variance file
C
C 25 Aug 94 lec
C  Updated s/w to handle 15 char station IDs which were 10 chars.
C******************************************************************************

      program var

      implicit none

      include 'stntosscom.h'
      include 'stncom.h'

      character*20 parmnames(NQCPARMS)	!parameter names
      real varb(NQCPARMS)	!variance amounts for bad qc flag
      real varq(NQCPARMS)	!variance amounts for questionable qc flag
      real varo(NQCPARMS)	!offset for variance file parm values
      real rocb(NQCPARMS)	!rate of change amounts for bad qc flag
      real rocq(NQCPARMS)	!rate of change amounts for questionable qc flag
      character*8 day1		!day 1 for dates in variance file

C structure for the print variance statistics line
      structure /prt/
         integer n
         real ave
         real lvar
         real hvar
         real tvar
         real max
         real dev
      end structure
      record /prt/ p(NPARMS)

C misc. variables
      integer iargc,narg
      integer*2 rc,getvar
      integer tm,time
      character*26 ctime
      character*6 debugarg
      real var
      integer i,j,si

C Parameter names expected in the config file
      data parmnames /'Station Pressure    ',
     +                'Sea Level Pressure  ',
     +                'Calc Sea Level Press',
     +                'Temperature         ',
     +                'Dew Pt Temperature  ',
     +                'Wind Speed          ',
     +                'Wind Direction      ',
     +                'Precipitation       ',
     +                'Squall amount       ',
     +                'Present weather     ',
     +                'Visibility          ',
     +                'Ceiling height 1    ',
     +                'Cloud amount 1      ',
     +                'Ceiling height 2    ',
     +                'Cloud amount 2      ',
     +                'Ceiling height 3    ',
     +                'Cloud amount 3      '/
C************************** end variable definitions *********************

      tm=time()

      write(*,*)  'Running stntoss program: Begin: ',ctime(tm)

      debug = .false.
C get command line arguments
      narg = iargc()
      if(narg .eq. 1) then
         call getarg(1,qc_toss)
      else if(narg .eq.2) then
         call getarg(1,debugarg)
         call getarg(2,qc_toss)
         if(debugarg .eq. '-debug' .or. debugarg .eq. -DEBUG) then
            debug = .true.
            write(*,*) 'Running in DEBUG mode'
         end if
      else
         write(0,*) 'Invalid number of arguments'
      end if

      nstns=0
      write(*,*) 'Processing file: ',qc_toss

C open the files
      open(unit_toss,name=qc_toss,status='old',iostat=ios,err=901)
      open(unit_var,name=qc_var,status='old',iostat=ios,err=901)

C get station from station master file
      call getstns()

C get variances limits from config file
      call getvarlim(parmnames,varb,varq,varo,rocb,rocq,day1)

C loop through the file of var records
      do while (.true.)
         rc=getvar(day1)		!get first var record
	    if(rc .eq. -2) goto 200
	    si=v.stn
         do 100 i=1,NPARMS
            if(v.var(i) .eq. -999.99) goto 100	!skip if no variance
	       s(si,i).nobs=s(si,i).nobs+1
100      continue	! end of while loop for each parm
      end do		! end of while loop for each record in var file

C loop through the file of toss records
200   do while (.true.)
         rc=gettoss(day1)		!get first var record
	    if(rc .eq. -2) goto 900
	    si=t.stn
        if(t.code.le.8) i=t.code
        if(t.code.eq.13) i=8
        if(t.code.gt.8 .and. mod(t.code,NQCPARMS).le.8)
     +      i=mod(t.code,NQCPARMS)
        if(i.eq.0) i=17
        if(t.code.eq.35) i=5
        if(t.qc .eq. 'B') s(si,i).nb=s(si,i).nb+1
        if(t.qc .eq. 'D') s(si,i).nd=s(si,i).nd+1
	   var=t.val-t.maps
        if(var .eq. -999.99) goto 100	!skip if no variance

C accumulate statistics by station
	   s(si,i).lvar=s(si,i).lvar+v.var(i)
	   s(si,i).hvar=s(si,i).hvar+v.var(i)
	  if(abs(v.var(i)) .gt. abs(s(si,i).max)) s(si,i).max=v.var(i)

100      continue	! end of while loop for each parm
      end do		! end of while loop for each record in var file

900   continue

C print statistics for station pressure
      write(*,1000)
      write(*,1006) 'stn'
      do 143 i=1,nstns
         do 163 j=1,1
            if(s(i,j).nobs .eq. 0) goto 163
            p(j).n=s(i,j).nobs
            p(j).ave=(abs(s(i,j).lvar)+s(i,j).hvar)/s(i,j).nobs
            p(j).lvar=s(i,j).lvar
            p(j).hvar=s(i,j).hvar
            p(j).tvar=abs(s(i,j).lvar)+s(i,j).hvar
            p(j).max=s(i,j).max
C           p(j).dev=sqrt( abs((s(i,j).var2-p(j).tvar**2) /
C    +                     (p(j).n*(p(j).n-1))) )
163      continue
         if(s(i,1).nobs .eq.0) goto 143
         write(*,1023) i,(p(j).n,p(j).ave,p(j).max,
     +         p(j).lvar,p(j).hvar,p(j).tvar,j=1,1)
C    +         p(j).lvar,p(j).hvar,p(j).tvar,p(j).dev,j=1,1)
143   continue

C print statistics for sea level pressures

      write(*,1001)
      write(*,1005) 'stn'
	 do 103 i=1,nstns
	    do 123 j=2,3
            if(s(i,j).nobs .eq. 0) goto 123
            p(j).n=s(i,j).nobs
            p(j).ave=(abs(s(i,j).lvar)+s(i,j).hvar)/s(i,j).nobs
            p(j).lvar=s(i,j).lvar
            p(j).hvar=s(i,j).hvar
            p(j).tvar=abs(s(i,j).lvar)+s(i,j).hvar
            p(j).max=s(i,j).max
C           p(j).dev=sqrt( abs((s(i,j).var2-p(j).tvar**2) /
C    +                     (p(j).n*(p(j).n-1))) )
123      continue
         if(s(i,2).nobs .eq.0 .and. s(i,3).nobs .eq.0) goto 103
         write(*,1022) i,(p(j).n,p(j).ave,p(j).max,
     +        p(j).lvar,p(j).hvar,p(j).tvar,j=2,3)
C    +        p(j).lvar,p(j).hvar,p(j).tvar,p(j).dev,j=2,3)
103   continue

C print statistics for temperatures

      write(*,1002)
      write(*,1005) 'stn'
	 do 106 i=1,nstns
	    do 126 j=4,5
            if(s(i,j).nobs .eq. 0) goto 126
            p(j).n=s(i,j).nobs
            p(j).ave=(abs(s(i,j).lvar)+s(i,j).hvar)/s(i,j).nobs
            p(j).lvar=s(i,j).lvar
            p(j).hvar=s(i,j).hvar
            p(j).tvar=abs(s(i,j).lvar)+s(i,j).hvar
            p(j).max=s(i,j).max
C           p(j).dev=sqrt( abs((s(i,j).var2-p(j).tvar**2) /
C    +                     (p(j).n*(p(j).n-1))) )
126      continue
         if(s(i,4).nobs .eq.0 .and. s(i,5).nobs .eq.0) goto 106
         write(*,1022) i,(p(j).n,p(j).ave,p(j).max,
     +        p(j).lvar,p(j).hvar,p(j).tvar,j=4,5)
C    +        p(j).lvar,p(j).hvar,p(j).tvar,p(j).dev,j=4,5)
106   continue

C print statistics for winds

      write(*,1003)
      write(*,1005) 'stn'
	 do 109 i=1,nstns
	    do 129 j=6,7
            if(s(i,j).nobs .eq. 0) goto 129
            p(j).n=s(i,j).nobs
            p(j).ave=(abs(s(i,j).lvar)+s(i,j).hvar)/s(i,j).nobs
            p(j).lvar=s(i,j).lvar
            p(j).hvar=s(i,j).hvar
            p(j).tvar=abs(s(i,j).lvar)+s(i,j).hvar
            p(j).max=s(i,j).max
C           p(j).dev=sqrt( abs((s(i,j).var2-p(j).tvar**2) /
C    +                     (p(j).n*(p(j).n-1))) )
129      continue
         if(s(i,6).nobs .eq.0 .and. s(i,7).nobs .eq.0) goto 109
         write(*,1022) i,(p(j).n,p(j).ave,p(j).max,
     +        p(j).lvar,p(j).hvar,p(j).tvar,j=6,7)
C    +        p(j).lvar,p(j).hvar,p(j).tvar,p(j).dev,j=6,7)
109   continue

C print statistics for precipitation

C     write(*,1003)
C     write(*,1005) 'stn'
C     do 112 i=1,nstns
C        do 132 j=8,8
C           if(s(i,j).nobs .eq. 0) goto 132
C           p(j).n=s(i,j).nobs
C           p(j).ave=(abs(s(i,j).lvar+s(i,j).hvar)/s(i,j).nobs
C           p(j).lvar=s(i,j).lvar
C           p(j).hvar=s(i,j).hvar
C           p(j).tvar=abs(s(i,j).lvar)+s(i,j).hvar
C           p(j).max=s(i,j).max
C           p(j).dev=sqrt( abs((s(i,j).var2-p(j).tvar**2) /
C    +                     (p(j).n*(p(j).n-1))) )
C132      continue
C        if(s(i,8).nobs .eq.0) goto 112
C        write(*,1023) i,(p(j).n,p(j).ave,p(j).max,
C    +        p(j).lvar,p(j).hvar,p(j).tvar,j=8,8)
C    +        p(j).lvar,p(j).hvar,p(j).tvar,p(j).dev,j=8,8)
C112   continue

      close(unit_var)
      goto 999
901	 errstr=gerror(errstr)
      write(0,*) 'stntoss(): I/O Error: ',ios,' ',errstr
999   write(*,*) 'In  stntoss.f'
      call standard_arithmetic()
      stop
1051  format(/////,'PRESSURE Variance statistics')
1052  format(/////,'TEMPERATURE Variance statistics')
1053  format(/////,'WINDS Variance statistics')
C1054  format(/////,'PRECIPITATION Variance statistics')
1000  format(///,4x,
     +  '| **************** Station Pressure *****************')
1001  format(///,4x,
     +  '| ************** NWS Surface Pressure ***************',
     + ' | *********** Calculated Surface Pressure ***********')
1002  format(///,4x,
     +  '| ************** Dry Bulb Temperature ***************',
     + ' | ************** Dew Point Temperature **************')
1003  format(///,4x,
     +  '| ***************** Wind Speed  *********************',
     + ' | ***************** Wind Direction ******************')
C1004  format(///,4x,
C     +  '| ***************** Precipitation *******************')
C1005  format(8x,
C     + 'no.     ave     max       low      high     total     std',5x,
C     + 'no.     ave     max       low      high     total     std',
C     + /,a3,5x,
C     + 'obs     var     var       var       var       var     dev',5x,
C     + 'obs     var     var       var       var       var     dev')
C1006  format(8x,
C     + 'no.     ave     max       low      high     total     std',
C     + /,a3,5x,
C     + 'obs     var     var       var       var       var     dev')
C1008  format(///,24x,
C     +    '***************** ALL stations; ALL time ******************',
C     + /,25x,
C     + 'no.     ave     max       low      high     total     std',
C     + /,'parameter',17x,
C     + 'obs     var     var       var       var       var     dev')
1005  format(4x,
     +  '|   no.     ave     max       low      high     total',
     + ' |   no.     ave     max       low      high     total',
     + /,a3,5x,
     +  '|   obs     var     var       var       var       var',
     + ' |   obs     var     var       var       var       var')
1006  format(4x,
     +  '|   no.     ave     max       low      high     total',
     + /,a3,5x,
     + ' |   obs     var     var       var       var       var')
C1021  format(i3,3(' | ',i5,2(x,f7.2),3(x,f9.2)))
1022  format(i3,2(' | ',i5,2(x,f7.2),3(x,f9.2)))
1023  format(i3,' | ',i5,2(x,f7.2),3(x,f9.2))
1025  format(a20,3x,i5,2(x,f7.2),3(x,f9.2))
      end
C************************************************************************
C
C function: getstns()
C description: load the qc station master file
C
C************************************************************************

      subroutine getstns()

      implicit none

      include 'varcom.h'
      include 'stncom.h'
      character*1 yn
      character*10 network,station
      integer stncode,dx,dy
      real    lat,lon,elev
      integer i

C open the qc_stns file
      open(unit_stns,name=qc_stns,status='old',iostat=ios,err=901)
      if(debug) then
         write(*,*) "Reading in qc_stns file:"
         write(*,*)
      end if

C loop through the records.
      do while (.true.)
          read(unit_stns,1001,iostat=ios,err=901,end=900) stncode,
     +         network,station,lat,lon,elev,dx,dy
          stns(stncode).network=network
          stns(stncode).station=station
          stns(stncode).lat=lat
          stns(stncode).lon=lon
          stns(stncode).elev=elev
          stns(stncode).x=dx
          stns(stncode).y=dy
		nstns=nstns+1
      end do

900   if(debug) then
	    type *,' Display stations?'
         accept 1004, yn
         if(yn .eq. 'N' .or. yn .eq. 'n') goto 999
         write(*,*)
         write(*,*) 'num stations=',nstns
         do i=1,nstns
            write(*,1002) i,stns(i).network,stns(i).station,
     +        stns(i).lat,stns(i).lon,stns(i).elev,
     +        stns(i).x,stns(i).y
         end do
	    type *,' Continue?'
         accept 1004, yn
         if(yn .eq. 'N' .or. yn .eq. 'n') stop
      end if
      goto 999
901   errstr=gerror(errstr)
      write(0,*) 'getstns(): I/O Error: ',ios,' ',errstr
999   close(unit_stns)
      return
1001  format(i3,a10,a15,f10.5,f11.5,f7.2,i5,i5)
1002  format(i3,2x,a10,2x,a15,2x,f10.5,2x,f11.5,2x,f7.2,2x,
     +       i5,2x,i5)
1004  format(a1)
      end
C************************************************************************
C
C function: getvarlim()
C description: get the variance limits from the variance config file
C
C************************************************************************

      subroutine getvarlim(parmnames,varb,varq,varo,rocb,rocq,day1)

      implicit none

      include 'varcom.h'
      character*(*) parmnames(*)
      character*80 varbuf
      character*1 yn
      character*8 day1
      real varb(*),varq(*),varo(*),rocb(*),rocq(*)
      integer i

C open the config file
      open(unit_config,name=qc_config,status='old',iostat=ios,err=901)
      if(debug) then
         write(*,*) "Reading in qc_config file:"
         write(*,*)
      end if

C initialize the bad and questionable variance arrays.
C -999.99 means a variance test is not applicable.
      do i=1,NQCPARMS
         varb(i)=-999.99
         varq(i)=-999.99
         varo(i)=0.0
         rocb(i)=-999.99
         rocq(i)=-999.99
      end do

C loop through the records.
      do while (.true.)
          read(unit_config,1003,iostat=ios,err=901,end=900) varbuf
          if(debug) then
             write(*,1003) varbuf
          end if
          do i=1,NQCPARMS
             if(varbuf(1:20) .eq. parmnames(i)) then
                read(varbuf,1001)
     +               varlim.parmname,varlim.varbad,varlim.varquest,
     +               varlim.varoffset,varlim.rocbad,varlim.rocquest
                varb(i)=varlim.varbad
                varq(i)=varlim.varquest
                varo(i)=varlim.varoffset
                rocb(i)=varlim.rocbad
                rocq(i)=varlim.rocquest
             else if(varbuf(1:7) .eq. 'Day 1: ') then
                read(varbuf,1005) day1
             end if
          end do
      end do

900   if(debug) then
         write(*,*)
         write(*,*) 'Arrays varb(), varq(), varo(), rocb() and rocq():'
         do i=1,NQCPARMS
            write(*,1002) i,parmnames(i),varb(i),varq(i),varo(i),
     +                    rocb(i),rocq(i)
         end do
	    type *,' Continue?'
         accept 1004, yn
         if(yn .eq. 'N' .or. yn .eq. 'n') stop
      end if
      goto 999
901   errstr=gerror(errstr)
      write(0,*) 'getvarlim(): I/O Error: ',ios,' ',errstr
999   close(unit_config)
      return
1001  format(a20,5(3x,f7.2))
1002  format(i2,3x,a20,5(3x,f7.2))
1003  format(a80)
1004  format(a1)
1005  format(7x,a8)
      end
C***********************************************************************
C
C function: getvar()
C description: input the variance records
C
C************************************************************************

      integer*2 function getvar(day1)

      implicit none

      include 'varcom.h'
      character*8 day1
      integer i
      character*2 yr,mo,day
      character*1 yn
      integer i
      integer*2 min
      real maps(NPARMS)
	 data yn /'y'/

C convert begin date to julian
      read(day1,1002) mo,day,yr
C write the variance record
      read(unit_var,1004,iostat=ios,err=901,end=900)
     +  v.stn,v.day,v.hr,min,(maps(i),v.var(i),i=1,NPARMS)
      if(debug .and. yn.ne.'q') then
         write(*,1003,iostat=ios,err=901) 
     +     v.stn,v.day,v.hr,(i,v.var(i),i=1,NPARMS)
	    type *,' Continue?'
         accept 1006, yn
         if(yn .eq. 'N' .or. yn .eq. 'n') stop
      end if
100   continue
      getvar=0
      goto 999
900   getvar=-2
      goto 999
901   errstr=gerror(errstr)
      write(0,*) 'getvar(): I/O Error: ',ios,' ',errstr
      getvar=ios
999   return
1002  format(a2,'/',a2,'/',a2)
1003  format('getvar(): Variance record',/,
     +      'Station: ',i4,'   day: ',i3,'   hour: ',i2,6x,'VAR',
     +      7(/,i2,2x,f7.2))
1004  format(i4,i3,i2,i2,14(f7.2))
1005  format(i2)
1006  format(a1)
      end
C************************************************************************
C
C function: gettoss()
C description: get the toss records
C
C************************************************************************

      integer function gettoss(day1)

      implicit none

      include 'tosscom.h'
	 logical prtb,prtd
      character*8 day1
      character*1 yn
      integer*2 stn
      integer*2 code
      character*1 qc
      integer bmo,bday,byr
      real maps
      real val
      integer i,j
      integer yr,day,hr,min,h,m
	 data yn /'y'/
	 data stn /0/
	 data maps /0.0/
	 data val /0.0/
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

      read(unit_toss,1001,iostat=ios,err=901,end=900) t.stn,
     +      t.day,t.hr,t.min,t.code,t.qc,t.maps,t.val
      if(debug) then
         write(*,1002) t.stn,byr,t.day,t.hr,t.min,t.code,t.qc
	    type *,' Continue?'
         accept 1004, yn
         if(yn .eq. 'N' .or. yn .eq. 'n') stop
      end if
100   gettoss=0
      goto 999
900   gettoss=-2
      goto 999
901   errstr=gerror(errstr)
      write(*,*) 'gettoss(): I/O Error: ',ios,' ',errstr
      gettoss=ios
999   return
1000  format(i2,'/',i2,'/',i2)
1001  format(i4,i3,i2,i2,i2,a1,2(f7.2))
1002  format('Station: ',i4,'   date/time: ',i2,'.',i3,' ',i2,':',i2,
     +      ' code:',i2,' flg: ',a1)
1004  format(a1)
      end
