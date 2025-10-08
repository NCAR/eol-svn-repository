c
C $Id$
C $Log$
C

      program cvt_coop
      implicit none
c***********************************************************************
c     By Wayne Brazille, USWRP Project Office, UCAR, Boulder
c     November, 1993
c
c     Purpose: Convert NCDC Cooperative Summary of the Day data in E-BUFR 
c              Format to an ASCII tabular format.  Daily precip values
c              are also written to a separate file.
c
c
c***********************************************************************
c    Modified by Wayne Brazille on March 24, 1995.
c    Modifications were:
c         1.  Changed subroutine get_stn to match on Coop station number
c             instead of lat/lon, since coop station number is now in 
c             the data.
c         2.  Modified main read loop to read in coop station number from
c             the E-BUFR file.
c***********************************************************************
c    Modified by Wayne Brazille on April 15, 1995.
c    Modifications were:
c	  1.  Modified subroutine stninf to read new station
c             history file.
c***********************************************************************
c    Include files for E-BUFR routines 
c***********************************************************************
      include "ebufr_parm.inc"
      include "ebufr_vars.inc"
      include "ebufr_obs.inc"
c***********************************************************************
c    Include file for QCF format
c***********************************************************************
      include "dqcf.inc"

      record /dqcf/ q
c***********************************************************************
c    Include file for E-BUFR routines (data statements)
c***********************************************************************
c      include "ebufr_dat.inc"
c***********************************************************************
c   Main program variables
c***********************************************************************
c
      integer iunit,ounit,msunit,numdat,stnunit,punit
      integer rdcnt,wtcnt,errnum,i,k,tmp,nproc
      integer scale,ref,pcount,ilat,ilon
      integer skip_ct
      real ckel2c,rval,elev,rmin
      logical EOF,DSCSCN,FAIL,FSTTIM,gtfils,LOADED
      logical ERR,CHKALL
      character*1 valtyp
      character*50 ifile,ofile,msfile,stnfile,pfile,sfile
      character*32 name1,name2
      character*128 cval
      character*24 units
      character*20 tstr
      double precision comlat,comlon

c***********************************************************************
c  variables for station information
c***********************************************************************
      integer MAXSTNS,nstns
      PARAMETER(MAXSTNS=10000)
      double precision lat(MAXSTNS),lon(MAXSTNS)

      real elev(MAXSTNS)
c     real lat(MAXSTNS),lon(MAXSTNS),elev(MAXSTNS)
      real tzone(MAXSTNS)
      character*10 coopnum(MAXSTNS)
      character*10 stnnum
      character*2 st(MAXSTNS)
      character*20 name(MAXSTNS)
      character*8 bdate(MAXSTNS),edate(MAXSTNS)
      character*8 sbegin(MAXSTNS),send(MAXSTNS)
      logical stnseen(MAXSTNS)
      integer no_stn

c***********************************************************************
c  variables for storing daily precipitation
c***********************************************************************
      real dlyprcp(31)
      integer dflag(31)
      integer obshour(31)
      integer dindx
      integer onumdat
c***********************************************************************
c  Common block for qc stats
c***********************************************************************
      integer ucnt,gcnt,mcnt,dcnt,bcnt,ecnt,tcnt,acnt
      common /stats/ ucnt,gcnt,mcnt,dcnt,bcnt,ecnt,tcnt,acnt
c***********************************************************************
c  Include data statements for low-level E-BUFR routines 
c***********************************************************************
      include "ebufr_init.inc"
c
c***********************************************************************
c  Start of main program code
c***********************************************************************
c
c Initialize variables
c

      rdcnt = 0
      wtcnt = 0
      skip_ct = 0
      ucnt = 0
      gcnt = 0
      mcnt = 0
      dcnt = 0
      bcnt = 0
      ecnt = 0
      tcnt = 0
      acnt = 0
      nstns = 0
      no_stn = 0
      EOF = .false.
      FAIL = .false.
      DSCSCN = .false.
      FSTTIM = .true.
      do 5, i = 1,MAXSTNS
         stnseen(i) = .false.
    5 continue
      do 6, i = 1,31
         dlyprcp(i) = -999.99
         dflag(i) = 9
    6 continue
c
c
c Get filenames and open files
c
      FAIL = gtfils(iunit,ounit,msunit,stnunit,punit,
     +              ifile,ofile,msfile,stnfile,pfile,sfile)
      if (FAIL) then
          write(6,*) ' Aborting Program'
          stop
      end if

c
c Read stn information into arrays
c
      call stninf(stnfile,stnunit,msunit,MAXSTNS,lat,lon,coopnum,
     +           elev,name,st,sbegin,send,tzone,nstns)

c
c Loop thru E-BUFR data records and convert to QCF
c
      FAIL = .false.
      onumdat = 0
 10   continue
c
c test termination
c
c     if (rdcnt.gt.100) then !end for test
c        goto 1000
c     end if

      call getobs (iunit,msunit,datum,fmt,numdat,xout,yout,
     +             FAIL,EOF,FSTTIM,CHKALL)
      if (FAIL) then
          goto 2000
      else
          if (EOF) then
             goto 1000
          end if
      end if
      rdcnt = rdcnt + 1

c
c check for changes in number of data elements
c
      if (onumdat.eq.0) then
        write (msunit,*) 'Initial Number of Data Elements = ',numdat
        onumdat = numdat
      else if (onumdat.ne.numdat) then
        write(msunit,*) 'WARNING: Number of data elements now = ',numdat
        write(msunit,*) 'Previous Number of data elements = ',onumdat
        onumdat = numdat
      end if 
c
c Initialize QCF record
c
      call dqrset(q,.true.)
c
c Move info from data hdr into qcf format
c
       q.year = clyear
       q.date(1) = clmon

c  day, hour encoded in each data group.  
c  determination of elevation deffered until ready to write daily
c  record, since elevation may change in the middle of the month

       q.qtime(2) = '00'
       q.qnet = "COOP"

c the NCDC E-BUFR file has lat/lon expressed as a decimal number, but
c the values are really in degrees and minutes.  This problem is
c not corrected here so that it will be easier to match the data
c to the station location metadata, which is also in degrees and minutes
c
c  values are rounded to two decimal places as that is all the
c  precision avaialable and to facilitate matching the precision of
c the station history file.
c
c  The correct latitude/longitude (in decimal degrees) is inserted
c  in the output records just before writting them.
c
c  Modified September 5, 1996.  Lat and lon will be converted to a true 
c  decimal number here.  Code now matches on station id rather than 
c  lat and lon, so we should have these in the proper format.  RIS
c
       comlat = cllat + .00005
       write(tstr,'(f10.4)') comlat
       read(tstr,'(f10.4)') comlat
       ilat = comlat
       rmin = (comlat - ilat)*100.0/60.0
       comlat = ilat + rmin

       comlon = cllon - .00005
       write(tstr,'(f11.4)') comlon
       read(tstr,'(f11.4)') comlon
       ilon = comlon
       rmin = (comlon - ilon)*100.0/60.0
       comlon = ilon + rmin

       q.ll(1) = comlat
       q.ll(2) = comlon
       read(clibyt,'(I3)') q.occur

c
c Get coop station number out of the E-BUFR data record
c
       call getval(datum(1),xout(1),yout(1),fmt(1),rval,cval,
     +                    valtyp,msunit,ERR)
       if (ERR) then
              goto 2000
       end if
          if (xout(1).eq.57.and.yout(1).eq.13) then  !Coop station number
              if (valtyp.ne.'M') then
                 stnnum = cval
              else
                 write (msunit,*) 'Missing coop number encountered'
              end if
          else
              write (msunit, *) 
     +           'Coop number is not first entry in record'
          end if

c
c Loop through E-BUFR descriptors to extract data values.
c

      nproc = 0
c      dindx = 1
      k = 1
      do 60 while (k.le.numdat)

c
c Get appropriately typed data value out of datum
c
          call getval(datum(k),xout(k),yout(k),fmt(k),rval,cval,
     +                    valtyp,msunit,ERR)
          if (ERR) then
              goto 2000
          end if
c
c move data value to appropriate place in DQCF record and
c convert units if needed.
c

          LOADED = .false.
          if (xout(k).eq.12.and.yout(k).eq.16) then  !Max temp
c             write(msunit,*)'Max temp = ', rval
              if (valtyp.ne.'M') then
                  q.mxtmp = ckel2c(rval)
                  if (q.mxtmp.lt.-270.0) then
                     write(msunit,*)'Bad MAX TEMP of ',q.mxtmp,
     +                    ' set to missing'
                     q.mxtmp = -999.99
                  end if   
              end if
              call gtflg(msunit,MAXDAT,datum,fmt,xout,yout,k,
     +             q.mxflg1,q.mxflg2)
              LOADED = .true.
          else if (xout(k).eq.12.and.yout(k).eq.17) then !Min temp
c             write(msunit,*)'Min temp = ', rval
              if (valtyp.ne.'M') then
                  q.mntmp = ckel2c(rval)
                  if (q.mntmp.lt.-270.0) then
                     write(msunit,*)'Bad MIN TEMP of ',q.mntmp,
     +                    ' set to missing'
                     q.mntmp = -999.99
                  end if  
              end if
              call gtflg(msunit,MAXDAT,datum,fmt,xout,yout,k,
     +             q.mnflg1,q.mnflg2)
              LOADED = .true.
          else if (xout(k).eq.13.and.yout(k).eq.13) then !total snow depth
c             write(msunit,*)'Total snow depth = ', rval
              if (valtyp.ne.'M') then
                  q.sndep = rval
              end if
              call gtflg(msunit,MAXDAT,datum,fmt,xout,yout,k,
     +              q.sdflg1,q.sdflg2)
              LOADED = .true.
          else if (xout(k).eq.13.and.yout(k).eq.23) then !total precip
c             write(msunit,*)'Total precip = ', rval
              if (valtyp.ne.'M') then
                  if (rval .ne. -1) then
                     if (rval .lt. 1000) then
                         q.precip = rval
c                         dlyprcp(dindx) = rval
                     else
                         q.precip = rval - 1000.0
c                         dlyprcp(dindx) = q.precip !accumulation
                     end if
                  else
                     q.precip = 0.0
c                     dlyprcp(dindx) = 0.0 !trace
                  end if
              else
c                 dlyprcp(dindx) = -999.99       !missing
              end if
              call gtflg(msunit,MAXDAT,datum,fmt,xout,yout,k,
     +             q.prcflg1,q.prcflg2)
c              dflag(dindx) = q.prcflg1
              LOADED = .true.
          else if (xout(k).eq.13.and.yout(k).eq.194) then !daily snowfall
c             write(msunit,*)'Daily snowfall = ', rval
              if (valtyp.ne.'M') then
                  q.snfall = rval
              end if
              call gtflg(msunit,MAXDAT,datum,fmt,xout,yout,k,
     +              q.sfflg1,q.sfflg2)
              LOADED = .true.
          else if (xout(k).eq.4.and.yout(k).eq.3) then !day of observation
c             write(msunit,*)'Day = ', rval
              if (valtyp.ne.'M') then
                  tmp = int(rval)
                  if (tmp.eq.0) then
                     write(msunit,*)'Invalid day of 00 found'
                  end if
                  write(q.date(2),'(I2.2)') tmp
              end if
              LOADED = .true.
          else if (xout(k).eq.4.and.yout(k).eq.4) then !time of observation
c             write(msunit,*)'Hour = ', rval
              if (valtyp.ne.'M') then
                  tmp = int(rval)
                  write(q.qtime(1),'(I2.2)') tmp
                  if (q.qtime(1).eq.'  ') then
                     write(msunit, *)'Invalid blank time found', tmp
                  end if 
c                  obshour(dindx) = tmp
c              else
c                  write(msunit,*)'Missing time found on:', q.date
              end if
              LOADED = .true.
          else if (xout(k).eq.7.and.yout(k).eq.1) then !elevation
c             write(msunit,*) 'k = ', k
c             write(msunit,*) 'Elevation = ', rval
              if (valtyp.ne.'M') then
                 q.staelv = rval/10.0
              else
                 write(msunit,*)'Missing elevation for ', stnnum
              end if
              LOADED = .true.
          end if

c
c  Print warning message if descriptor not recognized
c
         if (LOADED .and. k.ne.4) then
            nproc = nproc + 1
c            write(msunit,1001, err=1000) nproc,q.year,q.date,
c     *q.qtime,q.qnet,q.statn,q.ll,q.occur,q.staelv,
c     *q.mxtmp,q.mxflg1,q.mxflg2,q.mntmp,q.mnflg1,q.mnflg2,
c     *q.sndep,q.sdflg1,q.sdflg2,q.precip,q.prcflg1,q.prcflg2,
c     *q.snfall,q.sfflg1,q.sfflg2

c 1001       format(i2,1x,a4,'/',a2,'/',a2,1x,a2,':',a2,1x,a10,1x,a10,
c     *          1x,f10.5,1x,f11.5,1x,i3,1x,f7.2,1x,
c     *          5(f7.2,1x,i3,1x,i3,1x),f7.2,1x,i3,i3)
         else
            if ((xout(k).ne.5.and.yout(k).ne.1).and.
     +         (xout(k).ne.6.and.yout(k).ne.1).and.
     +         (xout(k).ne.4.and.yout(k).ne.1).and.
     +         (xout(k).ne.7.and.yout(k).ne.1).and.
     +         (xout(k).ne.13.and.yout(k).ne.197).and.
     +         (xout(k).ne.57.and.yout(k).ne.13).and.
     +         (xout(k).ne.4.and.yout(k).ne.2)) then  !Not converting these params
                write(msunit,*) 
     +        ' Descriptor not recognized: X = ',xout(k),' Y = ',yout(k)
            end if
         end if
     
c
c if correct number of elements processed, Write QCF record to file
c
      if (nproc.eq.7) then
c
c use the day as the index into the precip array 
c and set it here - 10/21/96 RIS
c
c         write(msunit,*)'The day is ',q.date(2)
         read(q.date(2),'(I2)') dindx ! use day as index
c         write(msunit,*)'The index used for precip stuff is ', dindx
         dlyprcp(dindx) = q.precip
         dflag(dindx) = q.prcflg1
         read(q.qtime(1),'(I2)') obshour(dindx)

          call get_stn(msunit,q,no_stn,MAXSTNS,nstns,lat,lon,elev,
     +                 coopnum,stnseen,bdate,edate,sbegin,send,
     +                 comlat,comlon,stnnum)
c         write(q.statn, '(A10)') stnnum
          call wrdqcf(q,ounit)
          wtcnt = wtcnt + 1 
c now use day of month as index          dindx = dindx + 1
          nproc = 0
          call dqrset(q,.false.)
      end if 

      k = k + 1
  60  continue                         ! loop back to process next element

c
c Now write out daily precip record for this station-month
c
      call wdlypc(q,31,dlyprcp,dflag,obshour,punit,msunit)
      do 65, i = 1,31
         dlyprcp(i) = -999.99
         dflag(i) = 9
   65 continue
c
      pcount = pcount + 1
c      dindx = 1
c
c Loop back for next record
c
      goto 10
c
c End of E-BUFR file encountered; close files and print stats
c
 1000 continue
      write(msunit,*)'*** PROCESSING COMPLETE ***'
      goto 9999

c
c Processing error encountered; close files and print stats
c
 2000 continue 
      write(msunit,*)'*** PROCESSING ABORTED ***'

 9999 continue
      call clsfil(iunit,ifile,errnum,msunit)
      call clsfil(ounit,ofile,errnum,msunit)
      call clsfil(stnunit,stnfile,errnum,msunit)
      call clsfil(punit,pfile,errnum,msunit)

      call wrstns(sfile,stnunit,msunit,MAXSTNS,stnseen,tzone,
     +           lat,lon,coopnum,elev,name,st,bdate,edate,nstns)

      write(msunit,*) no_stn,' qcf recs with no entry in station file'
      write(msunit,*) rdcnt,' E-BUFR Records Read'
      write(msunit,*) wtcnt,' QCF Records Written'
      write(msunit,*) pcount,' Daily Precip Records Written'
      write(msunit,*) ' '
      write(msunit,*) 'Daily Precip QC Stats:'
      write(msunit,*) '		Unchecked	',ucnt
      write(msunit,*) '		Good		',gcnt
      write(msunit,*) '		Missing		',mcnt
      write(msunit,*) '		Dubious		',dcnt
      write(msunit,*) '		Bad		',bcnt
      write(msunit,*) '		Estimated	',ecnt
      write(msunit,*) '		Trace		',tcnt
      write(msunit,*) ' '
      write(msunit,*) 'Number of Bad Accumulations = ',acnt
      if (msunit .ne.6) then
          call clsfil(msunit,msfile,errnum,6)
      end if
      STOP
      END

c*********************************************************************
      subroutine get_stn(msunit,q,no_stn,mstns,nstns,lat,lon,elev,
     +                    coopnum,stnseen, bdate,edate,sbegin,send,
     +                    comlat,comlon,stnnum)
c*********************************************************************
c
c search station arrays for match.
c load station information into  header, and save station
c dates from data to build new station information file.
c
c parameters
c
      integer msunit,no_stn,mstns,nstns

      real elev(mstns)
      double precision lat(mstns),lon(mstns)
      double precision comlat,comlon
      character*10 coopnum(mstns)
      logical stnseen(mstns)
      character*8 bdate(mstns),edate(mstns),sbegin(mstns),send(mstns)
      character*10 stnnum

c***********************************************************************
c    Include file for QCF format
c***********************************************************************
      include "dqcf.inc"

      record /dqcf/ q
c
c local variables
c
      integer i
      integer imatch
      integer imcount
      integer savei
      character*8 tdate
      character*10 previd
      character*10 matchid


C
C Truncate coop num to match number of characters in
C station file (6).
C
      matchid = stnnum(1:6)
      write(tdate,'(A4,A2,A2)') q.year,q.date(1),q.date(2)

      imatch = 0
      savei = 0

      do 15, i = 1,nstns
          if (matchid(1:6) .eq. coopnum(i)(1:6)) then
             if (tdate.ge.sbegin(i).and.tdate.le.send(i)) then

                 imatch = imatch + 1  !increment number of matches found

c                 write (msunit, *) 'Found match - matchid = ',matchid,
c     +              ', coopnum(',i,') = ',coopnum(i)

                 elev(i) = q.staelv      !Use elev from data.
                 q.statn(1:6) = stnnum(1:6)
                 lat(i) = q.ll(1)        !Use lat from data.
                 lon(i) = q.ll(2)        !Use lon from data
                 if (stnseen(i).eq..false.) then
                    stnseen(i) = .true.
                    bdate(i)(1:4) = q.year
                    bdate(i)(5:6) = q.date(1)
                    bdate(i)(7:8) = q.date(2)
                    edate(i) = bdate(i)
                 else
                     edate(i)(1:4) = q.year
                     edate(i)(5:6) = q.date(1)
                     edate(i)(7:8) = q.date(2)
                 end if
             else if (tdate.lt.sbegin(i).or.tdate.gt.send(i)) then
                 savei = i       ! save index of partial match
c                 write (msunit, *) 'savei: ', savei, 'tdate: ', tdate
c                 write (msunit, *) 'sbegin(',i,'):', sbegin(i), 
c     +              'send(',i,'):', send(i) 
             end if
          end if

 15   continue

      if ( imatch .eq. 1 ) then  !found just one match
         goto 18
      else if (imatch .eq. 0 .and. savei .gt. 0) then  !partial match found
         write (msunit, *) 'WARNING:  Station ',stnnum,
     +         ' not open at time ',tdate
         q.staelv = elev(savei)
         q.statn = stnnum(1:6)
         q.ll(1) = lat(savei)
         q.ll(2) = lon(savei)
         if (stnseen(savei).eq..false.) then
            stnseen(savei) = .true.
            bdate(savei)(1:4) = q.year
            bdate(savei)(5:6) = q.date(1)
            bdate(savei)(7:8) = q.date(2)
            edate(savei) = bdate(i)
         else
             edate(savei)(1:4) = q.year
             edate(savei)(5:6) = q.date(1)
             edate(savei)(7:8) = q.date(2)
         end if
         goto 18
      else if ( imatch .gt. 1 ) then
c
c        Found more than one match for this stn's stnnum/tdate
c        in the provided metadata. Since not a unique match, declare
c        this stn as Unknown. Want to Reset stnseen flag to false. These stns
c        will be stripped out of data as Unknown. Don't want in final
c        stn list. To move on....left s/w asis and manually stripped 
c        multiples out of output stn list file.
c
         if ((previd .eq. '9999999999') .or. (previd.ne.stnnum) ) then

            imcount = imcount + 1  !increment number of stns with multi matches

            write(msunit,
     +      '(''found '',i5,'' Multiple matches -- last lat used'')' ) 
     +       imatch

            write(msunit,
     +      '(''Total found '',i5,'' Total Multiple matches:'')' ) 
     +       imcount

         end if
c
c        Don't set...consider multi matches unknown.
c
         previd = stnnum

      end if

      write(msunit,*) 'station info not found, stnnum = ',matchid,
     +              'date = ',tdate

      q.ll(1) = comlat
      q.ll(2) = comlon

      write(msunit,*) 'computed lat/lon for stn = ',q.ll(1),'  ',
     +                q.ll(2)

      q.staelv = -999.99

      if (imatch .gt. 1) then
         write(msunit,*) 'Set stn name to UnknownX'
         q.statn = "UnknownX"
      else
         q.statn = "Unknown"
      end if

      no_stn = no_stn + 1

 18   continue
 
C     write(msunit,'(/,''** Leaving get_stn subroutine **'',/)' )

      return
      end

c*********************************************************************
      subroutine gtflg(msunit,max,datum,fmt,xout,yout,k,flag1,flag2)
c*********************************************************************
c
c decode NCDC qc flags for each variable
c

         integer msunit
         integer k,max
         integer xout(max),yout(max)
         character*128 datum(max)
         character*8 fmt(max)
         integer flag1,flag2


      logical ERR
      real rval
      character*32 cval
      character*1 valtyp

c     write(msunit,'(/,''Entering gt_flg subroutine'')' )
          k = k + 1

          call getval(datum(k),xout(k),yout(k),fmt(k),rval,cval,
     +                    valtyp,msunit,ERR)
          if (ERR) then
              write(msunit,*)'*** PROCESSING ABORTED in getflg ***'
              stop
          end if

          if (xout(k).eq.13.and.yout(k).eq.195) then  !SOD FLag #1
              flag1 = int(rval)
          else
              write(msunit,*)'*** QC Flags out of Sequence ***'
              write(msunit,*)'*** PROCESSING ABORTED in getflg ***'
              stop
          end if

          k = k + 1

          call getval(datum(k),xout(k),yout(k),fmt(k),rval,cval,
     +                    valtyp,msunit,ERR)
          if (ERR) then
              write(msunit,*)'*** PROCESSING ABORTED in getflg ***'
              stop
          end if

          if (xout(k).eq.13.and.yout(k).eq.196) then  !SOD FLag #2
                flag2 = int(rval)
          else
              write(msunit,*)'*** QC Flags out of Sequence ***'
              write(msunit,*)'*** PROCESSING ABORTED in getflg ***'
              stop
          end if
c     write(msunit,'(/,''Leaving gt_flg subroutine'')' )
      RETURN
      END

c*********************************************************************
      subroutine wdlypc(q,size,dlyprcp,dlyflg,obshour,
     +                  punit,msunit)
c*********************************************************************
c
c QC the daily precip by checking against gross limits, then
c write daily record to seperate file.
c

c
c  paramters
c
c
c  qcf record layout
c
      include "dqcf.inc"

      record /dqcf/ q
c
      integer size
      real dlyprcp(size)
      integer dlyflg(size)
      integer obshour(size)
      integer punit,msunit

c
c routine variables
c
      character*1 qcflag(31)
      character*15 tmpnam
      integer qual(31)
      integer i,accdays
      logical ACCUM,BADREC
      real BADMAX,DUBMIN
      PARAMETER(BADMAX=127.0,DUBMIN=101.6)  !units are millimeters

c***********************************************************************
c  Common block for qc stats
c***********************************************************************
      integer ucnt,gcnt,mcnt,dcnt,bcnt,ecnt,tcnt,acnt
      common /stats/ ucnt,gcnt,mcnt,dcnt,bcnt,ecnt,tcnt,acnt

c
c Do QC on precip values.  dlyflg contains NCDC flags
c qcflag is filled with OFPS QC flags, qual with the precip qualifier flags
c
      ACCUM = .false.
      BADREC = .false.

cxxx  write(msunit, '(''Entering wdlypc subroutine'')' )
      do 50, i = 1,size
         qual(i) = 0
         if (dlyprcp(i).eq.-999.99.and.dlyflg(i).ne.5) then
c            write(msunit,*) 'Missing found, orig flag = ',dlyflg(5)
             qcflag(i) = 'M'
             qual(i) = 7
             mcnt = mcnt + 1
         else if (dlyflg(i).eq.5) then    !in accumulation
             if (.not.ACCUM) then
                ACCUM = .true.
c               write(msunit,1500) 'Begin Accumulation - ',
c    +                  q.year,q.date(1),q.qnet,q.statn,q.ll,q.occur
             end if
             accdays = accdays + 1
             qcflag(i) = 'U'
             ucnt = ucnt + 1
             qual(i) = 1
         else if (dlyflg(i).eq.1.or.dlyflg(i).eq.2) then
             if (ACCUM) then
                ACCUM = .false.
                accdays = accdays + 1
c               write(msunit,1500) 'End Accumulation - ',
c    +                  q.year,q.date(1),q.qnet,q.statn,q.ll,q.occur
c               write(msunit,*) 'Accumulation number of days = ',accdays
                accdays = 0
             else
c               write(msunit,*) 'End of accum with no beginning'
                acnt = acnt + 1
                BADREC = .true.
             end if
             qcflag(i) = 'U'
             ucnt = ucnt + 1
             qual(i) = 2
         else if (dlyflg(i).eq.3) then
             if (dlyprcp(i).gt.BADMAX) then
                 qcflag(i) = 'B'
                 bcnt = bcnt + 1
                 BADREC = .true.
             else
                 qcflag(i) = 'E'
                 ecnt = ecnt + 1
             end if
         else if (dlyflg(i).eq.6) then          !TRACE
             qcflag(i) = 'T'
             tcnt = tcnt + 1
         else if (dlyprcp(i).gt.BADMAX) then
             BADREC = .true.
             qcflag(i) = 'B'
             bcnt = bcnt + 1
         else if (dlyprcp(i).gt.DUBMIN) then
             qcflag(i) = 'D'
             dcnt = dcnt + 1
             BADREC = .true.
         else
             qcflag(i) = 'G'
             gcnt = gcnt + 1
         end if
  50  continue

c     if (ACCUM) then
c        write(msunit,*) 'Warning, Accumulation spans end of month'
c        acnt = acnt + 1
c        BADREC = .true.
c     end if

c
c write out daily precip file
c
      tmpnam = '               '
      tmpnam(1:6) = q.statn(1:6)
      write(punit,1000,err=2000) q.year,q.date(1), q.qnet, tmpnam,
     +      q.ll,q.occur,
     +      (dlyprcp(i),qual(i),qcflag(i),obshour(i),i=1,31) 

c
c log bad records to message file
c
c     if (BADREC) then
c        write(msunit,1000,err=2000) q.year,q.date(1),q.qnet,tmpnam,
c    +         q.ll,q.occur,
c    +         (dlyprcp(i),qual(i),qcflag(i),obshour(i),i=1,31) 
c     end if

cxxx  write(msunit, '(''Leaving wdlypc subroutine'')' )
      goto 9999

 1000 format(a4,'/',a2,1x,a10,1x,a15,1x,f10.5,
     *          1x,f11.5,1x,i3,
     *          31(1x,f7.2,1x,i1,1x,a1,1x,i2))

 1500 format(a25,1x,a4,'/',a2,1x,a10,1x,a15,1x,f10.5,
     *          1x,f11.5,1x,i3)

 2000 write(6,'('' Error writing record to the daily precip file'')' )
      stop

 9999 return
      end
