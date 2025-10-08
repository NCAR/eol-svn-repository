C
C $Id: hplainsqcf.f,v 1.1 1992/06/22 17:03:35 wayne Exp $
C $Log: hplainsqcf.f,v $
c Revision 1.1  1992/06/22  17:03:35  wayne
c Initial Version
c
C

      program hplainsqcf

c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     April, 1992
c
c     Purpose: Convert High Plains AWDN Network Hourly Obs to QCF format.
c
c     This program expects all the data to be in one file, in synoptic sort.
c     It produces one output file (without qcf header).  This output file
c     should then be sorted on time and station.   After the sort, the files should
c     be split into a single file per day, using the program "splt_hplains".
c
c     13 Jan 94 lec
c        Modified file names to be more generic for includes and output file.
c        Expanded date check beyond STORMFEST period to Jan-May. Added
c        check on temp flag for call to rh2dew() since need dry bulb
c        temp to compute dew pt.
c
c     01 Jan 95 lec
c        Now handles complete year, not project specific. Now set nominal
c        date/time to be same as actual time. This assumes data is always
c        taken exactly on the hour. So far this has always be true of all
c        hplains data processed.
c
c     14 Mar 96 ds
c        Added integer variable obs_yr for year.  Changed precision
c        of last 7 values read in from *.dat files to F8.3, to reflect
c        added decimal place in raw data.
c***********************************************************************
c    Include file for QCF format
c***********************************************************************
      include "../../inc/qcf.inc"

      record /qcf/ q
c***********************************************************************
c   Main program variables
c***********************************************************************
c
      integer iunit,ounit,msunit,i,err_flg,ieee,ieee_hand,ieee_handler
      integer rdcnt,wtcnt,filcnt,ios,errnum,prev_day
      integer*2 obs_mo,obs_day,obs_hr,obs_min
      integer*4 obs_yr
      integer stn_num,stn_ptr
      real celsus,rh2dew,mph2msec,in2mm
      real temp,rh,soil_temp,spd,dir,solar_in,prcp      
      logical FAIL,gtfils,FTIME
      character*50 ifile,ofile,msfile
      character*5 stn_id
      character*1 temp_flg,rh_flg,soil_flg,spd_flg,dir_flg
      character*1 solar_flg,prcp_flg
      CHARACTER*4  NETWRK(150)
      integer SITEID(150)
      CHARACTER*26 NAME(150)
      CHARACTER*2  ST(150)
      CHARACTER*70 BUFFER
      REAL  LAT(150)
      REAL  LON(150)
      INTEGER ELEV(150)
      integer gmt_off(150)
      integer index

c     external ieee_hand
c***********************************************************************
c  Start of main program code
c***********************************************************************
c
c Initialize variables
c
      rdcnt = 0
      wtcnt = 0
      filcnt = 0
      prev_day = 0
      ofile = "../out/hplains.cvt"
      ounit = 12
      FTIME = .true.
c
c Set IEEE error handler
c
c     ieee = ieee_handler('set','division',ieee_hand)
c     if (ieee.ne.0) print *,'Unable to set ieee handler'
c
c
c Get filenames and open files
c
      FAIL = gtfils(iunit,msunit,ifile,msfile)
      if (FAIL) then
          write(6,*) ' Aborting Program'
          stop
      end if
c
c Read in list of station locations from file
c
      call opnfil(15,"hplains.stns","OLD",errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*) 'ERROR in opening STATION FILE'
          goto 2000
      end if
      INDEX = 0
   1  continue
          READ ( 15, '(A70)', END=3 ) BUFFER
          INDEX = INDEX + 1
          NETWRK( INDEX ) = BUFFER(1:4)
          read(buffer(7:9),'(I4)') SITEID(INDEX)
          NAME(INDEX)   = BUFFER(13:38)
          ST(INDEX)     = BUFFER(40:41)
          READ ( BUFFER(46:52), '(F7.4)' ) LAT(INDEX)
          READ ( BUFFER(55:62), '(F8.4)' ) LON(INDEX)
          lon(index) = -1.00 * lon(index)
          READ ( BUFFER(64:67), '(I4)' ) ELEV(INDEX)
          read ( buffer(70:70), '(I4)') gmt_off(index)
          GOTO 1
   3  continue
      call clsfil(15,"hplains.stns",errnum,msunit)
c
c Open output data file
c
      open(unit=ounit,file=ofile,iostat=ios,status='UNKNOWN')
      if (ios.ne.0) then
          write(msunit,*) 'ERROR opening next data file'
          goto 2000
      end if
c
c Loop thru high plains data records and convert to QCF
c
      prev_day = 0
 10   continue
c   Format for pre-1995 data is commented out:
c          read(iunit,5,end=1000,err=2000) stn_id,obs_mo,obs_day,obs_hr,
c     +         obs_min,temp,temp_flg,rh,rh_flg,soil_temp,soil_flg,spd,
c     +         spd_flg,dir,dir_flg,solar_in,solar_flg,prcp,prcp_flg
c    5     format(A5,1X,I2,1X,I2,1X,I2,I2,1X,6(F7.2,A1,1X),F7.2,A1) 
c
          read(iunit,5,end=1000,err=2000) stn_id,obs_mo,obs_day,obs_yr,
     +         obs_hr,obs_min,temp,temp_flg,rh,rh_flg,soil_temp,
     +         soil_flg,spd,spd_flg,dir,dir_flg,solar_in,solar_flg,prcp,
     +         prcp_flg
    5     format(A5,1X,I2,1X,I2,1X,I4,1X,I2,I2,1X,6(F8.3,A1,1X),F8.3,A1) 
          rdcnt = rdcnt + 1
c
c Find station location info for this station
c
       read(stn_id(3:5),'(I4)') stn_num
       stn_ptr = 0
       do 30, i = 1,INDEX
           if (siteid(i) .eq. stn_num) then
               stn_ptr = i
           end if
   30  continue
       if (stn_ptr .eq. 0) then
           write(msunit,*)
     +         'ERROR:  Station Information not found for station: ',
     +          stn_id
           goto 2000
       end if
c
c Convert local time to UTC; 2400 obs to first obs (0000) of next day.
c NOTE: not a general solution; specific to STORMFEST Only....Extended
c       to include whole year.
c
      obs_hr = obs_hr + gmt_off(stn_ptr)
      if (obs_hr .ge. 24) then
          obs_hr = obs_hr - 24

          if (obs_mo .eq. 12) then
              if (obs_day .lt. 31) then
                   obs_day = obs_day + 1
              else
                  write(msunit,*)
     +              'Date Encountered Outside of Jan-Dec time period'
                  goto 2000
              end if
          else if (obs_mo .eq. 11) then
              if (obs_day .lt. 30) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 12
                  obs_day = 1
              end if
          else if (obs_mo .eq. 10) then
              if (obs_day .lt. 31) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 11
                  obs_day = 1
              end if
          else if (obs_mo .eq. 9) then
              if (obs_day .lt. 30) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 10
                  obs_day = 1
              end if
          else if (obs_mo .eq. 8) then
              if (obs_day .lt. 31) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 9
                  obs_day = 1
              end if
          else if (obs_mo .eq. 7) then
              if (obs_day .lt. 31) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 8
                  obs_day = 1
              end if
          else if (obs_mo .eq. 6) then
              if (obs_day .lt. 30) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 7
                  obs_day = 1
              end if
          else if (obs_mo .eq. 5) then
              if (obs_day .lt. 31) then
                   obs_day = obs_day + 1
              else
                 obs_mo =6 
                 obs_day = 1
              end if
          else if (obs_mo .eq. 4) then
              if (obs_day .lt. 30) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 5
                  obs_day = 1
              end if
          else if (obs_mo .eq. 3) then
              if (obs_day .lt. 31) then
                   obs_day = obs_day + 1
              else
                  obs_mo = 4 
                  obs_day = 1
              end if
          else if (obs_mo .eq. 2) then
              if (obs_day .lt. 29) then
                  obs_day = obs_day + 1
              else  
                  obs_mo = 3
                  obs_day = 1
              end if
          else if (obs_mo .eq. 1) then
              if (obs_day .lt. 31) then
                  obs_day = obs_day + 1
              else  
                  obs_mo = 2
                  obs_day = 1
              end if
          else
             write(msunit,*)
     +             'Date Encountered with BAD month value'
             goto 2000
          end if
      end if
c
c Initialize QCF record
c
      call qreset(q)

c
c Set Parameters that are "not observed"
c
          q.sqlflg = 'N'
          q.staflg = 'N'
          q.seaflg = 'N'
          q.cmpflg = 'N'
          q.pwflg = 'N'
          q.visflg = 'N'
          q.c1flg = 'N'
          q.c2flg = 'N'
          q.c3flg = 'N'
          q.ca1flg = 'N'
          q.ca2flg = 'N'
          q.ca3flg = 'N'

c
c Move info from hdr into qcf format
c
       q.nomdate(1) = "95"
       write(q.nomdate(2),'(I2.2)') obs_mo
       write(q.nomdate(3),'(I2.2)') obs_day
       write(q.nomtime(1),'(I2.2)') obs_hr
       write(q.nomtime(2),'(I2.2)') obs_min

       q.date(1) = "95"
       write(q.date(2),'(I2.2)') obs_mo
       write(q.date(3),'(I2.2)') obs_day
       write(q.qtime(1),'(I2.2)') obs_hr
       write(q.qtime(2),'(I2.2)') obs_min
       q.qnet = "HPLAINS"
       q.ll(1) = lat(stn_ptr)
       q.ll(2) = lon(stn_ptr)
       q.occur = 0
       q.statn = name(stn_ptr)
       q.staelv = elev(stn_ptr)

c
c Convert Data values and move into QCF rec
c
       if (temp_flg .ne. 'M' .and. temp_flg .ne. 'm') then
           q.temp = celsus(temp)
           if (temp_flg .eq. 'E' .or. temp_flg .eq. 'e') then
               q.tmpflg = 'E'
           else
               q.tmpflg = 'U'
           end if
       end if
       if (rh_flg .ne. 'M' .and. rh_flg .ne. 'm' .and.
     +     temp_flg .ne. 'M' .and. temp_flg .ne. 'm') then  !1Feb94-need dry bulb to compute dp
           q.dewpnt = rh2dew(rh,q.temp,real(elev(stn_ptr)),err_flg)
           if (err_flg.ne.0) then
              q.dewflg = 'M'
           else if (rh_flg .eq. 'E' .or. rh_flg .eq. 'e') then
               q.dewflg = 'E'
           else
               q.dewflg = 'U'
           end if
       end if
       if (spd_flg .ne. 'M' .and. spd_flg .ne. 'm') then
           q.wndspd = mph2msec(spd)
           if (spd_flg .eq. 'E' .or. spd_flg .eq. 'e') then
               q.spdflg = 'E'
           else
               q.spdflg = 'U'
           end if
       end if
       if (dir_flg .ne. 'M' .and. dir_flg .ne. 'm') then
           q.wnddir = dir
           if (dir_flg .eq. 'E' .or. dir_flg .eq. 'e') then
               q.dirflg = 'E'
           else
               q.dirflg = 'U'
           end if
       end if
       if (prcp_flg .ne. 'M' .and. prcp_flg .ne. 'm') then
           q.precip = in2mm(prcp)
           if (prcp_flg .eq. 'E' .or. prcp .eq. 'e') then
               q.prcflg = 'E'
           else
               q.prcflg = 'U'
           end if
       end if

c
c Write QCF record to file
c
      call qcfrec(q,ounit)

       wtcnt = wtcnt + 1 

c
c Loop back for next record
c
      goto 10
c
c End of High Plains file encountered; close files and print stats
c
 1000 continue
      call clsfil(iunit,ifile,errnum,msunit)
      call clsfil(ounit,ofile,errnum,msunit)
      write(msunit,*)'*** PROCESSING COMPLETE ***'
      write(msunit,*) rdcnt,' Records Read'
      write(msunit,*) wtcnt,' QCF Records Written'
      if (msunit.ne.6) then
          call clsfil(msunit,msfile,errnum,6)
      end if
      STOP

c
c Processing error encountered; close files and print stats
c
 2000 continue 
      call clsfil(iunit,ifile,errnum,msunit)
      call clsfil(ounit,ofile,errnum,msunit)
      write(msunit,*)'*** PROCESSING ABORTED ***'
      write(msunit,*) rdcnt,' Records Read'
      write(msunit,*) wtcnt,' QCF Records Written'
      if (msunit .ne.6) then
          call clsfil(msunit,msfile,errnum,6)
      end if
      STOP
      END
