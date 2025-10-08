C
C $Id$
C $Log$
C

c
c Subroutine to load station information from flat file
c into arrays.  checks for station moves, location changes,
c and prints appropriate warnings
c
      subroutine stninf(stnfile,stnunit,msunit,mstns,lat,lon,coopnum,
     +                  elev,name,st,sbegin,send,tzone,nstns)

      character*50 stnfile
      integer stnunit,msunit,mstns
      double precision lat(mstns),lon(mstns)
      real elev(mstns)
      real tzone(mstns)
      character*10 coopnum(mstns)
      character*2 st(mstns)
      character*20 name(mstns)
      character*8 sbegin(mstns),send(mstns)
      integer nstns

      character*121 buffer
      double precision tlat,tlon,glat,glon
      real temp,temp1,telev
      character*1 dir
      integer errnum,elevcnt,movecnt
      integer INDEX
      character*20 tbuf

      elevcnt = 0
      movecnt = 0
      INDEX = 0
c
c loop until stations read
c
   1  continue
          READ ( stnunit, '(A120)', END=3 ) BUFFER
c
c convert latitude to decimal degrees from degrees, minutes
c
          READ ( BUFFER(50:51), '(F2.0)' ) temp	
          READ ( BUFFER(52:53), '(F2.0)' ) temp1	
          READ ( BUFFER(54:54), '(A1)' ) dir
C
C         Round number to 4 places.
C
c          write(*,*) 'temp = ',temp,' temp1 = ',temp1

          tlat = temp + (temp1 / 60.0)  + 0.00005
          write(tbuf,'(f10.4)') tlat
          read(tbuf,'(f10.4)') tlat

cxxx      write(msunit,'('' tlat = '',f13.7)') tlat
c         tlat = temp + (temp1 / 100.0) + 0.001  !Make decimal degrees wrong to match E-BUFR err
c         write(tbuf,'(f7.2)') tlat
c         read(tbuf,'(f7.2)') tlat
c         write(*,*) tlat

C         Think there's something WRONG with glat and glon!!!
          glat = temp + (temp1 / 60.0)  !convert degrees,minutes to decimal degrees

          if (dir.eq.'S') then
              tlat = -1.00 * tlat
              glat = -1.00 * glat
          end if

c
c convert longitude to decimal degrees from degrees, minutes
c 
          READ ( BUFFER(56:58), '(F3.0)' ) temp	
          READ ( BUFFER(59:60), '(F2.0)' ) temp1	
          READ ( BUFFER(61:61), '(A1)' ) dir

c         write(*,*) 'temp = ',temp,' temp1 = ',temp1

          tlon = temp + (temp1 / 60.0) + 0.00005
          write(tbuf,'(f11.4)') tlon
          read(tbuf,'(f11.4)') tlon

cxxx      write(msunit,'('' tlon = '',f13.7)') tlon
c         write(*,'(f10.5)') tlon
c         tlon = temp + (temp1 / 100.)  +0.001 !Make decimal degrees wrong to match E-BUFR err
c         write(tbuf,'(f8.2)') tlon
c         read(tbuf,'(f8.2)') tlon
c         write(*,*) tlon

C         Think there's something WRONG with glat and glon!!!
          glon = temp + (temp1 / 60.0)   !convert degrees, minutes to decimal degrees

          if (dir.eq.'W') then
              tlon = -1.00 * tlon
              glon = -1.00 * glon
          end if

c
c Check to see if station is in the STORM-WAVE area
c   
c          if ((tlat.ge.30.00.and.tlat.le.45.00).and.
c     +        (tlon.ge.-109.00.and.tlon.le.-85.00)) then
c              goto 2
c          else
c              goto 1
c          end if

c
c read elevation, already in meters
c
    2     READ ( BUFFER(63:68), '(F6.1)' ) telev
c          write (msunit, *) 'Elevation = ', telev
c
c Check for blank chars at start of BUFFER.  Implies multiple entries for a station
c Try to determine if station moved, etc.
c
          if (BUFFER(1:2).eq.'  ') then
             if (tlat.eq.lat(INDEX).and.tlon.eq.lon(INDEX)) then
                 if (telev.eq.elev(INDEX)) then
                     send(INDEX) = BUFFER(41:48) !Station location has not changed
                     goto 1                      !set new end date and goto next entry
                 else
                     write(msunit,*) 
     +                'WARNING: 2 elevs; coop-num = ',coopnum(INDEX)
                     st(INDEX+1) = st(INDEX)
                     name(INDEX+1) = name(INDEX)
                     INDEX = INDEX + 1
                     elevcnt = elevcnt + 1
                 end if
             else               !station move
                 st(INDEX+1) = st(INDEX)
                 name(INDEX+1) = name(INDEX)
                 INDEX = INDEX + 1
                 movecnt = movecnt + 1
             end if
          else
              INDEX = INDEX + 1
              st(INDEX) = BUFFER(22:23)           !Coop State Code
              name(INDEX) = BUFFER(1:20)       !station name
          end if

c
c now pick apart rest of line
c
          coopnum(INDEX)   = BUFFER(22:27)  ! Coop station number
          lat(INDEX) = tlat
          lon(INDEX) = tlon

c23456789012345678901234567890123456789012345678901234567890123456789012
cxxx      write(msunit,'(\,''setting lat(INDEX) to '',f13.7)' )
cxxx +          lat(INDEX)

          elev(INDEX) = telev
          sbegin(INDEX) = BUFFER(32:39)
          send(INDEX) = BUFFER(41:48)
          tbuf = BUFFER(82:84)
          if (tbuf.eq."CEN") then
             tzone(INDEX) = 6.0
          else if (tbuf.eq."EAS") then
             tzone(INDEX) = 5.0
          else if (tbuf.eq."MTN") then
             tzone(INDEX) = 7.0
          else if (tbuf.eq."PAC") then
             tzone(INDEX) = 8.0
          else
             write(msunit,*) 'Unknown Time Zone in station file'
             write(msunit,*) 'tz = ',tbuf,' coopnum = ',coopnum(INDEX)
          end if

          GOTO 1                            ! Loop back for next record

   3  continue

      write(msunit,*) 'Number of Station Moves = ',movecnt
      write(msunit,*) 'Number of Elevation Changes = ',elevcnt
      nstns = INDEX
      call clsfil(stnunit,stnfile,errnum,msunit)
      write(*,*) 'Number of Station Moves = ',movecnt
      write(*,*) 'Number of Elevation Changes = ',elevcnt
      return
      end

