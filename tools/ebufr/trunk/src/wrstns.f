C
C $Id$
C $Log$
C

c
c Subroutine to write out station information to flat file
c from arrays
c
      subroutine wrstns(stnfile,stnunit,msunit,mstns,stnseen,tzone,
     +                  lat,lon,coopnum,elev,name,st,bdate,edate,nstns)

      character*50 stnfile
      integer stnunit,msunit,mstns


      double precision lat(mstns),lon(mstns)
      real   elev(mstns),tzone(mstns)

      character*10 coopnum(mstns)
      character*2 st(mstns)
      character*20 name(mstns)
      character*8 bdate(mstns),edate(mstns)
      logical stnseen(mstns)
      integer nstns

      integer i,stncnt,errnum,j,ncodes
      character*2 sabbr(100),cocode(100),nccode(100)
      character*2 state
      character*50 tmpnam
      character*15 tmpnet

      call opnfil(stnunit,stnfile,"UNK",errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*) 'ERROR in opening Output STATION FILE'
          stop
      end if

c
c load arrays to map ncdc state codes to codiac codes
c
      call opnfil(20,"states.ncdc","UNK",errnum,msunit)
      if (errnum .ne. 0) then
          write(msunit,*) 'ERROR in opening input States File'
          stop
      end if

      j = 1
      ncodes = 0
   5  continue
      read(20,'(A2,1x,A2,1x,A2)',END=7) cocode(j),nccode(j),sabbr(j)
      j = j + 1
      ncodes = ncodes + 1
      goto 5
   7  continue
     

c
c loop to write out stations
c
      stncnt = 0
      do 10, i = 1, nstns
         if (stnseen(i)) then
c
c convert state abbr to text
c
            state = '??'
            do 8, j = 1,ncodes
               if (nccode(j).eq.st(i)) then
                  state = cocode(j)
                  goto 9
               endif
 8          continue
            if (state.eq.'??') then
               write(msunit,*)'State Code not found, code = ',st(i)
            end if
 9          continue
c
c write the station record
c
            tmpnam(1:20) = name(i)
            tmpnet(1:15) = "STORM-WAVE"
            write(stnunit,1000,err=1500) tmpnet,
     +            0,
     +            lat(i),
     +            lon(i),
     +            0,
     +            2,
     +            tmpnam,
     +            bdate(i),
     +            edate(i),
     +            "US",
     +            state,
     +            "???",
     +            tzone(i),
     +            "y",
     +             2,
     +            "daily          ",
     +            elev(i),
     +            "f",
     +            coopnum(i)(1:6)
            stncnt = stncnt + 1
         end if
  10  continue
      goto 9999

 1000 format(A15,1x,I10,1x,F10.5,1x,F11.5,1x,I3,1x,I5,1x,A20,1x,a8,1x,
     + a8,1x,A2,1x,A2,1x,A3,1x,f6.2,1x,A1,1x,I4,1x,A15,f9.1,1x,A1,1x,A6)

 1500 write(msunit,*) 'ERROR in writing Output STATION FILE'

 9999 write(msunit,*) 'Input station count = ',nstns
      write(msunit,*) 'Output station count = ',stncnt
      call clsfil(stnunit,stnfile,errnum,msunit)

      return
      end

