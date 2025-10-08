C
CWARNING: Use stnstat instead of this program. This program has NOT been
C   kept up to date. E.G., this program does not handle 'I' flag.
C
C******************************************************************************
C
C program: stnstat2
C author:  K. Scully
C date:    06/08/92
C description: Print kickout statistics by station/day.
C              List only the worst xxx stations for each parameter.
C
C usage:   stnstat [-debug]
C        
C              where -debug     option will print variable info as processing
C                               is done.
C
C 07 Feb 94 lec
C  Modified so that will also handle qcf files with both nominal and observation
C  times.
C 25 Aug 94 lec
C  Update s/w to handle 15 char stn ID's instead of 10 chars.
C******************************************************************************

      program stnstat2

      implicit none

      include 'qcglobal.h'
      include 'stnstatcom.h'
      include 'debug.h'
      include 'ioerr.h'

C misc. variables
      character*9 qcftime,getqcftime
	 integer*2 qcfday
      integer iargc,narg
      character*9 arg1,arg2
      integer*2 getqcfrec,getqcfrec2
      character*60 fname
      integer nfile /0/			! count of files read
      integer nread /0/			! count of records read
      integer nrec /0/			! count of records within day range

C************************** end variable definitions *********************

      debug = .false.
      qcf_format = 0  !1=recs contain nominal time, also
C get command line arguments
      narg = iargc()
      if(narg .eq. 0) then
         continue
      else if(narg .eq. 1) then
         call getarg(1,arg1)
         if(arg1 .eq. '-debug' .or. arg1 .eq. -DEBUG) then
            debug = .true.
            write(0,*) 'Running in DEBUG mode'
         else if(arg1 .eq. '-nom' .or. arg1 .eq. '-nominal') then
            qcf_format = 1
            write(0,*) 'Running with nominal option '
         end if
      else if(narg .eq. 2) then
         call getarg(1,arg1)
         if(arg1 .eq. '-debug' .or. arg1 .eq. -DEBUG) then
            debug = .true.
            write(0,*) 'Running in DEBUG mode'
         else if(arg1 .eq. '-nom' .or. arg1 .eq. '-nominal') then
            qcf_format = 1
         end if
         call getarg(2,arg2)
         if(arg2 .eq. '-debug' .or. arg2 .eq. -DEBUG) then
            debug = .true.
            write(0,*) 'Running in DEBUG mode'
         else if(arg2 .eq. '-nom' .or. arg2 .eq. '-nominal') then
            qcf_format = 1
         end if
      else  
         write(0,*) 'Invalid number of arguments'
            goto 999
      end if

C loop through the files
      do while (.true.)
         read (*,1001) fname
         if(fname .eq. 'quit' .or. fname .eq.'q' .or.
     +      fname .eq. 'exit' .or. fname .eq.'e' .or.
     +      fname .eq. 'stop') goto 900

C open the qc_stns file
	    write(0,*) 'processing file: ',fname
         open(unit_qcf,name=fname,status='old',iostat=ios,err=901)
         nfile=nfile+1

C for each file, loop through the file of qcf input records
         do while (.true.)
            if(qcf_format .eq. 1) then
               if(getqcfrec2() .eq. -2) goto 200        ! -2 = eof
            else
               if(getqcfrec() .eq. -2) goto 200         ! -2 = eof
            end if
            nread=nread+1
            qcftime=getqcftime()
            read(qcftime,1002) qcfday
C skip records outside the range of firstday and lastday
            if(qcfday .lt. FIRSTDAY  .or.  qcfday .gt. LASTDAY) goto 100

            call addstats(qcfday)
            nrec=nrec+1
100      continue
         end do          ! end of while loop for each record in the file
200      close(unit_qcf)
         goto 300
901      errstr=gerror(errstr)
         write(0,*) 'stnstat(): I/O Error: ',ios,' ',errstr
         write(0,*) '           Continuing with the next file...'
300   continue
      end do          ! end of while loop for each qcf file

C print the station stat report
900   call prtstats(nfile,nread,nrec,qcfday)
999   call standard_arithmetic()
      stop
1001  format(a60)
1002  format(2x,i3,4x)
      end
C************************************************************************
C
C function: addstats()
C description: calculate final statistics
C
C************************************************************************

      subroutine addstats(qcfday)

      implicit none

      include 'qcglobal.h'
      include 'stnstatcom.h'
      include 'qcfcom.h'
      include 'stncom.h'
      include 'debug.h'

	 integer*2 qcfday
	 integer*2 i,j,hr
	 integer*2 getplatform,pi,di,hi,si
	 integer getstncode
	 character*1 qcflg

	 if(debug) write(0,*) 'in addstats(): '

	 pi = getplatform()
	 si = getstncode()
	 di = qcfday-FIRSTDAY+1
      read(qcf.hr,1001) hr
      hi = hr+1
      do i=1,NPRTPARMS
         if(i .eq. 9) then
	       qcflg=qcf.squal.qc
	    else
	       qcflg=qcf.parms(i).qc
	    end if
         if(qcflg .eq. 'G') then
            stats(i,1)=stats(i,1)+1
            p(pi,i,1)=p(pi,i,1)+1
            s(si,i,1)=s(si,i,1)+1
            d(di,i,1)=d(di,i,1)+1
            h(hi,i,1)=h(hi,i,1)+1
         endif
         if(qcflg .eq. 'B')then
            stats(i,2)=stats(i,2)+1
            p(pi,i,2)=p(pi,i,2)+1
            s(si,i,2)=s(si,i,2)+1
            d(di,i,2)=d(di,i,2)+1
            h(hi,i,2)=h(hi,i,2)+1
         endif
         if(qcflg .eq. 'D') then
            stats(i,3)=stats(i,3)+1
            p(pi,i,3)=p(pi,i,3)+1
            s(si,i,3)=s(si,i,3)+1
            d(di,i,3)=d(di,i,3)+1
            h(hi,i,3)=h(hi,i,3)+1
         endif
         if(qcflg .eq. 'M') then
            stats(i,4)=stats(i,4)+1
            p(pi,i,4)=p(pi,i,4)+1
            s(si,i,4)=s(si,i,4)+1
            d(di,i,4)=d(di,i,4)+1
            h(hi,i,4)=h(hi,i,4)+1
         endif
         if(qcflg .eq. 'N') then
            stats(i,5)=stats(i,5)+1
            p(pi,i,5)=p(pi,i,5)+1
            s(si,i,5)=s(si,i,5)+1
            d(di,i,5)=d(di,i,5)+1
            h(hi,i,5)=h(hi,i,5)+1
         endif
         if(qcflg .eq. 'X') then
            stats(i,6)=stats(i,6)+1
            p(pi,i,6)=p(pi,i,6)+1
            s(si,i,6)=s(si,i,6)+1
            d(di,i,6)=d(di,i,6)+1
            h(hi,i,6)=h(hi,i,6)+1
         endif
         if(qcflg .eq. 'U') then
            stats(i,7)=stats(i,7)+1
            p(pi,i,7)=p(pi,i,7)+1
            s(si,i,7)=s(si,i,7)+1
            d(di,i,7)=d(di,i,7)+1
            h(hi,i,7)=h(hi,i,7)+1
         endif
         if(qcflg .eq. 'E') then
            stats(i,8)=stats(i,8)+1
            p(pi,i,8)=p(pi,i,8)+1
            s(si,i,8)=s(si,i,8)+1
            d(di,i,8)=d(di,i,8)+1
            h(hi,i,8)=h(hi,i,8)+1
         endif
      end do
	 i=10
      if(qcf.qcpw .eq. 'G') stats(i,1)=stats(i,1)+1
      if(qcf.qcpw .eq. 'B') stats(i,2)=stats(i,2)+1
      if(qcf.qcpw .eq. 'D') stats(i,3)=stats(i,3)+1
      if(qcf.qcpw .eq. 'M') stats(i,4)=stats(i,4)+1
      if(qcf.qcpw .eq. 'N') stats(i,5)=stats(i,5)+1
      if(qcf.qcpw .eq. 'X') stats(i,6)=stats(i,6)+1
      if(qcf.qcpw .eq. 'U') stats(i,7)=stats(i,7)+1
      if(qcf.qcpw .eq. 'E') stats(i,8)=stats(i,8)+1
	 i=11
      if(qcf.vis.qc .eq. 'G') stats(i,1)=stats(i,1)+1
      if(qcf.vis.qc .eq. 'B') stats(i,2)=stats(i,2)+1
      if(qcf.vis.qc .eq. 'D') stats(i,3)=stats(i,3)+1
      if(qcf.vis.qc .eq. 'M') stats(i,4)=stats(i,4)+1
      if(qcf.vis.qc .eq. 'N') stats(i,5)=stats(i,5)+1
      if(qcf.vis.qc .eq. 'X') stats(i,6)=stats(i,6)+1
      if(qcf.vis.qc .eq. 'U') stats(i,7)=stats(i,7)+1
      if(qcf.vis.qc .eq. 'E') stats(i,8)=stats(i,8)+1
	 j=12
      do i=1,3
         if(qcf.cc(i).ceilqc .eq. 'G') stats(j,1)=stats(j,1)+1
         if(qcf.cc(i).ceilqc .eq. 'B') stats(j,2)=stats(j,2)+1
         if(qcf.cc(i).ceilqc .eq. 'D') stats(j,3)=stats(j,3)+1
         if(qcf.cc(i).ceilqc .eq. 'M') stats(j,4)=stats(j,4)+1
         if(qcf.cc(i).ceilqc .eq. 'N') stats(j,5)=stats(j,5)+1
         if(qcf.cc(i).ceilqc .eq. 'X') stats(j,6)=stats(j,6)+1
         if(qcf.cc(i).ceilqc .eq. 'U') stats(j,7)=stats(j,7)+1
         if(qcf.cc(i).ceilqc .eq. 'E') stats(j,8)=stats(j,8)+1
	    j=j+1
         if(qcf.cc(i).cldqc .eq. 'G') stats(j,1)=stats(j,1)+1
         if(qcf.cc(i).cldqc .eq. 'B') stats(j,2)=stats(j,2)+1
         if(qcf.cc(i).cldqc .eq. 'D') stats(j,3)=stats(j,3)+1
         if(qcf.cc(i).cldqc .eq. 'M') stats(j,4)=stats(j,4)+1
         if(qcf.cc(i).cldqc .eq. 'N') stats(j,5)=stats(j,5)+1
         if(qcf.cc(i).cldqc .eq. 'X') stats(j,6)=stats(j,6)+1
         if(qcf.cc(i).cldqc .eq. 'U') stats(j,7)=stats(j,7)+1
         if(qcf.cc(i).cldqc .eq. 'E') stats(j,8)=stats(j,8)+1
	    j=j+1
      end do
      return
1001  format(i2)
      end
C************************************************************************
C
C function: prtstats()
C description: print final statistics
C
C 24 Jan 93 lec
C   Added def of ndays to make s/w more generic. Value was hardcoded as 44.
C   Add additional initialization and checks for divisions by zero.
C************************************************************************

      subroutine prtstats(nfile,nread,nrec,qcfday)

      implicit none

      include 'qcglobal.h'
      include 'stnstatcom.h'
      include 'qcfcom.h'
      include 'stncom.h'
      include 'platform.h'
      include 'parmnames.h'
      include 'debug.h'

	 integer*2 qcfday
	 integer nfile,nread,nrec
	 integer tstats(NQCFLAGS)
	 integer nobs,nqc
      real    pqc,pg,pflg,pb,pd
	 integer*2 i,j,k
	 integer tm,time
	 integer ndays
	 character*26 ctime
	 integer*2 MAXBADSTN
	 parameter (MAXBADSTN=10)		!max # worst stations per platform
	 structure /s2rec/
	    integer*2 stn
	    real pg
	 end structure
	 record /s2rec/ s2(MAXPLATFORM,MAXBADSTN)
	 integer*2 getplatform,n,m

	 if(debug) write(*,*) 'in prtstats(): '
	 tm=time()
C write out final summary statistics
      write(*,1001) ctime(tm),nfile,nread,nrec
      nobs = 0
      nqc = 0
      do i=1,NQCPARMS
         nobs=stats(i,1)+stats(i,2)+stats(i,3)+stats(i,7)+stats(i,8)
         nqc=stats(i,1)+stats(i,2)+stats(i,3)+stats(i,8)
         if (nobs .le. 0) then
            nobs = 0
            pqc = 0
         else
            pqc=100.* real(nqc)/real(nobs)
         end if

         if (nqc .le. 0) then
            nqc = 0
            pg = 0
            pflg = 0
            pb = 0
            pd = 0
         else
            pg=100.* real(stats(i,1)+stats(i,8))/real(nqc)
            pflg=100.* real(stats(i,2)+stats(i,3))/real(nqc)
            pb=100.* real(stats(i,2))/real(nqc)
            pd=100.* real(stats(i,3))/real(nqc)
         end if 

         write(*,1002) parmnames(i),nobs,nqc,(stats(i,j),j=1,NQCFLAGS),
     +                 pqc,pg,pflg,pb,pd
      end do
      do k=1,NQCFLAGS
         tstats(k)=0
      end do
      do i=1,NPRTPARMS
         do j=1,NQCFLAGS
            tstats(j)=tstats(j)+stats(i,j)
         end do
      end do
      nobs = 0
      nqc = 0
      nobs=tstats(1)+tstats(2)+tstats(3)+tstats(7)+tstats(8)
      nqc=tstats(1)+tstats(2)+tstats(3)+tstats(8)
      if (nobs .le. 0) then
         nobs = 0
         pqc = 0
      else
         pqc=100.* real(nqc)/real(nobs)
      end if

      if (nqc .le. 0) then
         nqc = 0
         pg = 0
         pflg = 0
         pb = 0
         pd = 0
      else
         pg=100.* real(tstats(1)+tstats(8))/real(nqc)
         pflg=100.* real(tstats(2)+tstats(3))/real(nqc)
         pb=100.* real(tstats(2))/real(nqc)
         pd=100.* real(tstats(3))/real(nqc)
      end if

      write(*,*)
      write(*,1002) 'Grand Totals (1-8)  ',nobs,nqc,
     +    (tstats(j),j=1,NQCFLAGS),pqc,pg,pflg,pb,pd

C write out statistics by platform
      write(*,1011)
      do i=1,np
        write(*,1012) platform(i)
        do j=1,NPRTPARMS
           nobs = 0
           nqc = 0
           nobs=p(i,j,1)+p(i,j,2)+p(i,j,3)+p(i,j,7)+p(i,j,8)
           nqc=p(i,j,1)+p(i,j,2)+p(i,j,3)+p(i,j,8)

           if (nobs .le. 0) then
              nobs = 0
              pqc = 0
           else
              pqc=100.* real(nqc)/real(nobs)
           end if

           if (nqc .le. 0) then
              nqc = 0
              pg = 0
              pflg = 0
              pb = 0
              pd = 0
           else
             pg=100.* real(p(i,j,1)+p(i,j,8))/real(nqc)
             pflg=100.* real(p(i,j,2)+p(i,j,3))/real(nqc)
             pb=100.* real(p(i,j,2))/real(nqc)
             pd=100.* real(p(i,j,3))/real(nqc)
           end if

           write(*,1002) parmnames(j),nobs,nqc,(p(i,j,k),k=1,NQCFLAGS),
     +                   pqc,pg,pflg,pb,pd
        end do
        do k=1,NQCFLAGS
           tstats(k)=0
        end do
        do j=1,NPRTPARMS
           do k=1,NQCFLAGS
              tstats(k)=tstats(k)+p(i,j,k)
           end do
        end do

        nobs = 0
        nqc = 0
        nobs=tstats(1)+tstats(2)+tstats(3)+tstats(7)+tstats(8)
        nqc=tstats(1)+tstats(2)+tstats(3)+tstats(8)
        if (nobs .le. 0) then
            nobs = 0
            pqc = 0
        else
            pqc=100.* real(nqc)/real(nobs)
        end if

        if (nqc .le. 0) then
            nqc = 0
            pg = 0
            pflg = 0
            pb = 0
            pd = 0
        else
            pg=100.* real(tstats(1)+tstats(8))/real(nqc)
            pflg=100.* real(tstats(2)+tstats(3))/real(nqc)
            pb=100.* real(tstats(2))/real(nqc)
            pd=100.* real(tstats(3))/real(nqc)
        end if

        write(*,*)
        write(*,1002) 'Grand Totals        ',nobs,nqc,
     +      (tstats(j),j=1,NQCFLAGS),pqc,pg,pflg,pb,pd
      end do

C write out statistics by parameter for worst offending stations
      nqc = 0
      pg = 0
      do j=1,NPRTPARMS
        write(*,1051) parmnames(j)
        write(*,1022)
        do n=1,np
           do k=1,MAXBADSTN
	   	    s2(n,k).stn=0
	   	    s2(n,k).pg=0.0
           end do
        end do
        do i=1,nstns
           nqc=s(i,j,1)+s(i,j,2)+s(i,j,3)+s(i,j,8)
		 if(nqc .le. 0) goto 50
           pg=real(s(i,j,1)+s(i,j,8))/real(nqc)
		 if(pg .eq. 1.0) goto 50
	      qcf.network = stns(i).network
	      n = getplatform()
           do k=1,MAXBADSTN
C     write(*,*) 'j=',j,'; n=',n,'; k=',k,'; np=',np,'; i=',i,
C    +           '; pg=',pg,'; s2(n,k).pg=',s2(n,k).pg,'; nqc=',nqc
              if(s2(n,k).stn .eq. 0 .or. pg .lt. s2(n,k).pg) then
                 do m=MAXBADSTN,k+1,-1
	      	     s2(n,m).stn=s2(n,m-1).stn
	      	     s2(n,m).pg=s2(n,m-1).pg
                 end do
	      	  s2(n,k).stn=i
	      	  s2(n,k).pg=pg
			  goto 50
              end if
           end do
50         continue
        end do
        do n=1,np
           do k=1,MAXBADSTN
		    i=s2(n,k).stn
		    if(i .le. 0) goto 60

              nobs = 0
              nqc = 0
              nobs=s(i,j,1)+s(i,j,2)+s(i,j,3)+s(i,j,7)+s(i,j,8)
              nqc=s(i,j,1)+s(i,j,2)+s(i,j,3)+s(i,j,8)

              if (nobs .le. 0) then
                nobs = 0
                pqc = 0
              else
                pqc=100.* real(nqc)/real(nobs)
              end if

              if (nqc .le. 0) then
                nqc = 0
                pg = 0
                pflg = 0
                pb = 0
                pd = 0
              else
                pg=100.* real(s(i,j,1)+s(i,j,8))/real(nqc)
                pflg=100.* real(s(i,j,2)+s(i,j,3))/real(nqc)
                pb=100.* real(s(i,j,2))/real(nqc)
                pd=100.* real(s(i,j,3))/real(nqc)
              end if

              write(*,1023) platform(n),stns(i).station,nobs,nqc,
     +           (s(i,j,m),m=1,NQCFLAGS),pqc,pg,pflg,pb,pd
           end do
60         continue
        end do
      end do

C write out statistics by station
C       do j=1,NPRTPARMS
C       write(*,1061) parmnames(j)
      write(*,1021)
      write(*,1022)
      do i=1,nstns
        do k=1,NQCFLAGS
           tstats(k)=0
        end do
        do j=1,NPRTPARMS
           do k=1,NQCFLAGS
              tstats(k)=tstats(k)+s(i,j,k)
           end do
        end do

        nobs = 0
        nqc = 0

        nobs=tstats(1)+tstats(2)+tstats(3)+tstats(7)+tstats(8)
        nqc=tstats(1)+tstats(2)+tstats(3)+tstats(8)

         if (nobs .le. 0) then
            nobs = 0
            pqc = 0
         else
            pqc=100.* real(nqc)/real(nobs)
         end if

         if (nqc .le. 0) then
            nqc = 0
            pg = 0
            pflg = 0
            pb = 0
            pd = 0
         else
            pg=100.* real(tstats(1)+tstats(8))/real(nqc)
            pflg=100.* real(tstats(2)+tstats(3))/real(nqc)
            pb=100.* real(tstats(2))/real(nqc)
            pd=100.* real(tstats(3))/real(nqc)
        end if

        write(*,1023) stns(i).network,stns(i).station,nobs,nqc,
     +      (tstats(m),m=1,NQCFLAGS),pqc,pg,pflg,pb,pd
      end do
C       end do

C write out statistics by day
      ndays = LASTDAY - FIRSTDAY +1
      write(*,1031)
      write(*,1032)
      do i=1,ndays
        do k=1,NQCFLAGS
           tstats(k)=0
        end do
        do j=1,NPRTPARMS
           do k=1,NQCFLAGS
              tstats(k)=tstats(k)+d(i,j,k)
           end do
        end do

        nobs = 0
        nqc = 0
        nobs=tstats(1)+tstats(2)+tstats(3)+tstats(7)+tstats(8)
        nqc=tstats(1)+tstats(2)+tstats(3)+tstats(8)

        if (nobs .le. 0) then
           nobs = 0
           pqc = 0
        else
           pqc=100.* real(nqc)/real(nobs)
        end if

        if (nqc .le. 0) then
           nqc = 0
           pg = 0
           pflg = 0
           pb = 0
           pd = 0
        else
           pg=100.* real(tstats(1)+tstats(8))/real(nqc)
           pflg=100.* real(tstats(2)+tstats(3))/real(nqc)
           pb=100.* real(tstats(2))/real(nqc)
           pd=100.* real(tstats(3))/real(nqc)
        end if

        write(*,1033) i+FIRSTDAY-1,nobs,nqc,
     +      (tstats(j),j=1,NQCFLAGS),pqc,pg,pflg,pb,pd
      end do

C write out statistics by hour
      write(*,1041)
      write(*,1042)
      do i=1,24
        do k=1,NQCFLAGS
           tstats(k)=0
        end do
        do j=1,NPRTPARMS
           do k=1,NQCFLAGS
              tstats(k)=tstats(k)+h(i,j,k)
           end do
        end do

        nobs = 0
        nqc = 0
        nobs=tstats(1)+tstats(2)+tstats(3)+tstats(7)+tstats(8)
        nqc=tstats(1)+tstats(2)+tstats(3)+tstats(8)
        if (nobs .le. 0) then
           nobs = 0
           pqc = 0
        else
           pqc=100.* real(nqc)/real(nobs)
        end if

        if (nqc .le. 0) then
           nqc = 0
           pg = 0
           pflg = 0
           pb = 0
           pd = 0
        else
           pg=100.* real(tstats(1)+tstats(8))/real(nqc)
           pflg=100.* real(tstats(2)+tstats(3))/real(nqc)
           pb=100.* real(tstats(2))/real(nqc)
           pd=100.* real(tstats(3))/real(nqc)
        end if

        write(*,1043) i-1,nobs,nqc,
     +      (tstats(j),j=1,NQCFLAGS),pqc,pg,pflg,pb,pd
      end do
      return

1001  format(/,'QC Statistics:',/,
     +    '     Method:  Variance = Observation - MAPS Analyses',/,
     +    '     Date/time of report=',a26,/,
     +    '     Number of files read=',i7,/,
     +    '     Number of records read=',i10,/,
     +    '     Number of records within time range=',i10,//,
     +    '  Parameter',13x,
     +    'NOBS     NQC    GOOD     BAD   QUEST MISSNG NOT MEAS',
     +    '  GLITCH  NOT QC     EST    %QC %GOOD  %FLG  %BAD  %DUB')
1002  format(a20,10(i8),x,5(x,f5.1))
1011  format(///,'QC Statistics by PLATFORM',/)
1012  format(/,'Platform/Network: ',a10,//,'  Parameter',13x,
     +    'NOBS     NQC    GOOD     BAD   QUEST MISSNG NOT MEAS',
     +    '  GLITCH  NOT QC     EST    %QC %GOOD  %FLG  %BAD  %DUB')
1021  format(///,'QC Statistics by STATION',/)
1022  format('Network    Station',12x,
     +    'NOBS     NQC    GOOD     BAD   QUEST MISSNG NOT MEAS',
     +    '  GLITCH  NOT QC     EST    %QC %GOOD  %FLG  %BAD  %DUB')
1023  format(a10,x,a15,10(i8),x,5(x,f5.1))
1031  format(///,'QC Statistics by DAY',/)
1032  format('Day',7x,
     +    'NOBS     NQC    GOOD     BAD   QUEST MISSNG NOT MEAS',
     +    '  GLITCH  NOT QC     EST    %QC %GOOD  %FLG  %BAD  %DUB')
1033  format(i3,3x,10(i8),x,5(x,f5.1))
1041  format(///,'QC Statistics by HOUR',/)
1042  format('Hr',7x,
     +    'NOBS     NQC    GOOD     BAD   QUEST MISSNG NOT MEAS',
     +    '  GLITCH  NOT QC     EST    %QC %GOOD  %FLG  %BAD  %DUB')
1043  format(i2,3x,10(i8),x,5(x,f5.1))
1051  format(///,'QC Statistics for the worst STATIONS for ',a20,/)
1061  format(///,'QC Statistics by STATION for ',a20,/)
      end
