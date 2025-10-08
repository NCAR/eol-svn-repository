C******************************************************************************
C
C program: stnstat_4DYR - Modified stnstat.f to handle 4 Digit YR.
C author:  K. Scully
C date:    06/08/92
C description: Print kickout statistics by station/day.
C              List only the worst xxx stations for each parameter.
C
C usage:   stnstat [-debug]
C        
C              where -debug     option will print variable info as processing
C                               is done.
C 24 Jan 94 lec
C   Modified so that only the '-nom' and '-nominal' options result in
C   calling qcf read routine which expects both obs and nominal times
C   in the qcf files. Originally, the '-hrly' and '-hourly' options also
C   caused s/w to expect both times.
C 25 Aug 94 lec
C   Update s/w to handle 15 char station ID instead of 10 char.
C 27 Mar 95 lec
C   Updated s/w to handle new QC flag 'I' which occurs when derived parameters
C   can NOT be computed due to insufficient non-derived paramters.
C 9 Sept 2002 lec
C   Modified to handle 4 DYR. Consider rewriting in another language.
C******************************************************************************

      program stnstat

      implicit none

      include 'qcglobal.h'
      include 'stnstatcom.h'
      include 'debug.h'
      include 'ioerr.h'

C misc. variables
      character*11 qcftime,getqcftime_4DYR,getqcftime2_4DYR
      integer*2 qcfday
      integer iargc,narg
      character*9 arg1,arg2
      integer*2 getqcfrec_4DYR,getqcfrec2_4DYR
      character*60 fname
      integer nfile /0/			! count of files read
      integer nread /0/			! count of records read
      integer nrec /0/			! count of records within day range

C************************** end variable definitions *********************

      debug = .false.
      qcf_format = 1         !1=recs contain nominal time, also
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
         if(debug) write(0,*) 'File Name to process: ',fname
         if(fname .eq. 'quit' .or. fname .eq.'q' .or.
     +      fname .eq. 'exit' .or. fname .eq.'e' .or.
     +      fname .eq. 'stop') goto 900

C open the qc file
         write(0,*) 'processing file: ',fname
         open(unit_qcf,name=fname,status='old',iostat=ios,err=901)
         nfile=nfile+1
         write(0,*)'file is open! Loop thru the records'

C for each file, loop through the file of qcf input records
         do while (.true.)
            if(qcf_format .eq. 1) then
               if(debug) write(0,*) 'call getqcfrec2_4DYR'
               if(getqcfrec2_4DYR() .eq. -2) goto 200	! -2 = eof
            else
               if(debug) write(0,*) 'call getqcfrec_4DYR'
               if(getqcfrec_4DYR() .eq. -2) goto 200		! -2 = eof
            end if
            nread=nread+1

            if (qcf_format .eq. 1) then
               if(debug) write(0,*) 'call getqcftime2_4DYR'
               qcftime=getqcftime2_4DYR()
            else
               if(debug) write(0,*) 'call getqcftime_4DYR'
               qcftime=getqcftime_4DYR()
            end if

            read(qcftime,1002) qcfday
            if(debug) write(0,*)'SKIP? qcftime, qcfday:',qcftime,qcfday
            if(debug) write(0,*)'FIRSTDAY, LASTDAY', FIRSTDAY, LASTDAY

C skip records outside the range of firstday and lastday
            if(qcfday .lt. FIRSTDAY  .or.  qcfday .gt. LASTDAY) goto 100
            if(debug) write(0,*)'call addstats'

            call addstats(qcfday)
            nrec=nrec+1
            if (debug) write (0,*) 'Number of recs read: ', nrec

100      continue
         if (debug) write (0,*) 'After continue 100 - end do for recs'

         end do          ! end of while loop for each record in the file

200      continue
         if (debug) write (0,*) 'Before close 200'
         close(unit_qcf)
         if (debug) write (0,*) 'goto 300'
         goto 300

901      errstr=gerror(errstr)
         write(0,*) 'stnstat(): I/O Error: ',ios,' ',errstr
         write(0,*) '           Continuing with the next file...'

300   continue
      if (debug) write (0,*) 'After cont 300 - end do for qcf file'
      end do          ! end of while loop for each qcf file

C print the station stat report
900   call prtstats(nfile,nread,nrec,qcfday)
      if (debug) write (0,*) 'After call prtstats'
999   call standard_arithmetic()
      stop
1001  format(a60)
1002  format(4x,i3,4x)
      end
C************************************************************************
C
C function: addstats()
C description: calculate final statistics
C
C 27 Mar 95 lec - Even though only derived parameters (CSLP, DEWPT) can
C   or should be the only parameters set to 'I' with -999.99, the s/w
C   below was updated as if any parameter can be set to 'I'...just in
C   case.
C
C************************************************************************

      subroutine addstats(qcfday)

      implicit none

      include 'qcglobal.h'
      include 'stnstatcom.h'
      include 'qcfcom_4DYR.h'
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

         if(debug) write (0,*) 'in addstats(): qcfday, pi, si, di ', 
     +                          qcfday, pi, si, di

      if(qcf_format .eq. 1) then
         read(qcf.nomhr,1001) hr
      else
         read(qcf.hr,1001) hr
      end if
      hi = hr+1
      do i=1,NPRTPARMS
         if(i .eq. 9) then
	       qcflg=qcf.squal.qc
	    else
	       qcflg=qcf.parms(i).qc
	    end if
         if(qcflg .eq. 'G') then
            if(debug) write (0,*)'Found G flag'

            stats(i,1)=stats(i,1)+1
            p(pi,i,1)=p(pi,i,1)+1
            s(si,i,1)=s(si,i,1)+1
            d(di,i,1)=d(di,i,1)+1
            h(hi,i,1)=h(hi,i,1)+1
         endif
         if(qcflg .eq. 'B')then
            if(debug) write (0,*)'Found B flag'
            stats(i,2)=stats(i,2)+1
            p(pi,i,2)=p(pi,i,2)+1
            s(si,i,2)=s(si,i,2)+1
            d(di,i,2)=d(di,i,2)+1
            h(hi,i,2)=h(hi,i,2)+1
         endif
         if(qcflg .eq. 'D') then
            if(debug) write (0,*)'Found D flag'

            stats(i,3)=stats(i,3)+1
            p(pi,i,3)=p(pi,i,3)+1
            s(si,i,3)=s(si,i,3)+1
            d(di,i,3)=d(di,i,3)+1
            h(hi,i,3)=h(hi,i,3)+1
         endif
         if(qcflg .eq. 'M') then
            if(debug) write (0,*)'Found M flag'

            stats(i,4)=stats(i,4)+1
            p(pi,i,4)=p(pi,i,4)+1
            s(si,i,4)=s(si,i,4)+1
            d(di,i,4)=d(di,i,4)+1
            h(hi,i,4)=h(hi,i,4)+1
         endif
         if(qcflg .eq. 'N') then
            if(debug) write (0,*)'Found N flag'

            stats(i,5)=stats(i,5)+1
            p(pi,i,5)=p(pi,i,5)+1
            s(si,i,5)=s(si,i,5)+1
            d(di,i,5)=d(di,i,5)+1
            h(hi,i,5)=h(hi,i,5)+1
         endif
         if(qcflg .eq. 'X') then
            if(debug) write (0,*)'Found X flag'

            stats(i,6)=stats(i,6)+1
            p(pi,i,6)=p(pi,i,6)+1
            s(si,i,6)=s(si,i,6)+1
            d(di,i,6)=d(di,i,6)+1
            h(hi,i,6)=h(hi,i,6)+1
         endif
         if(qcflg .eq. 'U') then
            if(debug) write (0,*)'Found U flag'

            stats(i,7)=stats(i,7)+1
            p(pi,i,7)=p(pi,i,7)+1
            s(si,i,7)=s(si,i,7)+1
            d(di,i,7)=d(di,i,7)+1
            h(hi,i,7)=h(hi,i,7)+1
         endif
         if(qcflg .eq. 'E') then
            if(debug) write (0,*)'Found E flag'

            stats(i,8)=stats(i,8)+1
            p(pi,i,8)=p(pi,i,8)+1
            s(si,i,8)=s(si,i,8)+1
            d(di,i,8)=d(di,i,8)+1
            h(hi,i,8)=h(hi,i,8)+1
         endif
         if(qcflg .eq. 'I') then
            if(debug) write (0,*)'Found I flag'

            stats(i,9)=stats(i,9)+1
            p(pi,i,9)=p(pi,i,9)+1
            s(si,i,9)=s(si,i,9)+1
            d(di,i,9)=d(di,i,9)+1
            h(hi,i,9)=h(hi,i,9)+1
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
      if(qcf.qcpw .eq. 'I') stats(i,9)=stats(i,9)+1
	 i=11
      if(qcf.vis.qc .eq. 'G') stats(i,1)=stats(i,1)+1
      if(qcf.vis.qc .eq. 'B') stats(i,2)=stats(i,2)+1
      if(qcf.vis.qc .eq. 'D') stats(i,3)=stats(i,3)+1
      if(qcf.vis.qc .eq. 'M') stats(i,4)=stats(i,4)+1
      if(qcf.vis.qc .eq. 'N') stats(i,5)=stats(i,5)+1
      if(qcf.vis.qc .eq. 'X') stats(i,6)=stats(i,6)+1
      if(qcf.vis.qc .eq. 'U') stats(i,7)=stats(i,7)+1
      if(qcf.vis.qc .eq. 'E') stats(i,8)=stats(i,8)+1
      if(qcf.vis.qc .eq. 'I') stats(i,9)=stats(i,9)+1
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
         if(qcf.cc(i).ceilqc .eq. 'I') stats(j,9)=stats(j,9)+1
	    j=j+1
         if(qcf.cc(i).cldqc .eq. 'G') stats(j,1)=stats(j,1)+1
         if(qcf.cc(i).cldqc .eq. 'B') stats(j,2)=stats(j,2)+1
         if(qcf.cc(i).cldqc .eq. 'D') stats(j,3)=stats(j,3)+1
         if(qcf.cc(i).cldqc .eq. 'M') stats(j,4)=stats(j,4)+1
         if(qcf.cc(i).cldqc .eq. 'N') stats(j,5)=stats(j,5)+1
         if(qcf.cc(i).cldqc .eq. 'X') stats(j,6)=stats(j,6)+1
         if(qcf.cc(i).cldqc .eq. 'U') stats(j,7)=stats(j,7)+1
         if(qcf.cc(i).cldqc .eq. 'E') stats(j,8)=stats(j,8)+1
         if(qcf.cc(i).cldqc .eq. 'I') stats(j,8)=stats(j,9)+1
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
C 24 Jan 94 lec
C   Added ndays def and code to make more generic. Added more 
C   initializations.
C 30 Mar 94 lec
C   For some strange reason, I have notice while trying to process the 
C   very large hrly Sfc Cmp, that something in the code is either
C   overwriting memory, or failing in another manner. It does not always
C   fail. Ken S. apparently never operated this s/w with the number
C   of platforms (MAX) set to more than 10. I don't yet know why. I must
C   do this since I have 11 different networks. See platform.h. All
C   platform names are <= 10 chars. I believe that the output from
C   this program is fine when it completes successfully. I will continue
C   to search for why it periodically fails when max platforms is >10.
C   I have noticed that var 'np' apparently is overwritten during failures.
C   I moved this vars position in common. Also note the switch I made
C   below for debugging tests.
C************************************************************************

      subroutine prtstats(nfile,nread,nrec,qcfday)

      implicit none

      include 'qcglobal.h'
      include 'stnstatcom.h'
      include 'qcfcom_4DYR.h'
      include 'stncom.h'
      include 'platform.h'
      include 'parmnames.h'
      include 'debug.h'

	 integer*2 qcfday
	 integer nfile,nread,nrec
	 integer tstats(NQCFLAGS)
	 integer nobs,nqc
         real    pqc,pg,pflg,pb,pd
	 integer*2 i,j,k,l
	 integer ndays
	 integer tm,time
	 character*26 ctime
	 integer*2 n, m, getplatform            ! lec - moved form end, potential overwrite?
	 integer*2 MAXBADSTN
	 parameter (MAXBADSTN=10)		!max # worst stations per platform
	 structure /s2rec/
	    integer*2 stn
	    real pg
	 end structure
	 record /s2rec/ s2(MAXPLATFORM,MAXBADSTN)


	 if(debug) write(*,*) 'in prtstats(): '
	 tm=time()

C write out final summary statistics
      write(*,1001) ctime(tm),nfile,nread,nrec
      do i=1,NQCPARMS
         nobs = 0
         nqc = 0

         nobs=stats(i,1)+stats(i,2)+stats(i,3)+stats(i,7)+stats(i,8)
         nqc=stats(i,1)+stats(i,2)+stats(i,3)+stats(i,8)            !G+B+D+E

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
      do i=1,NQCPARMS
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
      write(*,1002) 'Grand Totals (all)  ',nobs,nqc,
     +    (tstats(j),j=1,NQCFLAGS),pqc,pg,pflg,pb,pd


      do k=1,NQCFLAGS
         tstats(k)=0
      end do
      do i=1,NPRTPARMS - 1
         do j=1,NQCFLAGS
		  if(j.ne.3) tstats(j)=tstats(j)+stats(i,j)
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

      if ( nqc .le. 0) then
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
C
C    Verify Comments in write below before putting
C    it back in!!!!
C
C      write(*,1002) 'Totals (1,2,4-7)    ',nobs,nqc,
C     +    (tstats(j),j=1,NQCFLAGS),pqc,pg,pflg,pb,pd
C
C write out statistics by platform
      nobs = 0
      nqc = 0
      write(*,1011)
      do i=1,np
        write(*,1012) platform(i)
        do j=1,NPRTPARMS
           nobs=p(i,j,1)+p(i,j,2)+p(i,j,3)+p(i,j,7)+p(i,j,8)
           nqc=p(i,j,1)+p(i,j,2)+p(i,j,3)+p(i,j,8)

           if (nobs .le. 0) then
              nobs = 0
              pqc = 0
           else
              pqc=100.* real(nqc)/real(nobs)
           endif

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
      do j=1,NPRTPARMS
        write(*,1051) parmnames(j)
        write(*,1022)
        do n=1,np
           do k=1,MAXBADSTN
	   	    s2(n,k).stn=0
	   	    s2(n,k).pg=0.0
           end do
        end do
        nqc = 0
        pg = 0
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

        nobs = 0
        nqc = 0
        do n=1,np
           do k=1,MAXBADSTN
		    i=s2(n,k).stn
		    if(i .le. 0) goto 60
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
      nobs = 0
      nqc = 0
      do i=1,nstns
        do k=1,NQCFLAGS
           tstats(k)=0
        end do
        do j=1,NPRTPARMS
           do k=1,NQCFLAGS
              tstats(k)=tstats(k)+s(i,j,k)
           end do
        end do
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
      ndays = LASTDAY - FIRSTDAY + 1

      write(*,1031)
      do j=1,NPRTPARMS
        write(*,1034) parmnames(j)
        write(*,1032)
        do i=1,ndays

          do k=1,NQCFLAGS
             tstats(k)=0
          end do
             do k=1,NQCFLAGS
                tstats(k)=tstats(k)+d(i,j,k)
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
     +        (tstats(l),l=1,NQCFLAGS),pqc,pg,pflg,pb,pd
        end do
      end do

C write out statistics by hour
      write(*,1041)
      do j=1,NPRTPARMS
        write(*,1034) parmnames(j)
        write(*,1042)
        do i=1,24
          nobs = 0
          nqc = 0

          do k=1,NQCFLAGS
             tstats(k)=0
          end do
          do k=1,NQCFLAGS
             tstats(k)=tstats(k)+h(i,j,k)
          end do

          nobs=tstats(1)+tstats(2)+tstats(3)+tstats(7)+tstats(8)
          nqc=tstats(1)+tstats(2)+tstats(3)+tstats(8)
          pqc = 0

C          write (*,*) 'nobs, nqc, pqc after add tstats:', nobs, nqc, pqc

          if(nobs .le. 0) then
            nobs = 0
            pqc = 0     
          else
            pqc=100.* real(nqc)/real(nobs)
          end if
C          write (*,*) 'nobs, nqc, pqc after check nobs:', nobs, nqc, pqc

          if(nqc .le. 0) then 
            nqc = 0
            pg=0
            pflg=0
            pb=0
            pd=0
          else
            pg=100.* real(tstats(1)+tstats(8))/real(nqc)
            pflg=100.* real(tstats(2)+tstats(3))/real(nqc)
            pb=100.* real(tstats(2))/real(nqc)
            pd=100.* real(tstats(3))/real(nqc) 
          end if 
C          write (*,*) 'nobs, nqc, pqc after check nqc:', nobs, nqc, pqc

          write(*,1043) i-1,nobs,nqc,
     +      (tstats(l),l=1,NQCFLAGS),pqc,pg,pflg,pb,pd
          end do
      end do
      return

1001  format(/,'QC Statistics:',/,
     +    '     Method:  Horizontal Quality Control',/,
     +    '     Date/time of report=',a26,/,
     +    '     Number of files read=',i7,/,
     +    '     Number of records read=',i10,/,
     +    '     Number of records within time range=',i10,//,
     +    '  Parameter',15x,
     +    'NOBS       NQC      GOOD       BAD     QUEST'
     +    '   MISSNG   NOT MEAS',
     +    '    GLITCH    NOT QC       EST    INSUFF'
     +    '    %QC %GOOD  %FLG  %BAD  %DUB')
1002  format(a20,11(i10),x,5(x,f5.1))       ! was: i8
1011  format(///,'QC Statistics by PLATFORM',/)
1012  format(/,'Platform/Network: ',a10,//,'  Parameter',15x,
     +    'NOBS       NQC      GOOD       BAD     '
     +    'QUEST   MISSNG   NOT MEAS',
     +    '    GLITCH    NOT QC       EST    INSUFF'
     +    '    %QC %GOOD  %FLG  %BAD  %DUB')
1021  format(///,'QC Statistics by STATION',/)
1022  format('Network    Station',14x,
     +    'NOBS       NQC      GOOD       BAD     '
     +    'QUEST    MISSNG  NOT MEAS',
     +    '    GLITCH    NOT QC       EST    INSUFF    %QC %GOOD  '
     +    '%FLG  %BAD  %DUB')
1023  format(a10,x,a15,11(i10),x,5(x,f5.1)) ! was i8
1031  format(///,'QC Statistics by DAY',/)
1034  format(//,'Parameter: ',a20,/)
1032  format('Day',9x,
     +    'NOBS       NQC      GOOD       BAD     '
     +    'QUEST   MISSNG   NOT MEAS',
     +    '    GLITCH    NOT QC       EST    INSUFF '
     +    '   %QC %GOOD  %FLG  %BAD  %DUB')
1033  format(i3,3x,11(i10),x,5(x,f5.1))     ! was i8
1041  format(///,'QC Statistics by HOUR',/)
1042  format('Hr',9x,
     +    'NOBS       NQC      GOOD       BAD     '
     +    'QUEST    MISSNG  NOT MEAS',
     +    '    GLITCH    NOT QC       EST    INSUFF '
     +    '   %QC %GOOD  %FLG  %BAD  %DUB')
1043  format(i2,3x,11(i10),x,5(x,f5.1))     ! was i8
1051  format(///,'QC Statistics for the worst STATIONS for ',a20,/)
1061  format(///,'QC Statistics by STATION for ',a20,/)
      end
