C BEWARE - ensure that this program does same computations as qc_2times!
C
C Fall94 - This program is NO longer used. Only use qc_2times with 
C          confidence. Several mods have only been updated to qc_2times.
C
C******************************************************************************
C
C program: qc
C author:  K. Scully
C date:    03/26/92
C description: Perform quality control functions on STORMFEST surface data.
C              QC is performed by comparing surface data against the MAPS
C              surface analysis.  Summary statistics are logged in file
C              "qc_log".
C
C usage:   qc [-debug] input_file
C        
C              where -debug     option will print variable info as processing
C                               is done.
C                    input_file is the name of the input file containing
C                               data to be checked againts the analysis values.
C                               The input file is required.
C
C 21 Jan 94 lec
C   Added additional comments. Removed s/w that changed ISWS precip qc flag
C   to X or glitch for stn llc. This will now be handled in conversion s/w.
C   Updated path names for files.
C 23 Aug 94 lec
C   Updated s/w to accept MAPS data in NetCDF format. General Cleanup.
C
C******************************************************************************

      program qc

      implicit none

      include 'qcglobal.h'
      include 'qccom.h'
      include 'gridcom.h'
      include 'qcfcom.h'
      include 'stncom.h'
      include 'debug.h'
      include 'ioerr.h'
      include 'parmnames.h'

      real        analvals(NANALPARMS) !analyses values to compare with the qc point
      real        var(NQCPARMS)        !calculated variances from the analyses/limits
      character*9 gridtm(GRIDT)

C
C misc. variables
C
      integer      iargc,rindex,narg,x,i,j,n
      integer*2    trc,t,loadgrid,getqcrec
      integer      tm,time

      character*26 ctime
C***  character*30 tfname
      character*6  debugarg
      character*3  filetype

      integer nread /0/			! count of records read
      integer nrec /0/			! count of records qc'd

      data gridtm /'         ',
     +             '         '/
C************************** end variable definitions *********************

      tm=time()

      write(*,*)  'Running qc program: Begin: ',ctime(tm)

      debug = .false.

C
C get command line arguments
C
      narg = iargc()
      if(narg .eq. 1) then
         call getarg(1,qc_0qc)          ! qc_0qc is input 0QC file name.
      else if(narg .eq.2) then
         call getarg(1,debugarg)
         call getarg(2,qc_0qc)
         if(debugarg .eq. '-debug' .or. debugarg .eq. -DEBUG) then
            debug = .true.
            write(*,*) 'Running in DEBUG mode'
         end if
      else
         write(*,*) 'Invalid number of arguments'
      end if

      t=0
      nstns=0
      x=rindex(qc_0qc,'.')			!get char pos of file type
      filetype=qc_0qc(x+1:x+3)		        !extract file type
      if(filetype .ne. '0qc' .and. filetype .ne. '0QC') then
         write(*,*) 'Input file: ',qc_0qc,' is not a 0qc file.'
         write(*,*) 'Skipping...'
         goto 999
      end if
      write(*,*) 'Processing file: ',qc_0qc

C
C Form output and variance file names.
C Output names will be same prefix as input
C file being operated but with different suffix
C values. The .qcf file is the Quality Controlled
C final output file. The .var file is the 
C variance file. The .toss file contains records
C of data that 'failed' QC (bad and dubious recs).
C The 0qc file is the input file being processed.
C
C************************************************
C     tfname=qc_0qc(1:x)//'qcf'
C     qc_qcf='/fest/qc/hrly_sfc/qcf/'//tfname
C     tfname=qc_0qc(1:x)//'var'
C     qc_var='/fest/qc/hrly_sfc/var/'//tfname
C     tfname=qc_0qc(1:x)//'toss'
C     qc_toss='/fest/qc/hrly_sfc/toss/'//tfname
C************************************************

      qc_qcf=qc_0qc(1:x)//'qcf'
      qc_var=qc_0qc(1:x)//'var'
      qc_toss=qc_0qc(1:x)//'toss'

C
C Open the files
C
      open(unit_0qc,name=qc_0qc,status='old',iostat=ios,err=901)
      open(unit_qcf,name=qc_qcf,status='new',iostat=ios,err=901)
      open(unit_var,name=qc_var,status='new',iostat=ios,err=901)
      open(unit_toss,name=qc_toss,status='new',iostat=ios,err=901)

C
C Get station from station master file
C
      call getstns()

C
C Get variances limits from config file
C
      call getconfig()

C
C Loop through the file of input records
C
      do while (.true.)
          if(getqcrec() .eq. -2) goto 900		! -2 = eof
          nread=nread+1

C
C Reset analysis values to -999.99 (which means NO analysis value)
C
          do i=1,NANALPARMS
             analvals(i)=-999.99
          end do

C
C Initialize variance array with -999.99 which represents the case
C when a variance amount does not apply such as an unqc'd parameter or
C an unmeasured parameter, etc.
C
          do i=1,NQCPARMS
             var(i)=-999.99
          end do

C
C Load the grid values from the MAPS analyses.
C The loadgrid routine calls readmaps().
C
          trc=loadgrid(gridtm,t)

          if(trc.lt.0) goto 100
          t=trc

C
C Get the analysis values for x,y
C
          call getvals(analvals,t,gridtm)

C
C Compare qcf values to analysis and set flags
C
          call compvals(analvals,var)
          nrec=nrec+1

100       call putqcrec(var,analvals)
          continue
      end do          ! end of while loop for each record in the file

C
C Write out final station master file
C
900   call putstns()

C
C Write out final summary statistics
C
      write(*,1008) ctime(tm),nread,nrec
      do i=1,NQCPARMS
         write(*,1001) parmnames(i),(stats(i,j),j=1,NQCFLAGS)
      end do
      write(*,1005)

      do i=1,NANALPARMS+1
         n=stats(i,1)+stats(i,2)+stats(i,3)
         write(*,1002) parmnames(i),dev(i,1)/n,
     +    (dev(i,j)/stats(i,j-1),j=2,4),(dev(i,j),j=5,8)
      end do
      write(*,1006)

      do i=1,NANALPARMS+1
         write(*,1003) parmnames(i),
     +    (dev(i,j),j=13,16),(dev(i,j),j=17,20)
      end do
      write(*,1007)

      do i=1,NANALPARMS+1
         n=stats(i,1)+stats(i,2)+stats(i,3)
         write(*,1004) parmnames(i),(dev(i,j)+dev(i,j+4),j=13,16),
     +     sqrt( (dev(i,9)*n-dev(i,1)**2) / (n*(n-1)) ),
     +    (sqrt( (dev(i,j)*stats(i,j-9)-dev(i,j-8)**2) /
     +           (stats(i,j-9)*(stats(i,j-9)-1)) ),j=10,12)
      end do
      write(*,1011)

      do i=1,68
         if(tosscnt(i,1).gt.0 .or. tosscnt(i,2).gt.0) then
            write(*,1012) i,tosscnt(i,1),tosscnt(i,2)
         end if
      end do

C
C Close input and output files.
C
      close(unit_config)
      close(unit_0qc)
      close(unit_qcf)

      tm=time()
      write(*,*) 'Finish: ',ctime(tm)
      goto 999

C
C Error Handling and formats.
C
901   errstr=gerror(errstr)
      write(*,*) 'qc(): I/O Error: ',ios,' ',errstr

999   call standard_arithmetic()
      stop

1001  format(a20,8(3x,i5))
1002  format(a20,4(2x,f7.2),2x,4(2x,f7.2))
1003  format(a20,4(2x,f9.2),2x,4(2x,f9.2))
1004  format(a20,4(2x,f9.2),2x,4(2x,f7.2))
1005  format(/,22x,'******** Average Variance ********',4x,
     +             '******** Maximum Variance ********',/,
     +   '  Parameter',15x,
     +              'ALL     GOOD      BAD    QUEST',8x,
     +              'ALL     GOOD      BAD    QUEST')
1006  format(/,22x,'*********** Total Low Variance ***********',4x,
     +             '********** Total High Variance ***********',/,
     +   '  Parameter',17x,
     +              'ALL       GOOD        BAD      QUEST',10x,
     +              'ALL       GOOD        BAD      QUEST')
1007  format(/,22x,'************* Total Variance *************',4x,
     +             '********* Std Deviation **********',/,
     +   '  Parameter',17x,
     +              'ALL       GOOD        BAD      QUEST',8x,
     +              'ALL     GOOD      BAD    QUEST')
1008  format(/,'QC Statistics:',/,
     +    '     Method:  Variance = Observation - MAPS Analyses',/,
     +    '     Date/time of report=',a26,/,
     +    '     Number of records read=',i6,/,
     +    '     Number of records processed=',i6,/,
     +    '  Parameter',13x,
     +    'GOOD     BAD   QUEST MISSNG NOT MEAS  GLITCH',
     +    '  NOT QC     EST')
1009  format(/,'Network/Station codes:',/,
     +         'Code   Network     Station',8x,'Lat',10x,'Lon',7x,
     +         'Elev     x      y')
1010  format(i4,2x,a10,2x,a10,2x,f10.5,2x,f11.5,2x,f7.2,2x,i5,2x,i5)
1011  format(/,'Toss Code count:',/,
     +    'Code    BAD  QUEST')
1012  format(i4,2x,i5,2x,i5)

      end       ! ---------Main QC() program-----------


C************************************************************************
C
C function: putstns()
C description: output the new qc station master file
C
C************************************************************************

      subroutine putstns()

      implicit none

      include 'qcglobal.h'
      include 'stncom.h'
      include 'debug.h'
      include 'ioerr.h'

      integer i,j

C
C open the qc_stns file
C
      open(unit_stns2,name=qc_stns2,status='new',iostat=ios,err=901)
      if(debug) then
         write(*,*) "Writing out qc_stns file:"
         write(*,*)
      end if

C
C loop through the records.
C
       do i=1,nstns
          write(unit_stns2,1001) i,stns(i).network,stns(i).station,
     +      stns(i).lat,stns(i).lon,stns(i).elev,
     +      stns(i).x,stns(i).y,
     +      (stns(i).val(j),j=1,NSTNPARMS),
     +      (stns(i).dt(j),j=1,NSTNPARMS)
         end do

      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'putstns(): I/O Error: ',ios,' ',errstr
999   close(unit_stns2)
      return
C was: 1001  format(i3,a10,a10,f10.5,f11.5,f7.2,i5,i5,8(f7.2),8(i9))
1001  format(i3,a10,a15,f10.5,f11.5,f7.2,i5,i5,8(f7.2),8(i9))

      end             ! putstns()

C************************************************************************
C
C function: getqcrec()
C
C description: get the qcf records. Following is description of qcf vars.
C              Each qcf var has a qc flag associated with it.
C
C   qcf.yr, qcf.mo, qcf.day - year, month, day
C   qcf.hr, qcf.min -  hour, minute
C   qcf.network - current network (e.g., HPLAINS, ISWS, NCDC, etc.)
C   qcf.station - current station name or id number. (n/a) Now allow 15 chars.
C   qcf.lat     - latitude  (decimal)
C   qcf.lon     - longitude (decimal)
C   qcf.occur   - occurrance. Indicates co-location of stations. (n/a)
C   qcf.elev    - station elevation. (m)
C   qcf.parms(1)- station pressure (mb) 
C   qcf.parms(2)- sea level pressure (mb)
C   qcf.parms(3)- calculated sea level pressure (mb)
C   qcf.parms(4)- temperature (C)
C   qcf.parms(5)- dew point temperature (C)
C   qcf.parms(6)- wind speed (m/s) 
C   qcf.parms(7)- wind direction (degrees) 
C   qcf.parms(8)- precipitation (mm) 
C   qcf.sqind   - squall indicator
C   qcf.squal   - squall value
C   qcf.pw      - present weather
C   qcf.vis     - visibility
C   qcf.ceilhgt - ceiling height
C   qcf.cldamt  - cloud amount
C   
C
C WARNING:
C   This s/w assumes that input 0qc files have specific number of 
C   header records. This may not be true of all input files!!!!!!
C************************************************************************

      integer*2 function getqcrec()

      implicit none

      include 'qcglobal.h'
      include 'qcfcom.h'
      include 'debug.h'
      include 'ioerr.h'

      character*255 qcfbuf

      logical header 
C***  data header /.true./  ! Yes hdrs (set according to input 0qc files.)
      data header /.false./ ! no hdrs in 0qc files.

C
C read the qcf header records and write them to the qcf output file
C
      if(header) then
         if(debug) then
            write(*,*)
            write(*,*) 'Read qcf header records'
         end if
         read(unit_0qc,1002,iostat=ios,err=901,end=900) qcfbuf
         write(unit_qcf,1002,iostat=ios,err=901) qcfbuf
         read(unit_0qc,1002,iostat=ios,err=901,end=900) qcfbuf
         write(unit_qcf,1002,iostat=ios,err=901) qcfbuf
         read(unit_0qc,1002,iostat=ios,err=901,end=900) qcfbuf
         write(unit_qcf,1002,iostat=ios,err=901) qcfbuf
	    header=.false.
      end if

C
C loop through the data records
C
      getqcrec = 0
      read(unit_0qc,1002,iostat=ios,err=901,end=900) qcfbuf
CCCC  if(qcfbuf(1:31) .eq. '********  END OF FILE  ********') then
      if(qcfbuf(1:8) .eq. '********' .or. 
     +   qcfbuf(1:8) .eq. '**/**/**') then
         write(*,*) 'End of File found'
         write(unit_qcf,1006,iostat=ios,err=901) qcfbuf(1:31)
         goto 900
      end if

      read(qcfbuf,1001,iostat=ios,err=901,end=900)
     +qcf.yr,qcf.mo,qcf.day,qcf.hr,qcf.min,
     +qcf.network,qcf.station,qcf.lat,qcf.lon,qcf.occur,qcf.elev,
     +qcf.parms(1).val,qcf.parms(1).qc,qcf.parms(2).val,qcf.parms(2).qc,
     +qcf.parms(3).val,qcf.parms(3).qc,qcf.parms(4).val,qcf.parms(4).qc,
     +qcf.parms(5).val,qcf.parms(5).qc,qcf.parms(6).val,qcf.parms(6).qc,
     +qcf.parms(7).val,qcf.parms(7).qc,qcf.parms(8).val,qcf.parms(8).qc,
     +qcf.sqind,qcf.squal.val,qcf.squal.qc,
     +qcf.pw,qcf.qcpw,qcf.vis.val,qcf.vis.qc,
     +qcf.cc(1).ceilhgt,qcf.cc(1).ceilflg,qcf.cc(1).ceilqc,
     +qcf.cc(1).cldamt,qcf.cc(1).cldqc,
     +qcf.cc(2).ceilhgt,qcf.cc(2).ceilflg,qcf.cc(2).ceilqc,
     +qcf.cc(2).cldamt,qcf.cc(2).cldqc,
     +qcf.cc(3).ceilhgt,qcf.cc(3).ceilflg,qcf.cc(3).ceilqc,
     +qcf.cc(3).cldamt,qcf.cc(3).cldqc

      if(debug) then

         if(yn.eq.'q') debug=.false.
         write(*,1003,iostat=ios,err=901)
     +     qcf.yr,qcf.mo,qcf.day,qcf.hr,qcf.min,
     +     qcf.network,qcf.station,qcf.lat,qcf.lon,qcf.occur,qcf.elev,
     +     qcf.parms(1).val,qcf.parms(1).qc,
     +     qcf.parms(2).val,qcf.parms(2).qc,
     +     qcf.parms(3).val,qcf.parms(3).qc,
     +     qcf.parms(4).val,qcf.parms(4).qc,
     +     qcf.parms(5).val,qcf.parms(5).qc,
     +     qcf.parms(6).val,qcf.parms(6).qc,
     +     qcf.parms(7).val,qcf.parms(7).qc,
     +     qcf.parms(8).val,qcf.parms(8).qc,
     +     qcf.sqind,qcf.squal.val,qcf.squal.qc,
     +     qcf.pw,qcf.qcpw,qcf.vis.val,qcf.vis.qc
         write(*,1004,iostat=ios,err=901)
     +     qcf.cc(1).ceilhgt,qcf.cc(1).ceilflg,qcf.cc(1).ceilqc,
     +     qcf.cc(1).cldamt,qcf.cc(1).cldqc,
     +     qcf.cc(2).ceilhgt,qcf.cc(2).ceilflg,qcf.cc(2).ceilqc,
     +     qcf.cc(2).cldamt,qcf.cc(2).cldqc,
     +     qcf.cc(3).ceilhgt,qcf.cc(3).ceilflg,qcf.cc(3).ceilqc,
     +     qcf.cc(3).cldamt,qcf.cc(3).cldqc
	    type *,' Continue?'

         accept 1005, yn
      end if

      goto 999

900   getqcrec = -2
      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'getqcrec(): I/O Error: ',ios,' ',errstr

999   return

C
C     Updated formats 1001,1002, 1003 to handle 15 char stn IDs.
C
1001  format(a2,'/',a2,'/',a2,1x,a2,':',a2,1x,a10,1x,a15,1x,f10.5,
     *          1x,f11.5,1x,i3,1x,f7.2,1x,
     *          8(f7.2,1x,a1,1x),a1,1x,f7.2,1x,a1,
     *          1x,i4,1x,a1,1x,f8.2,1x,a1,1x,
     *          3(f7.2,1x,i2,1x,a1,1x,i2,1x,a1,1x))
C was: 1002  format(a235)	!This must be equal to length of the qcf record
1002  format(a240)	!This must be equal to length of the qcf record
1006  format(a31)	!This must be equal to length of the eof indicator
1003  format('Record read:',/,
     +'Date/Time: ',a2,'/',a2,'/',a2,x,a2,':',a2,/,
     +'Network: ',a10,'   Station: ',a15,/,
     +'Lat: ',f10.5,'   Lon: ',f11.5,'   Occur: ',i3,'   Elev: ',f7.2/,
     +'  Stn Press: ',f8.2,'   QC: ',a1,/,
     +'  SL Press:  ',f8.2,'   QC: ',a1,/,
     +'  Calc SL Press: ',f8.2,' QC: ',a1,/,
     +'  Temp:       ',f8.2,'    QC: ',a1,/,
     +'  Dew Point:  ',f8.2,'    QC: ',a1,/,
     +'  Wind Speed: ',f8.2,'    QC: ',a1,/,
     +'  Wind Dir:   ',f8.2,'    QC: ',a1,/,
     +'  Precip:     ',f8.2,'    QC: ',a1,/,
     +'  Squall Ind: ',a1,'   Amt: ',f8.2,'   QC: ',a1,/,
     +'  Present weather: ',i4,' QC: ',a1,/,
     +'  Visibility: ',f8.2,'    QC: ',a1)
1004  format(
     +'  Ceiling 1:: Height: ',f8.2,'   Flag: ',i2,
     +'   QC: ',a1,'   Cloud Amt: ',i2,'   QC: ',a1,/,
     +'  Ceiling 2:: Height: ',f8.2,'   Flag: ',i2,
     +'   QC: ',a1,'   Cloud Amt: ',i2,'   QC: ',a1,/,
     +'  Ceiling 3:: Height: ',f8.2,'   Flag: ',i2,
     +'   QC: ',a1,'   Cloud Amt: ',i2,'   QC: ',a1,/)
1005  format(a1)

      end          !getqcrec()


C***********************************************************************
C
C function: putqcrec()
C description: output the qcf and variance records
C
C************************************************************************

      integer function putqcrec(var,analvals)

      implicit none

      include 'qcglobal.h'
      include 'qcfcom.h'
      include 'configcom.h'
      include 'debug.h'
      include 'ioerr.h'

      real     var(*),analvals(*)
      integer  i
      integer  stncode			!station code (index) of stns()
      integer  getstncode		!function to return station code
      integer  days,getdays

C
C write the qcf record to the output file
C
      if(debug) then
         write (*,*) 'write the qcf record'
      end if

      write(unit_qcf,1001,iostat=ios,err=901)
     +qcf.yr,qcf.mo,qcf.day,qcf.hr,qcf.min,
     +qcf.network,qcf.station,qcf.lat,qcf.lon,qcf.occur,qcf.elev,
     +qcf.parms(1).val,qcf.parms(1).qc,qcf.parms(2).val,qcf.parms(2).qc,
     +qcf.parms(3).val,qcf.parms(3).qc,qcf.parms(4).val,qcf.parms(4).qc,
     +qcf.parms(5).val,qcf.parms(5).qc,qcf.parms(6).val,qcf.parms(6).qc,
     +qcf.parms(7).val,qcf.parms(7).qc,qcf.parms(8).val,qcf.parms(8).qc,
     +qcf.sqind,qcf.squal.val,qcf.squal.qc,qcf.pw,qcf.qcpw,
     +qcf.vis.val,qcf.vis.qc,
     +qcf.cc(1).ceilhgt,qcf.cc(1).ceilflg,qcf.cc(1).ceilqc,
     +qcf.cc(1).cldamt,qcf.cc(1).cldqc,
     +qcf.cc(2).ceilhgt,qcf.cc(2).ceilflg,qcf.cc(2).ceilqc,
     +qcf.cc(2).cldamt,qcf.cc(2).cldqc,
     +qcf.cc(3).ceilhgt,qcf.cc(3).ceilflg,qcf.cc(3).ceilqc,
     +qcf.cc(3).cldamt,qcf.cc(3).cldqc

      if(debug) then
         write (*,*) 'write the qcf variance record'
      end if

C
C look up the network/station code
C
      stncode=getstncode()

C********************************
C see if any variance has been measured for this record.
C a variance of -999.99 means NOT measured and, therefore, not applicable.
C     do i=1,NANALPARMS
C        if(var(i) .ne. -999.99) goto 20
C     end do
C     goto 999
C*********************************

C
C write the variance record
C
20    days=getdays()

C
C for -999.99 values in maps offset the offset so the record comes out right
C in the variance file.
C
      do i=1,NANALPARMS
         if(analvals(i) .eq. -999.99) analvals(i)=analvals(i)+varo(i)
      end do
 
C*** write(*,*) 'putqcrec(): days=',days

      write(unit_var,1004,iostat=ios,err=901)
     +      stncode,days,qcf.hr,qcf.min,
     +      (analvals(i)-varo(i),var(i),i=1,NANALPARMS)
      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'putqcrec(): I/O Error: ',ios,' ',errstr

999   return
C
C     Update format to handle 15 char stn ID.
C
1001  format(a2,'/',a2,'/',a2,1x,a2,':',a2,1x,a10,1x,a15,1x,f10.5,
     *          1x,f11.5,1x,i3,1x,f7.2,1x,
     *          8(f7.2,1x,a1,1x),a1,1x,f7.2,1x,a1,
     *          1x,i4,1x,a1,1x,f8.2,1x,a1,1x,
     *          3(f7.2,1x,i2,1x,a1,1x,i2,1x,a1,1x))

1004  format(i4,i3,2(a2),16(f7.2))

      end       !putqcrec()

C***********************************************************************
C
C function: puttoss()
C description: output the toss record
C
C************************************************************************

      integer function puttoss(stncode,days,x,qc)

      implicit none

      include 'qcglobal.h'
      include 'qccom.h'
      include 'qcfcom.h'
      include 'debug.h'
      include 'ioerr.h'

      integer     stncode		!station code (index) of stns()
      integer     days			!no. days since day1
      integer     x			!toss code
      character*1 qc			!qc flag on toss record

C***************
C     real    analvals(*)
C     integer getdays
C     integer  getstncode		!function to return station code
C***************

C
C write the qcf record to the output file
C
      if(debug) then
         write (*,*) 'write the toss record'
      end if

C
C look up the network/station code
C
C     stncode=getstncode()
C     days=getdays()

      if(qc .eq. 'B') tosscnt(x,1)=tosscnt(x,1)+1
      if(qc .eq. 'D') tosscnt(x,2)=tosscnt(x,2)+1

      write(unit_toss,1001,iostat=ios,err=901) stncode,
     +   days,qcf.hr,qcf.min,x,qc
C    +   analvals(i),qcf.parms(i).val
      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'puttoss(): I/O Error: ',ios,' ',errstr

999   return

1001  format(i4,i3,a2,a2,i2,a1)
C1001  format(i4,i3,a2,a2,i2,a1,2(f7.2))

      end     !puttoss()


C************************************************************************
C
C function: loadgrid()
C description: load the grid with the analyses
C
C notes:   the time dimension of the grid alternates between 1 and 2 as
C          the earlier time so that whole grids do not need to be copied
C          as each hour is loaded.
C
C assumptions: the analyses are hourly
C              GRIDT (total hours stored in grid array) = 2
C
C returns:  -1 : error loading grid
C        n > 0 : grid loaded with index "n" as the current date, n+1 as
C                the next date, etc.
C************************************************************************/

      integer*2 function loadgrid(gridtm,t)

      implicit none

      include 'qcfcom.h'
      include 'debug.h'

      character*9 gridtm(*),btime,etime,getbtime,getntime
      character*9 fnameanal

      integer*2   readmaps,rc,t
      integer     y,m,d,h
      integer*4   bt,et,gt1,gt2

      loadgrid=0

C
C     Check to see if anything is loaded.  If so, check to see if the necessary
C     dates are loaded.  If so, there is nothing to do, just return.
C
      read(qcf.yr,1001) y
      read(qcf.mo,1001) m
      read(qcf.day,1001) d
      read(qcf.hr,1001) h

      btime = getbtime(y,m,d,h,0)   !Forms a file name for current data's date/time
      etime = getntime(btime)       !Increments current date to form next date/time file name. 

C
C     Read date/time character string as integers
C
      read(btime,1003) bt
      read(etime,1003) et
      read(gridtm(1),1003) gt1
      read(gridtm(2),1003) gt2

C     write(*,*) 'loadgrid(): btime=',btime,' etime=',etime,' t=',t
C     write(*,*) '     gridtm1=',gridtm(1),' gridtm2=',gridtm(2)

      if(t.eq.1 .and. bt.ge.gt1 .and. bt.lt.gt2) then
         loadgrid=1
         goto 999
      end if

      if(t.eq.2 .and. bt.ge.gt2 .and. bt.lt.gt1) then
         loadgrid=2
         goto 999
      end if

C
C     Call readmaps routine which actually read in the MAPS data. Two time periods
C     worth of data are retained. The first flag passed into readmaps() indicates
C     were the new data should be loaded.
C
      if(bt.eq.gt1) then
         fnameanal=etime
         rc=readmaps(2,fnameanal) ! load next hr's data into grid time slot 2

         if(rc .ne. 0) goto 900
         gridtm(2)=fnameanal
         loadgrid=1
         goto 999

      else if(bt.eq.gt2) then
         fnameanal=etime
         rc=readmaps(1,fnameanal) ! load next hr's data into grid time slot 1 

         gridtm(1)=fnameanal
         if(rc .ne. 0) goto 900
         loadgrid=2
         goto 999

      else if(bt.lt.gt1 .and. bt.lt.gt2) then
         loadgrid=t
         goto 999

      else
         fnameanal=btime
         rc=readmaps(1,fnameanal) ! load current hr's data into grid time slot 1 
         if(rc .ne. 0) goto 900

         gridtm(1)=fnameanal
         fnameanal=etime
         rc=readmaps(2,fnameanal) ! load next hr's data into grid time slot 2
         if(rc .ne. 0) goto 900

         gridtm(2)=fnameanal
         loadgrid=1
      end if

C      write(*,*) 'loadgrid():  gridtm1=',gridtm(1),' gridtm2=',gridtm(2)

      goto 999

900   write(*,1002) rc,qcf.network,qcf.station,
     +        qcf.yr,qcf.mo,qcf.day,qcf.hr,qcf.min
      loadgrid=-1

999   return

C
C     Updated formats to handle 15 char stn ID.
C
1001  format(i2)
1002  format('Could not load MAPS analyses (rc=',i4,').  No QC done',/,
     +  'qcf record: ',a10,2x,a15,2x,a2,'/',a2,'/',a2,2x,a2,':',a2)
1003  format(i9)

      end  ! loadgrid()

C************************************************************************
C
C function: readmaps()
C description: read the grid with the analyses
C
C   This function originally read the MAPS surface analyses in packed
C   ASCII format, but has since be updated to read the MAPS data from
C   an NetCDF file. MAPS provides 9 variables, but we only use 7. 
C   We use vars (1,2,3,4,5,8 and 9) during the comparison process.
C
C	9 variables are provided by MAPS: 
C
C	  *1=MAPS sea-level pressure (mb), 
C         *2=potential temperature (deg K),
C	  *3=u component (m/s), 
C         *4=v component (m/s) 
C	  *5=dew point temperature (deg K), 
C          6=dew point depression (deg C),
C	   7=altimeter change in 3 hours (mb/3h),
C	  *8=NWS sea-level pressure (mb), 
C         *9=altimeter (mb)
C
C 21 Jan 94 lec
C   Previously updated path name for MAPS files...was /spare/tmp.
C 23 Aug 94 lec
C   Updated s/w to read from NetCDF file. Original data was in a packed
C   ASCII format where a numbers were between 0 and 9999 which is a 
C   fraction of the range between the max and min values for the field.
C   The original unpacking algorithm is: value = range * IG / 10000. + min
C
C************************************************************************

      integer*2 function readmaps(t,fnameanal)

      implicit none

      include "qcglobal.h"
      include "gridcom.h"
      include 'debug.h'
      include 'ioerr.h'
      include '/usr/local/include/netcdf.inc'

C
C     NetCDF variables. Note that in FORTRAN, the dimensions are reversed
C     from the CDL (NetCDF) declaration with the first dimension varying 
C     fastest and the record dimension as the last dimension of a record 
C     variable. So the FORTRAN vars (as per examples in the NetCDF docs)
C     should be defined as follows.
C
      integer   ndims, rec, y, x

      parameter (ndims=3)           ! number of dimensions
      parameter (x=81, y=62, rec=1) ! dimension sizes; Currently only one rec per file.

      integer   id, valtid
      integer   ncid, rcode

      integer start(ndims), count(ndims)   !hyperslap

      real    vals(x,y,rec)

      data start /1, 1, 1/                 ! start at first value
      data count /x, y, rec/               ! Get hyperslap of all the data at
                                           ! that rec. These files currently
                                           ! have only one record.

      double precision   valtime           ! time all other data is valid
      integer            valindex(1)
      data               valindex /1/      ! time vars only have one dimension

      double precision   secs_1970_1994    ! # secs between 1970-1-1 and 1994-1-1
      data               secs_1970_1994 /757296000.0/  ! can this be an int???

      integer            julian_date, fname_jul, yr


C
C     The First 9-digits of input filename time is in 
C     format yyjjjhh00 where yy is the year,
C     jj is the Julian date, and hh is the hour UTC.
C     Verify that file name matches date inside file with data.
C
      character*9  getntime  ! fn that increments current date
                             ! to determine next hr file name.
      character*9  fnameanal
      character*80 analname

      character*8  var_name(GRIDP)
      character*8  varname2(GRIDP)

      integer      t
      integer      i,j,k

C
C     Actual variable names in the NetCDF file.
C
      data var_name /'alti', 'slp', 'mslp',
     +               'pot', 'dpt', 'u', 'v'/

C 
C     Conversion factors which must be applied to
C     each variable to get into proper units. Order 
C     same as for var_name() array above. Want:
C     mb, mb, mb, K, K, m/s, m/s". Note that pressures 
C     arrive in Pascals so divide by 100 to get mb. 
C 

      real          fac
      real          conv_fac(GRIDP) 
 
      data conv_fac / 0.01, 0.01, 0.01,
     +                1.00, 1.00, 1.00, 1.00 / 
 
C
C     Following array indicates the order of data 
C     in output grid.
C
      data varname2 /'Altimetr','NWS SLP ','MAPS SLP',
     +               'Pot Temp','Dew Pt  ',
     +               'U Wind  ','V Wind  '/

C******************************************************************

      if(debug) then
         write(*,*) 'Reading ',fnameanal,
     +              ' MAPS analyses into grid time: ', t
      end if

C
C     Try to open the NetCDF MAPS file for the current
C     hour. If that hour is not available then search for
C     any of the next 3 hours. We allow interpolation of
C     data values over a max of 3 hours. If none of the
C     next three hours is available, then the current
C     hours data can not be Quality Controlled. 
C
C     Note that NetCDF form of MAPS file names is:
C         YYJJJHHMM0000 where JJJ is the julian date.
C
      do i=1,4 

         readmaps=0
         analname='../MAPS/'//fnameanal//'0000'

C
C        Open the NetCDF file Turn off fatal error
C        handling, to determine if file exists.
C
         call NCPOPT (NCVERBOS)
         ncid = ncopn(analname, ncnowrit, rcode)
C        write(*,*) 'ncid, rcode: ', ncid, rcode

C
C        If the MAPS file is missing, try to 
C        load the next hour instead. May not
C        have proper error code for file does
C        not exist. It's not clear which if any
C        of the NetCDF error codes means this.
C
         if(ncid.lt.0 .and. rcode.ne.0) then                   !NetCDF MAPS file does not exist?
            write(*,*) 'Problem loading File: ',analname,
     +                 '  Loading the next hour instead. rcode = ',rcode

            fnameanal = getntime(fnameanal)    !Determines name of next hr's file.

            if(debug) then
               write(*,*) 'Reading ',fnameanal,
     +                    ' MAPS analyses into grid time: ', t
            end if

         else if(rcode.eq.0) then	       !NetCDF MAPS file opened okay
            goto 10
         else			               !Error opening NetCDF MAPS file
            goto 901
         end if

      end do    ! on i=1,4

      write(*,*) 'Too many MAPS file missing.  No qc done'
      readmaps=-999
      return

C----------------------------------------------------------------

10    continue

      call NCPOPT( NCVERBOS+NCFATAL)    ! Turn NetCDF fatal error handling back on.

C
C     First retrieve the valid time of the data
C     and ensure it matches the file's name.
C
C     Set file mode to data mode??
C
      valtid = ncvid (ncid, 'valtime', rcode)                ! get variable id
C     write(*,*) 'valid time id = ', valtid

      call ncvgt1 (ncid, valtid, valindex, valtime ,rcode)   ! get single value.
C     write(*,*) 'valid time = ', valtime

C
C     Convert valid time (number of seconds since 1970-1-1
C     to a julian date. Very large integer values.
C
      julian_date = (valtime - secs_1970_1994)/86400

      read(fnameanal,22) yr,fname_jul                        ! parse julian day out
22    format(i2,i3)                                          ! of input filename.

C     write(*,*) 'julian_date, fname_jul ', julian_date, fname_jul

      if(julian_date .ne. fname_jul) then
         write(*,*) 'readmaps():',
     +      'maps file name=',fnameanal,
     +      ' does not match julian date in the file: ',julian_date
         stop
      endif

C
C     Got correct date, now extract data. All variables in 
C     NetCDF file are floats with 3 dimensions. The last
C     dimension is 'record' which is currently always one.
C     vals( x, y, rec ). T is the time slot (1 or 2) that
C     the data is being placed into the final grid.
C
      do K = 1,GRIDP

         id = ncvid (ncid, var_name(K), rcode)            ! get the vars ids
C        write(*,*)'id: ', id,' var_name(K): ', var_name(K)

         call ncvgt (ncid, id, start, count, vals, rcode)  ! get the data

C        write(*,*)'Called ncvgt, vals: ', vals

         do j=1, GRIDY
            do i=1,GRIDX

              if (vals(i,j,1) .lt. -999.00) then
                 fac = 0.01      ! Convert Netcdf missing -99999. to -999.99
              else
                 fac = conv_fac(K)
              end if

              grid(i,j,K,T) = vals(i,j,1)*fac  ! place in final grid. 
            end do
         end do


      end do  ! K

C
C     List grid if debug flag set.
C
      if (debug .and. yn .ne. 'q') then

         type *,' Grid is loaded.  Display it?'
         accept 101, YN
101      format(A1)

         if (YN .eq. 'N' .or. YN .eq. 'n') goto 999

         write(*,1007) (varname2(i),i=1,GRIDP)

         do j=1,GRIDY
            do i=1,GRIDX
               write(*,1008) i,j,(GRID(i,j,k,T),k=1,GRIDP)
            end do

            type *,' Continue?'
            accept 101, YN
            if (YN.eq.'N' .or. YN.eq.'n' .or. yn.eq.'q') goto 999

        end do 
      end if

      goto 999

901   errstr=gerror(errstr)
      write(*,*) 'File: ',analname,'readmaps(): I/O Error: ', 
     +           ios,' ',errstr
      readmaps=ios

999   call ncclos (ncid, rcode)
      return 

1007  format(' ',//,'  X  Y   ',9(A8,3X),/)
1008  format(' ',2(I2,X),6(F10.3,X))

      end

C************************************************************************
C
C function: getvals()
C description: get the parameter values at the qcf point by interpolating
C              the values based on the surrounding 4 grid point values.
C
C returns:  analvals : array of interpolated parameter values at
C                      the qcf point
C           xd       : the x distance from the SW MAPS grid point
C           yd       : the y distance from the SW MAPS grid point
C************************************************************************

      subroutine getvals(analvals,b,gridtm)

      implicit none

      include 'qcglobal.h'
      include 'qccom.h'
      include 'gridcom.h'
      include 'qcfcom.h'
      include 'stncom.h'
      include 'debug.h'
      include 'parmnames.h'

      character*9 gridtm(*)
      character*9 getqcftime,qcfdt
      real        analvals(*)				! array of analyses values
      integer*2   b					! begin time dimension of grid array
      integer     dt    ! t offset (min) from nearest SW grid point of qc point
      integer     xd    ! x offset (m) from grid point (1,1) of qc point
      integer     yd    ! y offset (m) from grid point (1,1) of qc point
      real        valxt,valxb,valxy1,valxy2
      real        xratio,yratio,tratio
      real        p,pt,u,v,m
      real        convpres,convtemp,convws,convwd	!conversion functions
      real        x_grid,y_grid				!MAPS ll2xy functions
      integer     x1,y1
      integer     i,e
      integer     qcfmin,getmin

      if(debug) write(*,*) 'Interpolate the analyses values'
      e=2
      if(b.eq.2) e=1
	 m=GRIDSIZE*1000
C
C interpolate analysis coordinates for x,y
C
      xd=x_grid(qcf.lat,qcf.lon)
      yd=y_grid(qcf.lat,qcf.lon)
      x1=xd/m+1
      y1=yd/m+1
      stndx=xd - m*(x1-1)
      stndy=yd - m*(y1-1)

      if(x1.ge.GRIDX .or. y1.ge.GRIDY .or.
     +   xd.lt.0.0 .or. yd.lt.0.0) then

      write(*,1003) qcf.network,qcf.station,
     +    qcf.yr,qcf.mo,qcf.day,qcf.hr,qcf.min,qcf.lat,qcf.lon,xd,yd

1003  format('qcf point is outside of the MAPS grid.  No MAPS QC done',/,
     +  'qcf record: ',a10,2x,a15,2x,a2,'/',a2,'/',a2,2x,a2,':',a2,
     +  '   lat: ',f10.5,'   lon: ',f11.5,'   xd=',i8,' yd=',i8)

      return
      end if

      xratio=mod(xd,m)/m
      yratio=mod(yd,m)/m

C
C convert qcf date to julian
C
      qcfdt=getqcftime()
      qcfmin=getmin(gridtm(b),qcfdt)
1002  format(i2)
      dt=getmin(gridtm(b),gridtm(e))

      if(dt .eq. 0) then
         tratio=0.0
      else
         tratio=real(qcfmin)/real(dt)
      end if

      if(debug .and. yn.ne.'q') then
         write(*,*) 'lat=',qcf.lat,' lon=',qcf.lon,' min=',qcf.min
         write(*,*) 'xd=',xd,' yd=',yd,' m=',m,
     +              'x1=',x1,' y1=',y1,' b=',b,' e=',e
         write(*,*) 'xratio=',xratio,' yratio=',yratio,' tratio=',tratio
         write(*,*) 'gridtm(1)=',gridtm(1),' gridtm(2)=',gridtm(2),
     +              ' qcfdt=',qcfdt,' dt=',dt,' qcfmin=',qcfmin
      end if

      do i=1,NANALPARMS
         valxt=grid(x1,y1+1,i,b)+
     +        (grid(x1+1,y1+1,i,b)-grid(x1,y1+1,i,b))*xratio
         valxb=grid(x1,y1,i,b)+
     +        (grid(x1+1,y1,i,b)-grid(x1,y1,i,b))*xratio
         valxy1 = valxb+(valxt-valxb)*yratio

      if(debug .and. yn.ne.'q') then
1005  format('grid(',i2,',',i2,',',i1,',',i1,')=',f11.5)
         write(*,1005) x1+1,y1,i,b,grid(x1+1,y1,i,b)
         write(*,1005) x1+1,y1+1,i,b,grid(x1+1,y1+1,i,b)
         write(*,1005) x1,y1,i,b,grid(x1,y1,i,b)
         write(*,1005) x1,y1+1,i,b,grid(x1,y1+1,i,b)
         write(*,*) 'valxt=',valxt,' valxb=',valxb,' valxy1=',valxy1
      end if

         valxt=grid(x1,y1+1,i,e)+
     +        (grid(x1+1,y1+1,i,e)-grid(x1,y1+1,i,e))*xratio
         valxb=grid(x1,y1,i,e)+
     +        (grid(x1+1,y1,i,e)-grid(x1,y1,i,e))*xratio
         valxy2=valxb+(valxt-valxb)*yratio
         analvals(i)=valxy1+(valxy2-valxy1)*tratio

      if(debug .and. yn.ne.'q') then
         write(*,1005) x1+1,y1,i,e,grid(x1+1,y1,i,e)
         write(*,1005) x1+1,y1+1,i,e,grid(x1+1,y1+1,i,e)
         write(*,1005) x1,y1,i,e,grid(x1,y1,i,e)
         write(*,1005) x1,y1+1,i,e,grid(x1,y1+1,i,e)
         write(*,*) 'valxt=',valxt,' valxb=',valxb,' valxy2=',valxy2
         write(*,*) 'analvals(',i,')=',analvals(i)
      end if

      end do

      if(debug .and. yn.ne.'q') then
         write(*,*) 'Maps Analyses values:'
         do i=1,NANALPARMS
            write(*,1001) i,parmnames(i),analvals(i)
1001        format(i2,3x,a20,3x,f11.5,/)
         end do
      end if

C
C Convert the analyses values to qcf format and units
C
      p=analvals(1)
      pt=analvals(4)
      u=analvals(6)
      v=analvals(7)

      if (p .lt. -999.00) then
         analvals(1)=-999.99
      else
         analvals(1)=convpres(p,qcf.elev)       ! altimeter to stn pressure
      end if

      if ((pt .lt. -999.00) .OR. (analvals(1) .lt. -999.00)) then
         analvals(4)=-999.99
      else
         analvals(4)=convtemp(pt,analvals(1))   ! potential temp to temp
      end if

      if (analvals(5) .lt. -999.00) then
         analvals(5)=-999.99
      else
         analvals(5)=analvals(5)-273.16         ! kelvin to celcius
      end if
       
      if ((u .lt. -999.00) .or. (v .lt. -999.00)) then
         analvals(6)= -999.99
         analvals(7)= -999.99
      else
         analvals(6)=convws(u,v)                ! u,v winds to wind speed
         analvals(7)=convwd(u,v,analvals(6))    ! u,v winds to wind direction
      end if

      if(debug .and. yn.ne.'q') then
         write(*,*) 'Converted Maps Analyses values:'
         do i=1,NANALPARMS
            write(*,1001) i,parmnames(i),analvals(i)
         end do
	    type *,' Continue?'
         accept 1004, yn
1004     format(a1)
      end if

      return
      end   ! getvals()

C************************************************************************
C
C function: convpres()
C description: convert altimeter pressure to station pressure
C
C returns:  station pressure
C************************************************************************/

      real function convpres(p,elev)

      real p,elev
C     real p0,a,T0
      real n,c1,c2
C     parameter (p0=1013.25)		!std sea level pressure (mb)
C     parameter (a=0.0065)		!lapse rate
C     parameter (T0=288.0)		!std sea level temp
      parameter (n=0.190284)		!=aR, where R is dry air gas constant
      parameter (c1=.00008422881)	!=(p0**n)*a)/T0 is a constant
      parameter (c2=5.255303)		!=1/n is a constant

C     convpres=(((p**n)-((((p0**n)*a)/T0)*elev))**(1/n))+.3
      convpres=(p**n - c1*elev)**c2 + .3
      return
      end     !convpres()

C************************************************************************
C
C function: convtemp()
C description: convert potential temperature to station temperature
C
C returns:  dew point temperature
C************************************************************************/

      real function convtemp(pt,p)

      real pt
      real p

      convtemp=pt/((1000./p)**0.286)-273.16
      return
      end    ! convtemp()

C************************************************************************
C
C function: convws()
C description: convert u,v winds to wind speed
C
C returns:  convws  : wind speed
C************************************************************************/

      real function convws(u,v)

      real u,v

      convws=sqrt(u**2+v**2)
	 return
      end    ! convws()

C************************************************************************
C
C function: convwd()
C description: convert u,v winds to direction
C
C returns:  convwd  : wind direction
C************************************************************************/

      real function convwd(u,v,ws)

      real u,v,ws
      real RTA
      data RTA /57.2957795/

      convwd=0.0
      if(ws .gt. 0.01) then
         convwd=atan2(u,v)*RTA+180.0
      end if
      return
      end   ! convwd()

C************************************************************************
C
C function: addvar()
C description: add variance amounts to dev array for final statistics
C
C************************************************************************/

      subroutine addvar(parm,flag,amt)

      implicit none

      include 'qcglobal.h'
      include 'qccom.h'

      integer     parm
      character*1 flag
      real        amt
      integer     i

      if(flag .eq. 'G') i=2  !Good
      if(flag .eq. 'B') i=3  !Bad
      if(flag .eq. 'D') i=4  !Dubious

C add to flag columns
      dev(parm,i)=dev(parm,i)+abs(amt)
	 if(abs(amt) .gt. abs(dev(parm,i+4))) dev(parm,i+4)=amt
      dev(parm,i+8)=dev(parm,i+8)+amt**2
	 if(amt .lt. 0.0) dev(parm,i+12)=dev(parm,i+12)+amt
	 if(amt .gt. 0.0) dev(parm,i+16)=dev(parm,i+16)+amt

C add to totals
      dev(parm,1)=dev(parm,1)+abs(amt)
	 if(abs(amt) .gt. abs(dev(parm,5))) dev(parm,5)=amt
      dev(parm,9)=dev(parm,9)+amt**2
	 if(amt .lt. 0.0) dev(parm,13)=dev(parm,13)+amt
	 if(amt .gt. 0.0) dev(parm,17)=dev(parm,17)+amt

      return
      end   ! addvar()

C************************************************************************
C
C function: compvals()
C description: compare the parameter values at the qcf pointeger with the
C              interpolated analysis values. This s/w also applies
C              additional checks such as gross limit checks and corrects
C              some misc oversights from the data conversion process.
C
C returns:  analvals : array of interpolated parameter values at
C                      the qcf point
C*************************************************************************

      subroutine compvals(analvals,var)

      implicit none

      include 'qcglobal.h'
      include 'qccom.h'
      include 'configcom.h'
      include 'qcfcom.h'
      include 'stncom.h'
      include 'debug.h'

      integer  i
      real     analvals(*)
      real     var(*)
      real     roc(NQCPARMS)
      real     roctm
      integer  getmin
      character*9 rocdt,qcfdt,getqcftime
      integer  days
      integer  stncode			!station code (index) of stns()
      integer  getstncode		!function to return station code
      integer  getdays			!function to return no days from day1

      if(debug) write(*,*) 'Compare qcf values with analyses values'

      do i=1,NQCPARMS
         roc(i)=-999.99			! rate of change
      end do

C
C convert qcf date to julian
C
      qcfdt=getqcftime()

C
C look up the network/station code
C
      stncode=getstncode()

C
C get days from day1
C
      days=getdays()

C***********************************************************************
C correct some misc oversights in the data conversion
C
C 21 Jan 94 lec - Removed correction which changed ISWS precip qc flag
C                 for stn llc to 'X' for glitch.
C
C***********************************************************************
C awosq sea level pressure qc flag should be N instead of M
      if(qcf.parms(2).qc .eq. 'M' .and.  qcf.network .eq. 'AWOSQ')
     +   qcf.parms(2).qc = 'N'
C awos1 sea level pressure qc flag should be N instead of M
      if(qcf.parms(2).qc .eq. 'M' .and.  qcf.network .eq. 'AWOS1')
     +   qcf.parms(2).qc = 'N'
C pam1 sea level pressure qc flag should be N instead of M
      if(qcf.parms(2).qc .eq. 'M' .and.  qcf.network .eq. 'PAM1')
     +   qcf.parms(2).qc = 'N'
C wdpn calc sea level pressure qc flag should be N instead of M
      if(qcf.parms(3).qc .eq. 'M' .and. qcf.network .eq. 'WDPN')
     +   qcf.parms(3).qc = 'N'
C cloud layers 2 and 3 qc flag should be N instead of M if cloud layer
C 1 is present
      if(qcf.cc(1).cldqc .eq. 'U'  .or. qcf.cc(1).cldqc .eq. 'E') then
         if(qcf.cc(2).ceilqc .eq. 'M') qcf.cc(2).ceilqc = 'N'
         if(qcf.cc(3).ceilqc .eq. 'M') qcf.cc(3).ceilqc = 'N'
         if(qcf.cc(2).cldqc .eq. 'M') qcf.cc(2).cldqc = 'N'
         if(qcf.cc(3).cldqc .eq. 'M') qcf.cc(3).cldqc = 'N'
      end if
C if wind speed = 0 (calm) set wind direction to 0
      if(qcf.parms(6).val .eq. 0.0 .and. qcf.parms(7).qc .eq. 'U')
     +   qcf.parms(7).val = 0.0
C squall/gust winds should be N instead of M unless winds are missing
         if(qcf.squal.qc .eq. 'M' .and. qcf.parms(6).qc .ne. 'M')
     +      qcf.squal.qc = 'N'
C
C end of corrections
C***********************************************************************

C now loop through the parms and compare to MAPS
      do i=1,NANALPARMS

C leave M (missing), N (not measured), X (PAM glitch) qc codes untouched
         if(qcf.parms(i).qc .eq. 'M') then
            stats(i,4)=stats(i,4)+1
            goto 100
         end if
         if(qcf.parms(i).qc .eq. 'N') then
            stats(i,5)=stats(i,5)+1
            goto 100
         end if
         if(qcf.parms(i).qc .eq. 'X') then
            stats(i,6)=stats(i,6)+1
            goto 100
         end if
         if(qcf.parms(i).val .eq. -999.99) then
            qcf.parms(i).qc = 'N'
            stats(i,5)=stats(i,5)+1
            goto 100
         end if

C
C Calculate rate of change for this station
C
      if(stns(stncode).val(i) .eq. -999.99) then
         roc(i)=-999.99
	    rocdt="        0"
      else
         if(stns(stncode).dt(i) .gt. 0) then
            write(rocdt,1001) stns(stncode).dt(i)
            roctm=real(getmin(qcfdt,rocdt))/5.0
            if(roctm .le. 0) roctm = 1.0
         else
	       rocdt="        0"
            roctm=1.0
         end if
         roc(i)=(qcf.parms(i).val - stns(stncode).val(i))/roctm
      end if

C
C Compare the MAPS values to the qcf record values
C
         var(i)=qcf.parms(i).val-analvals(i)

C
C flag calc sea level pressure with the station pressure flag
C
         if(i.eq.3 .and. qcf.parms(1).qc .ne. 'M'
     +             .and. qcf.parms(1).qc .ne. 'N'
     +             .and. qcf.parms(1).qc .ne. 'X'
     +             .and. qcf.parms(1).qc .ne. 'U') then
            qcf.parms(3).qc = qcf.parms(1).qc
            if(qcf.parms(3).qc .eq. 'G') then
               stats(i,1)=stats(i,1)+1
			call addvar(3,'G',var(3))
               stns(stncode).val(i)=qcf.parms(i).val
               read(qcfdt,1001) stns(stncode).dt(i)
            endif
            if(qcf.parms(3).qc .eq. 'E') then
		     stats(3,8)=stats(3,8)+1
			call addvar(3,'G',var(3))
               stns(stncode).val(i)=qcf.parms(i).val
               read(qcfdt,1001) stns(stncode).dt(i)
            endif
            if(qcf.parms(3).qc .eq. 'B') then
               stats(i,2)=stats(i,2)+1
			call addvar(3,'B',var(3))
		     call puttoss(stncode,days,54,qcf.parms(3).qc)
            end if
            if(qcf.parms(3).qc .eq. 'D') then
               stats(i,3)=stats(i,3)+1
			call addvar(3,'D',var(3))
		     call puttoss(stncode,days,54,qcf.parms(3).qc)
            end if
	       goto 100
         end if

C
C If wind direction, adjust for var > 180 degrees
C
         if(i.eq.7 .and. var(i).gt.180.0) then
            var(i)=var(i)-360.0
         else if(i.eq.7 .and. var(i).lt.-180.0) then
            var(i)=var(i)+360.0
         end if

C
C wind speed must be >= 0 or it is bad
C
         if(i.eq.6 .and. (qcf.parms(i).val .lt. 0.0)
     +             .and. (qcf.parms(i).val .ne. -999.99)) then
            qcf.parms(i).qc = 'B'
            stats(i,2)=stats(i,2)+1
		  call puttoss(stncode,days,40,qcf.parms(i).qc)
            goto 100
         end if

C
C wind direction must be between 0 and 360 degrees or it is bad
C
         if(i.eq.7 .and. (qcf.parms(i).val .lt. 0.0
     +             .or. qcf.parms(i).val .gt. 360.0)
     +             .and. (qcf.parms(i).val .ne. -999.99)) then
            qcf.parms(i).qc = 'B'
            stats(i,2)=stats(i,2)+1
		  call puttoss(stncode,days,41,qcf.parms(i).qc)
            goto 100
         end if

C
C if wind speed = 0 (calm) then set wind dir qc flag same as wind speed
C
         if(i.eq.7 .and. qcf.parms(6).val .eq. 0.0 .and.
     +      (qcf.parms(7).qc .eq. 'U' .or.
     +       qcf.parms(7).qc .eq. 'E')) then
            qcf.parms(7).qc = qcf.parms(6).qc
            if(qcf.parms(7).qc .eq. 'G') stats(7,1)=stats(7,1)+1
            if(qcf.parms(7).qc .eq. 'E') stats(7,8)=stats(7,8)+1
            if(qcf.parms(7).qc .eq. 'B') then
                  stats(7,2)=stats(7,2)+1
		        call puttoss(stncode,days,58,qcf.parms(7).qc)
            end if
            if(qcf.parms(7).qc .eq. 'D') then
                  stats(7,3)=stats(7,3)+1
		        call puttoss(stncode,days,58,qcf.parms(7).qc)
            end if
	       goto 100
         end if

C
C MAPS values of -999.99 mean MAPS is unavailable for this parameter
C
         if(analvals(i) .eq. -999.99) then
            stats(i,7)=stats(i,7)+1
            goto 100
         end if

C
C Variance values of -999.99 means variance is unavailable for this parameter
C
         if(abs(var(i)).gt.varb(i) .and. varb(i).ne.-999.99) then
            qcf.parms(i).qc = 'B'
            stats(i,2)=stats(i,2)+1
            call addvar(i,'B',var(i))
		  call puttoss(stncode,days,i,qcf.parms(i).qc)
         else if(abs(var(i)).gt.varq(i) .and. varq(i).ne.-999.99) then
            if(qcf.parms(i).qc .eq. 'E') then
               stats(i,8)=stats(i,8)+1
               goto 100
            end if
            qcf.parms(i).qc = 'D'
            stats(i,3)=stats(i,3)+1
            call addvar(i,'D',var(i))
		  call puttoss(stncode,days,i,qcf.parms(i).qc)
         else if(qcf.parms(i).qc .eq. 'E') then
            stats(i,8)=stats(i,8)+1
            goto 100
         else if(roc(i).ne.-999.99 .and. abs(roc(i)).gt.rocb(i)
     +          .and. rocb(i).ne.-999.99) then
            qcf.parms(i).qc = 'B'
            stats(i,2)=stats(i,2)+1
		  call puttoss(stncode,days,i+NQCPARMS,qcf.parms(i).qc)
         else if(roc(i).ne.-999.99 .and. abs(roc(i)).gt.rocq(i)
     +          .and. rocq(i).ne.-999.99) then
            qcf.parms(i).qc = 'D'
            stats(i,3)=stats(i,3)+1
		  call puttoss(stncode,days,i+NQCPARMS,qcf.parms(i).qc)
         else if(varb(i).ne.-999.99 .or. varq(i).ne.-999.99) then
            qcf.parms(i).qc = 'G'
            stats(i,1)=stats(i,1)+1
            call addvar(i,'G',var(i))
            stns(stncode).val(i)=qcf.parms(i).val
            read(qcfdt,1001) stns(stncode).dt(i)
         else
            stats(i,7)=stats(i,7)+1
         end if
100      continue
      end do

C
C Miscellaneous qc checks
C
C Compare precip to gross limits
C leave M (missing), N (not measured), X (PAM glitch) 
C and E (Estimated) qc codes untouched
C
         if(qcf.parms(8).qc .eq. 'M') then
            stats(8,4)=stats(8,4)+1
            goto 110
         end if
         if(qcf.parms(8).qc .eq. 'N') then
            stats(8,5)=stats(8,5)+1
            goto 110
         end if
         if(qcf.parms(8).qc .eq. 'X') then
            stats(8,6)=stats(8,6)+1
            goto 110
         end if
         if(qcf.parms(8).val .eq. -999.99) then
            qcf.parms(8).qc = 'M'
            stats(8,4)=stats(8,4)+1
            goto 110
         end if

      var(8)=qcf.parms(8).val
      if(stns(stncode).val(8) .eq. -999.99) then
         roc(8)=-999.99
	    rocdt="        0"
      else
         if(stns(stncode).dt(8) .gt. 0) then
            write(rocdt,1001) stns(stncode).dt(8)
            roctm=real(getmin(qcfdt,rocdt))/5.0
         else
	       rocdt="        0"
            roctm=1.0
         end if
         roc(8)=(qcf.parms(8).val - stns(stncode).val(8))/roctm
      end if
	 if(qcf.parms(8).val .lt. 0) then
         qcf.parms(8).qc = 'B'
         stats(8,2)=stats(8,2)+1
	    call puttoss(stncode,days,42,qcf.parms(8).qc)
      else if(qcf.parms(8).val .gt. varb(8) .and.
     +        varb(8).ne.-999.99) then
         qcf.parms(8).qc = 'B'
         stats(8,2)=stats(8,2)+1
         call addvar(8,'B',var(8))
	    call puttoss(stncode,days,8,qcf.parms(8).qc)
      else if(qcf.parms(8).val .gt. varq(8)
     +         .and. varq(i).ne.-999.99) then
         if(qcf.parms(8).qc .eq. 'E') then
            stats(8,8)=stats(8,8)+1
            goto 110
         end if
         qcf.parms(8).qc = 'D'
         stats(8,3)=stats(8,3)+1
         call addvar(8,'D',var(8))
	    call puttoss(stncode,days,8,qcf.parms(8).qc)
      else if(roc(8).ne.-999.99 .and. abs(roc(8)).gt.rocb(8)
     +       .and. rocb(8).ne.-999.99) then
         qcf.parms(8).qc = 'B'
         stats(8,2)=stats(8,2)+1
	    call puttoss(stncode,days,8+NQCPARMS,qcf.parms(8).qc)
      else if(qcf.parms(8).qc .eq. 'E') then
         stats(8,8)=stats(8,8)+1
         goto 110
      else if(roc(8).ne.-999.99 .and. abs(roc(8)).gt.rocq(8)
     +       .and. rocq(8).ne.-999.99) then
         qcf.parms(8).qc = 'D'
         stats(8,3)=stats(8,3)+1
	    call puttoss(stncode,days,8+NQCPARMS,qcf.parms(8).qc)
      else
         qcf.parms(8).qc = 'G'
         stats(8,1)=stats(8,1)+1
         call addvar(8,'G',var(8))
         stns(stncode).val(8)=qcf.parms(8).val
         read(qcfdt,1001) stns(stncode).dt(8)
      end if
 
C
C Can't have precip in clear air
C
105   if(qcf.parms(8).val.gt.0 .and. qcf.parms(8).qc .eq.'G'
     +   .and. qcf.cc(1).cldamt.eq.0) then
         qcf.parms(8).qc = 'D'
         stats(8,1)=stats(8,1)-1
         stats(8,3)=stats(8,3)+1
         stats(13,3)=stats(13,3)+1
         qcf.cc(1).cldqc = 'D'
	    call puttoss(stncode,days,13,qcf.parms(8).qc)
      end if

110      continue

C-------------------------------------------------
C
C Compare squal winds to gross limits
C leave M (missing), N (not measured), X (PAM glitch)
C and E (Estimated) qc codes untouched
C
C 27 Sept 94 lec
C   It has been decided to drop gross limit
C   checks on the squal amount.
C
C         if(qcf.squal.qc .eq. 'M') then
C            stats(9,4)=stats(9,4)+1
C            goto 120
C         end if
C         if(qcf.squal.qc .eq. 'N') then
C            stats(9,5)=stats(9,5)+1
C            goto 120
C         end if
C         if(qcf.squal.qc .eq. 'X') then
C            stats(9,6)=stats(9,6)+1
C            goto 120
C         end if
C         if(qcf.squal.val .eq. -999.99) then
C            qcf.squal.qc = 'M'
C            stats(9,4)=stats(9,4)+1
C            goto 120
C         end if
C	 if(qcf.squal.val .lt. 0) then
C         qcf.squal.qc = 'B'
C         stats(9,2)=stats(9,2)+1
C	    call puttoss(stncode,days,43,qcf.squal.qc)
C      else if(qcf.squal.val .gt. varb(9) .and. varb(9).ne.-999.99) then
C         qcf.squal.qc = 'B'
C         stats(9,2)=stats(9,2)+1
C	    call puttoss(stncode,days,9,qcf.squal.qc)
C      else if(qcf.squal.val .gt. varq(9).and. varq(9).ne.-999.99) then
C         if(qcf.squal.qc .eq. 'E') then
C            stats(9,8)=stats(9,8)+1
C            goto 120
C         end if
C         qcf.squal.qc = 'D'
C         stats(9,3)=stats(9,3)+1
C	    call puttoss(stncode,days,9,qcf.squal.qc)
C      else if(qcf.squal.qc .eq. 'E') then
C         stats(9,8)=stats(9,8)+1
C         goto 120
C      else
C         qcf.squal.qc = 'G'
C         stats(9,1)=stats(9,1)+1
C      end if
C------------- End Squall amt check ----------------------------

C
C Dew Point temperature cannot be greater than dry bulb temperature
C
120   if(qcf.parms(5).val .gt. qcf.parms(4).val .and.
     +   qcf.parms(5).qc .eq. 'G' .and.
     +   qcf.parms(4).qc .eq. 'G') then
         if(qcf.network(1:3) .ne. 'PAM') then
            qcf.parms(4).qc = 'D'
            stats(4,1)=stats(4,1)-1
            stats(4,3)=stats(4,3)+1
         end if
         qcf.parms(5).qc = 'D'
         stats(5,1)=stats(5,1)-1
         stats(5,3)=stats(5,3)+1
	    call puttoss(stncode,days,39,qcf.parms(5).qc)
      end if

      if(debug .and. yn.ne.'q') then
         write (*,1003) qcf.station,qcf.yr,qcf.mo,qcf.day,qcf.hr,
     +      qcf.min,qcfdt,rocdt,roctm
         do i=1,NQCPARMS
            write (*,1007) i,var(i),roc(i)
         end do
         type *,' Continue?'
         accept 1004, yn
1004     format(a1)
      end if

      return
1001  format(i9)
1003  format('Variances for ',a10,/,
     +      '  Date: ',a2,'/',a2,'/',a2,'  Time: ',a2,':',a2,/,
     +      '  qcfdt=',a9,'; rocdt=',a9,'; roctm=',f6.2,';',/,
     +      '        var      roc',/)
1007  format(i2,2(2x,f7.2))
      end
C
C * * * * * * * * * * EARTH LENGTH CONVERSIONS* * * * * * * * * * * * * * *
C
C
C  Transformation functions from (LAT,LON) to (X,Y) in meters in the MAPS grid,
C  and the inverse functions.  (taken from the MAPS Developer Guide - 
C  Chapter 6). Added on October 29, 1984, from Stan Benjamin macros.
C
C	X_GRID and Y_GRID give (X,Y) as a function of (LAT,LON)
C
C
C GRIDSWAP IS A DUMMY FUNCTION WHICH CONTAINS THE GRID TRANSFORMATION
C FUNCTIONS AS ENTRY POINTS.
C WARNING: NON-STANDARD FORTRAN FUNCTIONS ARE UTILIZED
C          SIND,COSD,ATAND,ATAN2D
C
C History:
C
C	K. Brewster 8-85
C	B. Jewett   7-29-87	altered for use with different grid domains
C
C****************************************************************************
      Real FUNCTION GRIDSWAP()

      Implicit None

      REAL LAT,LON,X,Y
      REAL LAT_XY,LON_XY,X_GRID,Y_GRID
      REAL SIND,COSD,ATAN2D,ATAND
      REAL EARTH_RAD,TRUE_LAT,TRUE_LON,X_POLE,Y_POLE
      parameter (EARTH_RAD = 6371230.,	! Mean earth radius (m)
     +           TRUE_LAT=40.0,     	! True late of projection (deg N)
     +           TRUE_LON=-15.0,	    	! Meridian aligned w/x-axis (deg)
     +           X_POLE=1854642.0,		! x coord of North Pole (m)
     +           Y_POLE=6691869.0)	  	! y coord of North Pole (m)
C
      ENTRY X_GRID(LAT,LON)
      X_GRID=(X_POLE+((1.+SIND(TRUE_LAT))/
     +(1.+SIND(LAT)))*EARTH_RAD*COSD(LAT)*COSD(LON-TRUE_LON))
      RETURN
C
      ENTRY Y_GRID(LAT,LON)
      Y_GRID=(Y_POLE+((1.+SIND(TRUE_LAT))/
     +(1.+SIND(LAT)))*EARTH_RAD*COSD(LAT)*SIND(LON-TRUE_LON))
      RETURN
C
C	LAT_XY and LON_XY give (LAT,LON) as a function of (X,Y)
C
      ENTRY LAT_XY(X,Y)
      LAT_XY=90.-2.*ATAND(SQRT((X-X_POLE)**2+(Y-Y_POLE)**2)
     + /(EARTH_RAD*(1.+SIND(TRUE_LAT))))
      RETURN
C
      ENTRY LON_XY(X,Y)
      LON_XY=TRUE_LON+ATAN2D((Y-Y_POLE),(X-X_POLE))
      RETURN
      END
