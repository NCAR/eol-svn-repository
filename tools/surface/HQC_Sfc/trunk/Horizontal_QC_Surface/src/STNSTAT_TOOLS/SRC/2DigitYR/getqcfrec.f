C************************************************************************
C
C function: getqcfrec()
C description: get the qcf records from the qcf file
C
C 25 Aug 94 lec
C   Updated s/w to handle 15 char station ID which was 10 chars.
C************************************************************************

      integer*2 function getqcfrec()

      implicit none

      include 'qcglobal.h'
      include 'qcfcom.h'
      include 'debug.h'
      include 'ioerr.h'
      character*255 qcfbuf

C loop throught the data records
      getqcfrec = 0
100   read(unit_qcf,1002,iostat=ios,err=901,end=900) qcfbuf
      if(qcfbuf(1:8) .eq. '********' .or.
     +   qcfbuf(1:13) .eq. 'Date     Time' .or.
     +   qcfbuf(1:13) .eq. '         UTC ' .or.
     +   qcfbuf(1:8) .eq. '**/**/**') goto 100
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
         write(0,1003,iostat=ios,err=901)
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
         write(0,1004,iostat=ios,err=901)
     +     qcf.cc(1).ceilhgt,qcf.cc(1).ceilflg,qcf.cc(1).ceilqc,
     +     qcf.cc(1).cldamt,qcf.cc(1).cldqc,
     +     qcf.cc(2).ceilhgt,qcf.cc(2).ceilflg,qcf.cc(2).ceilqc,
     +     qcf.cc(2).cldamt,qcf.cc(2).cldqc,
     +     qcf.cc(3).ceilhgt,qcf.cc(3).ceilflg,qcf.cc(3).ceilqc,
     +     qcf.cc(3).cldamt,qcf.cc(3).cldqc
	    write(0,*) ' Continue?'
         accept 1005, yn
      end if
      goto 999
900   getqcfrec = -2
      goto 999
901   errstr=gerror(errstr)
      write(0,*) 'getqcfrec(): I/O Error: ',ios,' ',errstr
      write(0,*) 'qcfbuf=',qcfbuf
999   return
1001  format(a2,'/',a2,'/',a2,1x,a2,':',a2,1x,a10,1x,a15,1x,f10.5,
     *          1x,f11.5,1x,i3,1x,f7.2,1x,
     *          8(f7.2,1x,a1,1x),a1,1x,f7.2,1x,a1,
     *          1x,i4,1x,a1,1x,f8.2,1x,a1,1x,
     *          3(f7.2,1x,i2,1x,a1,1x,i2,1x,a1,1x))
1002  format(a255)	!This must be equal to length of the qcf record
C1002  format(a235)	!This must be equal to length of the qcf record
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
      end
