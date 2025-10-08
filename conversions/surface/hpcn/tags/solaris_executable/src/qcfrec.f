C
C     This is not the original qcfrec - LEC has been
C     modifying the version located for test purposes.
C
      subroutine qcfrec(q,fileno)

c     qcfrec.f - writes a single record into the qcf file

      include '../../inc/qcf.inc'
      integer fileno
      record /qcf/ q


      write(fileno,1000, err=2000) q.date,
     *q.qtime,q.qnet,q.statn,q.ll,q.occur,q.staelv,
     *q.staprs,q.staflg,q.seaprs,q.seaflg,
     *q.cmpsea,q.cmpflg,q.temp,q.tmpflg,
     *q.dewpnt,q.dewflg,q.wndspd,q.spdflg,q.wnddir,q.dirflg,
     *q.precip,q.prcflg,q.sg,q.squall,q.sqlflg,
     *q.prswea,q.pwflg,q.visib,q.visflg,
     *q.celht1,q.celfg1,q.c1flg,q.clamt1,q.ca1flg,
     *q.celht2,q.celfg2,q.c2flg,q.clamt2,q.ca2flg,
     *q.celht3,q.celfg3,q.c3flg,q.clamt3,q.ca3flg

1000  format(a2,'/',a2,'/',a2,1x,a2,':',a2,1x,a10,1x,a15,1x,f10.5,
     *          1x,f11.5,1x,i3,1x,f7.2,1x,
     *          8(f7.2,1x,a1,1x),a1,1x,f7.2,1x,a1,
     *          1x,i4,1x,a1,1x,f8.2,1x,a1,1x,
     *          3(f7.2,1x,i2,1x,a1,1x,i2,1x,a1,1x))
     goto 9999

2000  write(6,'('' qcfrec:Error writing record to the qcf file'')' )
      stop

9999  continue

      return
      end
