C
C $Id$
C $Log$
C


      subroutine wrdqcf(q,fileno)

c     qcfrec.f - writes a single record into the qcf file

      include '../inc/dqcf.inc'

      integer fileno
      record /dqcf/ q


      write(fileno,1000, err=2000) q.year,q.date,
     *q.qtime,q.qnet,q.statn,q.ll,q.occur,q.staelv,
     *q.mxtmp,q.mxflg1,q.mxflg2,q.mntmp,q.mnflg1,q.mnflg2,
     *q.sndep,q.sdflg1,q.sdflg2,q.precip,q.prcflg1,q.prcflg2,
     *q.snfall,q.sfflg1,q.sfflg2

      goto 9999

 1000  format(a4,'/',a2,'/',a2,1x,a2,':',a2,1x,a10,1x,a10,1x,f10.5,
     *          1x,f11.5,1x,i3,1x,f7.2,1x,
     *          5(f7.2,1x,i3,1x,i3,1x),f7.2,1x,i3,i3)


1500  write(6,'('' qcfrec:Error writing record to buffer'')' )
      stop

2000  write(6,'('' qcfrec:Error writing record to the qcf file'')' )
      stop

9999  continue

      return
      end
