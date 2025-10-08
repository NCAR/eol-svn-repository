
      subroutine qcfrec(q,fileno)

c   *************************************************************
c     qcfrec.f - writes a single record into the qcf file
c
c     Modified by Lia Pennington for the Vortex Project.
c      20apr94 - added nominal date/time strings to the record
c                buffer.
c   *************************************************************

      include '../inc/qcf.inc'

      integer fileno
      integer i
      character*255 buffer 
      character*1 slash  
      character*1 colon 

      record /qcf/ q
      slash = '/'
      colon = ':'

      do i = 1, 255
         buffer(i:i) = ' '
      end do

      write(buffer(1:2), '(a2)', err=1500) q.nomdate(1)
      write(buffer(3:3), '(a1)', err=1500) slash
      write(buffer(4:5), '(a2)', err=1500) q.nomdate(2)
      write(buffer(6:6), '(a1)', err=1500) slash
      write(buffer(7:8), '(a2)', err=1500) q.nomdate(3)

      write(buffer(10:11), '(a2)', err=1500) q.nomtime(1)
      write(buffer(12:12), '(a1)', err=1500) colon
      write(buffer(13:14), '(a2)', err=1500) q.nomtime(2)

      write(buffer(16:17), '(a2)', err=1500) q.date(1)
      write(buffer(18:18), '(a1)', err=1500) slash
      write(buffer(19:20), '(a2)', err=1500) q.date(2)
      write(buffer(21:21), '(a1)', err=1500) slash
      write(buffer(22:23), '(a2)', err=1500) q.date(3)

      write(buffer(25:26), '(a2)', err=1500) q.qtime(1)
      write(buffer(27:27), '(a1)', err=1500) colon
      write(buffer(28:29), '(a2)', err=1500) q.qtime(2)

      write(buffer(31:40), '(a10)', err=1500) q.qnet
      write(buffer(42:56), '(a15)', err=1500) q.statn

      write(buffer(58:67), '(f10.5)', err=1500) q.ll(1)
      write(buffer(69:79), '(f11.5)', err=1500) q.ll(2)

      write(buffer(81:83), '(i3)', err=1500) q.occur
      write(buffer(85:91), '(f7.2)', err=1500) q.staelv

      write(buffer(93:99), '(f7.2)', err=1500) q.staprs
      write(buffer(101:101), '(a1)', err=1500) q.staflg

      write(buffer(103:109), '(f7.2)', err=1500) q.seaprs
      write(buffer(111:111), '(a1)', err=1500) q.seaflg

      write(buffer(113:119), '(f7.2)', err=1500) q.cmpsea
      write(buffer(121:121), '(a1)', err=1500) q.cmpflg

      write(buffer(123:129), '(f7.2)', err=1500) q.temp
      write(buffer(131:131), '(a1)', err=1500) q.tmpflg

      write(buffer(133:139), '(f7.2)', err=1500) q.dewpnt
      write(buffer(141:141), '(a1)', err=1500) q.dewflg

      write(buffer(143:149), '(f7.2)', err=1500) q.wndspd
      write(buffer(151:151), '(a1)', err=1500) q.spdflg

      write(buffer(153:159), '(f7.2)', err=1500) q.wnddir
      write(buffer(161:161), '(a1)', err=1500) q.dirflg

      write(buffer(163:169), '(f7.2)', err=1500) q.precip
      write(buffer(171:171), '(a1)', err=1500) q.prcflg

      write(buffer(173:173), '(a1)', err=1500) q.sg
      write(buffer(175:181), '(f7.2)', err=1500) q.squall
      write(buffer(183:183), '(a1)', err=1500) q.sqlflg
      write(buffer(185:188), '(i4)', err=1500) q.prswea
      write(buffer(190:190), '(a1)', err=1500) q.pwflg
      write(buffer(192:199), '(f8.2)', err=1500) q.visib
      write(buffer(201:201), '(a1)', err=1500) q.visflg

      write(buffer(203:209), '(f7.2)', err=1500) q.celht1
      write(buffer(211:212), '(i2)', err=1500) q.celfg1
      write(buffer(214:214), '(a1)', err=1500) q.c1flg
      write(buffer(216:217), '(i2)', err=1500) q.clamt1
      write(buffer(219:219), '(a1)', err=1500) q.ca1flg

      write(buffer(221:227), '(f7.2)', err=1500) q.celht2
      write(buffer(229:230), '(i2)', err=1500) q.celfg2
      write(buffer(232:232), '(a1)', err=1500) q.c2flg
      write(buffer(234:235), '(i2)', err=1500) q.clamt2
      write(buffer(237:237), '(a1)', err=1500) q.ca2flg

      write(buffer(239:245), '(f7.2)', err=1500) q.celht3
      write(buffer(247:248), '(i2)', err=1500) q.celfg3
      write(buffer(250:250), '(a1)', err=1500) q.c3flg
      write(buffer(252:253), '(i2)', err=1500) q.clamt3
      write(buffer(255:255), '(a1)', err=1500) q.ca3flg


      write(fileno, '(A255)', err=2000) buffer
      goto 9999

1500  write(6,'('' qcfrec:Error writing data to buffer'')' )
      write(6, *) buffer
      stop

2000  write(6,'('' qcfrec:Error writing record to the qcf file'')' )
      stop

9999  continue

      return
      end
