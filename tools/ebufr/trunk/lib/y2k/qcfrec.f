
      subroutine qcfrec(q,fileno)

c   *************************************************************
c     qcfrec.f - writes a single record into the qcf file
c
c     Modified by Lia Pennington for the Vortex Project.
c      20apr94 - added nominal date/time strings to the record
c                buffer.
c
c     002 18oct2000 Janine Goldstein
c         Modified to be y2k compliant by including century in 
c     nominal date and actual date, and dividing date array, 
c     which was char*2 nomdate(3), into individual year, month, 
c     and day variables.
c   *************************************************************

      include '../inc/qcf.inc'

      integer fileno
      integer i
      character*259 buffer 
      character*1 slash  
      character*1 colon 

      record /qcf/ q
      slash = '/'
      colon = ':'

      do i = 1, 259
         buffer(i:i) = ' '
      end do

      write(buffer(1:4), '(a4)', err=1500) q.nomyear
      write(buffer(5:5), '(a1)', err=1500) slash
      write(buffer(6:7), '(a2)', err=1500) q.nommonth
      write(buffer(8:8), '(a1)', err=1500) slash
      write(buffer(9:10),'(a2)', err=1500) q.nomday

      write(buffer(12:13), '(a2)', err=1500) q.nomtime(1)
      write(buffer(14:14), '(a1)', err=1500) colon
      write(buffer(15:16), '(a2)', err=1500) q.nomtime(2)

      write(buffer(18:21), '(a4)', err=1500) q.year
      write(buffer(22:22), '(a1)', err=1500) slash
      write(buffer(23:24), '(a2)', err=1500) q.month
      write(buffer(25:25), '(a1)', err=1500) slash
      write(buffer(26:27), '(a2)', err=1500) q.day

      write(buffer(29:30), '(a2)', err=1500) q.qtime(1)
      write(buffer(31:31), '(a1)', err=1500) colon
      write(buffer(32:33), '(a2)', err=1500) q.qtime(2)

      write(buffer(35:44), '(a10)', err=1500) q.qnet
      write(buffer(46:60), '(a15)', err=1500) q.statn

      write(buffer(62:71), '(f10.5)', err=1500) q.ll(1)
      write(buffer(73:83), '(f11.5)', err=1500) q.ll(2)

      write(buffer(85:87), '(i3)', err=1500) q.occur
      write(buffer(89:95), '(f7.2)', err=1500) q.staelv

      write(buffer(97:103), '(f7.2)', err=1500) q.staprs
      write(buffer(105:105), '(a1)', err=1500) q.staflg

      write(buffer(107:113), '(f7.2)', err=1500) q.seaprs
      write(buffer(115:115), '(a1)', err=1500) q.seaflg

      write(buffer(117:123), '(f7.2)', err=1500) q.cmpsea
      write(buffer(125:125), '(a1)', err=1500) q.cmpflg

      write(buffer(127:133), '(f7.2)', err=1500) q.temp
      write(buffer(135:135), '(a1)', err=1500) q.tmpflg

      write(buffer(137:143), '(f7.2)', err=1500) q.dewpnt
      write(buffer(145:145), '(a1)', err=1500) q.dewflg

      write(buffer(147:153), '(f7.2)', err=1500) q.wndspd
      write(buffer(155:155), '(a1)', err=1500) q.spdflg

      write(buffer(157:163), '(f7.2)', err=1500) q.wnddir
      write(buffer(165:165), '(a1)', err=1500) q.dirflg

      write(buffer(167:173), '(f7.2)', err=1500) q.precip
      write(buffer(175:175), '(a1)', err=1500) q.prcflg

      write(buffer(177:177), '(a1)', err=1500) q.sg
      write(buffer(179:185), '(f7.2)', err=1500) q.squall
      write(buffer(187:187), '(a1)', err=1500) q.sqlflg
      write(buffer(189:192), '(i4)', err=1500) q.prswea
      write(buffer(194:194), '(a1)', err=1500) q.pwflg
      write(buffer(196:203), '(f8.2)', err=1500) q.visib
      write(buffer(205:205), '(a1)', err=1500) q.visflg

      write(buffer(207:213), '(f7.2)', err=1500) q.celht1
      write(buffer(215:216), '(i2)', err=1500) q.celfg1
      write(buffer(218:218), '(a1)', err=1500) q.c1flg
      write(buffer(220:221), '(i2)', err=1500) q.clamt1
      write(buffer(223:223), '(a1)', err=1500) q.ca1flg

      write(buffer(225:231), '(f7.2)', err=1500) q.celht2
      write(buffer(233:234), '(i2)', err=1500) q.celfg2
      write(buffer(236:236), '(a1)', err=1500) q.c2flg
      write(buffer(238:239), '(i2)', err=1500) q.clamt2
      write(buffer(241:241), '(a1)', err=1500) q.ca2flg

      write(buffer(243:249), '(f7.2)', err=1500) q.celht3
      write(buffer(251:252), '(i2)', err=1500) q.celfg3
      write(buffer(254:254), '(a1)', err=1500) q.c3flg
      write(buffer(256:257), '(i2)', err=1500) q.clamt3
      write(buffer(259:259), '(a1)', err=1500) q.ca3flg


      write(fileno, '(A259)', err=2000) buffer
      goto 9999

1500  write(6,'('' qcfrec:Error writing data to buffer'')' )
      write(6, *) buffer
      stop

2000  write(6,'('' qcfrec:Error writing record to the qcf file'')' )
      stop

9999  continue

      return
      end
