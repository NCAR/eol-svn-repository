      subroutine qreset(q)

c     written by R. A. Murdock
c
c     Date of last change - March 30, 1992
c                           May 1994 updated for Vortex project
c                             by Lia Pennington
c
c     initializes and/or resets all values in structure qcf
c     values are set to -999.99 for missing value for reals
c     other values are set to blank, zero, or special missing
c     value codes if any exist

      include '../inc/qcf.inc'
      record /qcf/ q
      q.nomdate(1)='  '
      q.nomdate(2) = '  '
      q.nomdate(3) = '  '
      q.nomtime(1) = '  ' 
      q.nomtime(2) = '  ' 
      q.date(1)='  '
      q.date(2) = '  '
      q.date(3) = '  '
      q.qtime(1) = '  ' 
      q.qtime(2) = '  ' 
      q.qnet = '    '
      q.statn = '     ' 
      q.ll(1) = -999.99
      q.ll(2) = -999.99
      q.occur = 0  
      q.staelv = -999.99
      q.staprs = -999.99
      q.staflg = 'M' 
      q.seaprs = -999.99
      q.seaflg = 'M'
      q.cmpsea = -999.99
      q.cmpflg = 'M'
      q.temp = -999.99
      q.tmpflg = 'M' 
      q.dewpnt = -999.99
      q.dewflg = 'M'
      q.wndspd = -999.99
      q.spdflg = 'M' 
      q.wnddir = -999.99
      q.dirflg = 'M' 
      q.precip = -999.99
      q.prcflg = 'M' 
      q.sg = ' '   
      q.squall = -999.99
      q.sqlflg = 'M'
      q.prswea = -999
      q.pwflg  = 'M'
      q.visib  = -999.99
      q.visflg = 'M'

      q.celht1 = -999.99
      q.celfg1 = 15
      q.c1flg = 'M'
      q.clamt1 = 15
      q.ca1flg = 'M'

      q.celht2 = -999.99
      q.celfg2 = 15
      q.c2flg = 'M'
      q.clamt2 = 15
      q.ca2flg = 'M'

      q.celht3 = -999.99
      q.celfg3 = 15
      q.c3flg = 'M'
      q.clamt3 = 15
      q.ca3flg = 'M'

      return
      end
