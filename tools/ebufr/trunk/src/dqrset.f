C
C $Id$
C $Log$
C

      subroutine dqrset(q,hdr)

c
c     initializes and/or resets values in structure dqcf
c     if hdr is true, all vars in dqcf are reset.  If false,
c     only data vars are reset.
c
c     values are set to -999.99 for missing value for reals
c     other values are set to blank, zero, or special missing
c     value codes if any exist

      include '../inc/dqcf.inc'
      record /dqcf/ q
      logical hdr

      if (hdr) then
          q.year = '    '
          q.date(1)='  '
          q.date(2) = '  '
c          q.qtime(1) = '  ' 
          q.qtime(2) = '  ' 
          q.qnet = '          '
          q.statn = '               ' 
          q.ll(1) = -999.99
          q.ll(2) = -999.99
          q.occur = 0  
          q.staelv = -999.99
      end if
      q.qtime(1) = '  '
      q.mxtmp = -999.99
      q.mxflg1 = 255
      q.mxflg2 = 255
      q.mntmp = -999.99
      q.mnflg1 = 255
      q.mnflg2 = 255
      q.sndep = -999.99
      q.sdflg1 = 255
      q.sdflg2 = 255
      q.precip = -999.99
      q.prcflg1 = 255
      q.prcflg2 = 255
      q.snfall = -999.99
      q.sfflg1 = 255
      q.sfflg2 = 255

      return
      end
