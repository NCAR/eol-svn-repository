C************************************************************************
C
C function: getmin()
C description: calculate the minutes between 2 times
C
C              format of the times are:  YYDDDHHMM
C
C                        where YY : year
C                             DDD : julian day of year
C                              HH : hour
C                              MM : minute
C
C returns:  the number of minutes between the two times
C************************************************************************

      integer function getmin(btime,etime)

      implicit none
      character*9 btime,etime
      integer yr,days,hr,min
      integer bmin,emin

      read(btime,101) yr,days,hr,min
101   format(i2,i3,i2,i2)
      bmin = min + hr*60 + days*1440 + yr*525600
      bmin = bmin + yr/4*1440		!add extra min for leap yrs
      read(etime,101) yr,days,hr,min
      emin = min + hr*60 + days*1440 + yr*525600
      emin = emin + yr/4*1440		!add extra min for leap yrs
      getmin = abs(emin-bmin)
C     write(0,*) 'getmin(): btime=',btime,' etime=',etime,
C    +                    ' getmin=',getmin
      return

      end
