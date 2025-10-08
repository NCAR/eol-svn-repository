C
C $Id: rh2dew.f,v 1.1 1992/06/22 17:03:35 wayne Exp $
C $Log: rh2dew.f,v $
c Revision 1.1  1992/06/22  17:03:35  wayne
c Initial Version
c
C

c     RH2DEW function takes relative humidity and elevation and returns dewpoint
c
c     written by Wayne Brazille
c
c     April, 1992
c
c     Input:
c       relative humidity in percent as a real number
c       Drybulb Temp in celsius as a real number
c       station elevation in meters as a real number
c
c     Output:
c       dewpoint in celsius as a real number
c       err_flg = 0 if ok, = 1 if error as an integer
c
c     Usage:
c       dewpt = rh2dew(rh,db,elev,err_flg)
c
c
c  RH to dewpoint algorithms taken from GEMPAK, which reference Bolton.
c  Pressure needed for equation derived from standard atmosphere and elevation
c
c 1 Feb 94 lec
c   Changed -999.00 to -999.99, since that's the standard for the
c   output.
c
      function rh2dew(rh,db,elev,err_flg)
      real rh2dew
      real rh,elev,db,vaps,vapr,press,log_val
      integer err_flg

c
c Range check input values
c
      if (elev.lt.15.0.or.elev.gt.1525.0) then
          err_flg = 1
          rh2dew = -999.99
          return
      end if
      if (rh.le.0.0.or.rh.gt.100.0) then
          err_flg = 1
          rh2dew = -999.99
          return
      end if

c
c First, compute pressure from station elevation
c
      err_flg = 0
      if (elev.lt.50.or.elev.gt.5000) then
          err_flg = 1
          rh2dew = -999.99
          return
      end if
      press = 1013.25 - (elev * 0.1110739)
c
c Compute Saturation Vapor Pressure from drybulb
c
      vaps = 6.112 * exp( (17.67 * db) / (db + 243.5))
c
c Compute Vapor Pressure from Relative Humidity
c
      vapr = vaps * (rh/100.0)
c
c Now compute the dewpoint
c
      log_val = alog(vapr/6.112)
      rh2dew = log_val * 243.5 / (17.67 - log_val)

      return
      end
