c     CK2M function takes knots and returns meters per second
c
c     written by Ronald A. Murdock
c
c     April 1, 1992
c
c     Input:
c       knots as a real number
c
c     Output:
c       meters per second as a real number
c
c     Usage:
c       rmeter = ck2m(rknots)
c
c
c     Reference:
c        CRC Handbook of Chemistry and Physics - 70th edition - 1989-1990
c        1 meter/sec = 1.943844 knots

      function ck2m(arg1)
      real ck2m
      real arg1
      ck2m = arg1 / 1.943844   !convert knots into meters per second
      return
      end
