c     CF2M function takes feet and returns meters
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       feet as a real number
c
c     Output:
c       meters as a real number
c
c     Usage:
c       rmeter = cf2m(rfeet)
c
c
      function cf2m(arg1)
      real cf2m
      real arg1
      cf2m = arg1 * 0.3048   !convert feet into meters
      return
      end
