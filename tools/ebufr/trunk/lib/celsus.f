c     CELSUS function takes degrees Fahrenheit and returns degrees Celsius
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       degrees Fahrenheit as a real number
c
c     Output:
c       degrees Celsius as a real number
c
c     Usage:
c       degree = celsus(farenh)
c
c
      function celsus(arg1)
      real celsus
      real arg1
      celsus = ((arg1 - 32.) * 5. ) / 9.
      return
      end
