c     CMI2ME function takes miles and returns meters
c
c     written by Ronald A. Murdock
c
c     April 16, 1992
c
c     Input:
c       miles as a real number
c
c     Output:
c       meters as a real number
c
c     Usage:
c       meters = cmi2me(miles)
c
c
      function cmi2me(arg1)
      real cmi2me
      real arg1
      cmi2me = arg1  * 1609.344
      return
      end
