c     CIN2MB function takes inches of Mercury and returns millibars
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       pressure in inches of Mercury as a real number
c
c     Output:
c       pressure in millibars
c
c     Usage:
c       press = cin2mb(rinch)
c
c
      function cin2mb(arg1)
      real cin2mb
      real arg1
      cin2mb = arg1 * 0.0338639 * 1000.
      return
      end
