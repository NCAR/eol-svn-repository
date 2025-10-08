c     CM2F function takes meters and returns feet
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       meters as a real number
c
c     Output:
c       feet as a real number
c
c     Usage:
c       rfeet = cm2f(rmeter)
c
c
      function cm2f(arg1)
      real cm2f
      real arg1
      cm2f = arg1 * 3.28083   !convert meters into feet
      return
      end
