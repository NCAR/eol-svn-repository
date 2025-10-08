c     CIN2MM function takes inches  and returns millimeters
c
c     written by Ronald A. Murdock
c
c     April 24, 1992
c
c     Input:
c       inches as a real number
c
c     Output:
c       millimeters as a real number
c
c     Usage:
c       rmm = cin2mm(rinch)
c
c
      function cin2mm(arg1)
      real cin2mm
      real arg1
      cin2mm = arg1 * 2.54 * 10.
      return
      end
