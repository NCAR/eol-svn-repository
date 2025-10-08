c     CC2KEL function takes degrees Celsius and returns degrees Kelvin
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       degrees Celsius as a real number
c
c     Output:
c       degrees Kelvin as a real number
c
c     Usage:
c       degree = cc2kel(celsius)
c
c
      function cc2kel(arg1)
      real cc2kel
      real arg1
      cc2kel = arg1 + 273.15  !convert degrees Celsius into degrees Kelvin
      return
      end
