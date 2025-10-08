c     CKEL2C function takes degrees Kelvin and returns degrees Celsius
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       degrees Kelvin as a real number
c
c     Output:
c       degrees Celsius as a real number
c
c     Usage:
c       degree = ckel2c(kelvin)
c
c
      function ckel2c(arg1)
      real ckel2c
      real arg1
      ckel2c = arg1 - 273.15  !convert degrees Kelvin into degrees Celsius
      return
      end
