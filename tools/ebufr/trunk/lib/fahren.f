c     FAHREN function takes degrees Celsius and returns degrees Fahrenheit
c
c     written by Ronald A. Murdock
c
c     April 2, 1992
c
c     Input:
c       degrees Celsius as a real number
c
c     Output:
c       degrees Fahrenheit as a real number
c
c     Usage:
c       degree = fahren(celsius)
c
c
      function fahren(arg1)
      real fahren
      real arg1
      fahren = ( ( arg1 * 9.) / 5.) + 32.
      return
      end
