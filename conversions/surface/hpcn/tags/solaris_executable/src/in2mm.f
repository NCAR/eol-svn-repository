C
C $Id: in2mm.f,v 1.1 1992/06/22 17:03:35 wayne Exp $
C $Log: in2mm.f,v $
c Revision 1.1  1992/06/22  17:03:35  wayne
c Initial Version
c
C

c     IN2MM function takes inches and converts to millimeters
c
c     written by Wayne Brazille
c
c     April, 1992
c
c     Input:
c       distance in inches as a real number
c
c     Output:
c       distance in millimeters as a real number
c
c     Usage:
c       mm = in2mm(in)
c
c
c  Conversion Factor taken from Smithsonian Met. Tables
c
      function in2mm(arg1)
      real in2mm
      real arg1
      in2mm = arg1 * 25.4
      return
      end
