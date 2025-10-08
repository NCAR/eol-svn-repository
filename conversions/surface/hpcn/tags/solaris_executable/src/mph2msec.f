C
C $Id: mph2msec.f,v 1.1 1992/06/22 17:03:35 wayne Exp $
C $Log: mph2msec.f,v $
c Revision 1.1  1992/06/22  17:03:35  wayne
c Initial Version
c
C

c     MPH2MSEC function takes miles per hour and returns meters per second 
c
c     written by Wayne Brazille
c
c     April, 1992
c
c     Input:
c       speed in miles per hour as a real number
c
c     Output:
c       speed in meters per second as a real number
c
c     Usage:
c       msec = mph2msec(mph)
c
c
c  Conversion Factor taken from Smithsonian Met. Tables
c
c  13 Jan 94 lec
c    Corrected Output comment.
c
      function mph2msec(arg1)
      real mph2msec
      real arg1
      mph2msec = arg1 * 0.44704
      return
      end
