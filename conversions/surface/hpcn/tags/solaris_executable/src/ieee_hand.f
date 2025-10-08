C
C $Id: ieee_hand.f,v 1.1 1992/06/22 17:03:35 wayne Exp $
C $Log: ieee_hand.f,v $
c Revision 1.1  1992/06/22  17:03:35  wayne
c Initial Version
c
C

      integer function ieee_hand ( sig, code, sigcontext )
      integer sig, code, sigcontext(5)
      character label*16
      if ( loc(code) .eq. 208 ) label='invalid'
      if ( loc(code) .eq. 200 ) label='division by zero'
      if ( loc(code) .eq. 212 ) label='overflow'
      if ( loc(code) .eq. 204 ) label='underflow'
      if ( loc(code) .eq. 196 ) label='inexact'

      if ( label .ne. 'inexact' ) then
          write ( 6, 77) loc(code), label, sigcontext(4)
          call abort()
      end if

77    format ('ieee exception code ',i3, ',',
     *a17, ',', ' at pc ', i5 )
      end
