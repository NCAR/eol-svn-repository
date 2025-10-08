      integer function hand ( sig, code, sigcontext, addr )
      integer sig, code, sigcontext(5), addr
      character label*16
      if ( loc(code) .eq. 208 ) label='invalid'
      if ( loc(code) .eq. 200 ) label='division by zero'
      if ( loc(code) .eq. 212 ) label='overflow'
      if ( loc(code) .eq. 204 ) label='underflow'
      if ( loc(code) .eq. 196 ) label='inexact'

      if ( label .ne. 'inexact' )
     * write ( 6, 77) loc(code), label, sigcontext(4)

77    format ('Error - ieee exception code ',i3, ',',
     *a17, ',', ' at pc ', i5 )

      if ( label .ne. 'inexact' )
     * write (6, 80) loc(addr)
80    format ('  address location = ', z8)

      end
