C
C $Id: getval.f,v 2.5 1994/12/08 23:26:28 john Exp $
C
C getval - get the decoded value for (E)BUFR data
c
c*
c* Copyright (C) 1992 by UCAR
c*      University Corporation for Atmospheric Research
c*
c* Permission to use, copy, modify, and distribute this software and its
c* documentation for any purpose and without fee is hereby granted, provided
c* that the above copyright notice and this permission notice appear in all
c* copies and in all supporting documentation, and that the name of UCAR
c* not be used in advertising or publicity pertaining to distribution of
c* the software in source or compiled form, whether alone or as part of
c* another program, without specific, written prior permission.
c*
c* Any modifications that you make to this software must be explicitly
c* identified as such and include your name and the date of modification.
c*
c* In addition, UCAR gives you permission to link the compiled version of
c* this file with other programs, and to distribute those without any
c* restriction coming from the use of this file.
c*
c* Although this software is provided in the hope that it will be useful,
c* UCAR makes no representations about the suitability of this software
c* for any purpose. This software and any accompanying written materials
c* are provided "as is" without warranty of any kind. UCAR expressly
c* disclaims all warranties of any kind, either express or implied,
c* including but not limited to the implied warranties of merchantibility
c* and fitness for a particular purpose. UCAR does not indemnify any
c* infringement of copyright, patent, or trademark through use or
c* modification of this software.
c*
c* UCAR does not normally provide maintenance or updates for its software.
c

      subroutine getval (datum,x,y,fmt,rval,cval,
     +                         valtyp,msunit,ERR)

c***********************************************************************
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Return an appropriately typed,scaled, and referenced
c              value from the character returned by the E-BUFR read
c              utilities
c***********************************************************************
c
c GETVAL - GET decoded VALue from "raw" character string data
c
c Returns a decoded data value appropriately scaled and referenced.
c
c In:
c  datum - character string holding the datum value
c  x - X descriptor of datum
c  y - Y descriptor of datum
c  fmt - character string with FORMAT with which to read datum
c  msunit - unit number of file to which to write error message
c
c Out:
c  rval - contains datum value if datum was numeric, code table,
c         or flag table value
c  cval - contains datum value if datum was character string
c  valtyp - indicates type of data; R means real value in rval, C means
c           character string in cval, M means missing data
c  err - error flag
c
c Errors are destructive --- outgoing data is probably garbage. An
c  error in getval() probably also indicates an error in the
c  EBUFR file which was undetected by getobs(). Therefore, you should
c  stop processing the current input file, even though getobs() will
c  probably discover the error when it is called next.
c
c***********************************************************************
c   Function parameters
c***********************************************************************
      integer msunit,x,y
      real rval
      character*1 valtyp
      character*8 fmt
      character*128 datum,cval
      logical ERR
c***********************************************************************
c   Local variables
c***********************************************************************
c
      integer errnum,ival,bfinfo
      character*32 name1,name2
      character*24 units
      integer scale,ref,numbits
      real bd

c
c Initialize variables
c
      ERR = .false.
c
c Get scale, reference values for descriptor
c
      errnum = bfinfo(x,y,scale,ref,numbits,
     +                name1,name2,units)
      if (errnum.ne.0) then
          write(msunit,*) 'Unknown Descriptor, X = ',x,' Y = ',y,
     +                    'encountered in GETVAL'
          call bferr(errnum,msunit)
          ERR = .true.
          goto 999
      end if
c
c Determine data type and convert value
c
      if (fmt(2:2).ne.'A') then
          if (numbits.lt.32) then
              read(datum,fmt) ival
              if (ival.eq.(2**numbits-1)) then
		  valtyp = 'M'
                  goto 999                    ! Missing Value
              end if
              rval = bd(ival,scale,ref)
              valtyp = 'R'
          else
              write(msunit,*) ' Value > 32 bits encountered'
              ERR = .true.
              goto 999
          end if
      else if (datum(1:1).eq.'$') then
	   valtyp = 'M'
           goto 999                          ! Missing value
      else
          cval = datum
          valtyp = 'C'
      end if

 999  return
      END
