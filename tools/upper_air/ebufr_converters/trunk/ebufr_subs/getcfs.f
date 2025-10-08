C
C $Id: getcfs.f,v 2.5 1992/11/20 20:48:39 john Exp $
C
C getcfs - get a code or flag table string
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

      subroutine getcfs (xin,yin,valin,valstr,ct,msunit,err)
C
C getcfs - Get Code or Flag String
C
C Written by: John J. Allison
C             NCAR/STORM Project Office, Boulder CO
C             05 Aug 1992
C
c Returns the descriptive string(s) associated with a code or flag table value.
c
c In:
c  xin - X descriptor for info desired
c  yin - Y descriptor for info desired
c   (F descriptor assumed 0)
c  valin - real number
c  msunit - unit number of file to which to write error message
c
c Out:
c  valstr - array of character strings with all applicable descriptions of valin
c  ct - number of strings in valstr
c  err - error flag
c
c Flag table values may correspond to more than one applicable description.
c All description strings returned in valstr apply simultaneously.
c Code tables only have one possible description for each value.
c
c An error indicates that the input parameters were bad, or the descriptor
C  specified by X Y is unknown.
c
C include files for EBUFR read routines
      include 'ebufr_parm.inc'
      include 'ebufr_vars.inc'
C parameter definitions
      integer xin,yin,ct,msunit
      real valin
      character*160 valstr(*)
      logical err
C function definitions
      integer bfinfo
      external bfinfo
C local variables
      integer i,j,valint,errno
      integer lscale,lref,lnbits
      character*32 lname1,lname2
      character*24 lunits

      ct = 0
      valint = int(valin)
      err = .false.
      if (valint .lt. 0) then
	write(msunit,*) 'getcfs(): valin < 0'
	err = .true.
	return
      end if
C Code Table Values are 0..max-1 so we need to increment for array indexing
      valint = valint + 1
      if (valint .gt. MAXENT) then
	write(msunit,*) 'getcfs(): valin > max'
	err = .true.
	return
      end if

      do 10 i=1,numtbl
         if ((xtbl(i) .eq. xin) .and. (ytbl(i) .eq. yin)) then
	    if (flag(i)) then
C Flag Table Values are 1..nbits so we need to un-increment valint
	      valint = valint - 1
C If valint is 0, then no descriptions apply
	      if (valint .eq. 0) then
		return
	      end if
C Need to get the number of bits for this table value (lnbits)
      errno = bfinfo(xin,yin,lscale,lref,lnbits,lname1,lname2,lunits)
	      if (errno .ne. 0) then
		call bferr(errno,msunit)
		err = .true.
		return
	      end if
C If valint is all 1's then return the missing description
C   (should never happen, since GETVAL does something special
C    for missing; see exampl.f comments for more info)
	      if (valint .eq. 2**lnbits - 1) then
		ct = 1
	    valstr(ct) = code(1,lnbits,i) // code(2,lnbits,i) //
     +               code(3,lnbits,i) // code(4,lnbits,i) //
     +               code(5,lnbits,i)
		return
	      end if
C Loop, finding each applicable description
C  A description is applicable if that bit is set to 1
	      do 15, j=1,lnbits
C A bit B is set to 1 if valint/2^B equals 1 (this a right shift B bits)
C ***** (assumes all bits left of B are set to 0, i.e. left-fill of 0)
C  Since BUFR counts bits 1..nbits left to right, then B=lnbits-j
		if (valint/(2**(lnbits-j)) .eq. 1) then
C Once we've looked at bit B, unset it so it doesn't confuse us later
C (note assumption above)
		  valint = valint - (2**(lnbits-j))
		  ct = ct + 1
	    valstr(ct) = code(1,j,i) // code(2,j,i) //
     +               code(3,j,i) // code(4,j,i) //
     +               code(5,j,i)
		end if
 15	      continue
	      return
C else it's a Code table
	    else
C if valint doesn't have a description, then go backwards until
C    we find one, e.g. Value 7 saying "7-14 Reserved" and then
C    Values 8-14 being blank
	    do while (
C next line there to test if it's undefined, which is not guarranteed
C  to be the same as all blanks
C but it's commented out because '' is an error for DEC Ultrix f77
C     +          (code(1,valint,i) .eq. '') .or.
     +          (code(1,valint,i) .eq.
     +          '                                '))
		valint = valint - 1
	    end do
C only 1 possible description for a code table
	    ct = 1
	    valstr(ct) = code(1,valint,i) // code(2,valint,i) //
     +               code(3,valint,i) // code(4,valint,i) //
     +               code(5,valint,i)
	    end if
            return
         end if
 10   continue
C note the return statements in the above code blocks in the if (flag) else
C that means that if we get here, then the X Y was not found
      write(msunit,*) 'getcfs(): X=',xin,' Y=',yin,' not found.'
      err = .true.
      return
      end
