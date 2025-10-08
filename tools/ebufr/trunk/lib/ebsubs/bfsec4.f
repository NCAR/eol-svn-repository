C
C $Id: bfsec4.f,v 2.4 1992/08/17 18:02:41 john Exp $
C
C bfsec4 - decode a BUFR section 4
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

************************************************************************
      integer function bfsec4(rec,reclen,offset,length,numsub,cmpdat,
     .     f,x,y,numdes,xout,yout,datum,fmt,MAXDAT,numdat)

c     Routine interprets a BUFR section 4 (data).
c
c Written by Mark Bradford at NCAR/Office of Field Project Support.

c     Note that NO checking is done here as to the validity of the
c     descriptor; this is left until the data is interpreted, in case
c     the descriptor is not defined until later in the EBUFR file.

c     On entry:
c     REC holds the BUFR data.
c     RECLEN is the maximum length of the REC array.
c     OFFSET is the beginning point of the BUFR section within REC.
c     NUMSUB is the number of data subsets in the section.
c     CMPDAT is 1 if data is compressed, 0 otherwise.
c     NUMDES is the number of descriptors currently in F, X, and Y.
c     F, X, and Y hold the descriptors themselves.
c     MAXDAT is the dimension of the output arrays XOUT, YOUT, DATUM,
c      and FMT.

c     On exit:
c     LENGTH is the number of octets in the section.
c     XOUT and YOUT hold the X and Y values of the descriptors
c      corresponding to the data encountered.
c     DATUM holds the data themselves, as a series of internal files.
c     FMT holds FORTRAN formats with which the data may be read from
c      DATUM.
c     NUMDAT is the number of data actually encountered.

c     Returns:
c     0 if no error.
c     41 if OFFSET greater than RECLEN.
c     42 if output arrays not large enough to hold all data.
c     43 if a replication factor with F.ne.0 is specified.
c     44 if a text field (F=2 X=05) is too large for the output array.
c     45 if an attempt to replicate a descriptor with F.ne.0 is made.
c     46 if an unimplemented operator (F=2) descriptor is found.
c     47 if sequence descriptors are found.
c     48 if F is out of range. (This should never happen.)
c     5x if error x occurs in ELMNT.

c     NOTE: If an integer is only 16 bits, LENGTH may have to be treated
c     as a real value rather than an integer, necessitating modifications.

c     NOTE: If a numeric datum has a size greater than 32 bits, this
c     routine will return ONLY THE FIRST 32 BITS.  A fix is in development.
      
      integer reclen,rec(reclen),offset,length,numsub,cmpdat,numdes
      integer f(numdes),x(numdes),y(numdes)
      integer MAXDAT,numdat
      integer xout(MAXDAT),yout(MAXDAT)
      character*(*) datum(MAXDAT)
      character*10 tmpdat
      character*8 fmt(MAXDAT),tmpfmt
      integer index,bitnum,i,j,k,kk,temp,err,elmnt,numrep,nxtbts

      bfsec4=0
      index=0
      bitnum=1
      numdat=0
      if (offset.ge.reclen) then
         bfsec4=41
         RETURN
      end if
      length=rec(offset+1)*65536+rec(offset+2)*256+rec(offset+3)
      do 20 j=1,numsub
c     Subset number j:
         i=0
   10    continue
            i=i+1
c     Branch on type of descriptor.
            if (f(i).eq.0) then
c     Element descriptor.  These are easy.
               if (numdat.ge.MAXDAT) then
                  bfsec4=42
                  RETURN
               end if
               numdat=numdat+1
               err = elmnt(rec,reclen,offset+5,index,bitnum,
     .              numsub,cmpdat,x(i),y(i),datum(numdat),fmt(numdat))
               if (err.ne.0) then
                  bfsec4=50+err
                  RETURN
               end if
               xout(numdat)=x(i)
               yout(numdat)=y(i)
            else if (f(i).eq.1) then
c     Replication descriptor!
                  if (y(i).eq.0) then
c     Descriptor (i+1) holds replication factor.
                     if (f(i+1).ne.0) then
                        bfsec4=43
                        RETURN
                     end if
                     err = elmnt(rec,reclen,offset+5,
     .                    index,bitnum,numsub,cmpdat,
     .                    x(i+1),y(i+1),
     .                    tmpdat,tmpfmt)
                     if (err.ne.0) then
                        bfsec4=50+err
                        RETURN
                     end if
                     read(tmpdat,tmpfmt)numrep
                     do 30 k=1,numrep
                        do 40 kk=1,x(i)
                           if (f(i+kk+1).eq.0) then
                              if (numdat.ge.MAXDAT) then
                                 bfsec4=42
                                 RETURN
                              end if
                              numdat=numdat+1
                              err = elmnt(rec,reclen,offset+5,
     .                             index,bitnum,numsub,cmpdat,
     .                             x(i+kk+1),y(i+kk+1),
     .                             datum(numdat),fmt(numdat))
                              if (err.ne.0) then
                                 bfsec4=50+err
                                 RETURN
                              end if
                              xout(numdat)=x(i+kk+1)
                              yout(numdat)=y(i+kk+1)
                           else
c     Can't yet replicate anything but F=0 descriptors.  Ignore
c     anything else.
                              bfsec4=45
                           end if
 40                     continue
 30                  continue
                     i=i+x(i)+1
                  else
c     Not delayed replication, so use the X and Y values directly.
                     do 50 k=1,y(i)
                        do 60 kk=1,x(i)
                           if (f(i+kk).eq.0) then
                              if (numdat.ge.MAXDAT) then
                                 bfsec4=42
                                 RETURN
                              end if
                              numdat=numdat+1
                              err = elmnt(rec,reclen,offset+5,
     .                             index,bitnum,numsub,cmpdat,
     .                             x(i+kk),y(i+kk),
     .                             datum(numdat),fmt(numdat))
                              if (err.ne.0) then
                                 bfsec4=50+err
                                 RETURN
                              end if
                              xout(numdat)=x(i+kk)
                              yout(numdat)=y(i+kk)
                           else
c     Can't yet replicate anything but F=0 descriptors.  Ignore
c     anything else.
                              bfsec4=45
                           end if
 60                     continue
 50                  continue
                     i=i+x(i)
                  end if
               else if (f(i).eq.2) then
c     It's an operator descriptor.
c     Need to install code to handle each operator.
                  if (x(i).eq.5) then
c     Descriptive text:
                     if (numdat.ge.MAXDAT) then
                        bfsec4=42
                        RETURN
                     end if
                     numdat=numdat+1
                     if (y(i).gt.len(datum(numdat))) then
                        bfsec4=44
                        RETURN
                     end if
                     xout(numdat)=-1
                     yout(numdat)=-1
                     write(fmt(numdat),'(''(A'',i3,'')'')')y(i)
                     do 70 k=1,y(i)
                        temp=nxtbts(8,rec,reclen,offset+5,index,bitnum)
                        if (temp.eq.255) then
c     Missing data
                           datum(numdat)(k:k)='$'
                        else
                           datum(numdat)(k:k)=char(temp)
                        end if
 70                  continue
                  else
c     This F=2 descriptor is not understood.
                     bfsec4=46
                  end if
               else if (f(i).eq.3) then
c     Sequence descriptors not yet supported.
c     (One way of handling them would be to loop through the list of
c     descriptors, expanding them each time, until no further expansions
c     are done.  To be general, this would have to be handled on the fly,
c     as certain sequences are of variable size depending on the data.
c     The most straightforward way of doing this, of course, is
c     recursive -- but this is FORTRAN.)
                  bfsec4=47
               else
c     F is not 0, 1, 2, or 3.  (This should NEVER happen.)
                  bfsec4=48
               end if
            if (i.lt.numdes) goto 10
 20      continue

         RETURN
         END
