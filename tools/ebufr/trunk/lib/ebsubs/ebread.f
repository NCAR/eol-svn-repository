C
C $Id: ebread.f,v 2.3 1992/08/17 18:02:44 john Exp $
C
C ebread - read and return an EBUFR record
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

      logical function ebread(iunit,msunit,ent,entlen,EOF)

c***********************************************************************
c
c     By Wayne Brazille, STORM Project Office, NCAR, Boulder
c     March, 1992
c
c     Purpose: Read in and return a complete E-BUFR record
c***********************************************************************
c  Include parameter definitions for low-level E-BUFR Routines
c***********************************************************************
      include 'ebufr_parm.inc'


c
c function arguments
c
      integer iunit,msunit
      integer entlen,ent(MAXSIZ)
      logical EOF

c
c local variables
c
      integer recnum,number,of,i
      integer reclen,rec(MAXLEN)
      logical COMPLT,ERR

c
c Initialize variables
c
         COMPLT=.FALSE.
         ERR = .false.
         entlen=0
         recnum=0
c
c Loop until a complete logical entity is read in
c         
c        WHILE (ENTITY NOT COMPLTE) DO

 30      continue
            call getrec(iunit,rec,reclen,EOF)
            if (EOF) then
                goto 100
            end if
            recnum=recnum+1

            if ((rec(1).gt.39).or.(rec(1).lt.0)) then
               write(msunit,*)' Record type ',rec(1),' unrecognized.'
               ERR = .true.
               COMPLT = .TRUE.
            else if (rec(1).gt.9) then
               write(msunit,*)' GRIB record type ',rec(1),
     .              ' not yet supported.'
               ERR = .true.
               COMPLT = .TRUE.
            else
               if (rec(18).lt.128) then
                  number=1
                  of=rec(18)+1
                  do 40 i=1,reclen
                     ent(i)=rec(i)
 40               continue
                  entlen=reclen
               else if (rec(18).lt.255) then
                  number=rec(18)-126
                  do 50 i=hdrsiz+1,reclen
                     entlen=entlen+1
                     if (entlen.gt.MAXSIZ) then
                        write(msunit,*)
     +                       ' Multi-record entity too large ! ',
     +                       'Maximum size: ',MAXSIZ,' bytes.'
                        ERR = .true.
                        COMPLT = .TRUE.
                     end if
                     ent(entlen)=rec(i)
 50               continue
               else
                  write(msunit,*)' Bogus rec(18) value: ',rec(18)
                  ERR = .true.
                  COMPLT = .TRUE.
               end if
            end if

            if(number.ge.of) COMPLT=.TRUE.
c        END WHILE
         if (.not.COMPLT) goto 30

c
c Record obtained; return to caller
c
 100     ebread = ERR
         return
         END 
