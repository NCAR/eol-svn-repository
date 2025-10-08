      subroutine cover(token, lentok,iclamt,ios,lognum)
******!****************************************************************!
C     subroutine cover
C     parses cloud cover variable from ASOS SAO report
c
c     returns integer cloud amount flag
c
c
C     
C
C     WRITTEN BY:  RONALD A. MURDOCK
C                  MMM/NCAR
C                  April 17, 1992 
C
C
C     HISTORY OF CHANGES:
c
c     17apr92 - creation
c
c     23apr92 - commented out all messages to log file except errors
C
C
******!****************************************************************!
c     Input parameters:
c        token - a string to be parsed containing cloud cover information
c        lentok - integer length of token
c        lognum - file unit for log file output

c     Returned parameters:
c        iclamt  - integer flag for cloud amount
c        ios - zero if everything OK; one if not OK

******!****************************************************************!
C   +    1    +    2    +    3    +    4    +    5    +    6    +    7
C23456789012345678901234567890123456789012345678901234567890123456789012
******!****************************************************************!

      integer lentok  !token length
      integer iclamt  !cloud amount flag
      integer ios    !zero if OK, one if not OK
      integer lognum  !file unit for log file output

C*****!****************************************************************!

      character*10 token    !cloud cover string to be parsed

C*****!****************************************************************!

c     write(lognum, '('' cover: Entering cover'')' )
c     write(lognum, '('' cover: token = '',A10)' ) token
c     write(lognum, '('' cover: lentok (token length) = '',i2)' ) lentok

C*****!****************************************************************!

      ios = 0

C*****!****************************************************************!

c     there are only 4 sky cover designators that this program will
c     process - CLR, SCT, BKN, or OVC
c
c     normaly CLR will be processed as part of a unit, i.e. 'CLR BLO 120',
c     but in case CLR shows up by itself, the code is in place
c
c     the following table shows the mapping of codes
c
c     SAO sky cover designator    Code    Description
c     ***********************     ****    *****************************
c         CLR                      0      Clear
c         SCT                     11      Scattered
c         BKN                     12      Broken
c         OVC                      8      Overcast
c
c      (missing)                  15      Cloud cover is indiscernible 
c                                         for reasons other than fog 
c                                         or other meteorological
c                                         phenomena, or observation 
c                                         is not made
c
c     Anything else will generate an error

C  ***!****************************************************************!

c     check for the letter 'M' only
c     (this would indicate a missing value)
c     (not sure if this actually appears anywhere, but do it anyway)
      if ( lentok .eq. 1 .and. token(1:1) .eq. 'M' ) then
         iclamt = 15  ! Missing value - exit subroutine
         goto 9999
      else
      end if


C*****!****************************************************************!

c     check for the other possible designators
c     generate an error message if no match

      if ( token .eq. 'SCT' ) then      !scattered
         iclamt = 11
         goto 9999

      else if (token .eq. 'BKN' ) then  !broken
         iclamt = 12
         goto 9999

      else if (token .eq. 'OVC' ) then  !overcast
         iclamt = 8
         goto 9999

      else if (token .eq. 'CLR' ) then  !clear
         iclamt = 0
         goto 9999

      else
         write(lognum, 
     *   '('' cover: unexpected sky cover designator'')' )
         write(lognum, 
     *   '('' cover: sky cover designator is '',a10)' ) token
         write(lognum,
     *   '('' cover: expected SCT, BKN, OVC, or CLR'')' )
         write(6, 
     *   '('' cover: unexpected sky cover designator'')' )
         write(6, 
     *   '('' cover: sky cover designator is '',a10)' ) token
         write(6,
     *   '('' cover: expected SCT, BKN, OVC, or CLR'')' )
         ios = 1
         goto 9999

      end if
C  ***!****************************************************************!


9999  CONTINUE
C  ***!****************************************************************!
C
C     EXIT PROGRAM NOW
C
C  ***!****************************************************************!

      return
      END
