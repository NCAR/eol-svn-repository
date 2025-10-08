      subroutine layer(token, lentok,ceilht,iflag,ios,lognum)
******!****************************************************************!
C     subroutine layer
C     parses cloud layer variable from ASOS SAO report
c
c     returns layer height and integer ceiling flag
c
c
C     
C
C     WRITTEN BY:  RONALD A. MURDOCK
C                  MMM/NCAR
C                  April 13, 1992 
c     Modified by: Lia Pennington
c                  May 20th, 1994
C
C
C     HISTORY OF CHANGES:
c     14apr92 - removed measur and variab variables
c     replaced them with iflag to indicate ceiling flag code
c     added code to process ceiling information
c
c     22APR92 - added code to handle W#X, where # is an integer
c
c     23apr92 - commented out all messages to log file except errors
c
c     24apr92 - added code to handle MM (missing cloud layer info)
c
c     27apr92 - added code to handle E# ( estimated??)
C
c     20may94 - added code to handle E#V (same as E#)
c
c     27may94 - added code to return ceiling height as missing if
c               the token value read is 120 or greater.
c               (ASOS does not report layers over 120 - 12000 feet.)
C
******!****************************************************************!
c     Input parameters:
c        token - a string to be parsed containing cloud layer information
c        lentok - integer length of token
c        lognum - file unit for log file output

c     Returned parameters:
c        ceilht - real number ceiling height
c        iflag  - integer flag for ceiling flag
c        ios - zero if everything OK; one if not OK
C
C
******!****************************************************************!
C   +    1    +    2    +    3    +    4    +    5    +    6    +    7
C23456789012345678901234567890123456789012345678901234567890123456789012
******!****************************************************************!



C     TYPE STATEMENTS
 
C
      real    ceilht

      integer tbeg    !beginning byte of layer height in token
      integer tend    !ending byte of layer height in token
      integer lentok  !token length
      integer lstr  !length of numeric part of cloud layer height
      integer iflag  !ceiling flag indicator code
      integer ios    !zero if OK, one if not OK
      integer lognum  !file unit for log file output
      integer last, i

C*****!****************************************************************!

C*****!****************************************************************!

C     CHARACTER*1  CHAR1         !GENERIC ONE-BYTE CHARACTER VARIABLE
      CHARACTER*1  BUF1          !ONE BYTE STRING FOR BUILDING FORMAT
      character*1  measur  !first byte of token if it is non-numeric
      character*1  variab  !last byte of token if it is non-numeric


      character*10      token    !cloud layer string to be parsed

      CHARACTER*10      VARFMT        !VARIABLE FORMAT FOR READING
C                                      DATA ITEMS
      CHARACTER*80      CHAR80        !80-BYTE STRING FOR GENERIC I/O

C*****!****************************************************************!

c     write(lognum, '('' layer: Entering layer'')' )
c     write(lognum, '('' layer: token = '',A10)' ) token
c     write(lognum, '('' layer: lentok (token length) = '',i2)' ) lentok

C*****!****************************************************************!
      ceilht = -999.99
      measur = ' '
      variab = ' '
      ios = 0

      last = lentok  !for convenience, use last as last byte of token
c                  (lentok is length of token = last byte of token)


C*****!****************************************************************!
c     several possibilities for token
c     (this is some variation on a cloud layer height)
c
c     number  ###  (number can be 1 to 3 digits long)
c     letter 'M' followed immediately by a number  M###
c     letter 'M' followed by number followed by letter 'V'  M###V
c     can the letter 'M' alone appear to indicate missing values?

c     letter 'W' followed by one or two integers followed by
c     the letter 'X'  e.g., W1X or W15X, etc.
c
c     letters 'MM' followed by a visibility token indicating
c     missing cloud layer info - vipr can handle the visibility token
c
c     letter 'E' followed by a single integer for estimated


c     determine status for measured cloud layer height and variability flags

C*****!****************************************************************!

c     check for the letter 'M' only
c     (this would indicate a missing value)
c     (not sure if this actually appears anywhere, but do it anyway)
      if ( lentok .eq. 1 .and. token(1:1) .eq. 'M' ) then
         iflag = 15  ! Missing value
         ceilht = -999.99  !Missing value flag
         goto 9999   !exit subroutine
      else
      end if

C*****!****************************************************************!

c     check for the letters 'MM'
c     (this would indicate a missing value)

      if ( lentok .eq. 2 .and. token(1:2) .eq. 'MM' ) then
         iflag = 15  ! Missing value
         ceilht = -999.99  !Missing value flag
         goto 9999  !exit subroutine
      else
      end if


C*****!****************************************************************!
c     set measur to the first byte of token if this byte is non-numeric
c     measur is a blank if the byte is numeric
c     set variab to the last byte of token if this byte is non-numeric
c     variab is a blank if the byte is numeric
c     use measur and variab to determine value for ceiling flag

c     set value of measur
      if ( ichar(token(1:1)) .ge. 48 .and.
     *     ichar(token(1:1)) .le. 57 ) then

         measur = ' '
c        write(lognum, '('' layer: measur = '',a1)' ) measur
      else

         measur = token(1:1)
c        write(lognum, '('' layer: measur = '',a1)' ) measur
      end if

c     set value of variab
      if ( ichar(token(last:last)) .ge. 48 .and.
     *     ichar(token(last:last)) .le. 57 ) then

         variab = ' '
c        write(lognum, '('' layer: variab = '',a1)' ) variab
      else

         variab = token(last:last)
c        write(lognum, '('' layer: variab = '',a1)' ) variab
      end if
C*****!****************************************************************!

      if ( measur .eq. 'M' .and. variab .eq. 'V') then
         iflag = 8  ! Measured/variable
      else if ( measur .eq. 'M' .and. variab .eq. ' ') then
         iflag = 4  ! Measured
      else if ( measur .eq. 'W' .and. variab .eq. ' ') then
         iflag = 5  ! Indefinite
      else if ( measur .eq. 'W' .and. variab .eq. 'V') then
         iflag = 11 ! Indefinite/Variable
      else if ( measur .eq. ' ' .and. variab .eq. ' ') then
         iflag = 0  ! None - this is just a number
      else if ( measur .eq. 'W' .and. variab .eq. 'X') then
         iflag = 25  !Indefinite  (X = sky obscured)
c        reset this value to iflag = 5 and cloud amount = 9
c        in main calling routine
      else if ( measur .eq. 'E' .and. variab .eq. ' ') then
         iflag = 10  !Estimated
      else if ( measur .eq. 'E' .and. variab .eq. 'V') then
         iflag = 10  !Estimated
      else
         write(6,
     *   '('' layer: Unexpected sky conditions in token '',
     *   A10)' ) token

         write(lognum,
     *   '('' layer: Unexpected sky conditions in token '',
     *   A10)' ) token
         ios = 1
         goto 9999

      end if



c     set the starting byte for the numeric part of the cloud height
      if ( ichar(token(1:1)) .lt. 48 .or.
     *   ichar(token(1:1)) .gt. 57 ) then
c        first byte is not a number - set tbeg = 2
         tbeg = 2
c        write(lognum, '('' layer: tbeg = '',i2)' ) tbeg
      else
c        first byte is a number - set tbeg = 1
         tbeg = 1
c        write(lognum, '('' layer: tbeg = '',i2)' ) tbeg
      end if


c     set the ending byte for the numeric part of the cloud height
c     (still using last as the last byte position in token)
      if ( ichar(token(last:last)) .lt. 48 .or.
     *   ichar(token(last:last)) .gt. 57 ) then
c        last byte is not a number - set tend = lentok - 1
         tend = lentok - 1
c        write(lognum, '('' layer: tend = '',i2)' ) tend
      else
c        last byte is a number - set tend = lentok
         tend = lentok
c        write(lognum, '('' layer: tend = '',i2)' ) tend
      end if



c     check that layer height is all numbers
c     (fractions may show up - no code in place for these yet)
      do i = tbeg, tend
c        write(lognum,
c    *   '('' layer: checking byte number '',i1,
c    *   '' in '',A10)' ) i,token
         if ( ichar(token (i:i)) .ge. 48 .and. 
     *        ichar(token (i:i)) .le. 57 ) then
c           write(lognum,
c    *   '('' layer: this is a digit - '',A1)' ) token (i:i)
         else
c           not a digit - failed this test
            write(6,
     *      '('' layer: layer height not all digits - '',
     *      A10)' ) token (1:10)
            write(lognum,
     *      '('' layer: layer height not all digits - '',
     *      A10)' ) token (1:10)

            ios = 1
            goto 9999
         end if
      end do


c     calculate the length of the numeric string
      lstr = ( tend - tbeg) + 1


c     quick check that length of numeric string makes sense
      if ( lstr .ge. 1 .and. lstr .le. 3 ) then 
         continue
      else
         write(6, 
     *   '('' layer: Unexpected length for cloud layer height'')' )
         write(6, '(A10)' ) token(1:10)
         write(lognum,
     *   '('' layer: Unexpected length for cloud layer height'')' )
         write(lognum, '(A10)' ) token(1:10)
         ios = 1
         goto 9999
      end if

C*****!****************************************************************!

c     convert length into one byte character string
      write(char80,'(I1)' ) lstr
      read (char80,'(A1)' ) buf1

c     build a format to read the string
      varfmt = '(F'//buf1//'.0)'

c     now read the string into a ceiling height variable
      read(token (tbeg:tend), varfmt ) ceilht

C     Automated asos stations do not report layers over 120
C     (12000 ft).
      if (ceilht .gt. 120.0) then
         ceilht = -999.99  !Missing value flag
         iflag = 15  ! Missing value
      end if

c     write(lognum, '('' layer: ceiling height is '',F6.2)' ) ceilht
      goto 9999


9999  CONTINUE
C  ***!****************************************************************!
C
C     EXIT PROGRAM NOW
C
C  ***!****************************************************************!

      return
      END
