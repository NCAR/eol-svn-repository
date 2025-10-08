
      subroutine advance( year, month, day )

******!****************************************************************!
C     subroutine advance.f
C     A subroutine to advance the integer month and day by one day
C     
C
C     WRITTEN BY:  RONALD A. MURDOCK
C                  MMM/NCAR
C                  June 9, 1992
C
C     DATE LAST MODIFIED:  June 9, 1992
C     DATE LAST MODIFIED:  June 7, 1994 by Lia Pennington
C
C     HISTORY OF CHANGES:
C     07jun94	Modified routine to be more generic (instead of only
C               covering the months from January to April).
C
C   +    1    +    2    +    3    +    4    +    5    +    6    +    7
C23456789012345678901234567890123456789012345678901234567890123456789012
******!****************************************************************!

C     ---------------
C     PARAMETERS
C     ---------------

      INTEGER NUMDAYS(12)
      DATA NUMDAYS / 31,28,31,30,31,30,31,31,30,31,30,31 /

C     ---------------
C     TYPE STATEMENTS
C     ---------------
C
      integer month    !month number for current observation
      integer day      !day number for current observation
      integer year     !year number for current observation
      INTEGER LOGNUM   !FILE UNIT FOR OUTPUT LISTING

C*****!****************************************************************!

      LOGICAL SCREEN !TRUE IS USER WANTS DEBUG MESSAGES SENT TO SCREEN
      LOGICAL DEBUG1 !LIGHT DEBUG MESSAGES WANTED
      LOGICAL DEBUG2 !LOTS OF DEBUG MESSAGES WANTED
      LOGICAL DEBUG3 !VERY HEAVY DEBUG INFORMATION WANTED

C*****!****************************************************************!
C   + !  1    +    2    +    3    +    4    +    5    +    6    +    7 !
C234+678901234+678901234+678901234+678901234+678901234+678901234+6789012
C*****!****************************************************************!


      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!

      call msg('advance:advance the date for this observation',3)
      call msgi('advance:current month = ',3,month)
      call msgi('advance:current day = ',3,day)

      if ( month .lt. 1 .or. month .gt. 12 ) then
         call msg('advance:error in month number',0)
         call msgi('advance:month = ',0,month)
         call msg('advance:expecting month = 1,2, or 3',0)
         goto 9999
      end if

      if ( day .lt. 1 .or. day .gt. 31 ) then
         call msg('advance:error in day number',0)
         call msgi('advance:day = ',0,day)
         call msg('advance:expecting day = 1,2, ... 31',0)
         goto 9999
      end if


C     Change the number of days for February, if a leap year.
      if (mod(year, 4) .eq. 0)  NUMDAYS(2) = 29 

C     Advance the day and month, if necessary.
C     ----------------------------------------
      IF ( day .LT. NUMDAYS(month) ) THEN
         day = day + 1

C     SET TO FIRST OF NEXT MONTH
      ELSE
          day = 1

          if (month .lt. 12) then
              month = month + 1
          else if (month .eq. 12) then
              month = 1
              year = year +  1
          end if
      END IF

C*****!****************************************************************!

9999  continue
      call msgi('advance:after advance, current year = ',3,year)
      call msgi('advance:after advance, current month = ',3,month)
      call msgi('advance:after advance, current day = ',3,day)
      return
      END
