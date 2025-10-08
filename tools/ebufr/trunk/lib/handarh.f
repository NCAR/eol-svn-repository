      subroutine handarh(awosnm,awosno,station,itime,ios)
******!****************************************************************!
C     subroutine handarh
C     A subroutine TO READ THE HANDAR AWOS FORMAT DATA FILE header
C     
C
C     WRITTEN BY:  RONALD A. MURDOCK
C                  MMM/NCAR
C                  JUNE 09, 1992
C
C     DATE LAST MODIFIED:  June 09, 1992

c     this subroutine will read the 6 records that make up a header
c     for the Handar AWOS file and will return a station id and
c     initial date and time when the report starts

c     Input parameters:
c        awosnm - name of file to read
c        awosno - file unit number of file to be read

c     Returned parameters:
c        station - character string with station name in first 3 bytes
c        itime   - integer array containing starting date and time
c                  as follows:

c        itime(1)- year
c        itime(2)- month
c        itime(3)- day
c        itime(4)- hour
c        itime(5)- minute
c        itime(6) - seconds (not used)
c
c        ios - return status flag (0=OK,1=not OK)

C
C     HISTORY OF CHANGES:

******!****************************************************************!
C   +    1    +    2    +    3    +    4    +    5    +    6    +    7
C23456789012345678901234567890123456789012345678901234567890123456789012
******!****************************************************************!


C*****!****************************************************************!
      INTEGER IOS              !RETURN CODE FLAG FOR I/O
      integer itime(6)         !initial date/time for report from handar
c                               header information
      INTEGER LOGNUM           !FILE UNIT FOR OUTPUT LISTING
      INTEGER AWOSNO           !UNIT NUMBER FOR AWOS DATA FILE
      integer i
C*****!****************************************************************!

      character*10  fmtstr   !format string for reading buffer
      character*10  station  !station id ( usually 3 bytes )
      character*50  awosnm   !name of AWOS file
      CHARACTER*200 BUFFER   !BUFFER FOR 1 RECORD READ

C*****!****************************************************************!


      LOGICAL SCREEN !TRUE IS USER WANTS DEBUG MESSAGES SENT TO SCREEN

      LOGICAL DEBUG1 !LIGHT DEBUG MESSAGES WANTED
      LOGICAL DEBUG2 !LOTS OF DEBUG MESSAGES WANTED
      LOGICAL DEBUG3 !VERY HEAVY DEBUG INFORMATION WANTED
      logical intflg !all integer flag returned from intchk

C*****!****************************************************************!
C   + !  1    +    2    +    3    +    4    +    5    +    6    +    7 !
C234+678901234+678901234+678901234+678901234+678901234+678901234+6789012
C*****!****************************************************************!

      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!

C     INITIALIZATIONS

      ios = 0

      FMTSTR = '(A200)'  !FORMAT STRING FOR READING RECORDS FOR AWOS

      call msgc('handarh:awosnm = ',3,awosnm,'a50')
      call msgi('handarh:awosno = ',3,awosno)




C  ***!****************************************************************!
C     READ NEXT 6 RECORDS
C  ***!****************************************************************!

      call msg('handarh:read the next 6 records',3)

C  ***!****************************************************************!
C  ***!****************************************************************!
c     PROCESSING FOR RECORD 1

      READ (AWOSNO, FMTSTR, IOSTAT=IOS, END=8200, ERR=8300) BUFFER
      if ( ios .ne. 0 ) then
         call msg('handarh:error reading record 1 in header',0)
         goto 9999
      end if

c     get station id from record 1


      call msg('handarh:parse the station id next',3)
      READ ( BUFFER(1:3), '(A3)' ,err=5420) station
      goto 5440

5420  continue  !error reading station id
      ios = 1
      call msg('handarh:error reading station id',0)
      call msg(buffer,0)
      goto 9999

5440  continue  !show station id

      call msgc('handarh:station id is ',3,station,'a3')

c     end of record 1 processing

C  ***!****************************************************************!
C  ***!****************************************************************!



C  ***!****************************************************************!
C  ***!****************************************************************!
c     PROCESSING FOR RECORD 2

      READ (AWOSNO, FMTSTR, IOSTAT=IOS, END=8200, ERR=8300) BUFFER
      if (ios .ne. 0 ) then
         call msg('handarh:error reading record 2 in header',0)
         call msg(buffer,0)
         goto 9999
      end if



c     get initial date and time from record 2




c     check for valid integer values in date/time fields before read

C  ***!****************************************************************!

c     check the UTC year field for integers only

      call intchk(buffer( (46):(47) ),2,intflg,ios)

      if ( intflg ) then
c        year field is integers
      else
c        year field not integers - warn user and skip observation
         call msg('handarh:checking columns 46:47 for integers',0)
         call msg('handarh:error - UTC year field not integers',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

C  ***!****************************************************************!


C  ***!****************************************************************!

c     check the UTC month field for integers only

      call intchk(buffer( (40):(41) ),2,intflg,ios)

      if ( intflg ) then
c        month field is integers
      else
c        month field not integers - warn user and skip observation

         call msg('handarh:checking columns 40:41 for integers',0)
         call msg('handarh:error - UTC month field not integers',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

C  ***!****************************************************************!


C  ***!****************************************************************!

c     check the UTC day field for integers only

      call intchk(buffer( (43):(44) ),2,intflg,ios)

      if ( intflg ) then
c        day field is integers
      else
c        day field not integers - warn user and skip observation

         call msg('handarh:checking columns 43:44 for integers',0)
         call msg('handarh:error - UTC day field not integers',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

C  ***!****************************************************************!


C  ***!****************************************************************!

c     check the UTC hour field for integers only

      call intchk(buffer( (50):(51) ),2,intflg,ios)

      if ( intflg ) then
c        hour is integers
      else
c        hour not integers - warn user and skip observation

         call msg('handarh:checking columns 50:51 for integers',0)
         call msg('handarh:error - UTC hour field not integers',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

C  ***!****************************************************************!
c     check the UTC minutes field for integers only

      call intchk(buffer( (52):(53) ),2,intflg,ios)

      if ( intflg ) then
c        minutes field is integers
      else
c        minutes field not integers - warn user and skip observation

         call msg('handarh:checking columns 52:53 for integers',0)
         call msg('handarh:error - UTC minutes field not integers',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

C  ***!****************************************************************!


c     date/time fields are all integers - continue


C  ***!****************************************************************!
C     PARSE THE TIME FOR THIS OBSERVATION INTO AN INTEGER ARRAY
C  ***!****************************************************************!


      read ( buffer(46:47), '(i2)' ) itime(1)  !read the year
      read ( buffer(40:41), '(i2)' ) itime(2)  !read the month
      read ( buffer(43:44), '(i2)' ) itime(3)  !read the day
      READ ( BUFFER(50:51), '(I2)' ) itime(4)  !READ THE HOUR
      READ ( BUFFER(52:53), '(I2)' ) itime(5)  !READ THE MINUTES
      itime(6) = 0


C  ***!****************************************************************!
c     check that UTC year/month/day is legal
C  ***!****************************************************************!

c     check the UTC year

      if ( itime(1) .ne. 95 ) then
         call msg('handarh:error - expecting year = 95',0)

         call msg('handarh:error - illegal year in UTC field',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if
C  ***!****************************************************************!


c     check the UTC month

      if ( itime(2) .lt. 0 .or. itime(2) .gt. 13 ) then
         call msg('handarh:error - expecting month between 0 and 13',0)

         call msg('handarh:error - illegal month in UTC field',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if
C  ***!****************************************************************!

c     check the UTC day

      if ( itime(3) .lt. 0 .or. itime(3) .gt. 31 ) then
         call msg('handarh:error - expecting day between 0 and 31',0)

         call msg('handarh:error - illegal day in UTC field',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if
C  ***!****************************************************************!


C  ***!****************************************************************!
c     check that UTC hours and minutes are legal
C  ***!****************************************************************!

c     check the UTC hour

      if ( itime(4) .lt. 0 .or. itime(4) .gt. 23 ) then
         call msg('handarh:error - expecting hour between 0 and 23',0)

         call msg('handarh:error - illegal hour in UTC field',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if
C  ***!****************************************************************!

c     check the UTC minutes

      if ( itime(5) .lt. 0 .or. itime(5) .gt. 59 ) then
         call msg('handarh:error - expecting minute between 0 and 59',0)

         call msg('handarh:error - illegal minute in UTC field',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if
C  ***!****************************************************************!


c     show values for date/time

      call msgi('handarh:report start year = ',3,itime(1) )
      call msgi('handarh:report start month = ',3,itime(2) )
      call msgi('handarh:report start day = ',3,itime(3) )
      call msgi('handarh:report start hour = ',3,itime(4) )
      call msgi('handarh:report start minute = ',3,itime(5) )

c     end of processing for record 2
C  ***!****************************************************************!
C  ***!****************************************************************!


c     skip the remaining 4 records

      do i = 1, 4
         READ (AWOSNO, FMTSTR, IOSTAT=IOS, END=8200, ERR=8300) BUFFER
      end do
      goto 9999



8200  CONTINUE  !end of file encountered
C
C  ***!****************************************************************!
C     REACHED THE END OF FILE
C
C     NOTIFY USER AND SHUT DOWN
C  ***!****************************************************************!
C
      call msgc('handarh:error - reached end of input file',
     *0,awosnm,'a50')
      ios = 1
      GOTO 9999


8300  CONTINUE !error while reading file
c     error occurred reading file - notify user and shut down

      call msgc('handarh:error reading file ',0,awosnm,'a50')
      ios = 1
      GOTO 9999
C  ***!****************************************************************!


9999  return
      END
