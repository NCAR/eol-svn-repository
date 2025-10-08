      subroutine chtoken(buffer,bufbeg,bufend,numtok,strtok,lentok,
     *ios)

c     AWOS Qualimetrics cloud height token string handler utility

c     written by Ronald A. Murdock

c     created on May 14, 1992

c     Pass in a character string with start and stop pointers
c     this subroutine will return three parameters:
c     1.  Number of tokens
c     2.  Array of token strings
c     3.  Array of token lengths

c     Assumptions:
c       string will not be blanks or 'MM'
c       input string will not start or end on a blank

c     Input parameters:
c       buffer - character string to be parsed
c       bufbeg - starting byte in character string
c       bufend - ending byte in character string
c       (starting and ending byte define the part of the string to
c        be parsed)

c     Returned parameters:
c       numtok - the number of tokens found
c       strtok - array of string tokens
c       lentok - length in bytes of each respective token
c       ios    - status flag  (0 = OK, 1 = trouble)


c     History of changes:

c     14may92 - creation of initial code (ripped off from token.f)
c
c     18may92 - finished debugging and testing of initial version

c     28may92 - added search for 'W##X' token

c     29may92 - changed minimum string length to 3 bytes
c             - eliminated extra write to log file


C*****!****************************************************************!


      character*200 buffer  !character string to be parsed
      integer bufbeg         !byte position for start of string
      integer bufend           !byte position for end of string
      integer numtok        !number of tokens found
      character*10 strtok(10) !array of tokens as character strings
      integer lentok(10)  !array of token lengths

      integer zero          !zero in a variable name
      integer start         !start position of current token
      integer stop          !stop position of current token
      integer lognum        !unit number of log file
      integer ios           !return code 0=OK, 1=error occurred
      integer i

      character*80 char80   !general purpose 80-byte string
      character*1 chrlen    !character equivalent of integer string length
      character*10 varfmt   !variable format used to read parts of buffer

      logical debug1, debug2, debug3, screen  !debug level variables
      logical flag  !flag returned from intchk -true if all integers

C*****!****************************************************************!

      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!

c     initializations:
      numtok = 0
      zero = 0
      ios = 0

C*****!****************************************************************!

      call msg('chtoken:Entering chtoken',3)
      call msg('chtoken:buffer is on next line',3)
      call msg(buffer,3)

      call msgi('chtoken:beginning byte = ',3,bufbeg)
      call msgi('chtoken:ending byte = ',3,bufend)


      if (( buffer(bufbeg:bufbeg) .eq. '<') .and.
     *    ( buffer(bufend:bufend) .eq. '>')) then
         bufbeg = bufbeg + 1
         bufend = bufend - 1
      end if

C*****!****************************************************************!

c     preliminary checking on string pointers
c     starting byte number must be greater than or equal to 21
c     ending byte must be less than or equal to 40
c     ending byte must be greater than starting byte

      if ( bufbeg  .le. 20 ) then
         call msg('chtoken:error in cloud height string pointer',0)
         call msg('chtoken:starting byte less than 21',0)
         call msgi('chtoken:starting byte = ',0,bufbeg)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

      if ( bufend  .gt. 40 ) then
         call msg('chtoken:error in cloud height string pointer',0)
         call msg('chtoken:ending byte gt 40',0)
         call msgi('chtoken:ending byte = ',0,bufend)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

      if ( bufend  .le. bufbeg ) then
         call msg('chtoken:error in cloud height string pointer',0)
         call msg('chtoken:ending byte le starting byte',0)
         call msgi('chtoken:starting byte = ',0,bufbeg)
         call msgi('chtoken:ending byte = ',0,bufend)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if


C*****!****************************************************************!
c     check for reasonable length of string
c     should be a minimum of 3 bytes long (W2X)
c     maximum length of 20 bytes (-X100BKN110SCT112BKN) - would not occur!
c     generate error and bail out if any tests fail


      if ( (( bufend - bufbeg )+1) .lt. 3 ) then
         call msg('chtoken:error - cloud height lt 3 bytes',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

      if ( (( bufend - bufbeg )+1) .gt. 20 ) then
         call msg('chtoken:error - cloud height gt 20 bytes',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if


C*****!****************************************************************!
c     if length is 8 bytes, check for 'NCB12000'
c     this string occurs frequently, so check for it first
c     if found, set proper values and exit

      if ( (( bufend - bufbeg )+1) .eq. 8 ) then
         if ( buffer(bufbeg:bufend) .eq. 'NCB12000' ) then
            call msg('chtoken: NCB12000 entry',3)
            strtok(1) = 'NCB12000'
            lentok(1) = 8
            numtok = 1
            ios = 0
            goto 9999
         end if
      end if


      start = bufbeg

C*****!****************************************************************!
c     is the first byte '-'?
c     if so, check for '-X'
c     if found, then this is the first token
c               reset starting byte to byte following 'X'
c     if not found, generate error and bail out

      if ( buffer(start:start) .eq. '-' ) then
c        first byte is a dash

         if ( buffer(start:(start+1)) .eq. '-X' ) then
            call msg('chtoken: obs starts with -X',3)
            numtok = 1
            strtok(1) = '-X'
            lentok(1) = 2

c           reset the start pointer
            if ( (start + 2 ) .lt. bufend ) then
               start = start + 2
               call msgi('chtoken:reset start pointer to ',3,start)
            else
               call msg('chtoken:error - reached end of string',0)
               call msg('chtoken:nothing following -X',0)
               call msg(buffer,0)
               ios = 1
               goto 9999
            end if

            goto 1000  !jump to top of loop - look for next token
         end if

c        obs started with '-', but it isn't '-X'
c        no code to handle this case
         call msg('chtoken:error - obs starts with dash',0)
         call msg('chtoken:error - but is not -X',0)
         call msg('chtoken:no code to handle this case',0)
         call msg(buffer,0)
         ios = 1
         goto 9999

      end if

C*****!****************************************************************!

1000  continue  !top of loop - looking for next token

c     top of loop - start here 
c     at this point '-X' will not be in substring to be parsed

c     have a dynamic starting point and a static ending byte for
c     delimiting the remaining string to be parsed

C*****!****************************************************************!

c     check current value of start pointer
c     verify that start is less than ending byte
c     if not, then we are at the end of the string

      if ( start .ge. bufend ) then
         call msg('chtoken:at end of string -no more tokens',3)
         ios = 0
         goto 9999
      end if


C*****!****************************************************************!
c     is the next byte a number?
c     if so, move to the right until an alphabetic character is found
c            if no alpha is found, generate an error and bail out
c            number can not be more than 3 digits
c            set the sequence of numbers as the next token & length
c            reset start to byte where alphabetic character is found
c            go to top of loop to look for next token

c     call intchk to determine if byte is a number
c     flag = true if integer
      call intchk(buffer(start:start),1,flag,ios )

c     check return flag for problems
      if ( ios .ne. 0 ) then
         call msg('chtoken:error - returned from intchk',0)
         call msgi('chtoken:checking buffer at byte number ',0,start)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if

      if ( flag ) then
c        byte is an integer - check next byte
         goto 2000  !look for alpha following number
      else
c        byte is not an integer - jump to alpha checks
         goto 3000  !jump to alpha checks
      end if

2000  continue  !look for an alpha following the number in byte number start
c     number is 3 bytes maximum

c     verify that start+3 doesn't exceed end of string to be parsed
c     start+3 will be ending byte to examine in following do loop
      if ( (start + 3) .gt. bufend ) then
         call msg('chtoken:error in parsing cloud height',0)
         call msg('chtoken:less than 3 bytes following number',0)
         call msg(buffer,0)
         ios = 1
         goto 9999
      end if


      do i = start+1, start+3
         call intchk ( buffer(i:i),1,flag,ios )

c        check return flag for problems
         if ( ios .ne. 0 ) then
            call msg('chtoken:error - returned from intchk',0)
            call msgi('chtoken:checking buffer at byte number ',0,start)
            call msg(buffer,0)
            ios = 1
            goto 9999
         end if

         if ( flag ) then
c           this byte is an integer - look at the next byte
         else
c           this byte is not an integer 
c           we have found the end of the numeric string
c           the numeric string is the newest token

c           numeric string starts in byte start and ends in i-1
c           length of numeric string is ( (i-1) - start ) + 1
c           which is the same as i - start

            numtok = numtok + 1  !increment current token counter
            call msgi('chtoken:located token number ',3,numtok)

            lentok(numtok) = i - start
            call msgi('chtoken:token length is ',3,lentok(numtok))

            write(char80,'(i1)') lentok(numtok)
            read (char80,'(A1)') chrlen
            varfmt = '(A'//chrlen//')'
            call msgc('chtoken:varfmt = ',3,varfmt,'a10')

            stop = i - 1  !set stop byte for following read

            read(buffer(start:stop), varfmt,err=2500) strtok(numtok)
            goto 2600 !skip over error msgs

2500        continue
            call msg('chtoken:error in reading numeric height',0)
            call msg(buffer,0)
            ios = 1
            goto 9999


2600        continue

            call msgc('chtoken:token string is ',3,strtok(numtok),'a8')

c           reset start pointer and look for next token
            start = i
            call msgi('chtoken:reset start pointer to byte ',3,start)
            goto 1000  !jump to top of loop - look for next token
         end if

      end do

C*****!****************************************************************!
c     if you reach this line, then no alpha was found following the
c     numbers  - warn user and exit

      call msg('chtoken:error - no alpha char following digit',0)
      call msg('chtoken:can not find sky cover designator',0)
      call msgi('chtoken:digit was in byte number ',0,start)
      call msg(buffer,0)
      ios = 1
      goto 9999



C*****!****************************************************************!

3000  continue  !alpha checks
c     byte is not a number - try for one of five letters
c     must be either 'S', 'B', 'O', 'N', or 'W'
c     generate error and bail out if none of these are found

c     is the first letter 'S'?
c     if yes, try to match 'SCT'
c        matches!  - set appropriate token string and length
c        is the byte 'T' the last byte in the string?
c        if last byte, then we are done
c        if not last byte, then reset starting byte to byte following 'T'
c        go to top of loop to look for next token

c        does not match! - generate error and bail out


      if ( buffer(start:start) .eq. 'S' ) then
c        verify that start+2 doesn't exceed end of string to be parsed
c        start+2 will be ending byte to examine in following comparison
         if ( (start + 2) .gt. bufend ) then
            call msg('chtoken:error in parsing sky cover',0)
            call msg('chtoken:less than 2 bytes following S',0)
            call msg(buffer,0)
            ios = 1
            goto 9999
         end if
      end if


      if ( buffer(start:start) .eq. 'S' ) then
         if ( buffer(start:(start+2)) .eq. 'SCT' ) then
            call msg('chtoken:found SCT for sky cover',3)
            numtok = numtok + 1
            call msgi('chtoken:this is token number ',3,numtok)
            strtok(numtok) = 'SCT'
            lentok(numtok) = 3
            call msgc('chtoken:this token is ',3,strtok(numtok),'a3')

c           reset start pointer and look for next token
            start = start + 3

c           if new start position is within string, then look for next token
c           if not, we are done
            if ( start .lt. bufend ) then
               call msgi('chtoken:reset start pointer to byte ',3,start)
               goto 1000  !jump to top of loop - look for next token
            else
               call msg('chtoken:at end of string - no more tokens',3)
               ios = 0
               goto 9999
            end if

         else
c           substring started with 'S', but it isn't 'SCT'
c           no code to handle this case
c           generate error and exit

            call msg('chtoken:error - obs starts with S',0)
            call msg('chtoken:error - but is not SCT',0)
            call msg('chtoken:no code to handle this case',0)
            call msg(buffer,0)
            ios = 1
            goto 9999

         end if
      end if



c     only reach here if string does not start with 'S'
c     try the next check



C*****!****************************************************************!
c     is the first letter 'B'?
c     if yes, try to match 'BKN'
c        matches!  - set appropriate token string and length
c        is the byte 'N' the last byte in the string?
c        if last byte, then we are done
c        if not last byte, then reset starting byte to byte following 'N'
c        go to top of loop to look for next token

c        does not match! - generate error and bail out


      if ( buffer(start:start) .eq. 'B' ) then
c        verify that start+2 doesn't exceed end of string to be parsed
c        start+2 will be ending byte to examine in following comparison
         if ( (start + 2) .gt. bufend ) then
            call msg('chtoken:error in parsing sky cover',0)
            call msg('chtoken:less than 2 bytes following S',0)
            call msg(buffer,0)
            ios = 1
            goto 9999
         end if
      end if


      if ( buffer(start:start) .eq. 'B' ) then
         if ( buffer(start:(start+2)) .eq. 'BKN' ) then
            call msg('chtoken:found BKN for sky cover',3)
            numtok = numtok + 1
            call msgi('chtoken:this is token number ',3,numtok)
            strtok(numtok) = 'BKN'
            lentok(numtok) = 3
            call msgc('chtoken:this token is ',3,strtok(numtok),'a3')

c           reset start pointer and look for next token
            start = start + 3

c           if new start position is within string, then look for next token
c           if not, we are done
            if ( start .lt. bufend ) then
               call msgi('chtoken:reset start pointer to byte ',3,start)
               goto 1000  !jump to top of loop - look for next token
            else
               call msg('chtoken:at end of string - no more tokens',3)
               ios = 0
               goto 9999
            end if

         else
c           substring started with 'B', but it isn't 'BKN'
c           no code to handle this case
c           generate error and exit

            call msg('chtoken:error - obs starts with B',0)
            call msg('chtoken:error - but is not BKN',0)
            call msg('chtoken:no code to handle this case',0)
            call msg(buffer,0)
            ios = 1
            goto 9999

         end if
      end if



c     only reach here if string does not start with 'B'
c     try the next check



C*****!****************************************************************!
c     is the first letter 'O'?
c     if yes, try to match 'OVC'
c        matches!  - set appropriate token string and length
c        is the byte 'C' the last byte in the string?
c        if last byte, then we are done
c        if not last byte, then reset starting byte to byte following 'C'
c        go to top of loop to look for next token

c        does not match! - generate error and bail out


      if ( buffer(start:start) .eq. 'O' ) then
c        verify that start+2 doesn't exceed end of string to be parsed
c        start+2 will be ending byte to examine in following comparison
         if ( (start + 2) .gt. bufend ) then
            call msg('chtoken:error in parsing sky cover',0)
            call msg('chtoken:less than 2 bytes following O',0)
            call msg(buffer,0)
            ios = 1
            goto 9999
         end if
      end if


      if ( buffer(start:start) .eq. 'O' ) then
         if ( buffer(start:(start+2)) .eq. 'OVC' ) then
            call msg('chtoken:found OVC for sky cover',3)
            numtok = numtok + 1
            call msgi('chtoken:this is token number ',3,numtok)
            strtok(numtok) = 'OVC'
            lentok(numtok) = 3
            call msgc('chtoken:this token is ',3,strtok(numtok),'a3')

c           reset start pointer and look for next token
            start = start + 3

c           if new start position is within string, then look for next token
c           if not, we are done
            if ( start .lt. bufend ) then
               call msgi('chtoken:reset start pointer to byte ',3,start)
               goto 1000  !jump to top of loop - look for next token
            else
               call msg('chtoken:at end of string - no more tokens',3)
               ios = 0
               goto 9999
            end if

         else
c           substring started with 'O', but it isn't 'OVC'
c           no code to handle this case
c           generate error and exit

            call msg('chtoken:error - obs starts with O',0)
            call msg('chtoken:error - but is not OVC',0)
            call msg('chtoken:no code to handle this case',0)
            call msg(buffer,0)
            ios = 1
            goto 9999

         end if
      end if



c     only reach here if string does not start with 'O'
c     try the next check



C*****!****************************************************************!
c     is the first letter 'N'?
c     if yes, try to match 'NCB12000'
c        matches!  - set appropriate token string and length
c
c        nothing should follow 'NCB12000'
c        if anything does, generate error and bail out

c        does not match! - generate error and bail out



      if ( buffer(start:start) .eq. 'N' ) then
c        verify that start+7 doesn't exceed end of string to be parsed
c        start+7 will be ending byte to examine in following comparison
         if ( (start + 7) .gt. bufend ) then
            call msg('chtoken:error in parsing sky cover',0)
            call msg('chtoken:less than 7 bytes following N',0)
            call msg(buffer,0)
            ios = 1
            goto 9999
         end if
      end if


      if ( buffer(start:start) .eq. 'N' ) then
         if ( buffer(start:(start+7)) .eq. 'NCB12000' ) then
            call msg('chtoken:found NCB12000 for sky cover',3)
            numtok = numtok + 1
            call msgi('chtoken:this is token number ',3,numtok)
            strtok(numtok) = 'NCB12000'
            lentok(numtok) = 8
            call msgc('chtoken:this token is ',3,strtok(numtok),'a8')

c           reset start pointer and look for next token
            start = start + 8

c           if new start position is within string, then look for next token
c           if not, we are done
            if ( start .lt. bufend ) then
               call msgi('chtoken:reset start pointer to byte ',3,start)
               goto 1000  !jump to top of loop - look for next token
            else
               call msg('chtoken:at end of string - no more tokens',3)
               ios = 0
               goto 9999
            end if

         else
c           substring started with 'N', but it isn't 'NCB12000'
c           no code to handle this case
c           generate error and exit

            call msg('chtoken:error - obs starts with N',0)
            call msg('chtoken:error - but is not NCB12000',0)
            call msg('chtoken:no code to handle this case',0)
            call msg(buffer,0)
            ios = 1
            goto 9999

         end if
      end if

C*****!****************************************************************!

c     only reach here if string does not start with 'N'
c     try the next check



C*****!****************************************************************!
c     is the first letter 'W'?
c     if yes, try to match 'W##X'



      if ( buffer(start:start) .eq. 'W' .and. 
     *     buffer(bufend:bufend) .eq. 'X') then

c        assume that this is a 'W##X' string
         call msg('chtoken:this is a W##X token',3)

         numtok = 1
         call msgi('chtoken:located token number ',3,numtok)

         lentok(1) = (bufend - start) + 1
         call msgi('chtoken:token length is ',3,lentok(numtok))

         call msg('chtoken:check for integers between W and X',3)


         if ( (bufend-1) .lt. (start+1) ) then
            call msg('chtoken:error in checking W##X string',0)
            call msg('chtoken:bufend-1 lt start+1',0)
            call msg(buffer,0)
            ios = 1
            goto 9999
         end if

         do i = start+1, bufend-1
            call intchk ( buffer(i:i),1,flag,ios )

c           check return flag for problems
            if ( ios .ne. 0 ) then
               call msg('chtoken:error - returned from intchk',0)
               call msgi('chtoken:checking for integers at byte ',
     *         0,start)
               call msg(buffer,0)
               ios = 1
               goto 9999
            end if

            if ( flag ) then
c              this byte is an integer - look at the next byte
            else
c              this byte is not an integer 
c              something is wrong here
c
c              notify user and exit
               call msg('chtoken:error - not all integers in W##X',0)
               call msg(buffer,0)
               ios = 1
               goto 9999

            end if
         end do

c        all bytes between W and X are integers
c        read the string into strtok(1)


         write(char80,'(i1)') lentok(numtok)
         read (char80,'(A1)') chrlen
         varfmt = '(A'//chrlen//')'
         call msgc('chtoken:varfmt = ',3,varfmt,'a10')


         read(buffer(start:bufend), varfmt) strtok(1)

         call msgc('chtoken:token string is ',3,strtok(numtok),'a10')

c        if 'W##X', then only one token 
c        no more tokens to look for
c        exit subroutine now

         goto 9999
      end if  !end of block for 'W##X' token

C*****!****************************************************************!


c     only reach here if string does not start with 'W'
c     nothing left to try - 


C*****!****************************************************************!
c     no match has been found!
c     generate error and bail out

      call msg('chtoken:error - could not parse sky cover designator',0)
      call msgi('chtoken:sky cover started in byte ',0,start)
      call msg(buffer,0)
      ios = 1
      goto 9999

C*****!****************************************************************!

9999  continue
      call msg('chtoken:Leaving chtoken',3)

      return
      end
