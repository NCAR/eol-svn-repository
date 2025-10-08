      subroutine token(buffer,bufbeg,bufend,delim,numtok,strtok,lentok,
     *ios)

C*****!****************************************************************!
c     Token string handler utility

c     written by Ronald A. Murdock
c                NCAR/MMM/OFPS/STORM Project Office

c     modified by Lia Pennington for Vortex project.

c     Pass in a character string with start and stop pointers and a
c     delimiter, and this subroutine will return three parameters:
c     1.  Number of tokens
c     2.  Array of token strings
c     3.  Array of token lengths

c     Input parameters:
c        buffer - 200-byte character string containing substring to
c                 be parsed into tokens
c        bufbeg - starting byte in buffer for beginning of substring
c                 that will be parsed
c        bufend - ending byte in buffer for end of substring
c                 that will be parsed
c        delim  - a one-byte delimiting character between tokens
c                 (usually a blank)

c     Returned parameters:
c        numtok - the number of tokens being returned
c        strtok - an array of the tokens being returned
c        lentok - an array of the respective token lengths


c     Assumptions:
c       String starts and stops on non-delimiters

c     29apr92 - added check on length of token
c             - added err= option on read statements

c     17jun92 - added ability to handle Handar AWOS sky cover strings
c               which use both commas and blanks to delimit tokens.
c               This change will allow token to return tokens without
c               commas and token length will also exclude commas.
c             - Examples of the Handar AWOS data are shown below:
c               120 CLR
c               100 SCT
c               80 SCT, 90 OVC
c               47 SCT, 60 BKN, 75 OVC
c               -X 3 OVC
c               -X 7 BKN, 13 OVC
c               -X 6 SCT, 19 BKN, 24 OVC
c               1 SCT
c               -X 1 BKN
c               W0X
c               MM  <--- this will be handled outside of token.f  ****
c
C      21apr94  For Vortex project:
C               - Function now used to return tokens after the 
C               sky string tokens.
C               - Changed maximum size of token to accomadate the
C               - Changed maximum number of tokens to retrieve (10).

C*****!****************************************************************!

      character*1 comma     !a comma
      character*1 delim     !delimiter between tokens
      character*30 strtok(10) !array of tokens as character strings
      character*200 buffer  !character string to be parsed

C*****!****************************************************************!

      integer bufbeg        !byte position for start of string
      integer bufend        !byte position for end of string
      integer ios           !return code 0=OK, 1=error occurred
      integer lentok(10)    !array of token lengths
      integer lognum        !unit number of log file
      integer numtok        !number of tokens found
      integer start         !start position of current token
      integer stop          !stop position of current token
      integer zero          !zero in a variable name
      integer i, icnt, k

C*****!****************************************************************!

      logical debug1, debug2, debug3, screen  !debug level variables

C*****!****************************************************************!

      COMMON /BUGBLK/ DEBUG1, DEBUG2, DEBUG3, SCREEN, LOGNUM

C*****!****************************************************************!

c     initializations:
      numtok = 0
      zero = 0
      ios = 0  !initialize ios to zero to indicate no problems
      comma = ','  !initialize to a single comma

C*****!****************************************************************!

      call msg('token:Entering token',3)
      call msg('token:buffer is on next line',3)
      call msg(buffer,3)

c     write(6,'('' bufbeg = '',i2)') bufbeg
c     write(6,'('' bufend = '',i2)' ) bufend
c     write(6,'('' delim = :'',A1,'':'')' ) delim


      start = 0
      do i = bufbeg, bufend

         if(buffer(i:i) .ne. delim ) then
            if ( start .eq. zero ) start = i

            if ( i .eq. bufend ) then
               numtok = numtok + 1

               stop = i
               lentok(numtok) = (stop - start) + 1

c              write(6,'('' lentok('',i2,'') = '',i2)' ) i,
c    *         lentok(numtok)

c              quick check for reasonable length of token
               if (lentok(numtok) .lt. 1 .or. 
     *         lentok(numtok) .gt. 30 ) then
                  call msg('token:Error in length of token',0)
                  call msg('token:token length lt 1 or gt 30',0)
                  call msg(buffer,0)
                  ios = 1
                  goto 9999
               end if

               strtok(numtok) = buffer(start:stop) 
               goto 2000

1000           continue
               call msg('token:Error reading token',0)
               call msg(buffer,0)
               ios = 1
               goto 9999

2000           continue  !no problem with read of strtok(numtok)
               
C***           Stop collecting tokens once we've gotten the 
C              pressure/temp/wind/alt string. -lia
               icnt = 0
               do k = 1, lentok(numtok)
                  if (strtok(numtok)(k:k) .eq. '/') then
                     icnt = icnt + 1
                  endif
               end do

               if ( icnt .ge. 4)  goto 9999
C***
c              write(6, '('' numtok = '',i2)' ) numtok
c              write(6, '('' strtok(numtok) is on next line'')' )
c              write(6, '(1x,A)' ) strtok(numtok)
            end if


        else if ( buffer(i:i) .eq. delim .and. start .ne. zero ) then
c          Found a delimiter and token has a start position

           numtok = numtok + 1

c          Determine the ending position - check for comma as last
c          byte and if so, then set end to byte prior to comma
c          this accomodates the Handar AWOS sky cover data string

           if ( buffer( (i-1):(i-1) ) .eq. comma ) then
              stop = i - 2
           else
              stop = i - 1
           end if

c          Compute length of token.
           lentok(numtok) = (stop - start) + 1

c          write(6,'('' token length = '',i2)' ) lentok(numtok)

c          Quick check for reasonable length of token
           if (lentok(numtok) .lt. 1 .or. 
     *     lentok(numtok) .gt. 30 ) then
              call msg('token:Error in length of token',0)
              call msg('token:token length lt 1 or gt 30',0)
              call msg(buffer,0)
              ios = 1
              goto 9999
           end if

           strtok(numtok) = buffer(start:stop) 
           goto 4000

3000       continue  !error reading strtok(numtok)
           call msg('token:Error reading token',0)
           call msg(buffer,0)
           ios = 0
           goto 9999

4000       continue  !no problem with read of strtok(numtok)
C***       Stop collecting tokens once we've gotten the 
C          pressure/temp/wind/alt string. -lia

           icnt = 0
           do k = 1, lentok(numtok)
              if (strtok(numtok)(k:k) .eq. '/') then
                 icnt = icnt + 1
              endif
           end do

           if ( icnt .ge. 4)  goto 9999
C***
c          write(6, '('' numtok = '',i2)' ) numtok
c          write(6, '('' strtok(numtok) is on next line'')' )
c          write(6, '(1x,A)' ) strtok(numtok)

           start = zero  !reset token start indicator

c       else
c          write(6,'('' WARNING - double delimiters found'')' )
c          write(6,'(A70)' ) buffer(1:70)
c          write(6, '('' numtok = '',i2)' ) numtok
c          write(6, '('' strtok(numtok) is on next line'')' )
c          write(6, '(1x,A)' ) strtok(numtok)

        end if

C      When maximum number of tokens have been retrieved, exit.
        if (numtok .eq. 10) goto 9999
      end do

9999  continue
      call msg('token:Leaving token',3)

      return
      end
