c          Program:  unixbufr.f
c
c   Function:  This program converts an original BUFR file (DOS format)
c    to a UNIX compatible BUFR format.  Essentially the program
c    reformats the green words (4-byte record length preceeding and 
c    following each BUFR record) in the BUFR file.  To accomplish
c    the conversion, the 4 bytes of a record length are reversed while
c    the data within the record remain the same.
c
c   Usage:  unixbufr.exe <InputBUFRFile> <OutputBUFRFile>
c
c     The program and data files can reside in the same directory or
c     each can be in different directories or some variation of the
c     mix.
c
      character*60 filein, fileout
      character*4 beglen, endlen, bufr
      character*1 achar
      integer narg, iargc, lenbeg, lenend
      integer in, iout, iend
      logical doloop, docopy
c
      equivalence (beglen, lenbeg), (endlen, lenend)
c
c                     CHECK THE ARGUMENT COUNT...
c
      narg = iargc()
      if (narg.ne.2) then
       print *,'unixbufr:  incorrect usage'
       print *,'Usage:  unixbufr inputBUFRfile outputBUFRfile'     
       stop
      endif
c
      call getarg(1, filein)
      call getarg(2, fileout)
c
      write(6, 10) filein, fileout
 10   format(/'Program:  unixbufr'
     &   /'  infile:   ',a
     &   /'  outfile:  ',a)
c
      open(11, file=filein, status='old', recl=1, access='direct')
c
c    STEP 1-
c    READ THROUGH FILE COUNTING BYTES AND VERIFYING FILE STRUCTURE
c
      doloop = .true.
      nsize = 0
      do while (doloop)
c                      READ A RECORD BEGINNING GREEN WORD...
c
c     EITHER READ 1ST BYTE OF A NEW RECORD OR DETECT EOF...
c
       in = in + 1
       call rdbyte(in, beglen(4:4), ios)
       if (ios.eq.0) then
c
c        THERE SHOULD BE A BUFR RECORD
c        
         do i=3,1,-1
          in = in + 1
          call rdbyte(in, beglen(i:i), ios)
          if (ios.ne.0) then
           call errend(in,ios,'READING BEGIN RECORD LENGTH')
          endif
         enddo
         nsize = nsize + 4
c
c     READ THROUGH RECORD AND VALIDATE...
         do i=1,lenbeg
          in = in + 1
          call rdbyte(in, achar, ios)
          if (ios.ne.0) then
           call errend(in,ios,'READING A BUFR RECORD')
          endif
          if (i.lt.4) then
           bufr(i:i) = achar
          elseif (i.eq.4) then
           bufr(i:i) = achar
           if (bufr .ne. 'BUFR') then
            write(6, 35)
 35         format(/'ERROR - First 4-Bytes Of Next Record Are Not BUFR')
            close(11)
            stop
           endif 
          endif
         enddo 
         nsize = nsize + lenbeg
c
c                 READ A RECORD-ENDING GREEN WORD AND VALIDATE
         do i=4,1,-1
          in = in + 1
          read(11,rec=in) endlen(i:i)
          call rdbyte(in, endlen(i:i), ios)
          if (ios.ne.0) then
           call errend(in,ios,'READING ENDING RECORD LENGTH')
          endif
         enddo
         nsize = nsize + 4
c
         if (lenbeg .ne. lenend) then
          write(6, 40) in, lenbeg, lenend
 40       format(/'ERROR:  Input Pos.=',i8,', Begin Record Size=',
     &     i6,', But End Record Size=',i6
     &     /'The Begin and End Size MUST Be Equal')
          close(11)
          stop
         endif
c
c         IOS NOT = 0, END OF FILE...
       else
         doloop = .false.
       endif
      enddo
c
      write (6, 60) nsize
 60   format(/'Input BUFR Files Has ',i8,' Bytes')
c
c     STEP 2-
c     NOW THAT FORMAT HAS BEEN VALIDATED, CREATE AN OUTPUT
c
      open(11, file=filein, status='old', recl=1, access='direct')
      open(21, file=fileout, status='unknown', recl=1, access='direct')
c
      docopy = .true.
      in = 0
      iout = 0
      irec = 0
      do while (in .lt. nsize)
c
c                      READ A RECORD BEGINNING GREEN WORD...
c
       do i=4,1,-1
        in = in + 1
        read(11,rec=in) beglen(i:i)
       enddo
c
c            DISPLAY RECORD SIZES, COMMENT OUT IF DESIRED
c
       irec = irec + 1
       write(6, 30)irec, lenbeg
 30    format('Record ',i3,' Has ',i6,' Bytes')
c
c               WRITE A RECORD BEGINNING GREEN WORD...
c
       do i=1,4
        iout = iout + 1
        write(21,rec=iout) beglen(i:i) 
       enddo
c
c            COPY INPUT RECORD TO OUTPUT RECORD, BYTE BY BYTE
c
       do i=1,lenbeg
        in = in + 1
        iout = iout + 1
        read(11, rec=in) achar
        write(21,rec=iout) achar
       enddo 
c
c                 READ A RECORD-ENDING GREEN WORD
c
       do i=4,1,-1
        in = in + 1
        read(11,rec=in) endlen(i:i)
       enddo
c
c                WRITE A RECORD-ENDING GREEN WORD
c
       do i=1,4
        iout = iout + 1
        write(21,rec=iout) endlen(i:i) 
       enddo
c
      enddo
c
      close(11)
      close(21)
      stop
      end
c
c============================================
c
      subroutine rdbyte(in,achar,ios)
c
      character*(*) achar
      integer in,ios
c
      read(11, rec=in, iostat=ios) achar
      return
      end
c
c============================================
c
      subroutine errend(in,ios,emsg)
c
      character*(*) emsg
c
      write(6, 10) in, ios, emsg
 10   format(/'ERROR: UNEXPECTED READ ERROR OR EOF (INPUT)'
     & /'Byte ',i8,', Read Status ',i5,', ',a)
      close(11)
      stop
      end
