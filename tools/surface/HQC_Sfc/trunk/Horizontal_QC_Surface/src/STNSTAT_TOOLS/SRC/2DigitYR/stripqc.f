C******************************************************************************
C
C program: stripqc
C author:  K. Scully
C date:    05/26/92
C description: Strip qcf header (and trailer) records from the qcf file for
C              merging.
C
C usage:   stripqc [-debug] input_file
C        
C              where -debug     option will print variable info as processing
C                               is done.
C                    input_file is the name of the input file containing
C                               data to be checked againts the analysis values.
C                               The input file is required.
C
C******************************************************************************

      program stripqc

      implicit none

      include 'ioerr.h'

C default unit nos and file names
      integer unit_input
      parameter (unit_input =2)		!qcf input file unit
      character*60 qc_input  			!qcf input file
      character*255 qcfbuf

C misc. variables
      integer iargc,narg
C     integer rindex,x
C     character*3 filetype

C************************** end variable definitions *********************

C get command line arguments
      narg = iargc()
      if(narg .eq. 1) then
         call getarg(1,qc_input)
      else
         write(0,*) 'Invalid number of arguments'
      end if

C     x=rindex(qc_input,'.')			!get char pos of file type
C     filetype=qc_input(x+1:x+3)		!extract file type
C     if(filetype .ne. '0qc') then
C        write(0,*) 'Input file: ',qc_input,' is not a 0qc file.'
C        write(0,*) 'Skipping...'
C        goto 999
C     end if
      write(0,*) 'Processing file: ',qc_input

C open the files
      open(unit_input,name=qc_input,status='old',iostat=ios,err=901)

C loop through the file of input records
      do while (.true.)
         read(unit_input,1002,iostat=ios,err=901,end=900) qcfbuf
         if(qcfbuf(1:13) .eq. 'Date     Time' ) goto 10
         if(qcfbuf(1:13) .eq. '         UTC ' ) goto 10
         if(qcfbuf(1:8) .eq. '********') goto 10
         write(*,1002,iostat=ios,err=901) qcfbuf
10       continue
      end do          ! end of while loop for each record in the file

900   close(unit_input)
      goto 999
901	 errstr=gerror(errstr)
      write(0,*) 'stripqc(): I/O Error: ',ios,' ',errstr
999   write(*,*) 'In stripqc'
      call standard_arithmetic()
      stop
1002  format(a235)	!This must be equal to length of the qcf record
      end
