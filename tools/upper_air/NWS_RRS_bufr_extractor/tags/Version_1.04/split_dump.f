c        split_dump.f
c
c  Program Execution:  split_dump.exe <DumpData_Output_File>
c
c  This Program splits a BUFR dump file into the various message files:
c  NC002019.DMP, NC002020.DMP, NC002021.DMP, NC002022.DMP, 
c  NC002023.DMP, NC002024.DMP, and NC002025.DMP.
c
      character filin*80, filout*40, rec*128
      logical doloop, dolo2
c
c     write (6,*)'Enter Dump Filename In'
c     read(5, '(a)') filin
c
      call getarg (1, filin)
c
      open (7, file=filin, status='old', err = 900)

c
c    LOCATE THE FIRST MESSAGE TYPE - NC002019
c
      doloop = .true.
      do while (doloop)
        read (7,'(a)',iostat=ios) rec
        if (ios.ne.0) then
         write(6,*) 'EOF finding NC002019'
         stop
        endif
        if (rec(15:22).eq.'NC002019') then
         open(8,file='NC002019.DMP',status='unknown')
         doloop = .false.
        endif
      enddo
c
c
c    WRITE LINES UNTIL NEXT MESSAGE TYPE - NC002020 IS FOUND
c
      doloop = .true.
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002019'
         close(7)
         stop
       endif
       if (rec(15:22).eq.'NC002020') then
        close(8)
        open(8,file='NC002020.DMP', status='unknown') 
        doloop = .false.
       endif
      enddo
c
c
c    WRITE LINES UNTIL NEXT MESSAGE TYPE - NC002021 IS FOUND
c
      doloop = .true.
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002020'
         close(7)
         stop
       endif
       if (rec(15:22).eq.'NC002021') then
        close(8)
        open(8,file='NC002021.DMP', status='unknown') 
        doloop = .false.
       endif
      enddo
c
c
c    WRITE LINES UNTIL NEXT MESSAGE TYPE - NC002022 IS FOUND
c
      doloop = .true.
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002021'
         close(7)
         stop
       endif
       if (rec(15:22).eq.'NC002022') then
        close(8)
        open(8,file='NC002022.DMP', status='unknown') 
        doloop = .false.
       endif
      enddo
c
c
c    WRITE LINES UNTIL NEXT MESSAGE TYPE - NC002023 IS FOUND
c
      doloop = .true.
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002022'
         close(7)
         stop
       endif
       if (rec(15:22).eq.'NC002023') then
        close(8)
        open(8,file='NC002023.DMP', status='unknown') 
        doloop = .false.
       endif
      enddo
c
c
c    WRITE LINES UNTIL NEXT MESSAGE TYPE - NC002024 IS FOUND
c
      doloop = .true.
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002023'
         close(7)
         stop
       endif
       if (rec(15:22).eq.'NC002024') then
        close(8)
        open(8,file='NC002024.DMP', status='unknown') 
        doloop = .false.
       endif
      enddo
c
c
c    WRITE LINES UNTIL NEXT MESSAGE TYPE - NC002025 IS FOUND
c
      doloop = .true.
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002024'
         close(7)
         stop
       endif
       if (rec(15:22).eq.'NC002025') then
        close(8)
        open(8,file='NC002025.DMP', status='unknown') 
        doloop = .false.
       endif
      enddo
      doloop = .true.
c
c
c    WRITE LINES UNTIL END OF FILE IS DETECTED
c
      do while (doloop)
c          FIND LAST NON-SPACE CHARACTER TO CONSERVE OUTPUT SPACE
       dolo2 = .true.
       i=128
       do while (dolo2)
        if (rec(i:i).eq.' ') then
          i = i - 1
          if (i.lt.2) then
           dolo2 = .false.
          endif
        else
         dolo2 = .false.
        endif
       enddo

       write (8, '(a)') rec(1:i)
       read (7,'(a)',iostat=ios) rec
       if (ios.ne.0) then
         write(6,*) 'EOF writing NC002025'
         close(7)
         doloop = .false.
       endif
      enddo
      stop
c
 900  continue
      write(6,*)'Error opening input file'
      stop
c
      end
      
