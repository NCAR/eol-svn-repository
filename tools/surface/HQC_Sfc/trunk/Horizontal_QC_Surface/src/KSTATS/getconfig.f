C************************************************************************
C
C function: getconfig()
C description: read the config file and get the variance limits from
C              the variance config file
C
C************************************************************************

      subroutine getconfig()

      implicit none

      include 'qcglobal.h'
      include 'configcom.h'
      include 'debug.h'
      include 'ioerr.h'
      include 'parmnames.h'

C structure for variance limits
      structure /varlimrec/
          character*20 parmname
          real varbad
          real varquest
          real varoffset
          real rocbad
          real rocquest
      end structure
      record /varlimrec/ varlim

      character*80 varbuf
      integer i

C open the config file
      open(unit_config,name=qc_config,status='old',iostat=ios,err=901)
      if(debug) then
         write(*,*) "Reading in qc_config file:"
         write(*,*)
      end if

C initialize the bad and questionable variance arrays.
C -999.99 means a variance test is not applicable.
      do i=1,NQCPARMS
         varb(i)=-999.99
         varq(i)=-999.99
         varo(i)=0.0
         rocb(i)=-999.99
         rocq(i)=-999.99
      end do

C loop through the records.
      do while (.true.)
          read(unit_config,1003,iostat=ios,err=901,end=900) varbuf
          write(*,1003) varbuf
          do i=1,NQCPARMS
             if(varbuf(1:20) .eq. parmnames(i)) then
                read(varbuf,1001)
     +               varlim.parmname,varlim.varbad,varlim.varquest,
     +               varlim.varoffset,varlim.rocbad,varlim.rocquest
                varb(i)=varlim.varbad
                varq(i)=varlim.varquest
                varo(i)=varlim.varoffset
                rocb(i)=varlim.rocbad
                rocq(i)=varlim.rocquest
             else if(varbuf(1:7) .eq. 'Day 1: ') then
                read(varbuf,1005) day1
             end if
          end do
      end do

900   if(debug) then
         write(*,*)
         write(*,*) 'Arrays varb(), varq(), varo(), rocb() and rocq():'
         do i=1,NQCPARMS
            write(*,1002) i,parmnames(i),varb(i),varq(i),varo(i),
     +                    rocb(i),rocq(i)
         end do
	    type *,' Continue?'
         accept 1004, YN
         if(yn .eq. 'N' .or. yn .eq. 'n') stop
      end if
      goto 999
901   errstr=gerror(errstr)
      write(*,*) 'getconfig(): I/O Error: ',ios,' ',errstr
999   close(unit_config)
      return
1001  format(a20,5(3x,f7.2))
1002  format(i2,3x,a20,5(3x,f7.2))
1003  format(a80)
1004  format(a1)
1005  format(7x,a8)
      end
