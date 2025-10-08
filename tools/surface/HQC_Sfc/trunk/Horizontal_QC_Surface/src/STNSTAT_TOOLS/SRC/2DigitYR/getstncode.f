C***********************************************************************
C
C function: getstncode()
C description: Use qcf.network and qcf.station to lookup station code.
C              Add the station to the station exists if the station does
C              not already exist in the station array
C
C returns:  x=0  for failure
C           x>0  for success, where x is the index of the station in the
C                station array.
C
C************************************************************************

      integer function getstncode()

      implicit none

      include 'qcglobal.h'
      include 'qcfcom.h'
      include 'stncom.h'
      integer i

      if(stndx .ge. 60000) stndx=-1
      if(stndy .ge. 60000) stndy=-1
C look up the network/station code
      if(nstns .eq. 0) then 
        getstncode=1
        stns(1).network = qcf.network
        stns(1).station = qcf.station
        stns(1).lat = qcf.lat
        stns(1).lon = qcf.lon
        stns(1).elev = qcf.elev
        stns(1).x = stndx
        stns(1).y = stndy
        do i=1,8
           stns(1).val(i) = -999.99
           stns(1).dt(i) = 0
        end do
        nstns=1
        goto 10
      end if
      getstncode=0
      do i=1,nstns
         if(qcf.network.eq.stns(i).network .and.
     +      qcf.station.eq.stns(i).station) then 
           getstncode=i
           goto 10
         end if
      end do
      nstns = nstns+1
      if(nstns.gt.MAXSTNS) then
          write(0,*) 'Overflow on # networks/stations.',
     +               '  No variance record written.'
          return
      end if
      stns(nstns).network = qcf.network
      stns(nstns).station = qcf.station
      stns(nstns).lat = qcf.lat
      stns(nstns).lon = qcf.lon
      stns(nstns).elev = qcf.elev
      stns(nstns).x = stndx
      stns(nstns).y = stndy
      do i=1,8
         stns(nstns).val(i) = -999.99
         stns(nstns).dt(i) = 0
      end do
      getstncode=nstns
10    continue

      return
      end
