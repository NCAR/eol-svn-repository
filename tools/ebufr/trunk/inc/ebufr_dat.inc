c******************************************************************
c Include file "ebufr_dat.inc"
c******************************************************************
      character*32 datum(MAXDAT)
      character*8 fmt(MAXDAT)
      integer xout(MAXDAT),yout(MAXDAT)

      structure /ebhdr/
         character*4 ebyear
         character*2 ebmonth
         character*2 ebday
         character*2 ebhour
         character*2 ebmin
         character*2 ebsec
         real eblat
         real eblon
         character*3 ebunique
      end structure

      record /ebhdr/ hdr
c******************************************************************
c End of include file "ebufr_dat.inc"
c******************************************************************
