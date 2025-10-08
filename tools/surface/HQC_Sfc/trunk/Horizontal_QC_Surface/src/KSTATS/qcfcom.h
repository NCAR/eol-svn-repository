C
C 25 Aug 94 lec
C    Updated station identification string from 10 to 15 characters.
C
C structure for the qcf format record
      common /qcfcom/ qcf 
       structure /parm/
          real val
          character*1 qc
       end structure
       structure /ceil/
          real ceilhgt
          integer ceilflg
          character*1 ceilqc
          integer cldamt
          character*1 cldqc
       end structure
      structure /qcfrec/
         character*2 nomyr
         character*2 nommo
         character*2 nomday
         character*2 nomhr
         character*2 nommin
         character*2 yr
         character*2 mo
         character*2 day
         character*2 hr
         character*2 min
         character*10 network
         character*15 station
         real lat
         real lon
         integer occur
         real elev
         record /parm/ parms(8)
         character*1 sqind
         record /parm/ squal
         integer pw
         character*1 qcpw
         record /parm/ vis
         record /ceil/ cc(3)
      end structure
      record /qcfrec/ qcf
