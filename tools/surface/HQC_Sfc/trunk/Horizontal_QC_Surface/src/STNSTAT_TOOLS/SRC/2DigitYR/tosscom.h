C default unit nos and file names
      integer unit_pres,unit_temp,unit_wind,unit_prcp
      parameter (unit_pres  =21,		!print file for pressure unit
     +           unit_temp  =22,		!print file for temp unit
     +           unit_wind  =23,		!print file for winds unit
     +           unit_prcp  =24)		!print file for precip unit

      character*30 qc_pres		 	!print file for pres
      character*30 qc_temp		 	!print file for temp
      character*30 qc_wind		 	!print file for winds
      character*30 qc_prcp		 	!print file for precip
      parameter (qc_pres ='prttoss.pres',			!pres file
     +           qc_temp ='prttoss.temp',			!temp file
     +           qc_wind ='prttoss.wind',			!wind file
     +           qc_prcp ='prttoss.prcp')			!prcp file

C structure for the var format record
      structure /parmrec/
         real maps
         real val
         integer*2 code
         character*1 qc
      end structure
      record /parmrec/ vh(24,8)	!24 hourly pre-toss print buckets
C      record /parmrec/ vm(288,8)	!288 5 min pre-toss print buckets
      record /parmrec/ vm(289,8)	!288 5 min pre-toss print buckets
      integer*2 tossstn 			!stn # of current toss station
      integer*2 tossday 			!day # of current toss day

C global common block
      common /global/ vh,vm,tossstn,tossday
