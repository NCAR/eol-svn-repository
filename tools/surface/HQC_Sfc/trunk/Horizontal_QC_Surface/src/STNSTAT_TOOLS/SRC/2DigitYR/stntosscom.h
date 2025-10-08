C I/O variables and global debug
      logical debug
      character*127 errstr,gerror
      integer ios

C default unit nos and file names
      integer unit_config,unit_var,unit_stns
      parameter (unit_config=1,		!qc config file unit
     +           unit_var   =2,		!qcf variance file unit
     +           unit_toss  =3,		!qcf toss file unit
     +           unit_stns  =4)		!station master input file unit

      character*30 qc_var    			!qcf variance file
      character*30 qc_toss   			!qcf toss file
      character*30 qc_stns   			!station master input file
      character*30 qc_config
      parameter (qc_config='./qc_config',	!qc config file
     +           qc_stns ='./qc_stns')	!stn input file

C Parameters for array sizing
      integer NPARMS
      parameter (NPARMS=8)		! number of parms with variance

C structure for configuration input file (for variance limits, etc.)
      structure /varlimrec/
          character*20 parmname
          real varbad
          real varquest
          real varoffset
          real rocbad
          real rocquest
      end structure
      record /varlimrec/ varlim

C structure for the variance input file
      structure /varrec/
         integer*2 stn
         integer day
         integer hr
         real var(NPARMS)
      end structure
      record /varrec/ v

C structure for the toss input file
      structure /tossrec/
         integer*2 stn
         integer day
         integer hr
         real maps
         real val
         real code
         real qc
      end structure
      record /tossrec/ t

C structures for the variance statistics reports
      structure /rec/
         integer nobs
         integer nb
         integer nd
         real lvar
         real hvar
         real max
      end structure
      record /rec/ s(2000,NPARMS)	!summary variance stats by station

      integer firstday			!first day of day array

C global common block
      common /global/ debug,qc_var,qc_toss,firstday,varlim,v,t,s

C Parameters for array sizing
      integer NQCPARMS
      parameter (NQCPARMS=17)		! number of parms to qc
