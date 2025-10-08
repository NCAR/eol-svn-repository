C I/O variables and global debug
      logical debug
      character*127 errstr,gerror
      integer ios

C default unit nos and file names
      integer unit_config,unit_var,unit_stns
      parameter (unit_config=1,		!qc config file unit
     +           unit_var   =2,		!qcf variance file unit
     +           unit_stns  =3)		!station master input file unit

      character*30 qc_var    			!qcf variance file
      character*30 qc_stns   			!station master input file
      character*30 qc_config
      parameter (qc_config='/fest/hrly_sfc/qc/qc_config',	!qc config file
     +           qc_stns ='/fest/hrly_sfc/qc/qc_stns')	!stn input file

C Parameters for array sizing
      integer NPARMS
      parameter (NPARMS=7)		! number of parms with variance

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

C structures for the variance statistics reports
      structure /rec/
         integer nobs
         real lvar
         real hvar
         real max
         real var2
      end structure
      record /rec/ d(45,NPARMS)	!summary variance stats by day
      record /rec/ h(24,NPARMS)	!summary variance stats by hour
      record /rec/ s(2000,NPARMS)	!summary variance stats by station

      integer firstday			!first day of day array
      integer tobs(NPARMS)		!total obs for each parm
      integer nobs(20,NPARMS)		!number obs for each level for each parm

C global common block
      common /global/ debug,qc_var,firstday,varlim,tobs,nobs,v,d,h,s

C Parameters for array sizing
      integer NQCPARMS
      parameter (NQCPARMS=17)		! number of parms to qc
