      program read_netcdf
      implicit none
c
c Program written by Randy Reeder and John Cassano
c 6 November 2002
c
c This program provides the basic subroutine calls needed
c to read the ARCMIP netCDF archive files
c
c Users should modify this program to read the files that
c they need using the three basic blocks of code to read
c the monthly mean, SHEBA column, and May 3h output ARCMIP
c netCDF files
c
      character *4 yymm      ! year and month for monthly mean files
      character *6 yymmdd    ! year, month, and day for column and May files
      character *6 model     ! model name used in netCDF filename
      character *4 runvers   ! model run version number used in
                             ! netCDF filename
      character *40 filename ! filename of netCDF file to be read
c
c define array sizes
c
      include 'domain.h'
c
c common blocks for arrays to be read from netCDF files
c
      include 'mon_netcdf.cb'
      include 'col_netcdf.cb'
      include 'may_netcdf.cb'
c
c start main program
c
c-------------------------------------------------------
c
c start code to read monthly mean data from netCDF file
c
      print *,' '
      print *,'Reading ARCMIP monthly mean output file'
      print *,' '
      print *,'Enter the yymm of data to be read'
      read(*,101) yymm
      print *,'Enter the model name (6 characters) of'
      print *,'the file to be read'
      read(*,102) model
      print *,' '
      print *,'Enter the model run version number (4 characters)'
      print *,'of the file to be read'
      read(*,101) runvers
      filename=model//'_'//runvers//'_mon_'//yymm//'.nc'
      call read_mon_netcdf(filename)
c
c end code to read monthly mean data from netCDF file
c
c-------------------------------------------------------
c
c start code to read SHEBA column data from netCDF file
c
      print *,' '
      print *,'Reading ARCMIP column output file'
      print *,' '
      print *,'Enter the yymmdd of data to be read'
      read(*,102) yymmdd
      print *,'Enter the model name (6 characters) of'
      print *,'the file to be read'
      read(*,102) model
      print *,' '
      print *,'Enter the model run version number (4 characters)'      
      print *,'of the file to be read'
      read(*,101) runvers
      filename=model//'_'//runvers//'_col_'//yymmdd//'.nc'
      call read_col_netcdf(filename)
c
c end code to read SHEBA column data from netCDF file
c
c-------------------------------------------------------
c
c start code to read May 3h data from netCDF file
c
      print *,' '
      print *,'Reading ARCMIP May 3h output file'
      print *,' '
      print *,'Enter the yymmdd of data to be read'
      read(*,102) yymmdd
      print *,'Enter the model name (6 characters) of'
      print *,'the file to be read'
      read(*,102) model
      print *,' '
      print *,'Enter the model run version number (4 characters)'  
      print *,'of the file to be read'
      read(*,101) runvers
      filename=model//'_'//runvers//'_may_'//yymmdd//'.nc'
      call read_may_netcdf(filename)
c
c end code to read May 3h data from netCDF file
c
c-------------------------------------------------------
c
  101 format(a4)
  102 format(a6)
c
c end of program
c
      stop
      end
c
c-------------------------------------------------------
c
      subroutine read_mon_netcdf(filename)
      implicit none
c
c Written by John Cassano and Randy Reeder (6 November 2002)
c
c subroutine to read ARCMIP monthly mean netCDF files
c
      character *40 filename ! name of file to be read
c
c variables to be read from netCDF file header
c
      character *4 yymm      ! current year and month
      character *7 type      ! type of model output being processed
                             ! = 'monthly' (monthly mean values)
      character *4 runvers   ! model run version number
      character *40 modeler, ! name of person that ran model
     +              fullname ! full name of model
      character *200 comments! any additional comments about model run
c
c netcdf parameters and variables
c
      include 'netcdf.inc'
      integer status, ncid
c
c domain size variables
c
      include 'domain.h'
c
c     Variable ids for netcdf code :
c
      integer psfc_id, tsfc_id, t2m_id, t2m_min_id,
     &     t2m_max_id, t1m_soil_id, soil_wat_id, precip_id,
     &     rain_id, snow_id, swe_id, tot_cld_frac_id,
     &     cld_base_id, lhf_id, shf_id, ust_id, swd_sfc_id,
     &     swu_sfc_id, lwd_sfc_id, lwu_sfc_id, swd_toa_id,
     &     swu_toa_id, lwu_toa_id
      integer u_id, v_id, w_id, t_id, z_id, rh_id, spechum_id,
     &     cld_id, ice_id, cld_frac_id
C
      include 'mon_netcdf.cb'
c
c start subroutine
c
      print *,' '
      print *,'Reading file: ',filename
C Open old netcdf file for reading :
      status = nf_open( filename, nf_nowrite, ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C Read global attributes :
      status = nf_get_att_text( ncid, nf_global, 'type', type )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'fullname', 
     &   fullname )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'runvers', 
     &   runvers )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'modeler', 
     &   modeler )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'yymm', 
     &   yymm )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'comments', 
     &   comments )
        if( status .ne. nf_noerr ) call handle_error(status)
C
c Output information from netCDF header
c
      print *,'ARCMIP file type: ',type
      print *,'Model name: ',fullname
      print *,'Model run version number: ',runvers
      print *,'Modeler name: ',modeler
      print *,'Model output date: ',yymm
      print *,'Comments: ',comments
      print *,'Number of vertical levels: ',nprs
      print *,'Number of e/w grid points: ',ix
      print *,'Number of n/s grid points: ',jx
C Obtain id's for two dimensional variables :
      status = nf_inq_varid( ncid, 'PSFC', psfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'TSFC', tsfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M', t2m_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M_MIN', t2m_min_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M_MAX', t2m_max_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T1M_SOIL', t1m_soil_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SOIL_WAT', soil_wat_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'PRECIP', precip_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'RAIN', rain_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SNOW', snow_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWE', swe_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'TOT_CLD_FRAC', tot_cld_frac_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD_BASE', cld_base_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LHF', lhf_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SHF', shf_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'UST', ust_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWD_SFC', swd_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWU_SFC', swu_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWD_SFC', lwd_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWU_SFC', lwu_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWD_TOA', swd_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWU_TOA', swu_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWU_TOA', lwu_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
C Obtain id's for three dimensional variables :
      status = nf_inq_varid( ncid, 'U', u_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'V', v_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'W', w_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T', t_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'Z', z_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'RH', rh_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SPECHUM', spechum_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD', cld_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ICE', ice_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD_FRAC', cld_frac_id )
        if( status .ne. nf_noerr ) call handle_error(status)
C
C Obtain all variable values :
      status = nf_get_var_real( ncid, psfc_id, psfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, tsfc_id, tsfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_id, t2m )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_min_id, t2m_min )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_max_id, t2m_max )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t1m_soil_id, t1m_soil )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, soil_wat_id, soil_wat )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, precip_id, precip )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, rain_id, rain )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, snow_id, snow )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swe_id, swe )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, tot_cld_frac_id, tot_cld_frac )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_base_id, cld_base )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lhf_id, lhf )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, shf_id, shf )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, ust_id, ust )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swd_sfc_id, swd_sfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swu_sfc_id, swu_sfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwd_sfc_id, lwd_sfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwu_sfc_id, lwu_sfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swd_toa_id, swd_toa )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swu_toa_id, swu_toa )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwu_toa_id, lwu_toa )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, u_id, u )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, v_id, v )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, w_id, w )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t_id, t )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, z_id, z )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, rh_id, rh )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, spechum_id, spechum )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_id, cld )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, ice_id, ice )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_frac_id, cld_frac )
        if( status .ne. nf_noerr ) call handle_error(status)
C Close netcdf data file :
      status = nf_close( ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
c
c end of subroutine
c
      return
      end
c
c-------------------------------------------------------
c
      subroutine read_col_netcdf(filename)
      implicit none
c
c Written by John Cassano and Randy Reeder (6 November 2002)
c
c subroutine to read ARCMIP SHEBA column netCDF files
c
      character *40 filename ! name of file to be read
c
c variables to be read from netCDF file header
c
      character *6 yymmdd    ! current year, month, and day
      character *7 type      ! type of model output being processed
                             ! = 'column'  (SHEBA column values)
      character *4 runvers   ! model run version number
      character *40 modeler, ! name of person that ran model
     +              fullname ! full name of model
      character *200 comments! any additional comments about model run
c
c netcdf parameters and variables
c
      include 'netcdf.inc'
      integer status, ncid
c
c domain size variables
c
      include 'domain.h'
c
      integer hour(nt)
      data hour /0, 3, 6, 9, 12 ,15, 18, 21/
c
c     Variable ids for netcdf code :
c
      integer psfc_id, tsfc_id, t2m_id, t2m_min_id,
     &     t2m_max_id, t1m_soil_id, soil_wat_id, precip_id,
     &     rain_id, snow_id, swe_id, tot_cld_frac_id,
     &     cld_base_id, lhf_id, shf_id, ust_id, swd_sfc_id,
     &     swu_sfc_id, lwd_sfc_id, lwu_sfc_id, swd_toa_id,
     &     swu_toa_id, lwu_toa_id, lat_id, lon_id
      integer u_id, v_id, w_id, t_id, z_id, rh_id, spechum_id,
     &     cld_id, ice_id, cld_frac_id, adv_t_id, adv_spechum_id,
     &     adv_cld_id, adv_ice_id
c
      include 'col_netcdf.cb'
c
c start subroutine
c
      print *,' '
      print *,'Reading file: ',filename
C Open old netcdf file for reading :
      status = nf_open( filename, nf_nowrite, ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C
C Read global attributes :
      status = nf_get_att_text( ncid, nf_global, 'type', type )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'fullname', 
     &   fullname )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'runvers', 
     &   runvers )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'modeler', 
     &   modeler )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'yymmdd', 
     &   yymmdd )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'comments', 
     &   comments )
        if( status .ne. nf_noerr ) call handle_error(status)
C
c Output information from netCDF header
c
      print *,'ARCMIP file type: ',type
      print *,'Model name: ',fullname
      print *,'Model run version number: ',runvers
      print *,'Modeler name: ',modeler
      print *,'Model output date: ',yymmdd
      print *,'Comments: ',comments
      print *,'Number of vertical levels: ',nprs
      print *,'Number of e/w grid points: ',ic
      print *,'Number of n/s grid points: ',jc
      print *,'Number of time periods in file: ',nt
c
C Obtain id's for three dimensional variables :
      status = nf_inq_varid( ncid, 'PSFC', psfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'TSFC', tsfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M', t2m_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M_MIN', t2m_min_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M_MAX', t2m_max_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T1M_SOIL', t1m_soil_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SOIL_WAT', soil_wat_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'PRECIP', precip_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'RAIN', rain_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SNOW', snow_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWE', swe_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'TOT_CLD_FRAC', tot_cld_frac_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD_BASE', cld_base_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LHF', lhf_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SHF', shf_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'UST', ust_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWD_SFC', swd_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWU_SFC', swu_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWD_SFC', lwd_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWU_SFC', lwu_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWD_TOA', swd_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWU_TOA', swu_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWU_TOA', lwu_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LAT', lat_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LON', lon_id )
        if( status .ne. nf_noerr ) call handle_error(status)
C Obtain id's for four dimensional variables :
      status = nf_inq_varid( ncid, 'U', u_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'V', v_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'W', w_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T', t_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'Z', z_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'RH', rh_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SPECHUM', spechum_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD', cld_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ICE', ice_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD_FRAC', cld_frac_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ADV_T', adv_t_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ADV_SPECHUM', adv_spechum_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ADV_CLD', adv_cld_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ADV_ICE', adv_ice_id )
        if( status .ne. nf_noerr ) call handle_error(status)
C
C Obtain all variable values :
      status = nf_get_var_real( ncid, psfc_id, psfc_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, tsfc_id, tsfc_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_id, t2m_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_min_id, t2m_min_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_max_id, t2m_max_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t1m_soil_id, t1m_soil_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, soil_wat_id, soil_wat_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, precip_id, precip_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, rain_id, rain_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, snow_id, snow_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swe_id, swe_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, tot_cld_frac_id, 
     &         tot_cld_frac_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_base_id, cld_base_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lhf_id, lhf_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, shf_id, shf_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, ust_id, ust_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swd_sfc_id, swd_sfc_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swu_sfc_id, swu_sfc_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwd_sfc_id, lwd_sfc_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwu_sfc_id, lwu_sfc_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swd_toa_id, swd_toa_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swu_toa_id, swu_toa_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwu_toa_id, lwu_toa_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lat_id, lat_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lon_id, lon_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, u_id, u_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, v_id, v_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, w_id, w_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t_id, t_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, z_id, z_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, rh_id, rh_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, spechum_id, spechum_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_id, cld_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, ice_id, ice_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_frac_id, cld_frac_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, adv_t_id, adv_t_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, adv_spechum_id,
     &         adv_spechum_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, adv_cld_id, adv_cld_col )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, adv_ice_id, adv_ice_col )
        if( status .ne. nf_noerr ) call handle_error(status)
C Close netcdf output file :
      status = nf_close( ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C
c end of subroutine
c
      return
      end
c
c-------------------------------------------------------
c
      subroutine read_may_netcdf(filename)
      implicit none
c
c Written by John Cassano and Randy Reeder (6 November 2002)
c
c subroutine to read ARCMIP May 3h data netCDF files
c
      character *40 filename ! name of file to be read
c
c variables to be read from netCDF file header
c
      character *6 yymmdd    ! current year, month, and day
      character *7 type      ! type of model output being processed
                             ! = '3h May'  (3 hourly May values)
      character *4 runvers   ! model run version number
      character *40 modeler, ! name of person that ran model
     +              fullname ! full name of model
      character *200 comments! any additional comments about model run
c
c netcdf parameters and variables
c
      include 'netcdf.inc'
      integer status, ncid
c
c domain size variables
c
      include 'domain.h'
c
      integer hour(nt)
      data hour /0, 3, 6, 9, 12, 15, 18, 21/
c
c     Variable ids for netcdf code :
c
      integer psfc_id, tsfc_id, t2m_id, t2m_min_id,
     &     t2m_max_id, t1m_soil_id, soil_wat_id, precip_id,
     &     rain_id, snow_id, swe_id, tot_cld_frac_id,
     &     cld_base_id, lhf_id, shf_id, ust_id, swd_sfc_id,
     &     swu_sfc_id, lwd_sfc_id, lwu_sfc_id, swd_toa_id,
     &     swu_toa_id, lwu_toa_id
      integer u_id, v_id, w_id, t_id, z_id, rh_id, spechum_id,
     &     cld_id, ice_id, cld_frac_id
c
      include 'may_netcdf.cb'
c
c start subroutine
c
      print *,' '
      print *,'Reading file: ',filename
C Open old netcdf file for reading :
      status = nf_open( filename, nf_nowrite, ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C
C Read global attributes :
      status = nf_get_att_text( ncid, nf_global, 'type', type )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'fullname', 
     &   fullname )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'runvers', 
     &   runvers )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'modeler', 
     &   modeler )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'yymmdd', 
     &   yymmdd )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_att_text( ncid, nf_global, 'comments', 
     &   comments )
        if( status .ne. nf_noerr ) call handle_error(status)
C
c Output information from netCDF header
c
      print *,'ARCMIP file type: ',type
      print *,'Model name: ',fullname
      print *,'Model run version number: ',runvers
      print *,'Modeler name: ',modeler
      print *,'Model output date: ',yymmdd
      print *,'Comments: ',comments
      print *,'Number of vertical levels: ',nprs
      print *,'Number of e/w grid points: ',ix
      print *,'Number of n/s grid points: ',jx
      print *,'Number of time periods in file: ',nt
c
C Obtain id's for three dimensional variables :
      status = nf_inq_varid( ncid, 'PSFC', psfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'TSFC', tsfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M', t2m_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M_MIN', t2m_min_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T2M_MAX', t2m_max_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T1M_SOIL', t1m_soil_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SOIL_WAT', soil_wat_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'PRECIP', precip_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'RAIN', rain_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SNOW', snow_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWE', swe_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'TOT_CLD_FRAC', tot_cld_frac_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD_BASE', cld_base_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LHF', lhf_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SHF', shf_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'UST', ust_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWD_SFC', swd_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWU_SFC', swu_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWD_SFC', lwd_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWU_SFC', lwu_sfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWD_TOA', swd_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SWU_TOA', swu_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'LWU_TOA', lwu_toa_id )
        if( status .ne. nf_noerr ) call handle_error(status)
C Obtain id's for four dimensional variables :
      status = nf_inq_varid( ncid, 'U', u_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'V', v_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'W', w_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'T', t_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'Z', z_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'RH', rh_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'SPECHUM', spechum_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD', cld_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'ICE', ice_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_inq_varid( ncid, 'CLD_FRAC', cld_frac_id )
        if( status .ne. nf_noerr ) call handle_error(status)
C
C Obtain all variable values :
      status = nf_get_var_real( ncid, psfc_id, psfc_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, tsfc_id, tsfc_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_id, t2m_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_min_id, t2m_min_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t2m_max_id, t2m_max_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t1m_soil_id, t1m_soil_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, soil_wat_id, soil_wat_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, precip_id, precip_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, rain_id, rain_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, snow_id, snow_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swe_id, swe_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, tot_cld_frac_id, 
     &         tot_cld_frac_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_base_id, cld_base_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lhf_id, lhf_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, shf_id, shf_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, ust_id, ust_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swd_sfc_id, swd_sfc_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swu_sfc_id, swu_sfc_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwd_sfc_id, lwd_sfc_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwu_sfc_id, lwu_sfc_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swd_toa_id, swd_toa_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, swu_toa_id, swu_toa_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, lwu_toa_id, lwu_toa_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, u_id, u_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, v_id, v_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, w_id, w_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, t_id, t_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, z_id, z_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, rh_id, rh_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, spechum_id, spechum_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_id, cld_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, ice_id, ice_may )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_get_var_real( ncid, cld_frac_id, cld_frac_may )
        if( status .ne. nf_noerr ) call handle_error(status)
C Close netcdf output file :
      status = nf_close( ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C
c end of subroutine
c
      return
      end
c
c-------------------------------------------------------
c
      subroutine handle_error( ret )
      implicit none
      include 'netcdf.inc'
      integer ret
      if( ret .ne. nf_noerr ) then
        write(6,*) 'Stopped due to netcdf error :', nf_strerror(ret)
        call abort
      endif
      end
c
c-----------------------------------------------------------------
c
