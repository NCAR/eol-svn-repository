      program domain_netcdf
      implicit none
c
c Program written by Randy Reeder and John Cassano
c 9 October 2002
c
      character *40 filename
      character *7 type      ! type of model output being processed
                             ! = 'domain' (model domain data)
      character *6 model     ! model name used for netCDF filenames
      character *4 runvers   ! model run version number
      character *40 modeler, ! name of person that ran model
     +              fullname ! full name of model
      character *200 comments! any additional comments about model run
c
c ********************************************************
c change value of following parameters to match your model
c
      parameter (model='arcsym') ! limit 6 characters in length, no blank spaces
      parameter (runvers='ar01', ! limit 4 characters in length, no blank spaces
     +           modeler='John Cassano',
     +           fullname='ARCSyM v4.0')
      parameter (comments='none')
c
c ********************************************************
c
c start main program
c
c read model domain information file
c
c
c ********************************************************
c need to change this to match users filenaming convention
c
      filename='constantfields.input.hdf'
c
c ********************************************************
c
      call read_domain(filename)
c
c output model domain data to netCDF file
c
      type='domain'
      filename=model//'_'//runvers//'_domain.nc'
      call output_netcdf(filename,type,
     +   runvers,modeler,fullname,comments)
c
c end of program
c
      stop
      end
c
c-------------------------------------------------------
c
      subroutine output_netcdf(filename,type,
     +   runvers,modeler,fullname,comments)
      implicit none
      include 'netcdf.inc'
c
c Written by John Cassano and Randy Reeder (9 October 2002)
c
c subroutine to output standard ARCMIP domain netCDF files
c
      character *40 filename ! name of file to be output
      character *7 type      ! type of model output being processed
                             ! = 'domain' (model domain data)
      character *4 runvers   ! model run version number
      character *40 modeler, ! name of person that ran model
     +              fullname ! full name of model
      character *200 comments! any additional comments about model run
c
c domain parameters
c
      integer ix,           ! number of model grid points in east/west direction
     +        jx            ! number of model grid points in north/south direction
c
c ********************************************************
c
c need to change ix and jx to match model domain size
c
      parameter (ix=70, jx=55)
c
c ********************************************************
C
C Variable id's , etc. for netcdf code :
      integer status, ncid, iew_id, jns_id
      integer twodim(2)
      integer zsfc_id, lat_id, lon_id
C
c
c 2d variables
c
      real zsfc(ix,jx),     ! surface elevation (m)
     +     lat(ix,jx),      ! latitude (deg)
     +     lon(ix,jx)       ! longitude (deg)
c
c define common block for variables to be output to netCDF file
c
      common /out_netcdf/ zsfc, lat, lon
c
c start subroutine
c
      print *,' '
      print *,'Outputting file: ',filename
C Create new netcdf file :
      status = nf_create( filename, nf_noclobber, ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C Set global attributes :
      status = nf_put_att_text( ncid, nf_global, 'title', 40,
     &         filename )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'type', 7,
     &         type )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'fullname', 40,
     &         fullname )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'runvers', 4,
     &         runvers )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'modeler', 40,
     &         modeler )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'ew_index', 50,
     &         'iew = index of grid points in east/west direction' )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'ns_index', 51,
     &       'jns = index of grid points in north/south direction' )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, nf_global, 'comments', 200,
     &         comments )
        if( status .ne. nf_noerr ) call handle_error(status)
C Set dimensions of arrays :
      status = nf_def_dim( ncid, 'iew', ix, iew_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_def_dim( ncid, 'jns', jx, jns_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      twodim(1) = iew_id
      twodim(2) = jns_id
C Two dimensional arrays and attributes :
      status = nf_def_var( ncid, 'ZSFC', nf_float, 2, twodim,
     &         zsfc_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, zsfc_id, 'long_name', 18,
     &         'surface elevation ' )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, zsfc_id, 'units', 2,
     &         'm ' )
        if( status .ne. nf_noerr ) call handle_error(status)
c
      status = nf_def_var( ncid, 'LAT', nf_float, 2, twodim,
     &         lat_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, lat_id, 'long_name', 9,
     &         'latitude ' )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, lat_id, 'units', 4,
     &         'deg ' )
        if( status .ne. nf_noerr ) call handle_error(status)
c
      status = nf_def_var( ncid, 'LON', nf_float, 2, twodim,
     &         lon_id )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, lon_id, 'long_name', 10,
     &         'longitude ' )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_att_text( ncid, lon_id, 'units', 4,
     &         'deg ' )
        if( status .ne. nf_noerr ) call handle_error(status)
c
C  End of variable definitions :
      status = nf_enddef( ncid )
        if( status .ne. nf_noerr ) call handle_error(status)
C Write all variables to netcdf output file :
      status = nf_put_var_real( ncid, zsfc_id, zsfc )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_var_real( ncid, lat_id, lat )
        if( status .ne. nf_noerr ) call handle_error(status)
      status = nf_put_var_real( ncid, lon_id, lon )
        if( status .ne. nf_noerr ) call handle_error(status)
C Close netcdf output file :
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
      subroutine handle_error(ret)
      implicit none
      include 'netcdf.inc'
      integer ret
      if( ret .ne. nf_noerr ) then
        write(6,*)  'Stoped due to netcdf error =', nf_strerror(ret)
        call abort
      endif
      end
C
c ********************************************************
c
c the following subroutines are specific to ARCSyM
c and will need to be modified for each groups model output
c
c ********************************************************
c
c-------------------------------------------------------
c
      subroutine read_domain(filename)
      implicit none
c
c Written by John Cassano (9 October 2002)
c
c subroutine to read ARCSyM domain file and fill
c arrays needed by output_netcdf subroutine
c
      character *40 filename ! name of file to be read
c
c domain parameters
c
      integer ix,           ! number of model grid points in east/west direction
     +        jx            ! number of model grid points in north/south direction
c
c ********************************************************
c
c need to change ix and jx to match model domain size
c
      parameter (ix=70, jx=55)
c
c ********************************************************
c
c 2d variables
c
      real zsfc(ix,jx),     ! surface elevation (m)
     +     lat(ix,jx),      ! latitude (deg)
     +     lon(ix,jx)       ! longitude (deg)
c
c define common block for variables to be output to netCDF file
c
      common /out_netcdf/ zsfc, lat, lon
c
c ********************************************************
c
c Other users may need to define different variables for
c reading their model output files
c
c dummy variables used when reading ARCSyM file
c
      real *4 dum2(ix,jx)       ! dummy 2d variable
c
c other variables needed to read ARCSyM file
c
      character *70 label
c
c ********************************************************
c
c begin subroutine
c
      print *,'Reading file: ',filename
      call rdhdf(filename,dum2,ix,jx,label) ! unused variable
      call rdhdf(filename,dum2,ix,jx,label) ! unused variable
      call rdhdf(filename,dum2,ix,jx,label) ! unused variable
      call rdhdf(filename,dum2,ix,jx,label)
      call flip2d(dum2,lat,ix,jx)
      call rdhdf(filename,dum2,ix,jx,label)
      call flip2d(dum2,lon,ix,jx)
      call rdhdf(filename,dum2,ix,jx,label) ! unused variable
      call rdhdf(filename,dum2,ix,jx,label) ! unused variable
      call rdhdf(filename,dum2,ix,jx,label)
      call flip2d(dum2,zsfc,ix,jx)
      call rdhdf(filename,dum2,ix,jx,label) ! unused variable
      return
      end
c
c-------------------------------------------------------
c
      subroutine flip2d(dum2,out2,ix,jx)
      implicit none
c
c Written by John Cassano
c
c Subroutine to flip ARCSyM file variables so
c that j index increases from south to north
c
      integer ix,jx
      real *4 dum2(ix,jx)
      real out2(ix,jx)
      integer i,j
c
      do i=1,ix
        do j=1,jx
          out2(i,jx-j+1)=dum2(i,j)
        enddo
      enddo
      return
      end
c
c-------------------------------------------------------
c
      subroutine rdhdf(filename,data,nx,ny,label)

      character*(*) filename,label
      integer nx,ny
      real*4 data(nx,ny)

      integer rank,maxrank,ierr,mx,my
      parameter (maxrank=2)
      integer*4 dimsizes(maxrank)
      character*5  unit,format
      character*11 coordsys

      integer dsgdims,dsgdata,dsgdast


      ierr = dsgdims(filename, rank, dimsizes, maxrank)

      if (ierr.ne.0) then
         print *,"Problem getting dimensions of file:"
         print '(a70)',filename
         stop
      endif

      mx = dimsizes(1)
      my = dimsizes(2)

      if ((nx.ne.mx).or.(ny.ne.my)) then
         print *,nx,ny,mx,my
         STOP 'Dimensions not the same as HDF file.'
      endif

      ierr = dsgdast(label,unit,format,coordsys)
      if (ierr.ne.0) STOP 'DFSDgetdatastrs error'

      ierr = dsgdata(filename, rank, dimsizes, data)
      if (ierr.ne.0) STOP 'DFSDgetdata error'

      return
      end
c
c-----------------------------------------------------------------
c
