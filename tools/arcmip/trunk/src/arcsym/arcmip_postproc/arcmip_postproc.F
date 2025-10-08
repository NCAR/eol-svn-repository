      program arcmip_postproc
c
c Written by J. Cassano (5 Sept. 2002)
c
c Conduct post-processing of ARCSyM output for ARCMIP project
c
c Program will:
c              Read one month of ARCSyM output files
c              Interpolate 3d variables to constant pressure levels
c              Calculate geopotential height
c              Calculate addition diagnostics needed for ARCMIP
c              Output: monthly mean values on constant pressure levels
c                      daily files with 3 h output for SHEBA column
c                      daily files with 3 h output for May 1997
c
c Input files: constantfields.input.hdf
c              runo.atmos.yymmdd.hdf
c              runo.ice.yymmdd.hdf (not needed for ARCMIP)
c              runo.lsm.yymmdd.hdf
c              runo.rad.yymmdd.hdf
c              arcsym_sheba_position.txt
c
c Output files: runo.mon.yymm.hdf (monthly means on pressure levels)
c               runo.col.yymmdd.hdf (SHEBA column output)
c               runo.may.yymmdd.hdf (daily May output files)
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
      character *4 yymm
      integer start_day,end_day
      integer cnt,day
      character *6 yymmdd
      integer n_hours,hour_cnt
      logical first_time,last_time


c Start main program
c
c Get time invariant variables and information
c about month to be processed
c
      call get_timeinvar
      call get_monthinfo(yymm,start_day,end_day)
c
c---- Loop over all model output times for current month
c
      cnt=0
      do day=start_day,end_day ! loop over days of month
        call get_yymmdd(yymm,day,yymmdd)
        n_hours=nint(24./out_freq)
c
c read ARCSyM LSM and radiation output files
c
        call read_lsm(yymmdd,runno,n_hours)
        call read_rad(yymmdd,runno,n_hours)
        do hour_cnt=1,n_hours ! loop over output times in daily file
c
c update counters and flags
c
          cnt=cnt+1
          if((hour_cnt.eq.1).and.(day.eq.start_day))then
            first_time=.true.
            last_time=.false.
          elseif((hour_cnt.eq.n_hours).and.
     +           (day.eq.end_day))then
            first_time=.false.
            last_time=.true.
          else
            first_time=.false.
            last_time=.false.
          endif
c
c zero monthly average arrays
c
          if(first_time)then
            call init_mon
          endif
c
c read ARCSyM atmospheric output file and 
c copy LSM / radiation variables to arcmip_2d
c and arcmip_sig_3d arrays
c
          call read_atmos(yymmdd,runno,first_time)
          call copy_lsmrad(hour_cnt)
c
c calculate geopotential height and height of cloud base (calc_geop)
c interpolate 3d data to pressure levels (press_level)
c calculate humidity variables (calc_humidity)
c
          call calc_geop
          call press_level
          call calc_humidity
c
c sum monthly variables
c
          call sum_monthly(first_time,last_time)
c
c store SHEBA column and May 1997 values
c
        call store_sheba(hour_cnt,yymmdd)
        if(yymm.eq.'9805')then
          call store_may(hour_cnt,yymmdd)
        endif
        enddo ! end hour_cnt loop
c
c output SHEBA column data
c
        call out_sheba(yymmdd,n_hours)
c
c output 3h values for May 1997
c
        if(yymm.eq.'9805')then
          call out_may(yymmdd,n_hours)
        endif
      enddo ! end day loop
c
c Calculate monthly mean values and output to hdf file
c
      call calc_monthly(cnt)
      call out_monthly(yymm,runno)
c
c end of main program
c
      stop
      end
c
c-----------------------------------------------------------------
c
      subroutine copy_lsmrad(hour_cnt)
c
c subroutine to copy LSM and radiation variables from lsm_rad_var
c arrays to arcmip_2d and arcmip_sig_3d arrays for current time
c
      implicit none
#include <domain_size.h>
#include <lsm_rad_var.cb>
#include <arcmip_2d.cb>
#include <arcmip_sig_3d.cb>
      integer hour_cnt
      integer i,j,k
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          t1m_soil(i,j)=t1m_soil_lsm(i,j,hour_cnt)
          soil_wat(i,j)=soil_wat_lsm(i,j,hour_cnt)
          tot_cld_frac(i,j)=tot_cld_frac_rad(i,j,hour_cnt)
          cld_base(i,j)=cld_base_rad(i,j,hour_cnt)
          do k=1,kx
            cld_frac_sig(i,j,k)=cld_frac_rad(i,j,k,hour_cnt)
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine store_sheba(hour_cnt,yymmdd)
c
c subroutine to extract 9 point SHEBA column from
c ARCSyM grid and store in arcmip_col_2d and arcmip_col_3d arrays
c
c Also calculate advective tendencies for SHEBA column
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_2d.cb>
#include <arcmip_col_2d.cb>
#include <arcmip_3d.cb>
#include <arcmip_col_3d.cb>
      integer hour_cnt
      character *6 yymmdd
      character *2 hh
      character *8 yymmddhh,read_date
      integer hour,i,j,p,i_loc,j_loc,i_col,j_col
c
c begin subroutine
c
c get current date
c
      hour=(hour_cnt-1)*out_freq
      if(hour.lt.10)then
        write(hh,101) hour
      else
        write(hh,102) hour
      endif
  101 format('0',i1)
  102 format(i2)
      yymmddhh=yymmdd//hh
c
c read sheba position file
c
      open(unit=21,file='arcsym_sheba_position.txt',
     +     form='formatted',status='old')
   10 continue
      read(21,103,end=999) read_date,i_loc,j_loc
  103 format(a8,2(i3))
      if(read_date.ne.yymmddhh)then
        goto 10
      endif
c
c extract SHEBA column data from arcmip_2d and arcmip_3d arrays
c
      if((i_loc.lt.2).or.(i_loc.gt.ix-1))then
        print *,'Error with i location read from file',i_loc
      endif
      if((j_loc.lt.2).or.(j_loc.gt.jx-1))then
        print *,'Error with j location read from file',j_loc
      endif
      do i=i_loc-1,i_loc+1
        do j=j_loc-1,j_loc+1
          i_col=(i-i_loc+2)
          j_col=(j-j_loc+2)
          lat_col(i_col,j_col,hour_cnt)=xlat(i,j)
          lon_col(i_col,j_col,hour_cnt)=xlon(i,j)
          psfc_col(i_col,j_col,hour_cnt)=psfc(i,j)
          tsfc_col(i_col,j_col,hour_cnt)=tsfc(i,j)
          t2m_col(i_col,j_col,hour_cnt)=t2m(i,j)
          t2m_min_col(i_col,j_col,hour_cnt)=t2m_min(i,j)
          t2m_max_col(i_col,j_col,hour_cnt)=t2m_max(i,j)
          t1m_soil_col(i_col,j_col,hour_cnt)=t1m_soil(i,j)
          soil_wat_col(i_col,j_col,hour_cnt)=soil_wat(i,j)
          rainc_col(i_col,j_col,hour_cnt)=rainc(i,j)
          rainnc_col(i_col,j_col,hour_cnt)=rainnc(i,j)
          precip_col(i_col,j_col,hour_cnt)=precip(i,j)
          rain_col(i_col,j_col,hour_cnt)=rain(i,j)
          snow_col(i_col,j_col,hour_cnt)=snow(i,j)
          swe_col(i_col,j_col,hour_cnt)=swe(i,j)
          tot_cld_frac_col(i_col,j_col,hour_cnt)=tot_cld_frac(i,j)
          cld_base_col(i_col,j_col,hour_cnt)=cld_base(i,j)
          lhf_col(i_col,j_col,hour_cnt)=lhf(i,j)
          shf_col(i_col,j_col,hour_cnt)=shf(i,j)
          ust_col(i_col,j_col,hour_cnt)=ust(i,j)
          swd_sfc_col(i_col,j_col,hour_cnt)=swd_sfc(i,j)
          swu_sfc_col(i_col,j_col,hour_cnt)=swu_sfc(i,j)
          lwd_sfc_col(i_col,j_col,hour_cnt)=lwd_sfc(i,j)
          lwu_sfc_col(i_col,j_col,hour_cnt)=lwu_sfc(i,j)
          swd_toa_col(i_col,j_col,hour_cnt)=swd_toa(i,j)
          swu_toa_col(i_col,j_col,hour_cnt)=swu_toa(i,j)
          lwu_toa_col(i_col,j_col,hour_cnt)=lwu_toa(i,j)
          do p=1,nprs
            u_mod_col(i_col,j_col,p,hour_cnt)=u_mod(i,j,p)
            v_mod_col(i_col,j_col,p,hour_cnt)=v_mod(i,j,p)
            u_col(i_col,j_col,p,hour_cnt)=u(i,j,p)
            v_col(i_col,j_col,p,hour_cnt)=v(i,j,p)
            w_col(i_col,j_col,p,hour_cnt)=w(i,j,p)
            t_col(i_col,j_col,p,hour_cnt)=t(i,j,p)
            z_col(i_col,j_col,p,hour_cnt)=z(i,j,p)
            rh_col(i_col,j_col,p,hour_cnt)=rh(i,j,p)
            qv_col(i_col,j_col,p,hour_cnt)=qv(i,j,p)
            spechum_col(i_col,j_col,p,hour_cnt)=spechum(i,j,p)
            cld_col(i_col,j_col,p,hour_cnt)=cld(i,j,p)
            ice_col(i_col,j_col,p,hour_cnt)=ice(i,j,p)
            cld_frac_col(i_col,j_col,p,hour_cnt)=cld_frac(i,j,p)
c
c calculate advective tendencies
c
            call calc_adv(ix,jx,nprs,i,j,p,hour_cnt,
     +          u_mod,v_mod,t,ic,jc,mt,
     +          i_col,j_col,adv_t_col,dx)
            call calc_adv(ix,jx,nprs,i,j,p,hour_cnt,
     +          u_mod,v_mod,spechum,ic,jc,mt,
     +          i_col,j_col,adv_spechum_col,dx)
            call calc_adv(ix,jx,nprs,i,j,p,hour_cnt,
     +          u_mod,v_mod,cld,ic,jc,mt,
     +          i_col,j_col,adv_cld_col,dx)
            call calc_adv(ix,jx,nprs,i,j,p,hour_cnt,
     +          u_mod,v_mod,ice,ic,jc,mt,
     +          i_col,j_col,adv_ice_col,dx)
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
      return
  999 continue
      print *,'!!!!! ERROR !!!!!!'
      print *,'Could not find date: ',yymmddhh
      stop
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_adv(ix,jx,nprs,i,j,p,hour_cnt,
     +          u_mod,v_mod,var,ic,jc,mt,
     +          i_col,j_col,adv_var_col,dx)
c
c calculate horizontal advective tendencies for SHEBA column
c
c adv_tend = - u * (dvar/dx) - v * (dvar/dy)
c
c Note: i index increases towards east
c       j index increases towards south
c       positive u is wind towards east
c       positive v is wind towards north
c
c Note: wind components located at dot points in model domain
c       scalar variables located at cross points in model domain
c
c dot(i,j) is 1/2 grid cell north and west of cross(i,j)
c
c Provide advective tendency per hour
c (multiply by 60 min/h * 60 s/min = 3600)
c
      implicit none
      integer ix,jx,nprs,i,j,p,hour_cnt
      real *4 u_mod(ix,jx,nprs), v_mod(ix,jx,nprs),
     +        var(ix,jx,nprs)
      integer ic,jc,mt,i_col,j_col
      real *4 adv_var_col(ic,jc,nprs,mt)
      real dx
      real *4 u1,u2,v1,v2
c
c begin subroutine
c
      u1=(u_mod(i+1,j,p)+u_mod(i+1,j+1,p))*
     +   (var(i+1,j,p)+var(i,j,p))
      u2=(u_mod(i,j,p)+u_mod(i,j+1,p))*
     +   (var(i,j,p)+var(i-1,j,p))
      v1=(v_mod(i,j,p)+v_mod(i+1,j,p))*
     +   (var(i,j-1,p)+var(i,j,p))
      v2=(v_mod(i+1,j+1,p)+v_mod(i,j+1,p))*
     +   (var(i,j,p)+var(i,j+1,p))
      adv_var_col(i_col,j_col,p,hour_cnt)= (-1./(4.*dx))*
     +     ((u1-u2)+(v1-v2))*3600.
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine out_sheba(yymmdd,n_hours)
c
c subroutine to output 9 point SHEBA column data
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_col_2d.cb>
#include <arcmip_col_3d.cb>
      character *6 yymmdd
      integer n_hours
      character *70 label
      character *40 filename
      integer hour_cnt,hour
      character *2 hh
      character *8 yymmddhh
      real *4 dum2(ic,jc),dum3(ic,jc,nprs)
      real *4 v_tmp(ic,jc,nprs)
c
c begin subroutine
c
c get filename for output file
c
      filename=runno//'.col.'//yymmdd//'.hdf'
      print *,'Writing SHEBA column file: ',filename
c
c loop over all output hours for current day
c
      do hour_cnt=1,n_hours
c
c get current yymmddhh for output file use
c
        hour=(hour_cnt-1)*out_freq
        if(hour.lt.10)then
          write(hh,101) hour
  101     format('0',i1)
        else
          write(hh,102) hour
  102     format(i2)
        endif
        yymmddhh=yymmdd//hh
        label="lat"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,lat_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="lon"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,lon_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="psfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,psfc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="tsfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,tsfc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="t2m"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,t2m_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="t2m_min"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,t2m_min_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="t2m_max"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,t2m_max_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="t1m_soil"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,t1m_soil_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="soil_wat"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,soil_wat_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="rainc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,rainc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="rainnc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,rainnc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="precip"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,precip_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="rain"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,rain_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="snow"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,snow_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="swe"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,swe_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="tot_cld_frac"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,tot_cld_frac_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="cld_base"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,cld_base_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="lhf"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,lhf_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="shf"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,shf_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="ust"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,ust_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="swd_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,swd_sfc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="swu_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,swu_sfc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="lwd_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,lwd_sfc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="lwu_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,lwu_sfc_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="swd_toa"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,swd_toa_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="swu_toa"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,swu_toa_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="lwu_toa"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ic,jc,mt,lwu_toa_col,dum2,hour_cnt)
        call mkhdf(filename,dum2,ic,jc,label)
        label="u_mod"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,u_mod_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="v_mod"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,v_mod_col,v_tmp,hour_cnt)
        call flipv(ic,jc,nprs,v_tmp,dum3)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="u"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,u_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="v"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,v_col,v_tmp,hour_cnt)
        call flipv(ic,jc,nprs,v_tmp,dum3)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="w"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,w_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="t"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,t_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="z"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,z_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="rh"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,rh_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="qv"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,qv_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="spechum"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,spechum_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="cld"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,cld_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="ice"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,ice_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="cld_frac"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,cld_frac_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="adv_t"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,adv_t_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="adv_spechum"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,adv_spechum_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="adv_cld"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,adv_cld_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
        label="adv_ice"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ic,jc,nprs,mt,adv_ice_col,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ic,jc,nprs,label)
      enddo ! end hour_cnt loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine prep3d(ix,jx,nprs,mt,in_var,out_var,hour_cnt)
c
c subroutine to copy 4d array to 3d array for output
c used to transfer hourly data from (i,j,p,t) array to (i,j,p) array
c
      implicit none
      integer ix,jx,nprs,mt
      real *4 in_var(ix,jx,nprs,mt),out_var(ix,jx,nprs)
      integer hour_cnt
      integer i,j,p
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          do p=1,nprs
            out_var(i,j,p)=in_var(i,j,p,hour_cnt)
          enddo ! end p loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine prep2d(ix,jx,mt,in_var,out_var,hour_cnt)
c
c subroutine to copy 3d array to 2d array for output
c used to transfer hourly data from (i,j,t) array to (i,j) array
c
      implicit none
      integer ix,jx,mt
      real *4 in_var(ix,jx,mt),out_var(ix,jx)
      integer hour_cnt
      integer i,j
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          out_var(i,j)=in_var(i,j,hour_cnt)
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine store_may(hour_cnt)
c
c subroutine to store ARCSyM variables in
c arcmip_may_2d and arcmip_may_3d arrays
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_2d.cb>
#include <arcmip_may_2d.cb>
#include <arcmip_3d.cb>
#include <arcmip_may_3d.cb>
      integer hour_cnt
      integer i,j,p
c
c begin subroutine
c
c store model output in arcmip_may_2d and arcmip_may_3d arrays
c
      do i=1,ix
        do j=1,jx
          psfc_may(i,j,hour_cnt)=psfc(i,j)
          tsfc_may(i,j,hour_cnt)=tsfc(i,j)
          t2m_may(i,j,hour_cnt)=t2m(i,j)
          t2m_min_may(i,j,hour_cnt)=t2m_min(i,j)
          t2m_max_may(i,j,hour_cnt)=t2m_max(i,j)
          t1m_soil_may(i,j,hour_cnt)=t1m_soil(i,j)
          soil_wat_may(i,j,hour_cnt)=soil_wat(i,j)
          rainc_may(i,j,hour_cnt)=rainc(i,j)
          rainnc_may(i,j,hour_cnt)=rainnc(i,j)
          precip_may(i,j,hour_cnt)=precip(i,j)
          rain_may(i,j,hour_cnt)=rain(i,j)
          snow_may(i,j,hour_cnt)=snow(i,j)
          swe_may(i,j,hour_cnt)=swe(i,j)
          tot_cld_frac_may(i,j,hour_cnt)=tot_cld_frac(i,j)
          cld_base_may(i,j,hour_cnt)=cld_base(i,j)
          lhf_may(i,j,hour_cnt)=lhf(i,j)
          shf_may(i,j,hour_cnt)=shf(i,j)
          ust_may(i,j,hour_cnt)=ust(i,j)
          swd_sfc_may(i,j,hour_cnt)=swd_sfc(i,j)
          swu_sfc_may(i,j,hour_cnt)=swu_sfc(i,j)
          lwd_sfc_may(i,j,hour_cnt)=lwd_sfc(i,j)
          lwu_sfc_may(i,j,hour_cnt)=lwu_sfc(i,j)
          swd_toa_may(i,j,hour_cnt)=swd_toa(i,j)
          swu_toa_may(i,j,hour_cnt)=swu_toa(i,j)
          lwu_toa_may(i,j,hour_cnt)=lwu_toa(i,j)
          do p=1,nprs
            u_mod_may(i,j,p,hour_cnt)=u_mod(i,j,p)
            v_mod_may(i,j,p,hour_cnt)=v_mod(i,j,p)
            u_may(i,j,p,hour_cnt)=u(i,j,p)
            v_may(i,j,p,hour_cnt)=v(i,j,p)
            w_may(i,j,p,hour_cnt)=w(i,j,p)
            t_may(i,j,p,hour_cnt)=t(i,j,p)
            z_may(i,j,p,hour_cnt)=z(i,j,p)
            rh_may(i,j,p,hour_cnt)=rh(i,j,p)
            qv_may(i,j,p,hour_cnt)=qv(i,j,p)
            spechum_may(i,j,p,hour_cnt)=spechum(i,j,p)
            cld_may(i,j,p,hour_cnt)=cld(i,j,p)
            ice_may(i,j,p,hour_cnt)=ice(i,j,p)
            cld_frac_may(i,j,p,hour_cnt)=cld_frac(i,j,p)
          enddo ! end p loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine out_may(yymmdd,n_hours)
c
c subroutine to output 3h values for May
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_may_2d.cb>
#include <arcmip_may_3d.cb>
      character *6 yymmdd
      integer n_hours
      character *70 label
      character *40 filename
      integer hour_cnt,hour
      character *2 hh
      character *8 yymmddhh
      real *4 dum2(ix,jx),dum3(ix,jx,nprs)
      real *4 v_tmp(ix,jx,nprs)
c
c begin subroutine
c
c get filename for output file
c
      filename=runno//'.may.'//yymmdd//'.hdf'
      print *,'Writing May 3h output file: ',filename
      print *,' '
      print *,'****** the output from this subroutine has'
      print *,'not been checked yet ***********'
c
c loop over all output hours for current day
c
      do hour_cnt=1,n_hours
c
c get current yymmddhh for output file use
c
        hour=(hour_cnt-1)*out_freq
        if(hour.lt.10)then
          write(hh,101) hour
  101     format('0',i1)
        else
          write(hh,102) hour
  102     format(i2)
        endif
        yymmddhh=yymmdd//hh
        label="psfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,psfc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="tsfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,tsfc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="t2m"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,t2m_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="t2m_min"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,t2m_min_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="t2m_max"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,t2m_max_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="t1m_soil"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,t1m_soil_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="soil_wat"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,soil_wat_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="rainc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,rainc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="rainnc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,rainnc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="precip"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,precip_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="rain"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,rain_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="snow"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,snow_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="swe"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,swe_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="tot_cld_frac"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,tot_cld_frac_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="cld_base"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,cld_base_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="lhf"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,lhf_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="shf"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,shf_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="ust"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,ust_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="swd_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,swd_sfc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="swu_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,swu_sfc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="lwd_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,lwd_sfc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="lwu_sfc"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,lwu_sfc_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="swd_toa"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,swd_toa_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="swu_toa"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,swu_toa_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="lwu_toa"
     +    //' '//runno//' '//yymmddhh
        call prep2d(ix,jx,mt,lwu_toa_may,dum2,hour_cnt)
        call mkhdf(filename,dum2,ix,jx,label)
        label="u_mod"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,u_mod_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="v_mod"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,v_mod_may,v_tmp,hour_cnt)
        call flipv(ix,jx,nprs,v_tmp,dum3)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="u"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,u_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="v"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,v_may,v_tmp,hour_cnt)
        call flipv(ix,jx,nprs,v_tmp,dum3)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="w"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,w_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="t"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,t_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="z"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,z_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="rh"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,rh_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="qv"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,qv_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="spechum"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,spechum_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="cld"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,cld_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="ice"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,ice_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
        label="cld_frac"
     +    //' '//runno//' '//yymmddhh
        call prep3d(ix,jx,nprs,mt,cld_frac_may,dum3,hour_cnt)
        call m3dhdf(filename,dum3,ix,jx,nprs,label)
      enddo ! end hour_cnt loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_humidity
c
c subroutine to calculate specific humidity and rh from
c ARCSyM output mixing ratio
c
      implicit none
#include <domain_size.h>
#include <arcmip_3d.cb>
      integer i,j,p,x
      real eps
      parameter (eps=0.622)
      real press,e,es,ws
      real *4 ac(7),temp
      data ac(1) /6.1117675/
      data ac(2) /0.443986062/
      data ac(3) /1.43053301E-02/
      data ac(4) /2.65027242E-04/
      data ac(5) /3.02246994E-06/
      data ac(6) /2.03886313E-08/
      data ac(7) /6.38780966E-11/

c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          do p=1,nprs
c
c calculate vapor pressure (e) from mixing ratio (qv)
c
            press=prs(p) ! hPa
            e=(qv(i,j,p)*press)/(qv(i,j,p)+eps) ! hPa
c
c calculate specific humidity (spechum) from vapor pressure and pressure
c
            spechum(i,j,p)=(eps*e)/(press-(1.-eps)*e) ! kg/kg
c
c calculate saturation vapor pressure (es) ! hPa
c using sixth-order polynomial fit from Flatau et al. (1992)
c
            temp=t(i,j,p)-273.15 ! deg C
            es = ac(1)
            do x = 2,7
              es = es + ac(x)*temp**(x-1)
            enddo
c
c calculate saturation mixing ratio (ws) and then relative humidity (rh)
c
            ws=(eps*es)/(press-es) ! kg/kg
            rh(i,j,p)=(qv(i,j,p)/ws)*100. ! %
          enddo ! end p loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine out_monthly(yymm,runno)
c
c subroutine to output monthly mean fields to hdf file
c
      implicit none
#include <domain_size.h>
#include <arcmip_mon_2d.cb>
#include <arcmip_mon_3d.cb>
      real *4 dum3(ix,jx,kx)
      character *4 yymm,runno
      character *70 label
      character *40 filename
c
c begin subroutine
c
c get filename for output file
c
      filename=runno//'.mon.'//yymm//'.hdf'
      print *,'Writing monthly mean file: ',filename
c
c output monthly mean fields to hdf file
c
      label="psfc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,psfc_mon,ix,jx,label)
      label="tsfc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,tsfc_mon,ix,jx,label)
      label="t2m"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,t2m_mon,ix,jx,label)
      label="t2m_min"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,t2m_min_mon,ix,jx,label)
      label="t2m_max"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,t2m_max_mon,ix,jx,label)
      label="t1m_soil"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,t1m_soil_mon,ix,jx,label)
      label="soil_wat"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,soil_wat_mon,ix,jx,label)
      label="rainc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,rainc_mon,ix,jx,label)
      label="rainnc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,rainnc_mon,ix,jx,label)
      label="precip"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,precip_mon,ix,jx,label)
      label="rain"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,rain_mon,ix,jx,label)
      label="snow"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,snow_mon,ix,jx,label)
      label="swe"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,swe_mon,ix,jx,label)
      label="tot_cld_frac"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,tot_cld_frac_mon,ix,jx,label)
      label="cld_base"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,cld_base_mon,ix,jx,label)
      label="lhf"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,lhf_mon,ix,jx,label)
      label="shf"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,shf_mon,ix,jx,label)
      label="ust"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,ust_mon,ix,jx,label)
      label="swd_sfc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,swd_sfc_mon,ix,jx,label)
      label="swu_sfc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,swu_sfc_mon,ix,jx,label)
      label="lwd_sfc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,lwd_sfc_mon,ix,jx,label)
      label="lwu_sfc"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,lwu_sfc_mon,ix,jx,label)
      label="swd_toa"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,swd_toa_mon,ix,jx,label)
      label="swu_toa"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,swu_toa_mon,ix,jx,label)
      label="lwu_toa"
     +  //' '//runno//' '//yymm
      call mkhdf(filename,lwu_toa_mon,ix,jx,label)
      label="u_mod"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,u_mod_mon,ix,jx,nprs,label)
      label="v_mod"
     +  //' '//runno//' '//yymm
      call flipv(ix,jx,nprs,v_mod_mon,dum3)
      call m3dhdf(filename,dum3,ix,jx,nprs,label)
      label="u"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,u_mon,ix,jx,nprs,label)
      label="v"
     +  //' '//runno//' '//yymm
      call flipv(ix,jx,nprs,v_mon,dum3)
      call m3dhdf(filename,dum3,ix,jx,nprs,label)
      label="w"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,w_mon,ix,jx,nprs,label)
      label="t"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,t_mon,ix,jx,nprs,label)
      label="z"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,z_mon,ix,jx,nprs,label)
      label="rh"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,rh_mon,ix,jx,nprs,label)
      label="qv"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,qv_mon,ix,jx,nprs,label)
      label="spechum"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,spechum_mon,ix,jx,nprs,label)
      label="cld"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,cld_mon,ix,jx,nprs,label)
      label="ice"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,ice_mon,ix,jx,nprs,label)
      label="cld_frac"
     +  //' '//runno//' '//yymm
      call m3dhdf(filename,cld_frac_mon,ix,jx,nprs,label)
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_monthly(cnt)
c
c Subroutine to calculate monthly mean values
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_mon_2d.cb>
#include <arcmip_mon_3d.cb>
      integer cnt
      real tot
      integer i,j,p
c
c begin subroutine
c
c check if cnt.eq.0
c
      if(cnt.eq.0)then
        print *,'!!!!!! ERROR !!!!!!!'
        print *,'cnt=0 in subroutine calc_monthly'
        stop
      endif
c
c calculate monthly averages from running sum
c
      print *,'Calculating monthly mean using',cnt,' values'
      tot=1.*cnt
      do i=1,ix
        do j=1,jx
          psfc_mon(i,j)=psfc_mon(i,j)/tot
          tsfc_mon(i,j)=tsfc_mon(i,j)/tot
          t2m_mon(i,j)=t2m_mon(i,j)/tot
          t1m_soil_mon(i,j)=t1m_soil_mon(i,j)/tot
          soil_wat_mon(i,j)=soil_wat_mon(i,j)/tot
          tot_cld_frac_mon(i,j)=tot_cld_frac_mon(i,j)/tot
c
c if no clouds are present during month set cld_base=25000.
c
          if(cld_base_cnt(i,j).gt.0)then
c
c calculate average cloud base (m AGL)
c
            cld_base_mon(i,j)=(cld_base_mon(i,j)/
     +          (cld_base_cnt(i,j)*1.))-zs(i,j)
          else
            cld_base_mon(i,j)=25000.
          endif
          lhf_mon(i,j)=lhf_mon(i,j)/tot
          shf_mon(i,j)=shf_mon(i,j)/tot
          ust_mon(i,j)=ust_mon(i,j)/tot
          swd_sfc_mon(i,j)=swd_sfc_mon(i,j)/tot
          swu_sfc_mon(i,j)=swu_sfc_mon(i,j)/tot
          lwd_sfc_mon(i,j)=lwd_sfc_mon(i,j)/tot
          lwu_sfc_mon(i,j)=lwu_sfc_mon(i,j)/tot
          swd_toa_mon(i,j)=swd_toa_mon(i,j)/tot
          swu_toa_mon(i,j)=swu_toa_mon(i,j)/tot
          lwu_toa_mon(i,j)=lwu_toa_mon(i,j)/tot
          do p=1,nprs
            u_mod_mon(i,j,p)=u_mod_mon(i,j,p)/tot
            v_mod_mon(i,j,p)=v_mod_mon(i,j,p)/tot
            u_mon(i,j,p)=u_mon(i,j,p)/tot
            v_mon(i,j,p)=v_mon(i,j,p)/tot
            w_mon(i,j,p)=w_mon(i,j,p)/tot
            t_mon(i,j,p)=t_mon(i,j,p)/tot
            z_mon(i,j,p)=z_mon(i,j,p)/tot
            rh_mon(i,j,p)=rh_mon(i,j,p)/tot
            qv_mon(i,j,p)=qv_mon(i,j,p)/tot
            spechum_mon(i,j,p)=spechum_mon(i,j,p)/tot
            cld_mon(i,j,p)=cld_mon(i,j,p)/tot
            ice_mon(i,j,p)=ice_mon(i,j,p)/tot
            cld_frac_mon(i,j,p)=cld_frac_mon(i,j,p)/tot
          enddo ! end p loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine sum_monthly(first_time,last_time)
c
c Subroutine to sum variables for monthly calculation
c
      implicit none
#include <domain_size.h>
#include <arcmip_2d.cb>
#include <arcmip_3d.cb>
#include <arcmip_mon_2d.cb>
#include <arcmip_mon_3d.cb>
      logical first_time,last_time
      integer i,j,p
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          psfc_mon(i,j)=psfc_mon(i,j)+psfc(i,j)
          tsfc_mon(i,j)=tsfc_mon(i,j)+tsfc(i,j)
          t2m_mon(i,j)=t2m_mon(i,j)+t2m(i,j)
          t2m_min_mon(i,j)=amin1(t2m_min(i,j),t2m_min_mon(i,j))
          t2m_max_mon(i,j)=amax1(t2m_max(i,j),t2m_max_mon(i,j))
          t1m_soil_mon(i,j)=t1m_soil_mon(i,j)+t1m_soil(i,j)
          soil_wat_mon(i,j)=soil_wat_mon(i,j)+soil_wat(i,j)
          if(first_time)then
            rainc_mon(i,j)=rainc(i,j)
            rainnc_mon(i,j)=rainnc(i,j)
          endif
          if(last_time)then
            rainc_mon(i,j)=rainc(i,j)-rainc_mon(i,j)
            rainnc_mon(i,j)=rainnc(i,j)-rainnc_mon(i,j)
            swe_mon(i,j)=swe(i,j)
          endif
          precip_mon(i,j)=precip_mon(i,j)+precip(i,j)
          rain_mon(i,j)=rain_mon(i,j)+rain(i,j)
          snow_mon(i,j)=snow_mon(i,j)+snow(i,j)
          tot_cld_frac_mon(i,j)=tot_cld_frac_mon(i,j)+
     +                          tot_cld_frac(i,j)
c
c if no clouds are present in column cld_base is set to 25000.
c and the current value of cld_base should not be included in
c the monthly average calculation
c
          if(cld_base(i,j).lt.24999.)then
            cld_base_mon(i,j)=cld_base_mon(i,j)+cld_base(i,j)
            cld_base_cnt(i,j)=cld_base_cnt(i,j)+1
          endif
          lhf_mon(i,j)=lhf_mon(i,j)+lhf(i,j)
          shf_mon(i,j)=shf_mon(i,j)+shf(i,j)
          ust_mon(i,j)=ust_mon(i,j)+ust(i,j)
          swd_sfc_mon(i,j)=swd_sfc_mon(i,j)+swd_sfc(i,j)
          swu_sfc_mon(i,j)=swu_sfc_mon(i,j)+swu_sfc(i,j)
          lwd_sfc_mon(i,j)=lwd_sfc_mon(i,j)+lwd_sfc(i,j)
          lwu_sfc_mon(i,j)=lwu_sfc_mon(i,j)+lwu_sfc(i,j)
          swd_toa_mon(i,j)=swd_toa_mon(i,j)+swd_toa(i,j)
          swu_toa_mon(i,j)=swu_toa_mon(i,j)+swu_toa(i,j)
          lwu_toa_mon(i,j)=lwu_toa_mon(i,j)+lwu_toa(i,j)
          do p=1,nprs
            u_mod_mon(i,j,p)=u_mod_mon(i,j,p)+u_mod(i,j,p)
            v_mod_mon(i,j,p)=v_mod_mon(i,j,p)+v_mod(i,j,p)
            u_mon(i,j,p)=u_mon(i,j,p)+u(i,j,p)
            v_mon(i,j,p)=v_mon(i,j,p)+v(i,j,p)
            w_mon(i,j,p)=w_mon(i,j,p)+w(i,j,p)
            t_mon(i,j,p)=t_mon(i,j,p)+t(i,j,p)
            z_mon(i,j,p)=z_mon(i,j,p)+z(i,j,p)
            rh_mon(i,j,p)=rh_mon(i,j,p)+rh(i,j,p)
            qv_mon(i,j,p)=qv_mon(i,j,p)+qv(i,j,p)
            spechum_mon(i,j,p)=spechum_mon(i,j,p)+
     +                         spechum(i,j,p)
            cld_mon(i,j,p)=cld_mon(i,j,p)+cld(i,j,p)
            ice_mon(i,j,p)=ice_mon(i,j,p)+ice(i,j,p)
            cld_frac_mon(i,j,p)=cld_frac_mon(i,j,p)+
     +                          cld_frac(i,j,p)
          enddo ! end p loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine press_level
c
c Subroutine to interpolate 3d sigma level data to
c constant pressure levels
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_sig_3d.cb>
#include <arcmip_3d.cb>
#include <arcmip_2d.cb>
      integer i,j,p
      real psigt,psigb,press
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          psigt=sigma(1)*(psfc(i,j)-ptop)+ptop ! pressure at top full sigma level
          psigb=sigma(kx)*(psfc(i,j)-ptop)+ptop ! pressure at lowest full sigma level
          do p=1,nprs
            press=prs(p) ! current pressure level
            if(press.le.psigt)then
c------------ pressure level above model top
              call p_top_bot(i,j,p,1)
            elseif(press.ge.psigb)then
c------------ pressure level below model bottom
              call p_top_bot(i,j,p,kx)
            else
c------------ interpolate from nearest full sigma levels to pressure level
              call interp_to_p(i,j,p)
            endif
          enddo ! end of p loop
        enddo ! end of j loop
      enddo ! end of i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine interp_to_p(i,j,p)
c
c interpolate from full sigma levels to constant pressure levels
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_2d.cb>
#include <arcmip_sig_3d.cb>
#include <arcmip_3d.cb>
      integer i,j,p
      integer k
      real p1,p2,ap1,ap2,dp
      real press,ap
      real tv,rdg
      parameter (rdg=287./9.81)
c
c begin subroutine
c
      k=kx-1
      press=prs(p)
      ap=alog(press)
   10 continue
      p1=sigma(k)*(psfc(i,j)+ptop)+ptop
      p2=sigma(k+1)*(psfc(i,j)+ptop)+ptop
      if((p1.le.press).and.(p2.ge.press))then
c------ pressure level located between sigma levels k and k+1
        ap1=alog(p1)
        ap2=alog(p2)
        dp=ap2-ap1
        call interp(u_mod,u_mod_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(v_mod,v_mod_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(u,u_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(v,v_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(w,w_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(t,t_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(rh,rh_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(qv,qv_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(spechum,spechum_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(cld,cld_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(ice,ice_sig,ap1,ap2,ap,dp,i,j,p,k)
        call interp(cld_frac,cld_frac_sig,ap1,ap2,ap,dp,i,j,p,k)
c------ calculate height of constant pressure surface
        tv=(1+0.622*(0.5*(qv_sig(i,j,k+1)+qv_sig(i,j,k))))*
     +     (0.5*(t_sig(i,j,k+1)+t_sig(i,j,k)))
        z(i,j,p)=z_sig(i,j,k+1)+rdg*tv*alog(p2/press)
      else
        if(k.eq.1)then
          print *,'!!!!!! ERROR !!!!!!'
          print *,'Out of range in interp_to_p'
          print *,'Pressure level = ',press
          print *,'k=',k
          print *,'p1=',p1
          print *,'p2=',p2
          stop
        else
          k=k-1
          goto 10
        endif
      endif
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine interp(var_press,var_sig,ap1,ap2,ap,dp,i,j,p,k)
c
c perform interpolate from sigma level to pressure level
c
      implicit none
#include <domain_size.h>
      real *4 var_press(ix,jx,nprs),var_sig(ix,jx,kx)
      real ap1,ap2,ap,dp
      integer i,j,p,k
c
c begin subroutine
c
      var_press(i,j,p)=(var_sig(i,j,k)*(ap2-ap)+
     +                 var_sig(i,j,k+1)*(ap-ap1))/dp
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine p_top_bot(i,j,p,k)
c
c For constant pressure level above model top:
c    Set constant pressure values to value at top full sigma level (k=1)
c For constant pressure level below model bottom:
c    Set constant pressure values to value at bottom full sigma level (k=kx)
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
#include <arcmip_sig_3d.cb>
#include <arcmip_3d.cb>
#include <arcmip_2d.cb>
      integer i,j,p,k
      real rdg
      parameter (rdg=287./9.81)
      real tv,p1,p2
c
c begin subroutine
c
      u_mod(i,j,p)=u_mod_sig(i,j,k)
      v_mod(i,j,p)=v_mod_sig(i,j,k)
      u(i,j,p)=u_sig(i,j,k)
      v(i,j,p)=v_sig(i,j,k)
      w(i,j,p)=w_sig(i,j,k)
      t(i,j,p)=t_sig(i,j,k)
      rh(i,j,p)=rh_sig(i,j,k)
      qv(i,j,p)=qv_sig(i,j,k)
      spechum(i,j,p)=spechum_sig(i,j,k)
      cld(i,j,p)=cld_sig(i,j,k)
      ice(i,j,p)=ice_sig(i,j,k)
      cld_frac(i,j,p)=cld_frac_sig(i,j,k)
c
c calculate geopotential height of pressure level
c
      tv=(1.+0.622*qv_sig(i,j,k))*t_sig(i,j,k) ! units of K
      p1=sigma(k)*(psfc(i,j)-ptop)+ptop ! units of mb
      p2=prs(p) ! units of mb
      z(i,j,p)=z_sig(i,j,k)+rdg*tv*alog(p1/p2)
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_geop
c
c Subroutine to calculate geopotential height
c on full sigma levels
c
c Also assign height to lowest cloud base
c
      implicit none
#include <domain_size.h>
#include <arcmip_sig_3d.cb>
#include <domain_var.cb>
#include <arcmip_2d.cb>
      real rdg
      parameter (rdg=287./9.81)
      real tv,p1,p2
      integer i,j,k
      
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          do k=kx,1,-1
            if(k.eq.kx)then
c------------ special case for lowest model level
              tv=(1.+0.622*qv_sig(i,j,k))*t_sig(i,j,k) ! units of K
              p1=psfc(i,j) ! units of mb
              p2=sigma(k)*(psfc(i,j)-ptop)+ptop ! units of mb
              z_sig(i,j,k)=zs(i,j)+rdg*tv*alog(p1/p2)
            else
c------------ no special treatment for other sigma levels
              tv=(1+0.622*(0.5*(qv_sig(i,j,k+1)+qv_sig(i,j,k))))*
     +           (0.5*(t_sig(i,j,k+1)+t_sig(i,j,k)))
              p1=sigma(k+1)*(psfc(i,j)-ptop)+ptop
              p2=sigma(k)*(psfc(i,j)-ptop)+ptop
              z_sig(i,j,k)=z_sig(i,j,k+1)+rdg*tv*alog(p1/p2)
            endif
          enddo ! end k loop
c
c Convert cld_base(i,j) k index cloud base height to
c actual cloud base height
c Note: cld_base=0 indicates no clouds present in column
c       Set cld_base=25000. for this case and do not use
c       this value when calculating monthly average
c
          if(cld_base(i,j).gt.0)then
            cld_base(i,j)=z_sig(i,j,cld_base(i,j))
          else
            cld_base(i,j)=25000.
          endif
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine read_rad(yymmdd,runno,n_hours)
c
c subroutine to read ARCSyM rad.*.hdf file
c
      implicit none
#include <domain_size.h>
#include <lsm_rad_var.cb>
      character *6 yymmdd
      character *4 runno
      integer n_hours
      integer hour_cnt
      real *4 dum2(ixr,jxr),dum3(ixr,jxr,kxr)
      character *40 filename
      character *70 label
      character *8 cdate
c
c begin subroutine
c
      filename=runno//'.rad'//yymmdd//'.hdf'
      print *,'Reading file: ',filename
c
c Read variables from file
c
      do hour_cnt=1,n_hours ! loop over all output times in daily file
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! o3mmr
        cdate=label(33:40)
        print *,'  Reading data for time: ',cdate
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! h2ommr
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! taucld
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! cvcld
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! cld
        call fillout_3d(ixr,jxr,kxr,dum3,
     +                  ix,jx,kx,mt,cld_frac_rad,hour_cnt)
        call cld_base_calc(ixr,jxr,kxr,dum3,dum2)
        call fillout_2d(ixr,jxr,dum2,
     +                  ix,jx,mt,cld_base_rad,hour_cnt)
        call tot_cld_calc(ixr,jxr,kxr,dum3,dum2)
        call fillout_2d(ixr,jxr,dum2,
     +                  ix,jx,mt,tot_cld_frac_rad,hour_cnt)
        call rdhdf(filename,dum2,ixr,jxr,label) ! total_clouds
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! clwp
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! qrs
        call r3dhdf(filename,dum3,ixr,jxr,kxr,label) ! qrl
        call rdhdf(filename,dum2,ixr,jxr,label) ! frsa
        call rdhdf(filename,dum2,ixr,jxr,label) ! frla
        call rdhdf(filename,dum2,ixr,jxr,label) ! slwd
        call rdhdf(filename,dum2,ixr,jxr,label) ! clrst
        call rdhdf(filename,dum2,ixr,jxr,label) ! clrss
        call rdhdf(filename,dum2,ixr,jxr,label) ! clrlt
        call rdhdf(filename,dum2,ixr,jxr,label) ! clrls
      enddo ! end of hour_cnt loop
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine fillout_2d(ixd,jxd,dum2,
     +                      ix,jx,mt,arcmip_2d,hour_cnt)
c
c Subroutine to copy 2d dummy array to
c 2d ARCMIP output array and fill outer row and column
c
      implicit none
      integer ixd,jxd,ix,jx,mt,hour_cnt
      real *4 dum2(ixd,jxd),arcmip_2d(ix,jx,mt)
      integer i,j
c
c begin subroutine
c
c
c Check that array bounds are reasonable
c
      if(ixd.ne.(ix-1))then
        print *,'!!!!!!!! WARNING !!!!!!!!!'
        print *,'ixd not equal to ix-1 in subroutine fillout_2d'
        print *,'ixd=',ixd
        print *,'ix=',ix
        stop
      endif
      if(jxd.ne.(jx-1))then
        print *,'!!!!!!!! WARNING !!!!!!!!!'
        print *,'jxd not equal to jx-1 in subroutine fillout_2d'
        print *,'jxd=',jxd
        print *,'jx=',jx
        stop
      endif
c
c copy dum2 to arcmip_2d
c
      do i=1,ixd
        do j=1,jxd
          arcmip_2d(i,j,hour_cnt)=dum2(i,j)
        enddo ! end j loop
      enddo ! end i loop
c
c fill outer row and column to arcmip_2d
c
      do i=1,ixd
        arcmip_2d(i,jx,hour_cnt)=dum2(i,jxd)
      enddo ! end i loop
      do j=1,jx
        arcmip_2d(ix,j,hour_cnt)=dum2(ixd,j)
      enddo ! end j loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine tot_cld_calc(ixr,jxr,kxr,dum3,dum2)
c
c Calculate total cloud cover using Tian and Curry (1989)
c method of random overlap
c
      implicit none
      integer ixr,jxr,kxr
      real *4 dum3(ixr,jxr,kxr),dum2(ixr,jxr)
      integer i,j,k
      real t_cld(3),max_cld,tot_cld
c
c begin subroutine
c
c
c calculate cloud cover in 3 seperate layers
c
      do i=1,ixr
        do j=1,jxr
          t_cld(1)=0.
          t_cld(2)=0.
          t_cld(3)=0.
          do k=1,10
            t_cld(1)=max(dum3(i,j,k),t_cld(1))
          enddo ! end k loop
          do k=11,15
            t_cld(2)=max(dum3(i,j,k),t_cld(2))
          enddo ! end k loop
          do k=16,kxr
            t_cld(3)=max(dum3(i,j,k),t_cld(3))
          enddo ! end k loop
          max_cld=max(t_cld(1),t_cld(2),t_cld(3))
          tot_cld=1.0d0-(1.0d0-t_cld(1))*
     +                  (1.0d0-t_cld(2))*
     +                  (1.0d0-t_cld(3))
          if(tot_cld.gt.0.7d0)then
            tot_cld=0.5d0*tot_cld+0.5d0*max_cld
          endif
          dum2(i,j)=tot_cld
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine cld_base_calc(ixr,jxr,kxr,dum3,dum2)
c
c Subroutine to determine cloud base 
c dum2 stores sigma level index (k=1 is model top; k=kxr is model bottom)
c of lowest cloud
c
      implicit none
      integer ixr,jxr,kxr
      real *4 dum3(ixr,jxr,kxr),dum2(ixr,jxr)
      integer i,j,k
c
c begin subroutine
c
      do i=1,ixr
        do j=1,jxr
          k=kxr
   10     continue
          if(dum3(i,j,k).gt.0.)then
c---------- cloud is present
            dum2(i,j)=k
          else
c---------- no cloud at this level
            if(k.eq.1)then
c------------ no cloud present in column
              dum2(i,j)=0
            else
c------------ keep checking column for cloud cover
              k=k-1
              goto 10
            endif
          endif
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine fillout_3d(ixd,jxd,kxd,dum3,
     +                      ix,jx,kx,mt,arcmip_3d,hour_cnt)
c
c Subroutine to copy 3d dummy array to
c 3d ARCMIP output array and fill outer row and column
c Note: This subroutine assumes that kxd = kx
c       This subroutine assumes that ixd=ix-1 and jxd=jx-1
c      
c
      implicit none
      integer ixd,jxd,kxd,ix,jx,kx,mt,hour_cnt
      real *4 dum3(ixd,jxd,kxd),arcmip_3d(ix,jx,kx,mt)
      integer i,j,k
c
c begin subroutine
c
c
c Check that array bounds are reasonable
c
      if(ixd.ne.(ix-1))then
        print *,'!!!!!!!! WARNING !!!!!!!!!'
        print *,'ixd not equal to ix-1 in subroutine fillout_3d'
        print *,'ixd=',ixd
        print *,'ix=',ix
        stop
      endif
      if(jxd.ne.(jx-1))then
        print *,'!!!!!!!! WARNING !!!!!!!!!'
        print *,'jxd not equal to jx-1 in subroutine fillout_3d'
        print *,'jxd=',jxd
        print *,'jx=',jx
        stop
      endif
      if(kxd.ne.kx)then
        print *,'!!!!!!!! WARNING !!!!!!!!!'
        print *,'kxd not equal to kx in subroutine fillout_3d'
        print *,'kxd=',kxd
        print *,'kx=',kx
        stop
      endif
c
c copy dum3 to arcmip_3d
c
      do i=1,ixd
        do j=1,jxd
          do k=1,kxd
            arcmip_3d(i,j,k,hour_cnt)=dum3(i,j,k)
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
c
c fill outer row and column to arcmip_2d
c
      do i=1,ixd
        do k=1,kxd
          arcmip_3d(i,jx,k,hour_cnt)=dum3(i,jxd,k)
        enddo ! end k loop
      enddo ! end i loop
      do j=1,jx
        do k=1,kxd
          arcmip_3d(ix,j,k,hour_cnt)=dum3(ixd,j,k)
        enddo ! end k loop
      enddo ! end j loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine read_lsm(yymmdd,runno,n_hours)
c
c subroutine to read ARCSyM lsm.*.hdf file
c
      implicit none
#include <domain_size.h>
#include <lsm_rad_var.cb>
      character *6 yymmdd
      character *4 runno
      integer n_hours
      integer hour_cnt
      real *4 dum2(ixl,jxl),dum3(ixl,jxl,msl),
     +        dum3a(ixl,jxl,kx),dum3b(ixl,jxl,kx+1)
      character *40 filename
      character *70 label
      character *8 cdate
c
c begin subroutine
c
      filename=runno//'.lsm'//yymmdd//'.hdf'
      print *,'Reading file: ',filename
c
c Read variables from file
c
      do hour_cnt=1,n_hours ! loop over all output times in daily file
        call rdhdf(filename,dum2,ixl,jxl,label) ! shflx
        cdate=label(35:42)
        print *,'  Reading data for time: ',cdate
        call rdhdf(filename,dum2,ixl,jxl,label) ! lhflx
        call rdhdf(filename,dum2,ixl,jxl,label) ! qflx
        call rdhdf(filename,dum2,ixl,jxl,label) ! taux
        call rdhdf(filename,dum2,ixl,jxl,label) ! tauy
        call rdhdf(filename,dum2,ixl,jxl,label) ! ts
        call rdhdf(filename,dum2,ixl,jxl,label) ! tref
        call rdhdf(filename,dum2,ixl,jxl,label) ! asdir
        call rdhdf(filename,dum2,ixl,jxl,label) ! asdif
        call rdhdf(filename,dum2,ixl,jxl,label) ! aldir
        call rdhdf(filename,dum2,ixl,jxl,label) ! aldif
        call rdhdf(filename,dum2,ixl,jxl,label) ! tam
        call rdhdf(filename,dum2,ixl,jxl,label) ! tah
        call rdhdf(filename,dum2,ixl,jxl,label) ! tv
        call rdhdf(filename,dum2,ixl,jxl,label) ! tg
        call rdhdf(filename,dum2,ixl,jxl,label) ! tsoi3
        call rdhdf(filename,dum2,ixl,jxl,label) ! tsoi4
        call rdhdf(filename,dum2,ixl,jxl,label) ! fsa
        call rdhdf(filename,dum2,ixl,jxl,label) ! fsr
        call rdhdf(filename,dum2,ixl,jxl,label) ! ndvi
        call rdhdf(filename,dum2,ixl,jxl,label) ! fctr
        call rdhdf(filename,dum2,ixl,jxl,label) ! fcev
        call rdhdf(filename,dum2,ixl,jxl,label) ! fgev
        call rdhdf(filename,dum2,ixl,jxl,label) ! fmicr
        call rdhdf(filename,dum2,ixl,jxl,label) ! fpsn
        call rdhdf(filename,dum2,ixl,jxl,label) ! frm
        call rdhdf(filename,dum2,ixl,jxl,label) ! frmf
        call rdhdf(filename,dum2,ixl,jxl,label) ! fco2
        call rdhdf(filename,dum2,ixl,jxl,label) ! dmi
        call rdhdf(filename,dum2,ixl,jxl,label) ! btran
        call rdhdf(filename,dum2,ixl,jxl,label) ! bevap
        call rdhdf(filename,dum2,ixl,jxl,label) ! qinfl
        call rdhdf(filename,dum2,ixl,jxl,label) ! qover
        call rdhdf(filename,dum2,ixl,jxl,label) ! qdrai
        call rdhdf(filename,dum2,ixl,jxl,label) ! rsw
        call rdhdf(filename,dum2,ixl,jxl,label) ! h2osno
        call rdhdf(filename,dum2,ixl,jxl,label) ! moz
        call rdhdf(filename,dum2,ixl,jxl,label) ! eah
        call rdhdf(filename,dum2,ixl,jxl,label) ! soot
        call rdhdf(filename,dum2,ixl,jxl,label) ! h2ocan
        call r3dhdf(filename,dum3,ixl,jxl,msl,label) ! tsoi
        call calc_t1msoil(ixl,jxl,msl,dum3,ix,jx,mt,
     +                    t1m_soil_lsm,hour_cnt)
        call r3dhdf(filename,dum3,ixl,jxl,msl,label) ! h2osoi
        call calc_soilwat(ixl,jxl,msl,dum3,ix,jx,mt,
     +                    soil_wat_lsm,hour_cnt)
        call r3dhdf(filename,dum3a,ixl,jxl,kx,label) ! heatrt
        call r3dhdf(filename,dum3b,ixl,jxl,kx+1,label) ! o3prof
      enddo ! end of hour_cnt loop
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_soilwat(ixl,jxl,msl,h2osoi,ix,jx,mt,
     +                        soil_wat_lsm,hour_cnt)
c
c Subroutine to calculate average volumetric soil water
c content from surface to depth of 1 m  from 3d LSM variable h2osoi
c
      implicit none
      integer ixl,jxl,msl
      real *4 h2osoi(ixl,jxl,msl)
      integer ix,jx,mt
      real *4 soil_wat_lsm(ix,jx,mt)
      integer hour_cnt
      integer i,j
c
c begin subroutine
c
c LSM soil depths
c
c  zbot - depth at bottom of each LSM soil layer
c
c  ------------------
c  |  m  | zbot (m) |
c  |  1  |   0.1    |
c  |  2  |   0.3    |
c  |  3  |   0.7    |
c  |  4  |   1.5    |
c  |  5  |   3.1    |
c  |  6  |   6.3    |
c  ------------------
c
c average h2osoi from surface to a depth of 1 m
c
      do i=1,ixl
        do j=1,jxl
          soil_wat_lsm(i,j,hour_cnt)=(h2osoi(i,j,1)*(0.1-0.0)+
     +                                h2osoi(i,j,2)*(0.3-0.1)+
     +                                h2osoi(i,j,3)*(0.7-0.3)+
     +                                h2osoi(i,j,4)*(1.0-0.7))/
     +                                (1.0-0.0)
        enddo ! end j loop
      enddo ! end i loop
c
c fill outer row and column of soil_wat_lsm
c
      do i=1,ixl
        soil_wat_lsm(i,jx,hour_cnt)=soil_wat_lsm(i,jxl,hour_cnt)
      enddo
      do j=1,jx
        soil_wat_lsm(ix,j,hour_cnt)=soil_wat_lsm(ixl,j,hour_cnt)
      enddo
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_t1msoil(ixl,jxl,msl,dum3,ix,jx,mt,
     +                        t1m_soil_lsm,hour_cnt)
c
c Subroutine to calculate 1m soil temperature from 
c 3d LSM variable tsoi
c
      implicit none
      integer ixl,jxl,msl
      real *4 dum3(ixl,jxl,msl)
      integer ix,jx,mt
      real *4 t1m_soil_lsm(ix,jx,mt)
      integer hour_cnt
      integer i,j
      real dz,dz1,tsoi3,tsoi4,dtsoi
c
c begin subroutine
c
c LSM soil depths
c
c  ------------------
c  |  m  | zsoi (m) |
c  |  1  |   0.05   |
c  |  2  |   0.2    |
c  |  3  |   0.5    |
c  |  4  |   1.1    |
c  |  5  |   2.3    |
c  |  6  |   4.7    |
c  ------------------
c
c interpolate from zsoi to 1 m soil depth
c
      dz=0.5-1.1 ! depth between soil layers 3 and 4
      dz1=0.5-1. ! depth between 1 m and soil layer 3
      do i=1,ixl
        do j=1,jxl
          tsoi3=dum3(i,j,3)
          tsoi4=dum3(i,j,4)
          dtsoi=tsoi3-tsoi4 ! difference in tsoi between layers 3 and 4
          t1m_soil_lsm(i,j,hour_cnt)=tsoi3-(dtsoi/dz)*dz1
        enddo
      enddo
c
c fill outer row and column of t1m_soil_lsm
c
      do i=1,ixl
        t1m_soil_lsm(i,jx,hour_cnt)=t1m_soil_lsm(i,jxl,hour_cnt)
      enddo
      do j=1,jx
        t1m_soil_lsm(ix,j,hour_cnt)=t1m_soil_lsm(ixl,j,hour_cnt)
      enddo
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine read_atmos(yymmdd,runno,first_time)
c
c subroutine to read ARCSyM atmos.*.hdf file
c Note: 3d variables are still on ARCSyM sigma levels in this sub
c
      implicit none
#include <domain_size.h>
#include <arcmip_2d.cb>
#include <arcmip_sig_3d.cb>
      character *6 yymmdd
      character *4 runno
      logical first_time
      real *4 dum2(ix,jx),dum3(ix,jx,kx)
      character *40 filename
      character *70 label
      character *8 cdate
      real *4 old_rainc(ix,jx),old_rainnc(ix,jx)
c
c begin subroutine
c
      filename=runno//'.atmos'//yymmdd//'.hdf'
      print *,'Reading file: ',filename
c
c Read variables from file
c
      call r3dhdf(filename,u_mod_sig,ix,jx,kx,label)
      cdate=label(35:42)
      print *,'  Reading data for time: ',cdate
      call r3dhdf(filename,dum3,ix,jx,kx,label)
      call flipv(ix,jx,kx,dum3,v_mod_sig)
      call rotate_wind(u_mod_sig,v_mod_sig,u_sig,v_sig)
      call r3dhdf(filename,t_sig,ix,jx,kx,label)
      call r3dhdf(filename,qv_sig,ix,jx,kx,label)
      call r3dhdf(filename,dum3,ix,jx,kx,label)
      call cld_ice(ix,jx,kx,dum3,cld_sig,ice_sig,t_sig)
      call r3dhdf(filename,dum3,ix,jx,kx,label)
      call r3dhdf(filename,w_sig,ix,jx,kx,label)
      call rdhdf(filename,psfc,ix,jx,label)
      call rdhdf(filename,tsfc,ix,jx,label)
      call store_precip(ix,jx,rainc,rainnc,
     +                  old_rainc,old_rainnc)
      call rdhdf(filename,rainc,ix,jx,label)
      call rdhdf(filename,rainnc,ix,jx,label)
      call calc_precip(ix,jx,kx,rainc,rainnc,
     +                 old_rainc,old_rainnc,
     +                 precip,rain,snow,t_sig,
     +                 first_time)
      call rdhdf(filename,dum2,ix,jx,label)
      call rdhdf(filename,shf,ix,jx,label)
      call rdhdf(filename,lhf,ix,jx,label)
      call conv_lhf(ix,jx,lhf)
      call rdhdf(filename,swe,ix,jx,label)
      call rdhdf(filename,ust,ix,jx,label)
      call rdhdf(filename,dum2,ix,jx,label)
      call rdhdf(filename,dum2,ix,jx,label)
      call rdhdf(filename,t2m,ix,jx,label)
      call rdhdf(filename,t2m_max,ix,jx,label)
      call rdhdf(filename,t2m_min,ix,jx,label)
      call rdhdf(filename,swd_sfc,ix,jx,label)
      call rdhdf(filename,swu_sfc,ix,jx,label)
      call rdhdf(filename,lwd_sfc,ix,jx,label)
      call rdhdf(filename,lwu_sfc,ix,jx,label)
      call rdhdf(filename,swd_toa,ix,jx,label)
      call rdhdf(filename,swu_toa,ix,jx,label)
      call rdhdf(filename,lwu_toa,ix,jx,label)
      if(cdate(7:8).eq.'00')then
c-------need to read previous time step values for 00 UTC output
        call r3dhdf(filename,dum3,ix,jx,kx,label)
        call r3dhdf(filename,dum3,ix,jx,kx,label)
        call r3dhdf(filename,dum3,ix,jx,kx,label)
        call r3dhdf(filename,dum3,ix,jx,kx,label)
        call r3dhdf(filename,dum3,ix,jx,kx,label)
        call r3dhdf(filename,dum3,ix,jx,kx,label)
        call rdhdf(filename,dum2,ix,jx,label)
        call rdhdf(filename,dum2,ix,jx,label)
      endif
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine flipv(ix,jx,kx,v_in,v_out)
c
c flip sign of v component of wind for use
c in transform and hdf files
c
      implicit none
      integer ix,jx,kx
      real *4 v_in(ix,jx,kx),v_out(ix,jx,kx)
      integer i,j,k
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          do k=1,kx
            v_out(i,j,k)=-1.*v_in(i,j,k)
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine conv_lhf(ix,jx,lhf,tsfc)
c
c convert units of lhf to W/m^2
c
      implicit none
      integer ix,jx
      real *4 lhf(ix,jx),tsfc(ix,jx)
      integer i,j
      real lh_vap,lh_subl
      parameter (lh_vap=2.500e6, lh_subl=2.834e6)
      real t_melt
      parameter (t_melt=273.15)
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          if(tsfc(i,j).lt.t_melt)then
c---------- ice surface
            lhf(i,j)=lhf(i,j)*lh_subl
          else
c---------- liquid water at surface
            lhf(i,j)=lhf(i,j)*lh_vap
          endif
        enddo ! end j loop
      enddo ! end i loop
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine store_precip(ix,jx,rainc,rainnc,
     +                        old_rainc,old_rainnc)
c
c store previous value of model accumulated rainc and rainnc for
c additional precip calculations in subroutine calc_precip
c
      implicit none
      integer ix,jx
      real*4 rainc(ix,jx),rainnc(ix,jx),
     +       old_rainc(ix,jx),old_rainnc(ix,jx)
      integer i,j
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          old_rainc(i,j)=rainc(i,j)
          old_rainnc(i,j)=rainnc(i,j)
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine calc_precip(ix,jx,kx,rainc,rainnc,
     +                       old_rainc,old_rainnc,
     +                       precip,rain,snow,t_sig,
     +                       first_time)
c
c determine if precipitation is rain or snow and
c return accumulated precip totals since last model output time
c
      implicit none
      integer ix,jx,kx
      real *4 rainc(ix,jx),rainnc(ix,jx),
     +        old_rainc(ix,jx),old_rainnc(ix,jx),
     +        precip(ix,jx),rain(ix,jx),
     +        snow(ix,jx),t_sig(ix,jx,kx)
      logical first_time
      integer i,j
      real t_thresh ! temperature threshold for rain or snow
      parameter (t_thresh=271.15)
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          if(first_time)then
            precip(i,j)=0.
          else
            precip(i,j)=(rainc(i,j)+rainnc(i,j))-
     +         (old_rainc(i,j)+old_rainnc(i,j))
          endif
          if(t_sig(i,j,kx).lt.t_thresh)then
c---------- precipiation is falling as snow
            snow(i,j)=precip(i,j)
            rain(i,j)=0.
          else
c---------- precipitation is falling as rain
            snow(i,j)=0.
            rain(i,j)=precip(i,j)
          endif
        enddo ! end j loop
      enddo ! end i loop
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine cld_ice(ix,jx,kx,qca,cld_sig,ice_sig,t_sig)
c
c subroutine to partition cloud water into ice and liquid parts
c
      implicit none
      integer ix,jx,kx
      real *4 qca(ix,jx,kx),cld_sig(ix,jx,kx),
     +        ice_sig(ix,jx,kx),t_sig(ix,jx,kx)
      real tfrz
      parameter (tfrz=258.16) ! liquid / ice cloud threshold 
                              ! from ARCSyM subroutine param
      integer i,j,k
c
c begin subroutine
c
c Assign values to cld_sig and ice_sig based on t_sig
c
      do i=1,ix
        do j=1,jx
          do k=1,kx
            if(t_sig(i,j,k).lt.tfrz)then
c------------ ice phase cloud
              ice_sig(i,j,k)=qca(i,j,k)
              cld_sig(i,j,k)=0.
            else
c------------ liquid phase cloud
              cld_sig(i,j,k)=qca(i,j,k)
              ice_sig(i,j,k)=0.
            endif
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine rotate_wind(u_mod_sig,v_mod_sig,u_sig,v_sig)
c
c subroutine to rotate wind components from model grid parallel
c to true e/w and n/s parallel components
c
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
      real *4 u_mod_sig(ix,jx,kx),v_mod_sig(ix,jx,kx),
     +        u_sig(ix,jx,kx),v_sig(ix,jx,kx)
      integer i,j,k
c      real spd,dir
      real theta
c
c begin subroutine
c
      do i=1,ix
        do j=1,jx
          do k=1,kx
c
cJJC - rotate wind components based on method shown in 
cJJC   notes on 10 Sept 2002
c
c (calculation based on that used in mm5read.v3.f program)
c calculate model grid relative wind speed and direction
c
c            spd=sqrt(u_mod_sig(i,j,k)*u_mod_sig(i,j,k)+
c     +               v_mod_sig(i,j,k)*v_mod_sig(i,j,k))
c            dir=(180./3.14159)*
c     +          atan2(u_mod_sig(i,j,k),v_mod_sig(i,j,k))+180.
cc convert model relative wind direction into true wind direction
c            dir=dir+(dlon(i,j)-clon)
c            if(dir.lt.0.0)then
c              dir=dir+360.
c            endif
c            if(dir.gt.360.0)then
c              dir=dir-360
c            endif
cc calculate wind components relative to true e/w and n/s direction
c            u_sig(i,j,k)=-1.*spd*sin((3.14159/180.)*dir)
c            v_sig(i,j,k)=-1.*spd*cos((3.14159/180.)*dir)
c
c calculate true wind components based on notes (10 sept 2002)
c
            theta=(clon-dlon(i,j))*(3.14159/180.)
            u_sig(i,j,k)=u_mod_sig(i,j,k)*cos(theta)-
     +                   v_mod_sig(i,j,k)*sin(theta)
            v_sig(i,j,k)=u_mod_sig(i,j,k)*sin(theta)+
     +                   v_mod_sig(i,j,k)*cos(theta)
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine init_mon
c
c Subroutine to zero monthly average variables
c
      implicit none
#include <domain_size.h>
#include <arcmip_mon_2d.cb>
#include <arcmip_mon_3d.cb>
      integer i,j,k
c
c start subroutine
c
      do i=1,ix
        do j=1,jx
          psfc_mon(i,j)=0.
          tsfc_mon(i,j)=0.
          t2m_mon(i,j)=0.
          t2m_min_mon(i,j)=1000.
          t2m_max_mon(i,j)=0.
          t1m_soil_mon(i,j)=0.
          soil_wat_mon(i,j)=0.
          rainc_mon(i,j)=0.
          rainnc_mon(i,j)=0.
          precip_mon(i,j)=0.
          rain_mon(i,j)=0.
          snow_mon(i,j)=0.
          swe_mon(i,j)=0.
          tot_cld_frac_mon(i,j)=0.
          cld_base_mon(i,j)=0.
          cld_base_cnt(i,j)=0
          lhf_mon(i,j)=0.
          shf_mon(i,j)=0.
          ust_mon(i,j)=0.
          swd_sfc_mon(i,j)=0.
          swu_sfc_mon(i,j)=0.
          lwd_sfc_mon(i,j)=0.
          lwu_sfc_mon(i,j)=0.
          swd_toa_mon(i,j)=0.
          swu_toa_mon(i,j)=0.
          lwu_toa_mon(i,j)=0.
          do k=1,nprs
            u_mod_mon(i,j,k)=0.
            v_mod_mon(i,j,k)=0.
            u_mon(i,j,k)=0.
            v_mon(i,j,k)=0.
            w_mon(i,j,k)=0.
            t_mon(i,j,k)=0.
            z_mon(i,j,k)=0.
            rh_mon(i,j,k)=0.
            qv_mon(i,j,k)=0.
            spechum_mon(i,j,k)=0.
            cld_mon(i,j,k)=0.
            ice_mon(i,j,k)=0.
            cld_frac_mon(i,j,k)=0.
          enddo ! end k loop
        enddo ! end j loop
      enddo ! end i loop
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine get_yymmdd(yymm,day,yymmdd)
c
c subroutine to get 6 digit date (yymmdd) for
c file to be processed
c
      implicit none
      character *4 yymm
      integer day
      character *6 yymmdd
      character *2 dd
c
c Begin subroutine
c
      if(day.lt.10)then
        write(dd,101) day
      else
        write(dd,102) day
      endif
      yymmdd=yymm//dd
  101 format('0',i1)
  102 format(i2)
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine get_monthinfo(yymm,start_day,end_day)
c Get information about month to be processed
      implicit none
      character *4 yymm
      integer start_day,end_day
      integer month,num_of_days(12)
      data num_of_days /31,28,31,30,31,30,
     +                  31,31,30,31,30,31/
c
c Begin subroutine
c
      print *,'Enter the year and month (yymm) of the',
     +        ' files to be processed'
      read(*,101) yymm
  101 format(a4)
c
c find number of days in month
c
      read(yymm(3:4),'(i2)') month
      start_day=1
      end_day=num_of_days(month)
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine get_timeinvar
c Calculate time invariant variables
c   - full sigma level values (sigma)
c   - latitude at cross point (xlat)
c   - longitude at cross point (xlon)
c   - latitude at dot point (dlat)
c   - longitude at dot point (dlon)
c   - surface elevation (zs)
c   - out_freq
c   - runno
c Will read constantfields.input.hdf file and calculate
c additional variables as needed
      implicit none
#include <domain_size.h>
#include <domain_var.cb>
      character *40 fname
      parameter (fname='constantfields.input.hdf')
      character *40 label
      real *4 dum2(ix,jx)
      integer k
c
c start subroutine
c
c Read constantfields.input.hdf file to get xlat, xlon, and zs
c
      call rdhdf(fname,dum2,ix,jx,label) ! Coriolis parameter
      call rdhdf(fname,dum2,ix,jx,label) ! Map factor (cross-points)
      call rdhdf(fname,dum2,ix,jx,label) ! Map factor (dot-points)
      call rdhdf(fname,xlat,ix,jx,label) ! Latitude (cross-points)
      call rdhdf(fname,xlon,ix,jx,label) ! Longitude (cross-points)
      call rdhdf(fname,dlat,ix,jx,label) ! Latitude (dot-points)
      call rdhdf(fname,dlon,ix,jx,label) ! Longitude (dot-points)
      call rdhdf(fname,zs  ,ix,jx,label) ! Topography
      call rdhdf(fname,dum2,ix,jx,label) ! Land use
c
c Calculate value of sigma on at full levels
c
      do k=1,kx
        sigma(k)=(sigmah(k)+sigmah(k+1))*0.5
      enddo
c
c set out_freq and runno
c
#include <set_param.h>
c
c end of subroutine
c
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine r3dhdf(filename,data,nx,ny,nz,label)

      character*(*) filename,label
      integer nx,ny,nz
      real*4 data(nx,ny,nz)

      integer rank,maxrank,ierr,mx,my,mz
      parameter (maxrank=3)
      integer*4 dimsizes(maxrank)
      character*5  unit,format
      character*11 coordsys


      integer dsgdims,dsgdata,dsgdast

      ierr = dsgdims(filename, rank, dimsizes, maxrank)

      if (ierr.ne.0) then
         print *,"Problem getting dimensions of file:"
         print *,nx,ny,nz
         print '(a70)',filename
         stop
      endif

      mx = dimsizes(1)
      my = dimsizes(2)
      mz = dimsizes(3)

      if ((nx.ne.mx).or.(ny.ne.my).or.(nz.ne.mz)) then
         print *,nx,ny,nz,mx,my,mz
         STOP 'Dimensions not the same as HDF file.'
      endif

      ierr = dsgdast(label,unit,format,coordsys)
      if (ierr.ne.0) STOP 'DFSDgetdatastrs error'

      ierr = dsgdata(filename, rank, dimsizes, data)
      if (ierr.ne.0) STOP 'DFSDgetdata error'

      return
      end
c
c-----------------------------------------------------------
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
c-----------------------------------------------------------
c
      subroutine m3dhdf(fname,field,ix,iy,iz,label)
      character *40 fname
      character *70 label
      character*70 filenam
      integer ix,iy,iz
      real*4 field(ix,iy,iz)

      integer rank,iret
      integer*4 shape(3)
      integer dssdims,dssdast,dsadata

c
c  define the size and dimensions of the hdf data set
c
      rank = 3

      shape(1) = ix
      shape(2) = iy
      shape(3) = iz
      filenam = fname
c
c  make the calls necessary to create an hdf dataset
c
      iret = dssdims(rank,shape)
      if (iret.ne.0) stop'dssdims problem'
      iret = dssdast(label,'Units','E15.9','Polar Ster.')
      if (iret.ne.0) stop'dssdast problem'
      iret = dsadata(filenam,rank,shape,field)
      if (iret.ne.0) stop'dsadata problem'

      return
      end
c
c-----------------------------------------------------------
c
      subroutine mkhdf(fname,field,ix,iy,label)
      character*(*) fname,label
      character*70 filenam
      integer ix,iy
      real*4 field(ix,iy)

      integer rank,iret
      integer*4 shape(2)
      integer dssdims,dssdast,dsadata

c
c  define the size and dimensions of the hdf data set
c
      rank = 2
      shape(1) = ix
      shape(2) = iy
      filenam = fname
c
c  make the calls necessary to create an hdf dataset
c
      iret = dssdims(rank,shape)
      if (iret.ne.0) stop'dssdims problems'
      iret = dssdast(label,'Units','E15.9','Polar Ster.')
      if (iret.ne.0) stop'dssdast problems'
      iret = dsadata(filenam,rank,shape,field)
      if (iret.ne.0) stop'dsadata problems'

      return
      end
c
c-----------------------------------------------------------
c
