#---------------------------------------------------------------------
# This will put the new variable names for mettwr2h and mettwr4h
# into the names used in the other networks, and expected by the s/w.
#---------------------------------------------------------------------

perl -p -i.bak -e 's/AtmPress/atmos_pressure/;' mettwr2h/*.dat
perl -p -i -e 's/qc_AtmPress/qc_atmos_pressure/;' mettwr2h/*.dat
perl -p -i -e 's/WinSpeed_U_WVT/wind_spd_mean/;' mettwr2h/*.dat
perl -p -i -e 's/qc_WinSpeed_U_WVT/qc_wind_spd_mean/;' mettwr2h/*.dat
perl -p -i -e 's/WinDir_DU_WVT/wind_dir_vec_avg/;' mettwr2h/*.dat
perl -p -i -e 's/qc_WinDir_DU_WVT/qc_wind_dir_vec_avg/;' mettwr2h/*.dat
perl -p -i -e 's/T2m_AVG/temp_mean/;' mettwr2h/*.dat
perl -p -i -e 's/qc_T2m_AVG/qc_temp_mean/;' mettwr2h/*.dat
perl -p -i -e 's/RH2m_AVG/relh_mean/;' mettwr2h/*.dat
perl -p -i -e 's/qc_RH2m_AVG/qc_relh_mean/;' mettwr2h/*.dat
perl -p -i -e 's/PCPRate/precip_rate/;' mettwr2h/*.dat
perl -p -i -e 's/qc_PCPRate/qc_precip_rate/;' mettwr2h/*.dat
perl -p -i -e 's/CumSnow/snow_depth/;' mettwr2h/*.dat
perl -p -i -e 's/qc_CumSnow/qc_snow_depth/;' mettwr2h/*.dat
perl -p -i -e 's/DP2m_AVG/dew_pt_temp_mean/;' mettwr2h/*.dat

perl -p -i.bak -e 's/AtmPress/atmos_pressure/;' mettwr4h/*.dat
perl -p -i -e 's/qc_AtmPress/qc_atmos_pressure/;' mettwr4h/*.dat
perl -p -i -e 's/WS10M_U_WVT/wind_spd_mean/;' mettwr4h/*.dat
perl -p -i -e 's/qc_WS10M_U_WVT/qc_wind_spd_mean/;' mettwr4h/*.dat
perl -p -i -e 's/WD10M_DU_WVT/wind_dir_vec_avg/;' mettwr4h/*.dat
perl -p -i -e 's/qc_WD10M_DU_WVT/qc_wind_dir_vec_avg/;' mettwr4h/*.dat
perl -p -i -e 's/T2M_AVG/temp_mean/;' mettwr4h/*.dat
perl -p -i -e 's/qc_T2M_AVG/qc_temp_mean/;' mettwr4h/*.dat
perl -p -i -e 's/RH2M_AVG/relh_mean/;' mettwr4h/*.dat
perl -p -i -e 's/qc_RH2M_AVG/qc_relh_mean/;' mettwr4h/*.dat
perl -p -i -e 's/PcpRate/precip_rate/;' mettwr4h/*.dat
perl -p -i -e 's/qc_PcpRate/qc_precip_rate/;' mettwr4h/*.dat
perl -p -i -e 's/CumSnow/snow_depth/;' mettwr4h/*.dat
perl -p -i -e 's/qc_CumSnow/qc_snow_depth/;' mettwr4h/*.dat
perl -p -i -e 's/DP2M_AVG/dew_pt_temp_mean/;' mettwr4h/*.dat

