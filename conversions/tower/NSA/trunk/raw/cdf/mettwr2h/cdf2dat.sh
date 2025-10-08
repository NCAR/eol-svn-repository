#!/bin/tcsh

#-------------------------
# for NSA mettwr2h
#-------------------------

foreach i (nsa*2008*.cdf) 
	../../../nesob_dump -v base_time,time_offset,lat,lon,alt,AtmPress,qc_AtmPress,WinSpeed_U_WVT,qc_WinSpeed_U_WVT,WinDir_DU_WVT,qc_WinDir_DU_WVT,T5m_AVG,qc_T5m_AVG,T2m_AVG,qc_T2m_AVG,RH5m_AVG,qc_RH5m_AVG,RH2m_AVG,qc_RH2m_AVG,DP2m_AVG,qc_DP2m_AVG,DP5m_AVG,qc_DP5m_AVG,PCPRate,qc_PCPRate,CumSnow,qc_CumSnow $i > $i:r.dat
end
