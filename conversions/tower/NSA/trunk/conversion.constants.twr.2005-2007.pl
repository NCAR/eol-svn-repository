#!/usr/bin/perl

use strict;
use Time::gmtime;

#---------------------------
# region (NSA, TWP...etc)
#---------------------------
my $region = "NSA";

#---------------------------
# station (C1, C2, C3..etc)
#---------------------------
my %station = (
  'C1' => "C1_Barrow",
  'C2' => "C2_Atqasuk"
);

#---------------------------
# time intervals 
#---------------------------
my %time_interval = (
  'input' => 1,		# 60 second data input
  'output' => 30 	# 30 minute output 
);

# need this # so that we know
# when we have missing data
my $num_data_points = 1440;

#---------------------------
# the filename pattern so that we
# can pull out metadata from
# the filename
#---------------------------
my $filename_pattern = 'nsa(\w+)([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})';

#---------------------------
# the parameter names for each network and heights
#---------------------------
my (%mettwr2h_fields, %mettwr4h_fields, %twr_fields, @category, $category_name);
# mettwr2h variable names where the value of the 
# array is a reference to a hash
# note: the category array is a list of categories.
# This is so we can access the fields in the order
# that they are inserted into the hash.  Therefore,
# the 'category' key of the hash must match the
# value in the category array
$mettwr2h_fields{'time_offset'} = 
  { 'station'=>'C2', 
    'height'=>-1, 
    'category'=>'time_offset',
    'data_format'=>'null'
  };
$mettwr2h_fields{'cse_id'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'cse_id',
    'data_format'=>'%-10s'
  };
$mettwr2h_fields{'site_id'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'site_id',
    'data_format'=>'%-15s'
  };
$mettwr2h_fields{'station_id'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'station_id',
    'data_format'=>'%-15s'
  };
$mettwr2h_fields{'snsor_ht'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'snsor_ht',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'lat'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'lat',
    'data_format'=>'%10.5f'
  };
$mettwr2h_fields{'lon'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'lon',
    'data_format'=>'%11.5f'
  };
$mettwr2h_fields{'alt'} =
  { 'station'=>'C2',
    'height'=>-1,
    'category'=>'alt',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'AtmPress'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'stn_pres',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_AtmPress'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'qc_stn_pres',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'WinSpeed_U_WVT'} =
  { 'station'=>'C2',
    'height'=>10, 
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_WinSpeed_U_WVT'} =
  { 'station'=>'C2',
    'height'=>10, 
    'category'=>'qc_wind_spd',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'WinDir_DU_WVT'} =
  { 'station'=>'C2',
    'height'=>10, 
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_WinDir_DU_WVT'} =
  { 'station'=>'C2',
    'height'=>10, 
    'category'=>'qc_wind_dir',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'T5m_AVG'} =
  { 'station'=>'C2',
    'height'=>5, 
    'category'=>'temp_air', 
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_T5m_AVG'} =
  { 'station'=>'C2',
    'height'=>5, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'T2m_AVG'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_T2m_AVG'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'RH2m_AVG'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_RH2m_AVG'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'RH5m_AVG'} =
  { 'station'=>'C2',
    'height'=>5, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_RH5m_AVG'} =
  { 'station'=>'C2',
    'height'=>5, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'DP2m_AVG'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_DP2m_AVG'} =
  { 'station'=>'C2',
    'height'=>2, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };
$mettwr2h_fields{'DP5m_AVG'} =
  { 'station'=>'C2',
    'height'=>5, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$mettwr2h_fields{'qc_DP5m_AVG'} =
  { 'station'=>'C2',
    'height'=>5, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };

# mettwr4h variable names where the value of the 
# array is a reference to a hash
$mettwr4h_fields{'time_offset'} = 
  { 'station'=>'C1', 
    'height'=>-1, 
    'category'=>'time_offset',
    'data_format'=>'null'
  };
$mettwr4h_fields{'cse_id'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'cse_id',
    'data_format'=>'%-10s'
  };
$mettwr4h_fields{'site_id'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'site_id',
    'data_format'=>'%-15s'
  };
$mettwr4h_fields{'station_id'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'station_id',
    'data_format'=>'%-15s'
  };
$mettwr4h_fields{'snsor_ht'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'snsor_ht',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'lat'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'lat',
    'data_format'=>'%10.5f'
  };
$mettwr4h_fields{'lon'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'lon',
    'data_format'=>'%11.5f'
  };
$mettwr4h_fields{'alt'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'alt',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'AtmPress'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'stn_pres',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_AtmPress'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'qc_stn_pres',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WS2M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WS2M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'qc_wind_spd',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WS10M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WS10M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'qc_wind_spd',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WS20M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WS20M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'qc_wind_spd',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WS40M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WS40M_U_WVT'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'qc_wind_spd',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WD2M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WD2M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'qc_wind_dir',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WD10M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WD10M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'qc_wind_dir',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WD20M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WD20M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'qc_wind_dir',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'WD40M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_WD40M_DU_WVT'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'qc_wind_dir',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'T2M_AVG'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_T2M_AVG'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'T10M_AVG'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_T10M_AVG'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'T20M_AVG'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_T20M_AVG'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'T40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_T40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'T40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_T40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'RH2M_AVG'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_RH2M_AVG'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'RH10M_AVG'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_RH10M_AVG'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'RH20M_AVG'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_RH20M_AVG'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'RH40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_RH40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'DP2M_AVG'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_DP2M_AVG'} =
  { 'station'=>'C1',
    'height'=>2, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'DP10M_AVG'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_DP10M_AVG'} =
  { 'station'=>'C1',
    'height'=>10, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'DP20M_AVG'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_DP20M_AVG'} =
  { 'station'=>'C1',
    'height'=>20, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };
$mettwr4h_fields{'DP40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$mettwr4h_fields{'qc_DP40M_AVG'} =
  { 'station'=>'C1',
    'height'=>40, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };

$twr_fields{'time_offset'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'time_offset',
    'data_format'=>'null'
  };
$twr_fields{'cse_id'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'cse_id',
    'data_format'=>'%-10s'
  };
$twr_fields{'site_id'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'site_id',
    'data_format'=>'%-15s'
  };
$twr_fields{'station_id'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'station_id',
    'data_format'=>'%-15s'
  };
$twr_fields{'lat'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'lat',
    'data_format'=>'%10.5f'
  };
$twr_fields{'lon'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'lon',
    'data_format'=>'%11.5f'
  };
$twr_fields{'alt'} =
  { 'station'=>['C1','C2'],
    'height'=>-1,
    'category'=>'alt',
    'data_format'=>'%7.2f'
  };
$twr_fields{'temp_mean'} =
  { 'station'=>['C1','C2'],
    'height'=>[[2,10,20,40],[2,5]],
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$twr_fields{'qc_temp_mean'} =
  { 'station'=>['C1','C2'],
    'height'=>[[2,10,20,40],[2,5]],
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$twr_fields{'rh_mean'} =
  { 'station'=>['C1','C2'],
    'height'=>[[2,10,20,40],[2,5]],
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$twr_fields{'qc_rh_mean'} =
  { 'station'=>['C1','C2'],
    'height'=>[[2,10,20,40],[2,5]],
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$twr_fields{'dew_point_mean'} =
  { 'station'=>['C1','C2'],
    'height'=>[[2,10,20,40],[2,5]],
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$twr_fields{'qc_dew_point_mean'} =
  { 'station'=>['C1','C2'],
    'height'=>[[2,10,20,40],[2,5]],
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$twr_fields{'wdir_vec_mean'} =
  { 'station'=>['C1'],
    'height'=>[[2,10,20,40]],
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$twr_fields{'qc_wdir_vec_mean'} =
  { 'station'=>['C1'],
    'height'=>[[2,10,20,40]],
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$twr_fields{'wspd_vec_mean'} =
  { 'station'=>['C1'],
    'height'=>[[2,10,20,40]],
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$twr_fields{'qc_wspd_vec_mean'} =
  { 'station'=>['C1'],
    'height'=>[[2,10,20,40]],
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };

my @header;
push(@header, {'id'=>'date', 'format'=>'%10s' });
push(@header, {'id'=>'time', 'format'=>'%5s' });
push(@header, {'id'=>'date', 'format'=>'%10s' });
push(@header, {'id'=>'time', 'format'=>'%5s' });
push(@header, {'id'=>'CSE ID', 'format'=>'%10s' });
push(@header, {'id'=>'Site ID', 'format'=>'%15s' });
push(@header, {'id'=>'Station ID', 'format'=>'%15s' });
push(@header, {'id'=>'lat', 'format'=>'%10s' });
push(@header, {'id'=>'lon', 'format'=>'%11s' });
push(@header, {'id'=>'alt', 'format'=>'%7s' });
push(@header, {'id'=>'height', 'format'=>'%7s' });
#push(@header, {'id'=>'stn_pres', 'format'=>'%7s' });
push(@header, {'id'=>'press', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'temp', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'dew_pt', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'rel_hum', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'spc_hum', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'wnd_spd', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'wnd_dir', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'U_wind', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
push(@header, {'id'=>'V_wind', 'format'=>'%7s' });
push(@header, {'id'=>'f', 'format'=>'%s' });
#---------------------------------
# a hash where the key is the station 
# name and the value is a reference to
# a hash containing information regarding
# station id, height, category
#---------------------------------
my %params = (
   "mettwr2h" => \%mettwr2h_fields,
   "mettwr4h" => \%mettwr4h_fields,
   "twr" => \%twr_fields,
);
#---------------------------------
# the directories where the data resides
#---------------------------------
my $base_dir = "/net/work/CEOP/version2/data_processing/other/NSA";
my %data_dir = (
#  'mettwr2h' => "$base_dir/raw/mettwr2h",
#  'mettwr4h' => "$base_dir/raw/mettwr4h",
#  'twr' => "$base_dir/raw/twr"
  'mettwr2h' => "$base_dir/raw/mettwr2h_all",
  'mettwr4h' => "$base_dir/raw/mettwr4h_all",
  'twr' => "$base_dir/raw/twr_all"
);

#---------------------------
# the DQR files
#---------------------------
my %dqr_fname;
#$dqr_fname{'mettwr2h'} = "$base_dir/raw/NSA_METTWR_flagging.txt";
#$dqr_fname{'mettwr4h'} = "$base_dir/raw/NSA_METTWR_flagging.txt";
$dqr_fname{'mettwr2h'} = "$base_dir/raw/NSA_METTWR_flagging_2005_2008.txt";
$dqr_fname{'mettwr4h'} = "$base_dir/raw/NSA_METTWR_flagging_2005_2008.txt";
$dqr_fname{'twr'} = "$base_dir/raw/NSA_TWR_flagging_2008_2009.txt";

#---------------------------
# the project begin and end dates
#---------------------------
my %project_date = (
  'begin' => '2005/01/01',
  'end' => '2007/12/31',
);

#---------------------------
# the output filename
#---------------------------
my $project_name = "CEOP";
my $CSE_id       = "Other";
my $site_id      = "NSA";
# get rid of the / from the project begin/end
# get rid of the / from the project begin/end
my $date_begin = $project_date{'begin'};
my $date_end = $project_date{'end'};
$date_begin =~ s/\///g;
$date_end =~ s/\///g;
my $output_dir = "$base_dir/out/final";
my $output_fname = "$output_dir/$CSE_id" . "_".$site_id . "_" . $site_id . "_" . $date_begin. "_" . $date_end. ".twr";

#-----------------------------------------
# the subroutines that return the constants
#-----------------------------------------
sub region { return $region; }
sub station { return \%station; }
sub time_interval { return \%time_interval; }
sub params { return \%params; }
sub filename_pattern { return $filename_pattern; }
sub data_dirs { return \%data_dir; }
sub project_dates { return \%project_date; }
sub output_fname { return $output_fname; }
sub category { return \@category; }
sub cse_id { return $CSE_id; }
sub site_id { return $site_id; }
sub station { return \%station; }
sub header { return \@header; }
#sub dqr_fname{ return \@dqr_fname; }
sub dqr_fname{ return \%dqr_fname; }
sub num_data_points{ return $num_data_points; }
sub list_of_categories {

  # return a list of categories, which
  # is really just the columns in the
  # data file
  my $ref = \%mettwr2h_fields;
  my ($key,$category, $category_ref);
  foreach $key (keys(%$ref)) {
    $category = $ref->{$key}->{category};
    # do this so we don't end up with
    # duplicates since one category
    # can represent multiple fields
    $category_ref->{$category} = '';
  } # end foreach

  # now, sort the categories
  my @sorted_list;
  foreach $key (sort(keys(%$category_ref))) {
    push(@sorted_list, $key) if ($key !~ /^qc_/);
  } # end foreach
  push(@sorted_list, 'date');
  push(@sorted_list, 'time');

  return \@sorted_list;

}
sub correct_constants {

  # correct the constants (lat, lon, alt..etc)
  my $category = shift;
  my $height = shift;
}
sub apply_corrections {

  my $file = shift;
  my $data_point = shift;
  my $missing_value = shift;	# the missing value
  #my $param_ref = shift;	# reference to a hash containing parameter information
  my $category = shift;		# parameter category
  my $height = shift;

  #----------------------------------------------------- 
  # the correction for the constants..here
  # we return a single value as opposed to
  # a data_point object
  #----------------------------------------------------- 
  if ( !$data_point ) {
    my $parameter = $file->fetch_parameter_by_category($category);
    my $value = $parameter->data_ref->[0];
    if ( $category eq 'lon' && $file->station() eq 'C2' ) {
      $value = -157.4066;
    }
    return $value;
  } # endif
  #----------------------------------------------------- 

  #----------------------------------------------------- 
  # the general variables used for the data point corrections
  #----------------------------------------------------- 
  # fetch the station
  my $station = $file->station();
  # fetch the timestamp for this datapoint
  my $timestamp = $data_point->timestamp();
  # fetch the value for this datapoint
  my $value = $data_point->value();
  # fetch the flag for this datapoint
  my $flag = $data_point->flag()->value();
  #----------------------------------------------------- 

  my ($begin, $end);

  #----------------------------------------------------- 
  # correct the station pressure where the units changed from hPa to kPa
  # 1 kPa = 10 hPa
  # the correction applies for both stations
  #----------------------------------------------------- 
  # the begin & end epoch time for the correction 
  $begin = &convert_to_epoch("2006/07/06 21:00:00");
  $end = &convert_to_epoch("2007/12/31 23:59:59");
  if ( $category eq 'stn_pres' ) {
    if ( $timestamp >= $begin && $timestamp <= $end ) {
      $value *= 10 if ( $value ne $missing_value );
      $data_point->value($value);
    } # endif
  } # endif
  #----------------------------------------------------- 

  #----------------------------------------------------- 
  # change the flag to 'D' for the partial file on 2007/04/30 14:00 C2
  #----------------------------------------------------- 
  $begin = &convert_to_epoch("2007/04/30 14:00:00");
  $end = &convert_to_epoch("2007/04/30 16:00:00");
  if ( $category eq 'wind_spd' && $station eq 'C2' && $height == 10) {
    if ( $timestamp >= $begin && $timestamp <= $end ) {
      if ($value ne $missing_value) {
        $data_point->flag()->value('D');
      } # endif
    } # endif
  } # endif

  #----------------------------------------------------- 
  # change the flag to 'D' where the incorrect parameter name
  # from 2006/11/26 06:08 to 2006/11/27 2226 should be
  # RH2m_AVG and DP2m_AVG (parameter names listed in DQR
  # file as RH2M_AVG and DP2M_AVG for C2 instead of RH2m_AVG
  # and DP2m_AVG)
  #----------------------------------------------------- 
  { # begin block
    $begin = &convert_to_epoch("2006/11/27 06:08:00");
    $end = &convert_to_epoch("2006/11/27 22:26:00");
    my $parameter = $file->fetch_parameter_by_category_and_height($category, $height);
    last if ( !$parameter);
    my $parameter_name = $parameter->name();
#    my $tm = gmtime($timestamp);
#    my $date_time = sprintf("%4d/%02d/%02d %02d:%02d:%02d",
#                            $tm->year+1900,
#                            $tm->mon+1,
#                            $tm->mday,
#                            $tm->hour,
#                            $tm->min,
#  			  $tm->sec);
#    print "date/time is: $date_time and parameter is $parameter_name\n";
    if ( $station eq "C2" && $parameter_name eq "RH2m_AVG") {
      # correct RH2m_AVG if within the specified date/time
      if ( $timestamp >= $begin && $timestamp <= $end ) {
        $data_point->flag()->value('D') if ($value ne $missing_value);
      } # endif
    } # endif
    if ( $station eq "C2" && $parameter_name eq "DP2m_AVG") {
      # correct DP2m_AVG if within the specified date/time
      if ( $timestamp >= $begin && $timestamp <= $end ) {
        $data_point->flag()->value('D') if ($value ne $missing_value);
      } # endif
    } # endif
  } # end block

  return $data_point;

}
#------------------------------------------
# convert a date/time (YYYY/MM/DD hh:mm:ss) to
# seconds since 1/1/1970
#------------------------------------------
sub convert_to_epoch {

  my $date_time = shift;        # YYYY/MM/DD hh:mm:ss

  $date_time =~ /(\d{4})\/(\d{2})\/(\d{2})(\s)(\d{2})\:(\d{2})\:(\d{2})/;
  my $year = int($1);
  $year -= 1900;
  my $month = int($2);
  $month -= 1;
  my $day = int($3);
  my $hour = int($5);
  my $min = int($6);
  my $sec = int($7);

  return timegm($sec, $min, $hour, $day, $month, $year);

}

#sub aaapply_corrections {
#
#  # the corrections listed here are for where there are
#  # errors in the data that need to be corrected
#  my $file = shift;
#  my $convert_to_epoch = shift;
#  my $missing_value = shift;
#  my $station = $file->station();
#  my ($begin, $end);
#
#  print "applying corrections for $station for ".$file->name()."\n";
#
#  my ($parameter, $qc_parameter, $index, @time_offset, @list_of_timestamps);
#  my ($timestamp, $i, $parameter_index, $value, $new_value,$data_point);
#
#  # get a list of timestamps for the parameter 
#  # the timestamp for the file
#  my $file_timestamp = $file->timestamp();
#  my $time_offset_parameter = $file->fetch_parameter_by_name("time_offset");
#  my $list_of_timestamps_ref = $time_offset_parameter->fetch_list_of_values();
#
#  #----------------------------------------------------- 
#  # correct the AtmPress field where it changes from hPa to kPa
#  # 1 kPa = 10 hPa
#  #----------------------------------------------------- 
#  # the begin & end epoch time for the correction 
#  $begin = $convert_to_epoch->("2006/07/06 21:00:00");
#  $end = $convert_to_epoch->("2007/12/31 23:59:59");
#  # the parameter to correct
#  $parameter = $file->fetch_parameter_by_name("AtmPress");
#  # the index in the parameter array for the file
#  $parameter_index = $file->find_index_of_parameter($parameter->name()); 
#  # loop through the data points and apply the correction
#  for ($i=0; $i <= $#$list_of_timestamps_ref; $i++ ) {
#    $timestamp = $list_of_timestamps_ref->[$i] + $file_timestamp;
#    if ( $timestamp >= $begin && $timestamp <= $end ) {
#      # finally, correct the value!!
#      $data_point = $parameter->data_ref()->{$timestamp};
#      $value = $data_point->value();
#      if ( $data_point && ($value ne $missing_value) ) {
#        #if ( $value ne $missing_value ) {
#          $new_value = $value * 10;
#          $data_point->value($new_value) if ( $value != $missing_value );
#          $parameter->add_datapoint($data_point);
#	#} # endif
#      } # endif $data_point 
#    } # endif $timestamp
#  } # end for
#  $file->parameter_ref()->[$parameter_index] = $parameter;
#  #----------------------------------------------------- 
#
#  #----------------------------------------------------- 
#  # change the flag to 'D' for the partial file on 2007/04/30 14:00 C2
#  #----------------------------------------------------- 
#  # the begin & end epoch time for the correction 
#  $begin = $convert_to_epoch->("2007/04/30 14:00:00");
#  $end = $convert_to_epoch->("2007/04/30 16:00:00");
#  for ($i=0; $i <= $#$list_of_timestamps_ref; $i++ ) {
#    $timestamp = $list_of_timestamps_ref->[$i] + $file_timestamp;
#    if ( $timestamp >= $begin && $timestamp <= $end && $station eq 'C2') {
#      $parameter = $file->fetch_parameter_by_name("WinSpeed_U_WVT");
#      $qc_parameter = $file->fetch_parameter_by_name("qc_WinSpeed_U_WVT");
#      $data_point = $parameter->data_ref()->{$timestamp};
#      if ( $data_point ) {
#        $qc_parameter->data_ref()->{$timestamp}->value(2);	# dubious
#      } # endif
#      $parameter->add_datapoint($data_point);
#    } # endif
#  } # end for
#
#  $file->parameter_ref()->[$parameter_index] = $parameter;
#
#  #----------------------------------------------------- 
#  # finally, return the file!!!
#  #----------------------------------------------------- 
#  return $file;
#
#}
sub apply_final_flag {

  # apply the post analysis flags
}
1;
