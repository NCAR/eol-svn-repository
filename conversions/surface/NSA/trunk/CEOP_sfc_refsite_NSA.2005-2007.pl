#!/usr/bin/perl -w 

use DQR::Flag;
use DQR::FlagList;
use Time::Local;

#--------------------------------------------------------------------------------------
# CEOP_sfc_refsite_NSA.pl
#
# This s/w is used in converting ARM NSA surface netCDF files into CEOP output.
#
# This Perl script is used in the processing of CEOP ARM surface NSA data.  Input files are
# previously prepared using nesob_dump (nesob_dump is a variation of nc_dump specifially 
# rewritten for extracting variables from a netCDF file) and consist of lines of data 
# matching the parameters needed. 
#
# A script named "get_vars.sh" is run in each directory where the raw data is. It 
# prepares the input files (naming them with a "*.dat" extension) and has a line in it  
# with the variable names to extract. These variables match the "@xxxx_fields" in this s/w. 
# If you change which variables to extract from the netCDF file, you will need to change 
# the "fields" array, as well.
# 
# 21 may 99, ds
# rev 26jul99, ds
#    added bowen, home_15, and home_30 variables to grab from netCDF file
# rev 30jul99, ds #    added section to create 3 station *.out files
# rev 03aug99, ds
#    added checks for AOI and TOI
# rev 09aug99, ds
#    made year 4 digits long in output file
# rev 19aug99, ds
#    all values are multiplied by -1.0
#    so output matches tilden output
# rev 17may00, ds
#    added stn_name hash to put out long name
# rev 12 Dec 02, ds
#    fixed up for use with CEOP data
# rev 10 Jan 03, ds
#    code added to deal with 2D arrays of data, with time and height indexes
# rev 11 Sep 03, ds
#    fixed up the section "divide into arrays by date" so it will work with more than one year
# rev 24 Sep 03,ds
#	 split into sfc and tower conversions 
# rev 14 Nov 03, ds
#    now checking QC fields in netCDF files to set our own flags:
#
#		0 - failed no checks - G
#		1 - missing	- M
#		2 - minimum	- D
#		3 - missing/min	- B
#		4 - maximum - D
#		5 - missing/max - B
#		7 - missing/min/max - B
#		8 - delta - D
#		9 - missing/delta - B
#		10 - min/delta - B
#		11 - missing/min/delta - B
#		12 - max/delta - B
#		14 - min/max/delta - B
#		15 - missing/min/max/delta - B
#
# rev 12 Jan 04, ds
#	 fixed up net rad flag so it took least flag from input to sum
# rev 6 May 04, ds
#    added flagging per DQR reports, per Scot
# rev 12 May 04, ds
#    negative snow depths flagged as "D", per Scot
# rev 11 Jun 04, ds
#   set "_" data values to Missing
#   fixed bug in net rad calc, where "0" values were set to Missing 
# rev 24 Oct 05, ds
#   2 new networks added in 2nd month of TOI, to replace the others
#   all the parameter names changed, and not same from 1 new network to the other
#   snow depths changed from meters to mm
#  
#  fix: get right params from net_CDF files, then rename them to the names we
#       have been using all along, using change_variable_names.pl
# rev 28 Oct 05, ds
#	skin temps change in 11/2004 from Celsius to Kelvin
# rev 25 Jun 08, ss
#	added code to accomodate the different field names
#       for mettwr2h(C2) and mettwr4h(C1) 
#	example: for relative humidity:
#		mettwr4h(C1) = RH2M_AVG
#		mettwr2h(C2) = RH2m_AVG
# rev 02 Jul 08, ss
# 	changed so that dqr flags are read in from
#	dqr file and applied
#--------------------------------------------------------------------------------------

$DEBUG = 1;					
$DEBUG1 = 0;				# for even more messages
$DEBUG2 = 0;				# logs all the snow obs and flags, only

#-----------------------------
# get our subroutines in
# NEW ones pass flags in
#-----------------------------
unshift (@INC, ".");
require ("./bin/calc_dewpoint_NEW.pl");
require ("./bin/calc_specific_humidity_NEW.pl");
require ("./bin/calc_UV_winds_NEW.pl");
require ("./corrections.pl");

#--------------------------------------------------------------------------------------
# These parameters are in each input file.
#       base_time:      the time offset is figured from this
#       lat:            latitude
#       lon:            longitude
#       alt:            elevation
#--------------------------------------------------------------------------------------
# mettwr heights = 40, 20, 10, 2 meters 
# mettiptwr heights = 6, 2 meters 
#--------------------------------------------------------------------------------------
my $numMetHeights = 4;
my $numMettipHeights = 2;
my $numMettwr2hHeights = 2;
my $numMettwr4hHeights = 4;

#--------------------------------------------------------------------------------------
# the parameters we want from each set of files, by platform
#--------------------------------------------------------------------------------------
my (@gndrad_fields, @mettwr2h_fields, @mettwr4h_fields, @skyrad_fields);
my ($U_wind, $V_wind);

#@gndrad_fields = ("time_offset", "up_short_hemisp", "qc_up_short_hemisp", "up_long_hemisp", "qc_up_long_hemisp", "sfc_ir_temp", "qc_sfc_ir_temp", "net", "qc_net");
# gndrad variable names 
push (@gndrad_fields, "time_offset");
push (@gndrad_fields, "up_short_hemisp");
push (@gndrad_fields, "qc_up_short_hemisp");
push (@gndrad_fields, "up_long_hemisp");
push (@gndrad_fields, "qc_up_long_hemisp");
push (@gndrad_fields, "sfc_ir_temp");
push (@gndrad_fields, "qc_sfc_ir_temp");
push (@gndrad_fields, "net");
push (@gndrad_fields, "qc_net");

# mettwr2h variable names
push (@mettwr2h_fields, "time_offset");
push (@mettwr2h_fields, "AtmPress");
push (@mettwr2h_fields, "qc_AtmPress");
push (@mettwr2h_fields, "WinSpeed_U_WVT");
push (@mettwr2h_fields, "qc_WinSpeed_U_WVT");
push (@mettwr2h_fields, "WinDir_DU_WVT");
push (@mettwr2h_fields, "qc_WinDir_DU_WVT");
push (@mettwr2h_fields, "T2m_AVG");
push (@mettwr2h_fields, "qc_T2m_AVG");
push (@mettwr2h_fields, "DP2m_AVG");
push (@mettwr2h_fields, "qc_DP2m_AVG");
push (@mettwr2h_fields, "RH2m_AVG");
push (@mettwr2h_fields, "qc_RH2m_AVG");
push (@mettwr2h_fields, "PCPRate");
push (@mettwr2h_fields, "qc_PCPRate");
push (@mettwr2h_fields, "CumSnow");
push (@mettwr2h_fields, "qc_CumSnow");

# mettwr4h variable names
push (@mettwr4h_fields, "time_offset");
push (@mettwr4h_fields, "AtmPress");
push (@mettwr4h_fields, "qc_AtmPress");
push (@mettwr4h_fields, "WS10M_U_WVT");
push (@mettwr4h_fields, "qc_WS10M_U_WVT");
push (@mettwr4h_fields, "WD10M_DU_WVT");
push (@mettwr4h_fields, "qc_WD10M_DU_WVT");
push (@mettwr4h_fields, "T2M_AVG");
push (@mettwr4h_fields, "qc_T2M_AVG");
push (@mettwr4h_fields, "DP2M_AVG");
push (@mettwr4h_fields, "qc_DP2M_AVG");
push (@mettwr4h_fields, "RH2M_AVG");
push (@mettwr4h_fields, "qc_RH2M_AVG");
push (@mettwr4h_fields, "PcpRate");
push (@mettwr4h_fields, "qc_PcpRate");
push (@mettwr4h_fields, "CumSnow");
push (@mettwr4h_fields, "qc_CumSnow");

# skyrad variable names
push (@skyrad_fields, "time_offset");
push (@skyrad_fields, "down_short_hemisp");
push (@skyrad_fields, "qc_down_short_hemisp");
push (@skyrad_fields, "down_long_hemisp_shaded1");
push (@skyrad_fields, "qc_down_long_hemisp_shaded1");
# 
#@sfc_parameter_list = ("atmos_pressure", "temp_mean", "dew_pt_temp_mean", "relh_mean", "spec_hum", "wind_spd_mean", "wind_dir_vec_avg", "u_wind", "v_wind", "precip_rate", "snow_depth", "down_short_hemisp", "up_short_hemisp", "down_long_hemisp_shaded1", "up_long_hemisp", "net", "sfc_ir_temp", "par_in", "par_out"); 
my @sfc_parameter_list;
push(@sfc_parameter_list, "AtmPress");
push(@sfc_parameter_list, "T2m_AVG");		# temperature mettwr2h
push(@sfc_parameter_list, "T2M_AVG");		# temperature mettwr4h
push(@sfc_parameter_list, "DP2m_AVG");		# dew point mettwr2h
push(@sfc_parameter_list, "DP2M_AVG");		# dew point mettwr4h
push(@sfc_parameter_list, "RH2m_AVG");		# relative humidity mettwr2h
push(@sfc_parameter_list, "RH2M_AVG");		# relative humidity mettwr4h
push(@sfc_parameter_list, "spec_hum");		# specific humidity (calculated)
push(@sfc_parameter_list, "WinSpeed_U_WVT");	# wind speed mettwr2h
push(@sfc_parameter_list, "WS10M_U_WVT");	# wind speed mettwr4h
push(@sfc_parameter_list, "WinDir_DU_WVT");	# wind direction mettwr2h
push(@sfc_parameter_list, "WD10M_DU_WVT");	# wind direction mettwr4h
push(@sfc_parameter_list, "u_wind");		# calculated
push(@sfc_parameter_list, "v_wind");		# calculated
push(@sfc_parameter_list, "PCPRate");		# precip rate mettwr2h
push(@sfc_parameter_list, "PcpRate");		# precip rate mettwr4h
push(@sfc_parameter_list, "CumSnow");
push(@sfc_parameter_list, "down_short_hemisp");		
push(@sfc_parameter_list, "up_short_hemisp");		
push(@sfc_parameter_list, "down_long_hemisp_shaded1");	
push(@sfc_parameter_list, "up_long_hemisp");		
push(@sfc_parameter_list, "net");			
push(@sfc_parameter_list, "sfc_ir_temp");		
push(@sfc_parameter_list, "par_in");			
push(@sfc_parameter_list, "par_out" );			

# the indices for each parameter where the parameter
# names are different for each station
my %param_index;
$param_index{'C1'}{'AtmPress'} = 0;
$param_index{'C2'}{'AtmPress'} = 0;
$param_index{'C1'}{'T2M_AVG'} = 1;
$param_index{'C2'}{'T2m_AVG'} = 1;
$param_index{'C1'}{'DP2M_AVG'} = 2;
$param_index{'C2'}{'DP2m_AVG'} = 2;
$param_index{'C1'}{'RH2M_AVG'} = 3;
$param_index{'C2'}{'RH2m_AVG'} = 3;
$param_index{'C1'}{'spec_hum'} = 4;
$param_index{'C2'}{'spec_hum'} = 4;
$param_index{'C1'}{'WS10M_U_WVT'} = 5;
$param_index{'C2'}{'WinSpeed_U_WVT'} = 5;
$param_index{'C1'}{'WD10M_DU_WVT'} = 6;
$param_index{'C2'}{'WinDir_DU_WVT'} = 6;
$param_index{'C1'}{'u_wind'} = 7;
$param_index{'C2'}{'u_wind'} = 7;
$param_index{'C1'}{'v_wind'} = 8;
$param_index{'C2'}{'v_wind'} = 8;
$param_index{'C1'}{'PcpRate'} = 9;
$param_index{'C2'}{'PCPRate'} = 9;
$param_index{'C1'}{'CumSnow'} = 10;
$param_index{'C2'}{'CumSnow'} = 10;
$param_index{'C1'}{'down_short_hemisp'} = 11;
$param_index{'C2'}{'down_short_hemisp'} = 11;
$param_index{'C1'}{'up_short_hemisp'} = 12;
$param_index{'C2'}{'up_short_hemisp'} = 12;
$param_index{'C1'}{'down_long_hemisp_shaded1'} = 13;
$param_index{'C2'}{'down_long_hemisp_shaded1'} = 13;
$param_index{'C1'}{'up_long_hemisp'} = 14;
$param_index{'C2'}{'up_long_hemisp'} = 14;
$param_index{'C1'}{'net'} = 15;
$param_index{'C2'}{'net'} = 15;
$param_index{'C1'}{'sfc_ir_temp'} = 16;
$param_index{'C2'}{'sfc_ir_temp'} = 16;
$param_index{'C1'}{'par_in'} = 17;
$param_index{'C2'}{'par_in'} = 17;
$param_index{'C1'}{'par_out'} = 18;
$param_index{'C2'}{'par_out'} = 18;

my @sfc_obs;
push(@sfc_obs, "stn_pres");	# column 1
push(@sfc_obs, "temp_air");	# column 2
push(@sfc_obs, "dew_pt");	# column 3
push(@sfc_obs, "rel_hum");	# column 4
push(@sfc_obs, "spec_hum");	# column 5
push(@sfc_obs, "wind_spd");	# column 6
push(@sfc_obs, "wind_dir");	# column 7
push(@sfc_obs, "U_wind");	# column 8
push(@sfc_obs, "V_wind");	# column 9
push(@sfc_obs, "precip");	# column 10
push(@sfc_obs, "snow");		# column 11
push(@sfc_obs, "short_in");	# column 12
push(@sfc_obs, "short_out");	# column 13
push(@sfc_obs, "long_in");	# column 14
push(@sfc_obs, "long_out");	# column 15
push(@sfc_obs, "net_rad");	# column 16
push(@sfc_obs, "skintemp");	# column 17
push(@sfc_obs, "par_in");	# column 18
push(@sfc_obs, "par_out");	# column 19
$sfc_param_count = $#sfc_parameter_list+1;

#@sfc_obs            = (   "stn_pres",     "temp_air",      "dew_pt",       "rel_hum",  "spec_hum",    "wind_spd",       "wind_dir",     "U_wind", "V_wind",    "precip",      "snow",         "short_in",        "short_out",             "long_in",             "long_out", "net_rad", "skintemp",  "par_in", "par_out");

#my @sfc_parameter_list;
#push(@sfc_parameter_list, "atmos_press");		# column 1
#push(@sfc_parameter_list, "temp_mean");			# column 2
#push(@sfc_parameter_list, "dew_pt_temp_mean");		# column 3
#push(@sfc_parameter_list, "relh_mean");			# column 4
#push(@sfc_parameter_list, "spec_hum");			# column 5
#push(@sfc_parameter_list, "wind_spd_mean");		# column 6
#push(@sfc_parameter_list, "wind_dir_vec_avg");		# column 7
#push(@sfc_parameter_list, "u_wind");			# column 8
#push(@sfc_parameter_list, "v_wind");			# column 9
#push(@sfc_parameter_list, "precip_rate");		# column 10
#push(@sfc_parameter_list, "snow_depth");		# column 11
#push(@sfc_parameter_list, "down_short_hemisp");		# column 12
#push(@sfc_parameter_list, "up_short_hemisp");		# column 13
#push(@sfc_parameter_list, "down_long_hemisp_shaded1");	# column 14
#push(@sfc_parameter_list, "up_long_hemisp");		# column 15
#push(@sfc_parameter_list, "net");			# column 16
#push(@sfc_parameter_list, "sfc_ir_temp");		# column 17
#push(@sfc_parameter_list, "par_in");			# column 18
#push(@sfc_parameter_list, "par_out" );			# column 19
#$sfc_param_count = $#sfc_parameter_list+1;

#@gndrad_fields = ("time_offset", "up_short_hemisp", "qc_up_short_hemisp", "up_long_hemisp", "qc_up_long_hemisp", "sfc_ir_temp", "qc_sfc_ir_temp", "net", "qc_net");
#@mettip_fields = ("time_offset", "atmos_pressure", "qc_atmos_pressure", "wind_spd_mean", "qc_wind_spd_mean", "wind_dir_vec_avg", "qc_wind_dir_vec_avg", "temp_mean", "qc_temp_mean", "relh_mean", "qc_relh_mean", "dew_pt_temp_mean", "qc_dew_pt_temp_mean");
#@mettwr_fields = ("time_offset", "atmos_pressure", "qc_atmos_pressure", "wind_spd_mean", "qc_wind_spd_mean", "wind_dir_vec_avg", "qc_wind_dir_vec_avg", "temp_mean", "qc_temp_mean", "relh_mean", "qc_relh_mean", "dew_pt_temp_mean", "qc_dew_pt_temp_mean");
#@mettwr2h_fields = ("time_offset", "atmos_pressure", "qc_atmos_pressure", "wind_spd_mean", "qc_wind_spd_mean", "wind_dir_vec_avg", "qc_wind_dir_vec_avg", "temp_mean", "qc_temp_mean", "relh_mean", "qc_relh_mean", "precip_rate", "qc_precip_rate", "snow_depth", "qc_snow_depth");
#@mettwr4h_fields = ("time_offset", "atmos_pressure", "qc_atmos_pressure", "wind_spd_mean", "qc_wind_spd_mean", "wind_dir_vec_avg", "qc_wind_dir_vec_avg", "temp_mean", "qc_temp_mean", "relh_mean", "qc_relh_mean", "precip_rate", "qc_precip_rate", "snow_depth", "qc_snow_depth");
#@pws_fields    = ("time_offset", "precip_rate", "qc_precip_rate", "cumulative_snow", "qc_cumulative_snow");
#@skyrad_fields = ("time_offset", "down_short_hemisp", "qc_down_short_hemisp", "down_long_hemisp_shaded1", "qc_down_long_hemisp_shaded1");
#@snodep_fields = ("time_offset", "snow_depth", "snow_depth_out_of_range_error");
#--------------------------------------------------------------------------------------
# for future use
# @mettwr2h_fields = ("time_offset", "AtmPress", "qc_AtmPress", "WinSpeed_U_WVT", "qc_WinSpeed_U_WVT", "WinDir_DU_WVT", "qc_WinDir_DU_WVT", "T2m_AVG", "qc_T2m_AVG", "RH2m_AVG", "qc_RH2m_AVG", "PCPRate", "qc_PCPRate", "CumSnow", "qc_CumSnow"););
# @mettwr4h_fields = ("time_offset", "AtmPress", "qc_AtmPress", "WS10M_U_WVT", "qc_WS10M_U_WVT", "WD10M_DU_WVT", "qc_WD10M_DU_WVT", "T2M_AVG", "qc_T2M_AVG", "RH2M_AVG", "qc_RH2M_AVG", "PcpRate", "qc_PcpRate", "CumSnow", "qc_CumSnow");
#--------------------------------------------------------------------------------------

my %params = (
    "gndrad"   => \@gndrad_fields,
    "mettwr2h" => \@mettwr2h_fields,
    "mettwr4h" => \@mettwr4h_fields,
    "skyrad"   => \@skyrad_fields
); 


#--------------------------------------------------------------------------------------
# a list of the parameters we want, in order as printed to the output files
#--------------------------------------------------------------------------------------

# (param num)       =        1                2              3                 4            5             6                 7               8         9           10           11                 12                  13                      14                    15           16        17           18         19    
#@sfc_parameter_list = ("atmos_pressure", "temp_mean", "dew_pt_temp_mean", "relh_mean", "spec_hum", "wind_spd_mean", "wind_dir_vec_avg", "u_wind", "v_wind", "precip_rate", "snow_depth", "down_short_hemisp", "up_short_hemisp", "down_long_hemisp_shaded1", "up_long_hemisp", "net", "sfc_ir_temp", "par_in", "par_out"); 
#@sfc_obs            = (   "stn_pres",     "temp_air",      "dew_pt",       "rel_hum",  "spec_hum",    "wind_spd",       "wind_dir",     "U_wind", "V_wind",    "precip",      "snow",         "short_in",        "short_out",             "long_in",             "long_out", "net_rad", "skintemp",  "par_in", "par_out");
#$sfc_param_count    = 19;


#--------------------------------------------------------------------------------------
# where the data files are for input (previously created from the netCDF files)
#--------------------------------------------------------------------------------------

%dirs = (
    "gndrad" => "raw/gndrad/",
    "mettwr2h" => "raw/mettwr2h/",
    "mettwr4h" => "raw/mettwr4h/",
    "skyrad"   => "raw/skyrad/"
);

%filename_pattern = (
    "gndrad"   => 'nsagndrad60s([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "mettwr2h" => 'nsamettwr2h([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "mettwr4h" => 'nsamettwr4h([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})',
    "skyrad"   => 'nsaskyrad60s([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})'
);

#--------------------------------------------------------------------------------------
# the stations and their full names
#--------------------------------------------------------------------------------------
%stn_name = (
    "C1"=> "C1_Barrow",
    "C2"=> "C2_Atqasuk"
);

#-----------------------------
# project specific variables
#-----------------------------
                           
my $project_name = "CEOP";
$CSE_id       = "Other";
$site_id      = "NSA";
$platform_id  = "XXXXXX";	# e.g. "gndrad"           
$stn_id       = "Exx";		# e.g. "C1"
$network            = "ARM_$platform_id";
$project_begin_date = 20050101;		# 2005-2007
$project_end_date   = 20071231;     
#$project_begin_date = 20070900;		# 2005-2007
#$project_end_date   = 20071003;     

$precip_accum = 0;		# for accumulation of precip by the half hour
$missing_obs  = -9999;		# missing in data
$MISSING      = -999.99;	# our missing value
                               
#--------------------------------------------------------------------------------------
# the names of the dqr files for each platform
#--------------------------------------------------------------------------------------
my @dqr_fname;
#push(@dqr_fname,"raw/NSA_GNDRAD_flagging.txt");
#push(@dqr_fname,"raw/NSA_METTWR_flagging.txt");
#push(@dqr_fname,"raw/NSA_SKYRAD_flagging.txt");
push(@dqr_fname,"raw/NSA_GNDRAD_flagging_2008_2009.txt");
push(@dqr_fname,"raw/NSA_METTWR_flagging_2008.txt");
push(@dqr_fname,"raw/NSA_SKYRAD_flagging_2008_2009.txt");

# a list of flag objects
$flag_list = DQR::FlagList->new();

#-----------------------------
# read in the flags and return a reference
# to an array of DQR::Flag objects
#-----------------------------

# read the dqr file(s)
my ($flag_ref, $dqr_file, $flag_count, $i, $flag);
foreach $dqr_file (@dqr_fname) {
   $flag_ref = undef;
   print "processing $dqr_file..\n";
   $flag_ref = &read_dqr( $dqr_file );
   print "flag_ref: $flag_ref\n";
   $flag_count = $#{$flag_ref} + 1;
   foreach ($i = 0; $i <= $#{$flag_ref}; $i++) {
      $flag_list->add_flag( $flag_ref->[$i] );
   } # end foreach
} # end foreach

#-----------------------------
# files in, put in arrays
#-----------------------------
 
foreach $network (keys (%dirs)) {
    print "Reading filenames in $dirs{$network} directory... \n";
    opendir (FILEDIR, $dirs{$network}) || die "Can't open $dirs{$network}\n";
    #@this_dir = sort(readdir(FILEDIR));

    #-----------------------------
    # only grab the .dat files
    #-----------------------------
    @this_dir = sort( grep(/\.dat$/, readdir(FILEDIR)));

    #-----------------------------
    # get rid of "." and ".." 
    #-----------------------------
    #shift(@this_dir);
    #shift(@this_dir);
    #-----------------------------
    # add path to filenames
    #-----------------------------
    @this_dir = map($dirs{$network}.$_, @this_dir);
    push (@sfc_infiles,  @this_dir);
    closedir(FILEDIR);
}

#-----------------------------
# sort infile lists by  date
#-----------------------------
#@sfc_infiles = sort file_date_sort (@sfc_infiles);
@sfc_infiles = &sort_files_by_date(@sfc_infiles);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------
&clear_array("sfc");

#-----------------------------------------------------------------------------
# Divide the infile names into arrays by date, and then put 
# references to the arrays into a hash indexed on the date.
#-----------------------------------------------------------------------------

# number of days for each month
my @months=(0,31,28,31,30,31,30,31,31,30,31,30,31);
my @lmonths=(0,31,29,31,30,31,30,31,31,30,31,30,31);
$start_year  = substr($project_begin_date, 0, 4);
$start_month = substr($project_begin_date, 4, 2);
$end_year    = substr($project_end_date, 0, 4);

print "start month and year = $start_month/$start_year, end year = $end_year\n";
for ($i=$start_year; $i <= $end_year; $i++) {               # these are the years we want 
  my @m=(isLeap($i) ? @lmonths : @months);
  for ($j= ($i==$start_year) ? $start_month : 1; $j <= 12; $j++) {          
    for ($k=1; $k <= $m[$j]; $k++) {                        # these are the days of the months
        $the_date = sprintf("%4.4d%2.2d%2.2d", $i, $j, $k);  # put our date together with the month and day
	last if ($the_date > $project_end_date);
        #-----------------------------
        # divide into arrays by date
        #-----------------------------
        my @sfc_date_array = grep(/$the_date/, @sfc_infiles);   # get all files for sfc and this date into an array


        #-----------------------------
        # put refs to arrays into hash
        #-----------------------------
        $sfc{$the_date} = \@sfc_date_array;             # get reference to the date_array and store in sfc hash indexed by date
        print ("the files for sfc $the_date = @{$sfc{$the_date}}\n") if ($DEBUG1);;
    } # end for
  } # end for
} # end for
 
#-----------------------------
# files out
#-----------------------------
$outfile1 = $CSE_id . "_" . $site_id . "_" . $site_id . "_" . $project_begin_date . "_" . $project_end_date . ".sfc";
# $station_out = $CSE_id . "_".$site_id."_station.out";
# $CD_station_out = $CSE_id . "_".$site_id."_stationCD.out";
# $stn_id_out = $CSE_id . "_".$site_id."_stn_id.out";

#-----------------------------------------------------------------------------
# Open files used in conversion
#-----------------------------------------------------------------------------

open (OUTFILE_SFC, ">./out/final/$outfile1") || die "Can't open $outfile1";
open (WARNINGS, ">warning.log") || die "Can't open warning.log";
# open (STNSOUT1, ">./out/$station_out") || die "Can't open $station_out";
# open (STNSOUT2, ">./out/$CD_station_out") || die "Can't open $CD_station_out";
# open (STNSOUT3, ">./out/$stn_id_out") || die "Can't open $stn_id_out";

writeHeader("sfc") if ($DEBUG);

#-----------------------------
# set default values in our
# output arrays
#-----------------------------
foreach $obs (@sfc_obs) {
	${$obs} = $MISSING;
} # end for 

#-----------------------------------------------------------------------------
# start reading in the surface data, a day at a time
#-----------------------------------------------------------------------------
foreach $date (sort keys (%sfc)) { # get each date in sorted order
	print ("date = $date\n") if ($DEBUG);
	$num_files = @{$sfc{$date}};
	print "have $num_files files for $date\n" if ($DEBUG);
	foreach $infile (@{$sfc{$date}}) {                            	# now read in the filenames one at a time
    	  next if ($infile !~ /dat$/);                                # only take files ending in ".dat"
 	  print ("\n************************\nOpening $infile for input\n") if ($DEBUG1);
    	  open (INFILE, "$infile") || die ("Can't open $infile\n");
    	  $infile =~ /^raw\/([a-z2-4]{3,8})/;                            # get the platform name from the path
    	  $platform = ($1);                                      
    	  print ("this platform: $platform\n") if ($DEBUG);

    	  while ($this_line = <INFILE>) {

        	@line_value = split(" ", $this_line);
	        print "This line is: $this_line" if ($DEBUG1);

        	#--------------------------------------------------------------------------------------
        	# Test to see what variable we have on this line, and save the constants for this file.
        	# Files are read in order, so each time we see "netCDF" it is a new file being read.
        	#--------------------------------------------------------------------------------------
				    
        	if ($line_value[0] eq "netcdf") {
            	   $filename = $line_value[1];
	           print ("filename: $filename\n") if($DEBUG);
	            #--------------------------------------------------------------------------------------
	            # Get our station name and date from the filename 
	            # Check the date against project dates and if outside the TOI get another line.
	            #--------------------------------------------------------------------------------------
	            print ("matching $filename_pattern{$platform}\n") if($DEBUG1);
	            $filename =~ /$filename_pattern{$platform}/;
	            ($stn_id, $yr, $mon, $day, $hour, $min) = ($1, $2, $3, $4, $5, $6); 
	            print ("stn = $stn_id, yr = $yr, mon = $mon, day = $day, time = $hour:$min\n") if($DEBUG);
	            $this_date = $yr.$mon.$day;
	            if(($this_date < $project_begin_date) || ($this_date > $project_end_date)) {
	                print "this $filename data for $this_date is out of the TOI\n\n";
	                last;
	            } # endif

		    $stnlist{$stn_id} = 1;
	        #--------------------------------------------------------------------------------------
	        # Get the constants (lat, lon, alt, base time) from this file
	        #--------------------------------------------------------------------------------------
	        } elsif ($line_value[0] eq "lat:") {
	           $lat = $line_value[1];
		   ${$stn_id}{lat} = $lat;
	        } elsif ($line_value[0] eq "lon:") {
	           $lon = $line_value[1];
		   ${$stn_id}{lon} = $lon;
	        } elsif ($line_value[0] eq "alt:") {
	           $elev = $line_value[1];
		   ${$stn_id}{elev} = $elev;
	        } elsif ($line_value[0] eq "base_time:") {
	           $baseTime = $line_value[1];
	        } else {
    
                   #--------------------------------------------------------------------------------------
                   # All others should be the values for each time.  Get the variable name which will match
                   # one of our "fields" values (with a ":" after it), take off the trailing comma in each 
                   # slot of the "line_value" array, and copy the array to an array named after the variable. 
                   #--------------------------------------------------------------------------------------
	           $index = 0; 
	           foreach (@{$params{$platform}}) {
	              $obsName = ${$params{$platform}}[$index++];

	              if ($line_value[0] eq $obsName.":") {		
	 	         print ("this obs = $obsName for the $platform platform, and it is param number $index, while line value 0 = $line_value[0] and obs name = $obsName\n") if ($DEBUG1);
	                 $numObs = ($#line_value - 2) if ($line_value[0] eq "time_offset:");    

			 if (($platform eq "mettip") && ($obsName !~ /atmos_pressure/) && ($obsName ne "time_offset") && ($obsName !~ /wind_spd_mean/) && ($obsName !~ /wind_dir_vec_avg/)) {
		            $obsNum = 0;				
			    print "     we are checking the $obsName values within the $platform platform...\n" if ($DEBUG);

			    #----------------------------------------------------------------------
			    # met tower parameters are in array indexed by time and height
			    #   mettiptwr heights = 6, 2 meters 
			    # for mettiptwr, we want them at 2 meters
			    #----------------------------------------------------------------------

			    # 1 = height of 2m
			    $ht_index = 1;									
			    @{$obsName} = $line_value[0];
			    # first line has obs name, so obs at 2m has index of 2 for first line
			    push(@{$obsName}, $line_value[$ht_index + 1]);	
			    print " for $obsName array, pushed $line_value[$ht_index + 1] on to the array and now it is: @{$obsName}\n" if ($DEBUG);
			    $obsNum++;
    			    $this_line = <INFILE>; 

			    # we have multiple lines of heights, so get lines until end of this param
			    while ($this_line !~ /;$/) {					
	        	       # print "This line is: $this_line" if ($DEBUG1);
        		       @line_value = split(" ", $this_line);
		               #--------------------------------------------------------------------------------------
                	       # Check that each line of values has the same number of observations as heights.
                	       #--------------------------------------------------------------------------------------

			       # w/o semi-colon at end
   	                       $thisNumHeights = $#line_value + 1;         
       	            	       die "x: Different number of heights in the $obsName line; expecting $numMettipHeights, but have $thisNumHeights\n" if ($numMettipHeights != $thisNumHeights);

			       # all lines but the first has 2m obs index of 1
			       push(@{$obsName}, $line_value[$ht_index]);	
			       $obsNum++;
    			       $this_line = <INFILE>; 
                            } # end while

       			    @line_value = split(" ", $this_line);
			    # get rid of semi-colon at end of line
			    pop(@line_value);     							
	   
			    #--------------------------------------------------------------------------------------
               		    # Check that each line of values has the same number of observations as heights.
               		    #--------------------------------------------------------------------------------------

			    # w/o semi-colon at end
                	    $thisNumHeights = $#line_value + 1;         	
   	            	    die "y: Different number of heights in the $obsName line; expecting $numMettipHeights, but have $thisNumHeights\n" if ($numMettipHeights != $thisNumHeights);

			    # all lines but the first has 2m obs index of 1
			    push(@{$obsName}, $line_value[$ht_index]);		

		            #------------------------------------------------------------
		            # Get rid of the commas after the values, change
		            # any -0.0 values to 0.0, and "NAN" to -999.99,
			    # and set to Missing if less than -899.
		            #------------------------------------------------------------
		    
		            for ($i=1; $i <= $numObs + 1; $i++) {
		               ${$obsName}[$i] =~ s/,//g;
		               ${$obsName}[$i] = $MISSING if (${$obsName}[$i] eq "nan");
			       # catch very large numbers in data
		               ${$obsName}[$i] = $MISSING if (${$obsName}[$i] =~ /e+/);				
			       ${$obsName}[$i] = $MISSING if (${$obsName}[$i] eq "_");
		               ${$obsName}[$i] = $MISSING if ((${$obsName}[$i] < -899) || (${$obsName}[$i] == 99999) || (${$obsName}[$i] == 6999));
		               ${$obsName}[$i] = 0.00 if (sprintf("%8.2f", ${$obsName}[$i]) == -0.00); 
			    } # end for

        		    if ($obsNum != $numObs) {
            		       printf ("*** Had %d number of observations in $obsName of $filename, but was expecting %d!\n", $obsNum, $numObs);
	        	       printf  WARNINGS ("*** Had %d number of observations in $filename, but was expecting %d!\n", $obsNum - 1, $numObs);
			       warn "A - Wrong number of obs in $filename, beware!";
        		    } # endif

		            print "this array of obs is @{$obsName}\n" if ($DEBUG1);

		         } elsif (($platform eq "mettwr") && ($obsName !~ /atmos_pressure/) && ($obsName ne "time_offset")) {

			    $obsNum = 0;				
			    print "     we are checking the $obsName values within the mettwr platform...\n" if ($DEBUG);

			    #------------------------------------------------------------
			    # met tower parameters are in array indexed by time and height
			    # take the 3rd value (index of 2) for each time if for a wind 
			    # parameter, otherwise take the 4th value (index of 3)
			    #------------------------------------------------------------

			    # 2 = height of 10m; 3 = height of 2m
			    $ht_index = $obsName =~ /wind_/ ? 2 : 3;		
			    @{$obsName} = $line_value[0];
			    # first line has obs name, so obs at 10m has index of 3 for first line
			    push(@{$obsName}, $line_value[$ht_index + 1]);	
			    print " for $obsName array, added $line_value[$ht_index + 1]: @{$obsName}\n" if ($DEBUG);
			    $obsNum++;
    			    $this_line = <INFILE>; 

			    # we have multiple lines of heights, so get lines until end of this param
			    while ($this_line !~ /;$/) {					
	        	       # print "This line is: $this_line" if ($DEBUG1);
        		       @line_value = split(" ", $this_line);
                				
			       #--------------------------------------------------------------------------------------
                	       # Check that each line of values has the same number of observations as heights.
                	       #--------------------------------------------------------------------------------------

   	                       $thisNumHeights = $#line_value + 1;         # w/o semi-colon at end
       	            	       die "x: Different number of heights in the $obsName line; expecting $numMetHeights, but have $thisNumHeights\n" if ($numMetHeights != $thisNumHeights);

			       # all lines but the first has 10m obs index of 2
			       push(@{$obsName}, $line_value[$ht_index]);	
			       $obsNum++;
    			       $this_line = <INFILE>; 
 			    } # end while

       			    my @line_value = split(" ", $this_line);

			    # get rid of semi-colon at end of line
			    pop(@line_value);     							
	   
			    #--------------------------------------------------------------------------------------
               		    # Check that each line of values has the same number of observations as heights.
               		    #--------------------------------------------------------------------------------------

			    # w/o semi-colon at end
                	    $thisNumHeights = $#line_value + 1;         	
   	            	    die "y: Different number of heights in the $obsName line; expecting $numMetHeights, but have $thisNumHeights\n" if ($numMetHeights != $thisNumHeights);

			    # all lines but the first has 10m obs index of 2
			    push(@{$obsName}, $line_value[$ht_index]);		

		            #------------------------------------------------------------
		            # Get rid of the commas after the values, change
		            # any -0.0 values to 0.0, and "NAN" to -999.99,
			    # and set to Missing if less than -899.
		            #------------------------------------------------------------
		    
		            for ($i=1; $i <= $numObs; $i++) {
		               ${$obsName}[$i] =~ s/,//g;
		               ${$obsName}[$i] = -999.99 if (${$obsName}[$i] eq "nan");
			       # catch very large numbers in data
		               ${$obsName}[$i] = -999.99 if (${$obsName}[$i] =~ /e+/);				
			       ${$obsName}[$i] = -999.99 if (${$obsName}[$i] eq "_");
		               ${$obsName}[$i] = -999.99 if ((${$obsName}[$i] < -899) || (${$obsName}[$i] == 99999) || (${$obsName}[$i] == 6999));
		               ${$obsName}[$i] = 0.00 if (sprintf("%8.2f", ${$obsName}[$i]) == -0.00); 
		            } # end for

        		    if ($obsNum != $numObs) {
            		       printf ("*** Had %d number of observations in $obsName of $filename, but was expecting %d!\n", $obsNum, $numObs);
	        	       printf  WARNINGS ("*** Had %d number of observations in $filename, but was expecting %d!\n", $obsNum - 1, $numObs);
			       warn "B - Wrong number of obs in $filename, beware!";
        		    } # endif
		            print "this array of obs is @{$obsName}\n" if ($DEBUG1);

			 } else {

		            #--------------------------------------------------------------------------------------
		            # Check that each line of values has the same number of observations as the time line.
		            #    (Subtract 1 for the first value on the line, which is the variable name.)
		            #    (And subtract 1 for the last value on the line, which is ";".)
		            #--------------------------------------------------------------------------------------
		    
		            $numObs = ($#line_value - 2) if ($line_value[0] eq "time_offset:");    
		            my $thisNumObs = $#line_value - 2;                 
		            die "b: Different number of observations in the $line_value[0] line; expecting $numObs, but have $thisNumObs\n" if ($numObs != $thisNumObs);


		            #------------------------------------------------------------
		            # Get rid of the commas after the values, change
		            # any -0.0 values to 0.0, and "NAN" to -999.99,
			    # and set to Missing if less than -899.
		            #------------------------------------------------------------
		    
		            for ($i=1; $i <= $numObs; $i++) {
		               $line_value[$i] =~ s/,//g;
		               $line_value[$i] = $MISSING if ($line_value[$i] eq "nan");
			       # catch very large numbers in data
		               $line_value[$i] = $MISSING if ($line_value[$i] =~ /e+/);				
			       $line_value[$i] = $MISSING if ($line_value[$i] eq "_");
		               $line_value[$i] = $MISSING if (($line_value[$i] < -899) || ($line_value[$i] == 99999) || ($line_value[$i] == 6999));
		               $line_value[$i] = 0.00 if (sprintf("%8.2f", $line_value[$i]) == -0.00); 
		            } # end for
		   
		            print "and the line values are: @line_value\n" if ($DEBUG1);
		
		            #------------------------------------------------------------
		            # Put into an array named after the the variable
		            #------------------------------------------------------------
			    #print "putting values into $obsName\n";
		            @{$obsName} = @line_value;
		            print "this array of obs is @{$obsName}\n\n" if ($DEBUG1);
			 }  # <---- end if not special matches
	              } # <---- end if match obsName and param
	            } # <---- end foreach params
        	} # <---- end all other params
    	  } # <---- end while infile line
	
	  #--------------------------------------------------------------------------------------
	  # Check that the date/time in the filename matches the date/time in the data.
	  # Add the 1st time_offset to the base_time and convert to GMT to compare.
	  # The 0'th value is the variable name, so index of obs starts at 1.
	  # Add 1 to the month because gmtime() returns months indexed from 0 to 11.
	  # Add 1900 to the year because gmtime() returns years starting at 0 for 1900.
	  #--------------------------------------------------------------------------------------
		
	  @begin_time = gmtime($baseTime + $time_offset[1]);
	  print "base time = $baseTime, time offset = $time_offset[1]\n" if ($DEBUG);
	  $begin_month = $begin_time[4] + 1;
	  $begin_year = $begin_time[5] + 1900;
	  
	  if (($yr != $begin_year) || ($mon != $begin_month) || ($day != $begin_time[3]) || ($hour != $begin_time[2]) || ($min != $begin_time[1])) {
	     print "We have a time problem with $filename\n";
	     print "julian day: $begin_time[7], year: $yr,$begin_year, month: $mon,$begin_month, day: $day,$begin_time[3], hour: $hour,$begin_time[2], min: $min,$begin_time[1]\n";   
	     die "That's all!";
	  } # end if
	
	  $date_str = "$yr/$mon/$day";
	  print "Writing data for station: $stn_id platform: $platform on $date_str\n";
	  print ("Will write data for station $stn_id on $date_str\n\n") if ($DEBUG1);
		
	  #-----------------------------------------------------------------------
	  # Put the values and flags of the obs into separate arrays prepared by
	  # the clear_array() subroutine, and indexed by the time and station.
	  #-----------------------------------------------------------------------

	 # Note: params are indexed in the output line array by number
	 $j = 0;               				

	 foreach $param (@sfc_parameter_list) {

	    if (defined(@{$param})) {

	       $j = $param_index{$stn_id}{$param};

	       if ( !defined($j) ) {
	          print "j is not defined for $stn_id and $param\n";
		  next;
	       } # endif

	       #print "$param is defined and j = $j\n";
	    
	       $qc_flags = "qc_".$param;

	       # if there isn't a qc field, then tag the values for the field
               # as 'U' (unchecked)
	       if ( !defined(@{$qc_flags}) ) {
	         @{$qc_flags} = ();
	         for ($obsNum = 1; $obsNum <= $numObs; $obsNum++) { 
		   @{$qc_flags}[$obsNum] = 6; # set as unchecked 
		 } # end for
	       } # endif
	       $qc_flags = "snow_depth_out_of_range_error" if($param eq "snow_depth" && $platform eq "snodep");
	       # get every value, one at a time
	       for ($obsNum = 1; $obsNum <= $numObs; $obsNum++) {      		
	          @this_gmtime = gmtime($baseTime + $time_offset[$obsNum]);
	          $min = $this_gmtime[1];
	          $hour = $this_gmtime[2];
		  print "this qc_flags = $qc_flags, and the line of values = @{$qc_flags}\n" if($obsNum == 2 && $DEBUG1); 
 		  #die "No qc values exist for the $qc_flags obs\n" if (!defined(@{$qc_flags}));
		  my $value = @{$param}[$obsNum];
		  #if ( $obsNum < 5 ) {
		  #   print "platform: $platform: adding $value where std_id = $stn_id, hour = $hour, min = $min,  param = $param and j = $j\n";
		  #}
		  $sfc_out{$stn_id}[$hour][$min][$j] = @{$param}[$obsNum];

		  $sfc_flag{$stn_id}[$hour][$min][$j] = @{$qc_flags}[$obsNum] if(defined(@{$qc_flags}));
	       } # <----- end for obsNum   
	    } else {
	       print "$param is not defined for $platform on $date_str for $filename\n" if ($DEBUG1);
	       print "   in other words, the following is empty: @{$param}.\n" if ($DEBUG1);
	    } # endif
	    #$j++;

	 } # <---- end foreach param

	    #------------------------------------------------------------
		# we have the values, so clear out the params arrays used
	    #------------------------------------------------------------
	    foreach $param ("time_offset", @sfc_parameter_list) {
	       undef(@{$param});
	       $qc_flags = "qc_".$param;
	        undef(@{$qc_flags});
	    } # end foreach


	    if ($obsNum-1 != $numObs) {
	        printf ("*** Had %d number of observations in $filename, but was expecting %d!\n", $obsNum - 1, $numObs);
	    	printf  WARNINGS ("*** Had %d number of observations in $filename, but was expecting %d!\n", $obsNum - 1, $numObs);
			die "C - Wrong number of obs in $filename!";
	    } # endif
	}   # <----- end foreach infile

	&writeDate("sfc", $date_str);
	undef %sfc_out;
	undef %sfc_flag;
	&clear_array("sfc");
	foreach $id (keys %stnlist) {
	   undef($stnlist{$id});
	} # end foreach
}   # <----- end foreach date, SFC

close (OUTFILE_SFC);

#----------------------------------------------------------------------------------------
# Set up the array (@sfc_out) which will feed the values for all the parameters
# for one day into the sfc output lines. This equates to all the obs in all
# the files for one day. The array is indexed on each station ID, each hour and 
# minute in the day, and on the parameter's position in the output line (1-19).
#----------------------------------------------------------------------------------------
#   ( col num)        =        1                2              3                 4            5             6                 7               8         9           10           11                 12                  13                      14                    15           16        17           18         19    
# @sfc_parameter_list = ("atmos_pressure", "temp_mean", "dew_pt_temp_mean", "relh_mean", "spec_hum", "wind_spd_mean", "wind_dir_vec_avg", "u_wind", "v_wind", "precip_rate", "snow_depth", "down_short_hemisp", "up_short_hemisp", "down_long_hemisp_shaded1", "up_long_hemisp", "net", "sfc_ir_temp", "par_in", "par_out"); 
#----------------------------------------------------------------------------------------

#**********************************************************************
sub clear_array {
    my($array_name) = @_;
    $array_name = $array_name . "_out";
    $end_num = $sfc_param_count + 1;
    if ($array_name eq "sfc_out") {
        foreach $stn (keys (%stn_name)) { 
            for $hour_num (0..24) {
	      for $min_num (0..60) {
                for $param_num (0..$end_num) {
                    $sfc_out{$stn}[$hour_num][$min_num][$param_num] = $MISSING;
                    $sfc_flag{$stn}[$hour_num][$min_num][$param_num] = -99;
                } # end for
              } # end for
            } # end for
        } # end foreach
    } else {
        die "don't know this array to clear: $array_name\n";
    }  # endif 
}


sub sort_files_by_date {

  @list_of_files = @_;
  my %sort_hash;
  my $network;
  my $i;

  # stuff the files into a hash
  foreach $i (@list_of_files) {
    # get the filename
    @tmp = split(/\//, $i);
    $fname = $tmp[$#tmp];
    $fname =~ /nsa(\w+)([CE]\d{1,3}).{4}(\d{8})\.(\d{6})\.dat$/;
    # create the key for the hash so
    # it can be sorted
    #$sort_hash{"$3.$4_$1"} = $i;
    $sort_hash{"$3.$4_$1$2"} = $i;
  } # end foreach

  # now, sort the files by date
  my @sorted;
  foreach $key (sort(keys %sort_hash)) {
    push(@sorted, $sort_hash{$key});
  } # end foreach

  return @sorted;

}

sub file_date_sort {
 
    my $a =~ /([CE]\d{1,3}).{4}(\d{8})\.(\d{6})\.dat$/;
    my $a_id = $1;
    my $a_datetime = $2.$3;
    my $b =~ /([CE]\d{1,3}).{4}(\d{8})\.(\d{6})\.dat$/;
    my $b_id = $1;
    my $b_datetime = $2.$3;
 
    $retval = ($a_id cmp $b_id);
    if ($retval != 0) {
        return $retval;
    } else {
        $retval = $a_datetime <=> $b_datetime;
        return $retval;
    }
}


sub isLeap {
    my($year) = @_;
    return 1 if $year % 400 == 0;
    return 0 if $year % 100 == 0;
    return 1 if $year % 4 == 0;
    return 0;
}


#------------------------------------------------------------
# print out the header lines 
#------------------------------------------------------------       

sub writeHeader {
    my($out_type) = @_;
          
    if ($out_type eq "sfc") {
		print OUTFILE_SFC "   date    time     date    time    CSE ID      site ID        station ID        lat        lon      elev  ";
		foreach $param ("stn pres", " f", "temp_air", " f", " dew pt ", " f", " rel hum", " f", "spec hum", " f", " wnd spd", " f", " wnd dir", " f", " U wind ", " f", " V wind ", " f", " precip ", " f", "  snow  ", " f", " short in", " f", " shortout", " f", " long in ", " f", " long out", " f", "  net rad", " f", " skintemp", " f", " par_in  ", " f", " par_out ", " f") {
			print OUTFILE_SFC "$param"; 
		}
		print OUTFILE_SFC "\n";
		print OUTFILE_SFC "---------- ----- ---------- ----- ---------- --------------- --------------- ---------- ----------- -------"; 
		for ($i=0; $i<11; $i++) {
			print OUTFILE_SFC " ------- -"; 
		}
		for ($i=0; $i<8; $i++) {
			print OUTFILE_SFC " -------- -"; 
		}
		print OUTFILE_SFC "\n";
    } else {
        die "don't know this output type: $out_type!";
    } # endif
}

#--------------------------------------------------------------------------------   
##  writeDate - write all the data values for 
#               one day, for all stations.
#
#   input:		type of data (sfc, flx, stm, twr, etc)
#				date (yyyy/mm/dd)
#--------------------------------------------------------------------------------   
sub writeDate
{
	local ($met_type, $the_date) = @_;
	my $have_precip = 0;

 	#------------------------------------------------------------       
    # Loop through the stns, and the hours and minutes,
	# get the precip accumulated until then, calculate it,
    # and print the values at each half hour for each station.
    # Note: for the new reference version, we are putting out
    #       30-min records. Therefore, if real UTC time < 15 OR
    #       real UTC time > 45, then nominal UTC time = 00, else
    #       nominal UTC time = 30. This has not been implemented,
    #		since we are dealing with 1 minute data.
    #------------------------------------------------------------       
#$param_index{$sfc_parameter_list[14]} = 14;	# PCPRate 
    my %precip_index;
    foreach $stn (keys (%stn_name)) { 
      if (defined ($stnlist{$stn}) && $stnlist{$stn} != 0) {    
        for ($hour=0; $hour < 24; $hour++) {
            $real_hour = $hour;
            for ($min=0; $min < 60; $min++) {
	       if($met_type eq "sfc")  {
		  # precip is number 9 in array
	          $this_precip = $sfc_out{$stn}[$hour][$min][9];          
	          if ($this_precip > 0) {    							
		     # accum ALL precip values before the half hour 
	             $precip_accum += $this_precip;    					
		     $have_precip = 1;
		  } # endif

		  # if any of the accumulated values are missing then
		  # the precip accumulation is set to missing
		  #if ( ($this_precip == $MISSING) && ($have_precip = 1) ) {
		  #   $have_precip = 0;
		  #   $precip_accum = 0;
		  #} # endif

 	          print ("this precip for $stn at $hour:$min = $this_precip, and precip accum = $precip_accum\n") if ($DEBUG1);     

	       } # end if
 	       print ("this snow depth for $stn at $hour:$min on $the_date = $sfc_out{$stn}[$hour][$min][10], and flag = $sfc_flag{$stn}[$hour][$min][10]\n") if ($DEBUG2);     
	       if ($min == 0 || $min == 30) {
		   if ($met_type eq "sfc") {
		      # precips are mm/hr every min, so at half hour divide total by 60 
	              $precip_accum /= 60;        							
		      # to avoid negative values from calcs
	              $precip_accum = 0.0 if ($precip_accum < 0.005);    		
 	              print ("this precip for $stn at $hour:$min = $this_precip, and precip accum (adjusted for 30 min period) = $precip_accum\n") if ($DEBUG1);     
		      $sfc_out{$stn}[$hour][$min][9] = $precip_accum unless($have_precip == 0);
		      $real_min = $min;
           	      &writeSFCline($stn, $platform, $the_date, $hour, $min); 
		      $precip_accum = 0;
		      $have_precip  = 0;
		   } else {
           	      # &writeFLXline($stn, $the_date, $hour, $min); 
	           } # endif
	       } # <------- end min on half hour
	    }	# <------- end for minutes of hour 
	} # eid for hour
      } # end if
    } # end foreach
}


#--------------------------------------------------------------------------------   
##  writeSFCline - write the surface data values to the SFC output file. 
# 
#  input:   
#       $stn        the station with the readings
#       $platform   the source for the data
#	$date		yyyy/mm/dd
#       $hour 
#       $min 
#
#       global values:
#					$real_min, and all the data obs
#
#  output:  a single line of surface data for that time
#--------------------------------------------------------------------------------   

sub writeSFCline
{ 
    local ($id, $net_src, $date, $hour, $min) = @_;
    my $new_flag;
    $long_name = $stn_name{$id};
	my $lat  = ${$id}{lat};
	my $lon  = ${$id}{lon};
	my $elev = ${$id}{elev};

    #------------------------------------------------------------
	# Put the hash values into scalar values named after the obs
    #------------------------------------------------------------
    $print = 0;
    $j = 0;
    foreach $obs (@sfc_obs) {
       ${$obs} = $sfc_out{$id}[$hour][$min][$j];
       $the_flag = $obs."_flag";
       ${$the_flag} = $sfc_flag{$id}[$hour][$min][$j];
       print ("for $stn_name{$id}, at $hour:$min $obs: $sfc_out{$id}[$hour][$min][$j], which is same as ${$obs}, and $the_flag is ${$the_flag}, which is the same as $sfc_flag{$id}[$hour][$min][$j]\n") if ($DEBUG1);
       $print = 1 if (${$obs} != $MISSING);
       $j++;
    }  # end foreach

    #------------------------------------------------------------
	# decision made to include empty records, 12 May 04, per Scot
    #------------------------------------------------------------
    # don't print lines with all obs missing
    # return ("empty line") if ($print == 0);							
    #------------------------------------------------------------

    #------------------------------------------------------------
    # Print out the first part of our line for the output to SFC
    #------------------------------------------------------------       
   
    printf OUTFILE_SFC ("%10s %02d:%02d %10s %02d:%02d %-10s %-15s %-15s %10.5f %11.5f %7.2f", $date, $hour, $min, $date, $real_hour, $real_min, $CSE_id, $site_id, $long_name, $lat, $lon, $elev);
    #   format -   yyyy/mm/dd hh:mm yyyy/mm/dd hh:mm CSE_id site_id station_id dec_lat dec_lon elevation 
    
    #------------------------------------------------------------
    # Print out the stn_pressure (and the flag)
    #------------------------------------------------------------ 
    my ($field_name, $category);
    $category = $sfc_obs[0];
    $stn_pres = &correct_pressure( $stn_pres, $date, $hour, $min);
    $field_name = &find_field_name($category, $id);
    $stn_pres_flag = &get_flag(\$stn_pres, $stn_pres_flag, $field_name, $id, $date, $hour, $min);
    $stn_pres_flag = &get_post_analysis_flag( $id, $category, $stn_pres_flag, $date, $hour,$min);
    printf OUTFILE_SFC (" %7.2f", $stn_pres);
    printf OUTFILE_SFC (" %s", $stn_pres_flag);
        
    #------------------------------------------------------------
    # Print out the temperature
    #------------------------------------------------------------ 
    $field_name = &find_field_name($sfc_obs[1], $id);
    $temp_air_flag = &get_flag(\$temp_air, $temp_air_flag, $field_name, $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %7.2f", $temp_air);
    printf OUTFILE_SFC (" %s", $temp_air_flag);

    #------------------------------------------------------------
    # Print out the dew point temperature
    #  (using rel hum flag)
    #------------------------------------------------------------
    $field_name = &find_field_name( $sfc_obs[3], $id);
    $rel_hum_flag = &get_flag(\$rel_hum, $rel_hum_flag, $field_name, $id, $date, $hour, $min);
    #$rel_hum_flag = &get_flag(\$rel_hum, $rel_hum_flag, "relh_mean", $id, $date, $hour, $min);

    $field_name = &find_field_name( $sfc_obs[2], $id);
    $dew_pt_flag = &get_flag(\$dew_pt, $dew_pt_flag, $field_name, $id, $date, $hour, $min);
    #$dew_pt_flag = &get_flag(\$dew_pt, $dew_pt_flag, "dew_pt_temp_mean", $id, $date, $hour, $min);
    if ($dew_pt == $MISSING) {
       $hash_ref = &calc_dewpoint($rel_hum, $rel_hum_flag, $temp_air, $temp_air_flag);  
       #$dew_pt = $dew_point;
       #$dew_pt_flag = $dew_point_flag;
       $dew_pt = $hash_ref->{'dew_pt'};
       $dew_pt_flag = $hash_ref->{'dew_pt_flag'};

    } # endif
    # does the value fit the specified format??
    if ( &value_too_large($dew_pt, 7) ) {
      print "dew point value: $dew_pt is too large\n" if $DEBUG;
      $dew_pt = $MISSING;
      $dew_pt_flag = "C";
    } # endif
    printf OUTFILE_SFC (" %7.2f", $dew_pt);
    printf OUTFILE_SFC (" %s", $dew_pt_flag);

    #------------------------------------------------------------
    # Print out the relative humidity  value
    #------------------------------------------------------------
    printf OUTFILE_SFC (" %7.2f", $rel_hum);
    printf OUTFILE_SFC (" %s", $rel_hum_flag);

    #------------------------------------------------------------
    # Calculate the specific humidity, convert to 
    # g/kg from kg/kg, and print out the value
    #------------------------------------------------------------
    $hash_ref = &calc_specific_humidity($dew_pt, $dew_pt_flag, $stn_pres, $stn_pres_flag);
    my $specific_humidity = $hash_ref->{'specific_humidity'};
    $specific_humidity *= 1000 unless ($specific_humidity == $MISSING);
    $specific_humidity_flag = $hash_ref->{'specific_humidity_flag'};
    #printf OUTFILE_SFC (" %7.2f", $specific_humidity);
#	printf OUTFILE_SFC (" %s", $specific_humidity_flag);

    # does the value fit the specified format??
    if ( &value_too_large($specific_humidity, 7) ) {
      print "specific humidity value: $specific_humidity is too large\n" if $DEBUG;
      $specific_humidity = $MISSING;
      $specific_humidity_flag = "C";
    } # endif
    printf OUTFILE_SFC (" %7.2f", $specific_humidity);
    printf OUTFILE_SFC (" %s", $specific_humidity_flag);

    #------------------------------------------------------------
    # Print the wind speed value
    #------------------------------------------------------------
    $category = $sfc_obs[5];
    $field_name = &find_field_name( $category, $id);
    $wind_spd_flag = &get_flag(\$wind_spd, $wind_spd_flag, $field_name, $id, $date, $hour, $min);
    $wind_spd_flag = &get_post_analysis_flag( $id, $category, $wind_spd_flag, $date, $hour,$min);
    printf OUTFILE_SFC (" %7.2f", $wind_spd);
    printf OUTFILE_SFC (" %s", $wind_spd_flag);
        
    #------------------------------------------------------------
    # Print the wind direction value
    #------------------------------------------------------------

    $category = $sfc_obs[6];
    $field_name = &find_field_name($category, $id);
    $wind_dir_flag = &get_flag(\$wind_dir, $wind_dir_flag, $field_name, $id, $date, $hour, $min);
    $wind_dir_flag = &get_post_analysis_flag($id, $category, $wind_dir_flag, $date, $hour,$min);
    printf OUTFILE_SFC (" %7.2f", $wind_dir);
    printf OUTFILE_SFC (" %s", $wind_dir_flag);
    
    #------------------------------------------------------------
    # Calculate and print out the U wind component
    #------------------------------------------------------------
    
    $hash_ref = &calc_UV_winds($wind_spd, $wind_spd_flag, $wind_dir, $wind_dir_flag);
    #printf OUTFILE_SFC (" %7.2f", $U_wind);
    #printf OUTFILE_SFC (" %s", $U_wind_flag);
    $u_wind = $hash_ref->{u_wind};
    $u_wind_flag = $hash_ref->{u_wind_flag}; 
    $u_wind_flag = &get_post_analysis_flag($id, "U_wind", $u_wind_flag, $date, $hour,$min);
    # does the value fit the specified format??
    if ( &value_too_large($u_wind, 7) ) {
      print "u_wind value: $u_wind is too large\n" if $DEBUG;
      $u_wind = $MISSING;
      $u_wind_flag = "C";
    } # endif
    printf OUTFILE_SFC (" %7.2f", $u_wind );
    printf OUTFILE_SFC (" %s", $u_wind_flag );

    #------------------------------------------------------------
    # Print out the V wind component
    #------------------------------------------------------------
                
    #printf OUTFILE_SFC (" %7.2f", $V_wind);
    #printf OUTFILE_SFC (" %s", $V_wind_flag);
    $v_wind = $hash_ref->{v_wind};
    $v_wind_flag = $hash_ref->{v_wind_flag};
    $v_wind_flag = &get_post_analysis_flag($id, "V_wind", $u_wind_flag, $date, $hour,$min);
    # does the value fit the specified format??
    if ( &value_too_large($v_wind, 7) ) {
      print "v_wind value: $v_wind is too large\n" if $DEBUG;
      $v_wind = $MISSING;
      $v_wind_flag = "C";
    } # endif
    printf OUTFILE_SFC (" %7.2f", $v_wind);
    printf OUTFILE_SFC (" %s", $v_wind_flag);

    #------------------------------------------------------------
    # Print out the precipitation value
    #------------------------------------------------------------
    $field_name = &find_field_name( $sfc_obs[9], $id);
    $precip_flag = &get_flag(\$precip, $precip_flag, $field_name, $id, $date, $hour, $min);
    $precip_flag = &apply_precip_corrections($precip, $precip_flag, 50);
    #$precip_flag = &get_flag(\$precip, $precip_flag, "precip_rate", $id, $date, $hour, $min);
    # if *any* of the values in the accumulation are bad
    # then set the accumulated precip to MISSING value
    if ( $precip_flag eq "M" && $precip >= 0.0 ) {
       $precip = $MISSING;
    } elsif ( $precip_flag ne "M" && $precip == $MISSING ) {
       $precip_flag = "M";
    } # endif
    printf OUTFILE_SFC (" %7.2f", $precip);
    printf OUTFILE_SFC (" %s", $precip_flag);


    #------------------------------------------------------------
    # Print out the Snow depth, after converting from m to cm
    # for the pws and snodep platforms, and converting from
    # mm to cm for the mettwr2h and mettwr4h platforms.
    # negative snow/flag adjustment per Scot, 12 May 04
    #------------------------------------------------------------
    if ($net_src eq "pws" || $net_src eq "snodep") {
       $snow *= 100 unless($snow == $MISSING);
    } else {
       $snow *= 0.1 unless($snow == $MISSING);
    } # endif
    $category = $sfc_obs[10];
    $field_name = &find_field_name( $category, $id);
    $snow_flag = &get_flag(\$snow, $snow_flag, $field_name, $id, $date, $hour, $min);
    $snow_flag = &get_post_analysis_flag( $id, $category, $snow_flag, $date, $hour,$min);
    #$snow_flag = &get_flag(\$snow, $snow_flag, "snow_depth", $id, $date, $hour, $min);
    if ($snow < 0.0) {
        $snow_flag = "D" unless ($snow_flag eq "B" || $snow_flag eq "M");
    } # endif
    printf OUTFILE_SFC (" %7.2f", $snow);
    printf OUTFILE_SFC (" %s", $snow_flag);

    #------------------------------------------------------------
    # Print out the incoming shortwave radiation
    #------------------------------------------------------------ 
          
    $category = $sfc_obs[11];
    $field_name = &find_field_name($category, $id);
    $short_in_flag = &get_flag(\$short_in, $short_in_flag, $field_name, $id, $date, $hour, $min);
    $short_in_flag = &get_post_analysis_flag( $id, $category, $short_in_flag, $date, $hour,$min);
    printf OUTFILE_SFC (" %8.2f", $short_in);
    printf OUTFILE_SFC (" %s", $short_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing shortwave radiation
    #------------------------------------------------------------ 
          
    $category = $sfc_obs[12];
    $field_name = &find_field_name( $category, $id);
    $short_out_flag = &get_flag(\$short_out, $short_out_flag, $field_name, $id, $date, $hour, $min);
    #$short_out_flag = &get_flag(\$short_out, $short_out_flag, "up_short_hemisp", $id, $date, $hour, $min);
    $short_out_flag = &get_post_analysis_flag( $id, $category, $short_out_flag, $date, $hour,$min);
    printf OUTFILE_SFC (" %8.2f", $short_out);
    printf OUTFILE_SFC (" %s", $short_out_flag);


    #------------------------------------------------------------
    # Print out the incoming longwave radiation
    #------------------------------------------------------------ 
          
    $category = $sfc_obs[13];
    $field_name = &find_field_name( $category, $id);
    $long_in_flag = &get_flag(\$long_in, $long_in_flag, $field_name, $id, $date, $hour, $min);
    #$long_in_flag = &get_flag(\$long_in, $long_in_flag, "down_long_hemisp_shaded1", $id, $date, $hour, $min);
    $long_in_flag = &get_post_analysis_flag( $id, $category, $long_in_flag, $date, $hour,$min);
    printf OUTFILE_SFC (" %8.2f", $long_in);
    printf OUTFILE_SFC (" %s", $long_in_flag);


    #------------------------------------------------------------
    # Print out the outgoing longwave radiation
    #------------------------------------------------------------ 
          
    $field_name = &find_field_name( $sfc_obs[14], $id);
    $long_out_flag = &get_flag(\$long_out, $long_out_flag, $field_name, $id, $date, $hour, $min);
    #$long_out_flag = &get_flag(\$long_out, $long_out_flag, "up_long_hemisp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $long_out);
    printf OUTFILE_SFC (" %s", $long_out_flag);

    #------------------------------------------------------------
    # Print out the net radiation
	#  take net rad value and flag from file, if not missing
	#  otherwise, figure net rad, and get flags in order, 
	#	checking flag of each input to the net rad sum
    #------------------------------------------------------------ 
        $category = $sfc_obs[15];
        $field_name = &find_field_name( $category, $id);
	if ($net_rad != $MISSING) {
                #$field_name = &find_field_name( $sfc_obs[15], $id);
		$net_rad_flag = &get_flag(\$net_rad, $net_rad_flag, $field_name, $id, $date, $hour, $min);
		#$net_rad_flag = &get_flag(\$net_rad, $net_rad_flag, "net", $id, $date, $hour, $min);
	} else {
		$net_rad_flag = "U";
		if ($short_in != $MISSING && $long_in != $MISSING && $short_out != $MISSING  && $long_out != $MISSING) {
    		   $net_rad = $short_in + $long_in - $short_out - $long_out;   		# down_short + down_long - up_short - up_long
   	 	   $net_rad = $MISSING if($net_rad == 0 || sprintf("%8.2f", $net_rad) == -0.00);
		} else {
		   $net_rad = $MISSING;
		} # endif

		if ($net_rad == $MISSING) {
			$net_rad_flag = "M";
		} else {
			$net_rad_flag = "D" if($short_in_flag eq "D" || $long_in_flag eq "D" || $short_out_flag eq "D" || $long_out_flag eq "D");
			$net_rad_flag = "B" if($short_in_flag eq "B" || $long_in_flag eq "B" || $short_out_flag eq "B" || $long_out_flag eq "B");
			$net_rad_flag = "G" if($short_in_flag eq "G" && $long_in_flag eq "G" && $short_out_flag eq "G" && $long_out_flag eq "G");
                      # apply the corrections per Scot 03/2009
                      #$net_rad_flag = &apply_net_rad_corrections( $id, $net_rad, $net_rad_flag, $date, $hour,$min);
                      $net_rad_flag = &get_post_analysis_flag( $id, $category, $net_rad_flag, $date, $hour,$min);
		} # endif
	} # endif

    # does the value fit the specified format??
    if ( &value_too_large($net_rad, 8) ) {
      print "net_rad value: $net_rad is too large\n" if $DEBUG;
      $net_rad = $MISSING;
      $net_rad_flag = "C";
    } # endif

    # apply the corrections per Scot 03/2009
    #$net_rad_flag = &apply_net_rad_corrections( $id, $net_rad, $net_rad_flag, $date, $hour,$min);

    printf OUTFILE_SFC (" %8.2f", $net_rad);
    printf OUTFILE_SFC (" %s", $net_rad_flag);


    #------------------------------------------------------------
    # Print out skin temperature
    #------------------------------------------------------------ 
         
    
    $skintemp -= 273.15;			# convert to Celsius
    $category = $sfc_obs[16];
    $field_name = &find_field_name( $category, $id);
    $skintemp_flag = &get_flag(\$skintemp, $skintemp_flag, $field_name, $id, $date, $hour, $min);
    $skintemp_flag = &get_post_analysis_flag( $id, $category, $skintemp_flag, $date, $hour,$min);
    #$skintemp_flag = &get_flag(\$skintemp, $skintemp_flag, "sfc_ir_temp", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $skintemp);
    printf OUTFILE_SFC (" %s", $skintemp_flag);

    #------------------------------------------------------------
    # Print out the incoming PAR
    #------------------------------------------------------------ 
          
    $field_name = &find_field_name( $sfc_obs[17], $id);
    $par_in_flag = &get_flag(\$par_in, $par_in_flag, $field_name, $id, $date, $hour, $min);
    #$par_in_flag = &get_flag(\$par_in, $par_in_flag, "par_in", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $par_in);
    printf OUTFILE_SFC (" %s", $par_in_flag);

    #------------------------------------------------------------
    # Print out the outgoing PAR
    #------------------------------------------------------------ 
          
    $field_name = &find_field_name( $sfc_obs[18], $id);
    $par_out_flag = &get_flag(\$par_out, $par_out_flag, $field_name, $id, $date, $hour, $min);
    #$par_out_flag = &get_flag(\$par_out, $par_out_flag, "par_out", $id, $date, $hour, $min);
    printf OUTFILE_SFC (" %8.2f", $par_out);
    printf OUTFILE_SFC (" %s", $par_out_flag);

    #------------------------------------------------------------
	# finish line with a line feed
    #------------------------------------------------------------
    print OUTFILE_SFC ("\n");

}


#------------------------------------------------------------
# mapping of ARM flags to the JOSS QCF flags:
#
#		0 - failed no checks - G
#		1 - missing	- M
#		2 - minimum	- D
#		3 - missing/min	- B
#		4 - maximum - D
#		5 - missing/max - B
#		7 - missing/min/max - B
#		8 - delta - D
#		9 - missing/delta - B
#		10 - min/delta - B
#		11 - missing/min/delta - B
#		12 - max/delta - B
#		14 - min/max/delta - B
#		15 - missing/min/max/delta - B
#------------------------------------------------------------

sub get_flag {

   my $obs_ref = shift;
   my $obs_value = $$obs_ref;
   my $flag_val = shift;
   my $var = shift;
   my $id = shift;
   my $date = shift;			# like 2003/04/16
   my $hour = shift;
   my $min = shift;

   #print "flag value for $date $hour:$min $var is $flag_val\n";

   # first, get the missing values
   if(($obs_value == -999.99) || ($obs_value == $missing_obs)) {
      $$obs_ref = $MISSING;
      return ("M");  
   } elsif (($obs_value < -999.99) || ($obs_value > 9999.99)) {
      $$obs_ref = $MISSING;
      return ("C");  
   } elsif ( $flag_val == -99 ) {
      print ("setting value to $MISSING and flag to 'M' at $var ($id) at $date $hour:$min..\n") if ($DEBUG1);
      $$obs_ref = $MISSING;
      return ("M");
   } # endif

   my %flag;
   $flag{'0'} = "G";	# failed, no checks
   $flag{'1'} = "M";	# missing
   $flag{'2'} = "D";	# minimum
   $flag{'3'} = "B";	# missing/min
   $flag{'4'} = "D";	# maximum
   $flag{'5'} = "B";	# missing/max
   $flag{'6'} = "U";	# unchecked
   $flag{'7'} = "B";	# missing/min/max
   $flag{'8'} = "D";	# delta
   $flag{'9'} = "B";	# missing/delta
   $flag{'10'} = "B";	# min/delta
   $flag{'11'} = "B";	# missing/min/delta
   $flag{'12'} = "B";	# max/delta
   $flag{'13'} = "U"; 	# unchecked	
   $flag{'14'} = "B";	# min/max/delta
   $flag{'15'} = "B";	# missing/min/max/delta

   $new_flag = $flag{$flag_val};

   #return $new_flag;

   # now, apply the flags from the dqr file
   $date =~ /(\d{4})\/(\d{2})\/(\d{2})/;
   # convert date to mm/dd/yyyy
   $date = "$2/$3/$1";
   # convert time to hhmm
   my $time = sprintf ("%02d%02d", $hour, $min);
   # get the epoch time for the date/time
   my $timestamp = DQR::Flag->convert_to_epoch($date, $time); 
   # make sure the precip name is the one found
   # in the DQR file
   if ( $param_index{$id}{$var} == 9 ) {
     $var = fetch_dqr_precip_var($id);
   } # endif
   my $flag_obj = $flag_list->find( $id, $var, $timestamp );
   if ( $flag_obj != -1 ) {
      #print "found flag for: $id $var $date $time and it is ".$flag_obj->value()."\n" if $DEBUG;
      $new_flag = $flag_obj->value();
   } # endif

   return $new_flag;

}

sub gget_flag
{

   my $obs_ref = shift;
   my $flag_val = shift;
   my $var = shift;
   my $id = shift;
   my $date = shift;			# like 2003/04/16
   my $hour = shift;
   my $min = shift;

   local $obs_value = $$obs_ref;
   my $new_flag = "U";

   my $our_year	= substr($date, 0, 4);
   my $our_month 	= substr($date, 5, 2);
   my $our_day 	= substr($date, 8, 2);
   my $our_time 	= sprintf("%02d%02d", $hour, $min) * 0.0001;
   my $datetime = ($our_year.$our_month.$our_day) + $our_time;			# so 1/1/03 1230 = 20030101.123

   print "   in get_flag, obs_ref = $$obs_ref, flag_val = $flag_val, obs_value = $obs_value, id = $id, date = $date, this time = $hour:$min, our time = $our_time, date time = $datetime\n" if($DEBUG1);
   if(($obs_value == -999.99) || ($obs_value == $missing_obs)) {
      $$obs_ref = $MISSING;
      return ("M");  
   } elsif (($obs_value < -999.99) || ($obs_value > 9999.99)) {
      $$obs_ref = $MISSING;
      return ("C");  
   } elsif ($flag_val == 0) {
      $new_flag = "G";
   } elsif ($flag_val == 1) {
     $$obs_ref = $MISSING;
     return ("M");
   } elsif (($flag_val == 2) || ($flag_val == 4) || ($flag_val == 8)) {
      $new_flag = "D";
   } elsif ($flag_val == 3 || ($flag_val == 5) || ($flag_val == 7)) {
      $new_flag = "B";
   } elsif (($flag_val >= 9) && ($flag_val <= 15)) {
      $new_flag = "B";
   } elsif ($flag_val == -99) {
      $new_flag = "U";
   } elsif (($flag_val < 0) || ($flag_val == 6) || ($flag_val == 13) || ($flag_val > 15)) {
      print "Very BAD flag value of $flag_val for $date_str, $hour:$min and $id ID!!!\n"; 
      $new_flag = "U";
   } else {
      die "get flag problem! should not ever get here!\n";
   } # endif

   return $new_flag;

	#---------------------------------------------------------------------------
	# following section fixes flags according to DQRs
	#---------------------------------------------------------------------------
	# C1
	#---------------------------------------------------------------------------
	# skin temperature units change from C to Kelvin at 15:30 on 04 Nov 2004
	#---------------------------------------------------------------------------
	if ($id eq "C1") {
		if ($var eq "sfc_ir_temp") {
           if ($datetime >= 20041104.1530) {
        		$$obs_ref -= 273.15;			# convert to Celsius
		    }
           if ($datetime >= 20031001.0000 && $datetime <= 20031016.0000) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
           if ($datetime >= 20031001.0000 && $datetime <= 20031016.0000) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "snow_depth") {
				$new_flag = "B";
		}
		if ($var eq "wind_spd_mean" || $var eq "wind_dir_vec_avg" || $var eq "u_wind" || $var eq "v_wind") {
           if ($datetime >= 20041213.1030 && $datetime <= 20041231.2330) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "relh_mean") {
           if ($datetime >= 20041023.0245 && $datetime <= 20041102.2325) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "temp_mean") {
           if ($datetime >= 20041023.0245 && $datetime <= 20041102.2325) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "precip_rate") {
           if ($datetime <= 20031013.1946 && $temp_air < 25) { 
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040513.0000 && $datetime <= 20040518.0000) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20040513.0000 && $datetime <= 20040518.0000) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
	}

	#---------------------------------------------------------------------------
	# C2
	#---------------------------------------------------------------------------
	# skin temperature units change from C to Kelvin at 22:00 on 16 Nov 2004
	#---------------------------------------------------------------------------
	if ($id eq "C2") {
		if ($var eq "sfc_ir_temp") {
           if ($datetime >= 20041116.2200) {
        		$$obs_ref -= 273.15;			# convert to Celsius
		    }
		}
        if (($datetime >= 20040917.0130 && $datetime <= 20040917.0411) ||
            ($datetime >= 20040917.2217 && $datetime <= 20040917.0047) ||
            ($datetime >= 20040918.2324 && $datetime <= 20040919.0037)) {
			$new_flag = "B";
			print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
	    }
		if ($var eq "precip_rate") {
           if (($datetime >= 20040918.0048 && $datetime <= 20040918.2323) ||
              ($datetime >= 20040919.0037 && $datetime <= 20040919.1530)) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "up_long_hemisp") {
           if ($datetime >= 20040115.0239 && $datetime <= 20040527.1500) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "down_short_hemisp") {
           if ($datetime >= 20040513.0000 && $datetime <= 20040518.0000) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "down_long_hemisp_shaded") {
           if ($datetime >= 20040513.0000 && $datetime <= 20040518.0000) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "temp_mean" || $var eq "dew_pt_temp_mean" || $var eq "relh_mean" || $var eq "wind_spd_mean" || $var eq "wind_dir_vec_avg" || $var eq "u_wind" || $var eq "v_wind") {
           if ($datetime == 20040709.2130) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "atmos_pressure" || $var eq "temp_mean" || $var eq "relh_mean" || $var eq "wind_spd_mean" || $var eq "wind_dir_vec_avg" || $var eq "u_wind" || $var eq "v_wind") {
           if ($datetime == 20040630.1730 || $datetime == 20040804.1930) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "down_short_hemisp" || $var eq "down_long_hemisp_shaded" || $var eq "net") {
           if ($datetime == 20040413.2130 || $datetime == 20040413.2200) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "precip_rate") {
           if ($datetime == 20040210.2130 || $datetime <= 20040502.2130) {
				$new_flag = "D";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "up_short_hemisp" || $var eq "net") {
           if (($datetime >= 20040403.2230 && $datetime <= 20040404.0130) ||
			  ($datetime == 20040216.2030)) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "temp_mean" || $var eq "relh_mean") {
           if ($datetime >= 20040418.1830 && $datetime <= 20040418.2330) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "atmos_pressure") {
           if ($datetime >= 20031125.2100 && $datetime <= 20031126.1700) {
				$new_flag = "B";
				print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		    }
		}
		if ($var eq "snow_depth") {
			$new_flag = "D";
			print "overrode orig flag = $flag_val with DQR value = $new_flag\n" if($DEBUG1);
		}

	}

	return $new_flag;	
}

#************************
# subroutine to read the DQR file and return
# a reference to an array of DQR::Flag objects
sub read_dqr {

   my $dqr_fname = shift;
   my ($station, $parameter, $flag, $date_begin);
   my ($date_end, $time_begin, $time_end);
   my (@flag_arr);
   open(DQR, "$dqr_fname") || die "cannot read $dqr_fname";

   # read the dqr file
   my $flag_obj = 0;
   my $ii = "";
   while (<DQR> ) {
      chop;
      #if ( !/^Station/ ) {
      if ( !/^Station/ && !/^#/ ) {
         ($station, $parameter, $flag, $date_begin,
          $date_end, $time_begin, $time_end) =
          split(/\s+/, $_);
          # create the flag object
	  if ( &valid_parameter($parameter) ) {
             $flag_obj = DQR::Flag->new($station, $parameter, $flag,
                                        $date_begin, $date_end, $time_begin,
                                        $time_end);

             push(@flag_arr, $flag_obj);
	  } # end if
      } # end if

   } # end while

   return \@flag_arr;

   close(DQR);

} # end read_dqr
#************************
# check parameter name to see if is in the
# list of valid parameters (returns boolean)
sub valid_parameter {

   my $param_name = shift;
   my %valid_fields;
   $valid_fields{$param_name} = 0;
   foreach $key (keys %params ) {
      foreach $field ( @{$params{$key}} ) {
         $valid_fields{$field} = 1;
      } # end foreach
   } # end foreach

   return 1 if ( $valid_fields{$param_name} ); 
   return 0;

}
#************************
# find the parameter name based on the
# station and the sfc_obs value
sub find_field_name {

   my $field_to_find = shift;
   my $station = shift;
   my $i;

   # first, get the index of the sfc obs
   my $count = 0;
   foreach $i (@sfc_obs) {
     if ( $i eq $field_to_find ) {
        last;
     } # endif
     $count++;
   } # end foreach
   
   # now, get the parameter name for this index
   my $ref = $param_index{$station};
   my %hash = %$ref;
   my $key;
   foreach $key (keys %hash) {
      if ( $param_index{$station}{$key}==$count ) {
         return $key;
      }
   }
   return $field_to_find;

}
#************************
# correct the pressure value to reflect hPa from kPa
sub correct_pressure {

  my $value = shift;
  my $date = shift;
  my $hour = shift;
  my $min = shift;
  my $sec = 0;

  # the date/time of the station pressure
  my $actual_date_time = sprintf("%s %02d:%02d:%02d", $date, $hour, $min, $sec);
  # the epoch time for the correction
  #my $actual_epoch_time = &convert_to_epoch ($actual_date_time);
  my $actual_epoch_time = &convert_to_epoch($date, $hour, $min);

  # the date/time for the correction
  my $target_date = "2006/07/06";
  my $target_hour = "21";
  my $target_min = "00";
  #my $target_date_time = "2006/07/06 21:00:00";
  # the epoch time for the correction
  #my $target_epoch_time = &convert_to_epoch( $target_date_time );
  my $target_epoch_time = &convert_to_epoch( $target_date, $target_hour, $target_min);

  if ( $actual_epoch_time >= $target_epoch_time ) {
     # convert from hPa to kPa
     return $value*10;
  } # endif

  return $value;

}
#************************
# convert a date/time (YYYY/MM/DD hh:mm:ss) to
# seconds since 1/1/1970
sub cconvert_to_epoch {

  my $date_time = shift;	# YYYY/MM/DD hh:mm:ss

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
#************************
# check value to make sure it is not
# larger than the specified # of digits
sub value_too_large {

  my $value = shift;
  my $num_digits = shift;

  $value = sprintf("%.2f", $value);

  # first, get the total size of the value 
  my @tmp = split(//, $value);
  my $total = $#tmp+1;

  # value is too big for the format so return true 
  return 1 if ( $total > $num_digits );

  # value fits the format!!!
  return 0;
  
}
#************************
sub fetch_dqr_precip_var {

  # Return the variable name
  # for the precip of the
  # specified station.  This is
  # necessary because the precip name
  # in the DQR file doesn't match
  # the name in the data file.
  my $station = shift;

  my %var_name;

  $var_name{'C1'} = "PcpRate";
  $var_name{'C2'} = "PcpRate";

  return $var_name{$station};

}

