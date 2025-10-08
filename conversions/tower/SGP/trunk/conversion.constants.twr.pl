#!/usr/bin/perl

# this is the configuration file for CEOP_twr_refsite_SGP.pl
# the custom corrections are applied in the subroutine:
# apply_corrections

use strict;
use Time::gmtime;

#---------------------------
# region (NSA, TWP...etc)
#---------------------------
my $region = "SGP";

#---------------------------
# hash where the key is the station
# abbreviation (C1,C2..etc) and the 
# value is the station full name
#---------------------------
my %station = (
  'C1' => "C1_Lamont",
);

#---------------------------
# input and output time intervals (min)
#---------------------------
my %time_interval = (
  'input' => 30,        # 30 minute data input
  'output' => 30 	# 30 minute output 
);

#---------------------------
# the number of expected data points
# so we know when we have missing data
#---------------------------
my $num_data_points = 48;

#---------------------------
# the filename pattern so that we
# can pull out metadata from
# the filename (this rarely changes)
#---------------------------
my $filename_pattern = 'sgp(\w+)([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})';

my (%twr10x_fields, @category, $category_name);
#---------------------------
# a hash where the key is the name of the
# parameter as found in the raw data files
# and the value is a reference to a hash that
# contains the metadata associated with the
# parameter:
#  station: station for this parameter
#  height: height for this parameter (-1 for constants) 
#  category: parameter category which is really just 
#            the name of the column 
#  data_format: the data format for this parameter in 
#               the output file
# NOTE: usually the name of the hash matches the name
# of the raw data directory.  The '30' has been
# omitted since perl doesn't allow for variable names
# to begin with a number.  Adjustments has been made
# in CEOP_twr_refsite_SGP.pl to accommodate this change
#---------------------------
$twr10x_fields{'time_offset'} = 
  { 'station'=>'C1', 
    'height'=>-1, 
    'category'=>'time_offset',
    'data_format'=>'null'
  };
$twr10x_fields{'cse_id'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'cse_id',
    'data_format'=>'%-10s'
  };
$twr10x_fields{'site_id'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'site_id',
    'data_format'=>'%-15s'
  };
$twr10x_fields{'station_id'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'station_id',
    'data_format'=>'%-15s'
  };
$twr10x_fields{'snsor_ht'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'snsor_ht',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'lat'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'lat',
    'data_format'=>'%10.5f'
  };
$twr10x_fields{'lon'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'lon',
    'data_format'=>'%11.5f'
  };
$twr10x_fields{'alt'} =
  { 'station'=>'C1',
    'height'=>-1,
    'category'=>'alt',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'missing_stn_pres'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'stn_pres',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_missing_stn_pres'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'qc_stn_pres',
    'data_format'=>'%s'
  };
$twr10x_fields{'missing_wnd_spd'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'wind_spd',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_missing_wnd_spd'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'qc_wind_spd',
    'data_format'=>'%s'
  };
$twr10x_fields{'missing_wind_dir'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'wind_dir',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_missing_wind_dir'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'qc_wind_dir',
    'data_format'=>'%s'
  };
$twr10x_fields{'temp_60m'} =
  { 'station'=>'C1',
    'height'=>60, 
    'category'=>'temp_air', 
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_temp_60m'} =
  { 'station'=>'C1',
    'height'=>60, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$twr10x_fields{'temp_25m'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'temp_air',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_temp_25m'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'qc_temp_air',
    'data_format'=>'%s'
  };
$twr10x_fields{'rh_60m'} =
  { 'station'=>'C1',
    'height'=>60, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_rh_60m'} =
  { 'station'=>'C1',
    'height'=>60, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$twr10x_fields{'rh_25m'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'rel_hum',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_rh_25m'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'qc_rel_hum',
    'data_format'=>'%s'
  };
$twr10x_fields{'missing_dew_pt_60m'} =
  { 'station'=>'C1',
    'height'=>60, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_missing_dew_pt_60m'} =
  { 'station'=>'C1',
    'height'=>60, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
  };
$twr10x_fields{'missing_dew_pt_25m'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'dew_pt',
    'data_format'=>'%7.2f'
  };
$twr10x_fields{'qc_missing_dew_pt_25m'} =
  { 'station'=>'C1',
    'height'=>25, 
    'category'=>'qc_dew_pt',
    'data_format'=>'%s'
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
#   "30twr10x" => \%twr10x,
   "twr10x" => \%twr10x_fields,
);
#---------------------------------
# the directories where the data resides
#---------------------------------
my $base_dir = "/net/work/CEOP/version2/data_processing/cppa/SGP";
#my $base_dir = "/Users/snorman/CEOP/version2/data_processing/other/SGP";
my %data_dir = (
#  '30twr10x' => "$base_dir/raw/30twr10x",
   'twr10x' => "$base_dir/raw/30twr10x",
);

#---------------------------
# the DQR files...these are  the corrections
# as supplied by the data providers
#---------------------------
my %dqr_fname;
$dqr_fname{'30twr10x'} = "$base_dir/raw/SGP_TWR_flagging_2005_2009.txt";
#my @dqr_fname;
#push(@dqr_fname, "$base_dir/raw/SGP_TWR_flagging_2005_2008.txt");

#---------------------------
# the project begin and end dates
#---------------------------
my %project_date = (
  'begin' => '2009/01/01',
  'end' => '2009/12/31',
);

#---------------------------
# the output filename
#---------------------------
my $project_name = "CEOP";
#my $CSE_id       = "Other";
my $CSE_id       = "CPPA";
my $site_id      = "SGP";

#---------------------------
# the subroutine where the custom corrections are
# applied
#---------------------------
sub apply_corrections {

  my $file = shift;
  my $data_point = shift;
  my $missing_value = shift;	# the missing value
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
      # due to a rounding error, the value of the longitude changed
      # so we need to make sure that it's consistent
      $value = -157.4066;
    }
    return $value;
  } # endif

  #----------------------------------------------------- 
  # the corrections for the data point..here
  # we return the corrected data point
  #----------------------------------------------------- 
  return $data_point;  # no corrections for now

}

#***********************************************
# the variables below do not need to be changed
#***********************************************

#-----------------------------------------
# get rid of the / from the project begin/end
#-----------------------------------------
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
sub dqr_fname{ return \%dqr_fname; }
sub num_data_points{ return $num_data_points; }
sub list_of_categories {

  # return a list of categories, which
  # is really just the columns in the
  # data file
  my $ref = \%twr10x_fields;
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
1;
