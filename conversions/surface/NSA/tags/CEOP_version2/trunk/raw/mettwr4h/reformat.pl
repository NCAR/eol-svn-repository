#!/usr/bin/perl

use Getopt::Std;
use File::Basename;
use Switch;

#**************************************
# script to change the variable names to match those
# of previous years...we need to do this since the
# variable names have changed at different times
# for each station (sigh...) 
#**************************************
# options:
#       -i: name of input (.dat) file
#       -o: name of output (.dat) file
# SJS 01/2009
#**************************************
my $xx = 4%2;
#print "xx: $xx\n";
#exit();

if ($#ARGV < 0 ) {
  print "Usage: reformat.pl\n";
  print "\t-i: name of the input file (.dat)\n";
  exit();
}

my %option;
getopt("i", \%option);

my $in_fname = $option{'i'};

if ( !-e $in_fname ) {
  print "Oops...$in_fname doesn't exist";
  exit();
}

print "processing $in_fname\n";

my @static_vars = ("base_time", "time_offset", "lat", "lon", "alt", "height");
my %hash;

# C1
$hash{'temp_mean'}{'C1'}{'2'} = "T2M_AVG";
$hash{'qc_temp_mean'}{'C1'}{'2'} = "qc_T2M_AVG";
$hash{'temp_mean'}{'C1'}{'10'} = "T10M_AVG";
$hash{'qc_temp_mean'}{'C1'}{'10'} = "qc_T10M_AVG";
$hash{'temp_mean'}{'C1'}{'20'} = "T20M_AVG";
$hash{'qc_temp_mean'}{'C1'}{'20'} = "qc_T20M_AVG";
$hash{'temp_mean'}{'C1'}{'40'} = "T40M_AVG";
$hash{'qc_temp_mean'}{'C1'}{'40'} = "qc_T40M_AVG";

$hash{'rh_mean'}{'C1'}{'2'} = "RH2M_AVG";
$hash{'qc_rh_mean'}{'C1'}{'2'} = "qc_RH2M_AVG";
$hash{'rh_mean'}{'C1'}{'10'} = "RH10M_AVG";
$hash{'qc_rh_mean'}{'C1'}{'10'} = "qc_RH10M_AVG";
$hash{'rh_mean'}{'C1'}{'20'} = "RH20M_AVG";
$hash{'qc_rh_mean'}{'C1'}{'20'} = "qc_RH20M_AVG";
$hash{'rh_mean'}{'C1'}{'40'} = "RH40M_AVG";
$hash{'qc_rh_mean'}{'C1'}{'40'} = "qc_RH40M_AVG";

$hash{'dew_point_mean'}{'C1'}{'2'} = "DP2M_AVG";
$hash{'qc_dew_point_mean'}{'C1'}{'2'} = "qc_DP2M_AVG";
$hash{'dew_point_mean'}{'C1'}{'10'} = "DP10M_AVG";
$hash{'qc_dew_point_mean'}{'C1'}{'10'} = "qc_DP10M_AVG";
$hash{'dew_point_mean'}{'C1'}{'20'} = "DP20M_AVG";
$hash{'qc_dew_point_mean'}{'C1'}{'20'} = "qc_DP20M_AVG";
$hash{'dew_point_mean'}{'C1'}{'40'} = "DP40M_AVG";
$hash{'qc_dew_point_mean'}{'C1'}{'40'} = "qc_DP40M_AVG";

$hash{'wspd_vec_mean'}{'C1'}{'2'} = "WS2M_U_WVT";
$hash{'qc_wspd_vec_mean'}{'C1'}{'2'} = "qc_WS2M_U_WVT";
$hash{'wspd_vec_mean'}{'C1'}{'10'} = "WS10M_U_WVT";
$hash{'qc_wspd_vec_mean'}{'C1'}{'10'} = "qc_WS10M_U_WVT";
$hash{'wspd_vec_mean'}{'C1'}{'20'} = "WS20M_U_WVT";
$hash{'qc_wspd_vec_mean'}{'C1'}{'20'} = "qc_WS20M_U_WVT";
$hash{'wspd_vec_mean'}{'C1'}{'40'} = "WS40M_U_WVT";
$hash{'qc_wspd_vec_mean'}{'C1'}{'40'} = "qc_WS40M_U_WVT";

$hash{'wdir_vec_mean'}{'C1'}{'2'} = "WD2M_DU_WVT";
$hash{'qc_wdir_vec_mean'}{'C1'}{'2'} = "qc_WD2M_DU_WVT";
$hash{'wdir_vec_mean'}{'C1'}{'10'} = "WD10M_DU_WVT";
$hash{'qc_wdir_vec_mean'}{'C1'}{'10'} = "qc_WD10M_DU_WVT";
$hash{'wdir_vec_mean'}{'C1'}{'20'} = "WD20M_DU_WVT";
$hash{'qc_wdir_vec_mean'}{'C1'}{'20'} = "qc_WD20M_DU_WVT";
$hash{'wdir_vec_mean'}{'C1'}{'40'} = "WD40M_DU_WVT";
$hash{'qc_wdir_vec_mean'}{'C1'}{'40'} = "qc_WD40M_DU_WVT";

# C2
$hash{'temp_mean'}{'C2'}{'2'} = "T2m_AVG";
$hash{'qc_temp_mean'}{'C2'}{'2'} = "qc_T2m_AVG";
$hash{'temp_mean'}{'C2'}{'5'} = "T5m_AVG";
$hash{'qc_temp_mean'}{'C2'}{'5'} = "qc_T5m_AVG";

$hash{'rh_mean'}{'C2'}{'2'} = "RH2m_AVG";
$hash{'qc_rh_mean'}{'C2'}{'2'} = "qc_RH2m_AVG";
$hash{'rh_mean'}{'C2'}{'5'} = "RH5m_AVG";
$hash{'qc_rh_mean'}{'C2'}{'5'} = "qc_RH5m_AVG";

$hash{'dew_point_mean'}{'C2'}{'2'} = "DP2m_AVG";
$hash{'qc_dew_point_mean'}{'C2'}{'2'} = "qc_DP2m_AVG";
$hash{'dew_point_mean'}{'C2'}{'5'} = "DP5m_AVG";
$hash{'qc_dew_point_mean'}{'C2'}{'5'} = "qc_DP5m_AVG";

$hash{'dew_point_mean'}{'C2'}{'2'} = "DP2m_AVG";
$hash{'qc_dew_point_mean'}{'C2'}{'2'} = "qc_DP2m_AVG";
$hash{'dew_point_mean'}{'C2'}{'5'} = "DP5m_AVG";
$hash{'qc_dew_point_mean'}{'C2'}{'5'} = "qc_DP5m_AVG";

# get an array of heights
my $height_ref = fetch_heights();
$num_heights = $#$height_ref+1;

# get the station name (C1 or C2)
my $station = fetch_station($in_fname);

my $site_ref = {'C1' => 'mettwr4h', 'C2' => 'mettwr2h'};
my $out_fname = assemble_out_fname($in_fname, $station, $site_ref);

my ($parameter, @param, @values, $from_name, $to_name);
my ($height, $to_name, @find, %data, $data_points, $num_data_points);
my ($data_ref, $found, $line, $value, $num_values);
open(IN, "$in_fname") || die "cannot open $in_fname";

$process_flag = false;
$j = 0;

# read the file and convert the 2d arrays
# into 1d arrays where each array represents
# a given parameter for each height
while (<IN>) {
  $found = 0;
  s/\s+$//;
  if ( /netcdf/ ) {
    $data_ref->{netcdf}->[0] = $_;
    next;
  }
  if ( /data/ ) {
    $data_ref->{data}->[0] = $_;
    next;
  }
  if ( /base_time/ ) {
    $data_ref->{base_time}->[0] = $_;
    next;
  }
  next if (/\}/);
  if ( /:/ ) {
    chop;
    my @tmp = split(/:/, $_);
    $parameter = shift(@tmp);
    # the data that doesn't need to be changed
    $found = grep(/$parameter/, @static_vars);
    $data->{$parameter} = $_ if ( $found );
    if ( $data->{$parameter} =~ /time_offset/ ) {
      my $time_offset = $data->{$parameter};
      @tmp = split(/:/, $time_offset);
      @data_points = split(/,/, $tmp[1]);
      $interval = $data_points[1] - $data_points[0];
      $num_data_points = $data_points[$#data_points]/$interval; 
    } # endif
    $j = 0;
  } # endif
  if ($found == 1) {
    # store the array
    @tmp = split(/:/, $_);
    my @list_of_values = split(/,/, $tmp[1]);
    $data_ref->{$parameter} = \@list_of_values; 
  } else {
    s/$parameter//g;
    s/://g;
    my @list_of_values = split(/,/, $_);
    $list_of_values[$#list_of_values] =~ s/\s+$//; # get rid of any odd characters at end of line 
    $num_values = $#list_of_values+1;
    # only do this if we're actually processing data
    for ($i=0; $i < $num_values; $i++) {
      $tmp_name = "$parameter".";".$height_ref->[$i];
      $data_ref->{"$tmp_name"}->[$j] = $list_of_values[$i];
    }
    $j++;
  } # endif
} # end while
close(IN);

# now, print out the file
open(OUT, ">$out_fname") || die "cannot open $out_fname";
my %data_hash = %$data_ref;
print OUT $data_ref->{netcdf}->[0]."\n";
print OUT "data\:\n";
print OUT $data_ref->{base_time}->[0]."\n";
print OUT "time_offset: ";
my @time_offset_arr = @{$data_ref->{time_offset}};
my $time_offset = join(",", @time_offset_arr);
print OUT "$time_offset;\n";

my %params = %$data_ref;
my @keys = keys(%params);
my @list_of_params;
my ($new_param_name, $new_qc_param_name, @data_arr, $data_str);
foreach $key (@keys) {
  if ( $key =~ /;/ ) {
    next if $key =~ /^qc/;
    # first, print out the parameter
    ($parameter, $height) = split(/;/, $key); 
    $new_param_name = $hash{$parameter}{$station}{$height};
    @data = @{$data_ref->{$key}};
    $data_str = join(",", @data);
    print OUT "$new_param_name: $data_str\n";

    # now, print out the qc parameter
    my $qc_param = "qc_$parameter";
    my $new_qc_param_name = $hash{$qc_param}{$station}{$height};
    @data = @{$data_ref->{"qc_$key"}};
    $data_str = join(",", @data);
    print OUT "$new_qc_param_name: $data_str\n";
  } # endif
} # end foreach
print OUT "lat: ".$data_ref->{lat}->[0].";\n";
print OUT "lon: ".$data_ref->{lon}->[0].";\n";
print OUT "alt: ".$data_ref->{alt}->[0].";\n";
print OUT "}";
close(OUT);
#****************************
sub fetch_station {

  my $fname = shift;

  # get the station name from the filename
  my $fname_only = basename($fname);
  $fname_only =~ /(\w+)([C]\d)(\w+)/;
  my @tmp = split(/\./, $fname_only);
  $tmp[0] =~ /(\w+)([C]\d+)/;
  return $2;
}

sub fetch_heights {

  # find the heights for this station
  # returns an array containing the
  # heights
  open(IN, "$in_fname") || die "cannot open $in_fname";
  while ( <IN> ) {
    chop;
    if ( /^height:/ ) {
      $_ =~ s/height://g;
      $_ =~ s/;$//g;
      $_ =~ s/\s+//g;
      my @tmp = split(/,/, $_);
      return \@tmp;
    } # endif
  } # end while
  close(IN);
}

sub assemble_out_fname {
  my $in_fname = shift;
  my $station = shift;
  my $site_ref = shift;
  my $site = $site_ref->{$station};
  # get the date/time from the filename
  my $fname_only = basename($in_fname);

  my @tmp = split(/\./, $fname_only);
  my $date = $tmp[2];
  my $time = $tmp[3];

  return "nsa$site$station.b1.$date.$time.dat";

}
