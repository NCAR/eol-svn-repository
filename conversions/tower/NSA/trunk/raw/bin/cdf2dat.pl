#!/usr/bin/perl

use Getopt::Std;

#***************************************
# script to convert the raw netcdf files
# to .dat files for ingest into the
# conversion software
#***************************************

my $in_fname, $out_fname;
# relative to the raw/cdf/station directory
my $exe_fname = "../../../nesob_dump"; 

# fetch the arguments
my ($date_time1, $date_time2, $data_dir, $variable_list);
if ( $#ARGV < 0 ) {

  # show the argument list to the user
  print "USAGE: cdf2dat.pl\n";
  print "\t-i: input cdf directory\n";
  print "\t-b: begin date/time (YYYY/MM/DD hh:mm)\n";
  print "\t-e: end date/time (YYYY/MM/DD hh:mm)\n";
  print "\t-v: comma separated list of variable names\n";
  exit();

} else {

  # get the command line arguments
  getopt('ibev');
  if ( !-d $opt_i ) {
    print "ERROR: $opt_i is not a directory\n";
    exit();
  } # endif
  if ( !-e $opt_i ) {
    print "ERROR: $opt_i doesn't exist!\n";
    exit();
  } # endif
  if ( !$opt_v ) {
    print "ERROR: must specify variable list (-v)\n";
    exit();
  } # endif
  $data_dir = $opt_i;
  $date_time1 = $opt_b;
  $date_time2 = $opt_e;
  $variable_list = $opt_v;
} # endif

my ($year, $month, $day, $hour, $min, $sec);
$date_time1 =~ /(\d{4})\/(\d{2})\/(\d{2})\s(\d{2}):(\d{2})/;
$year = $1;
$month = $2;
$day = $3;
$hour = $4;
$min = $5;
$sec = $6;
$date_time1 = "$year$month$day\.$hour$min$sec";
$date_time2 =~ /(\d{4})\/(\d{2})\/(\d{2})\s(\d{2}):(\d{2})/;
$year = $1;
$month = $2;
$day = $3;
$hour = $4;
$min = $5;
$sec = $6;
$date_time2 = "$year$month$day\.$hour$min$sec";

# now, read in the netcdf files
opendir(CDF, "$data_dir") || die "cannot open $data_dir";

my @list_of_files = grep {/cdf$/} readdir(CDF);
my ($cdf_file, @tmp, $date, $time, $date_time, $dat_file);
chdir($data_dir);
foreach $cdf_file (sort(@list_of_files)) {
  @tmp = split(/\./, $cdf_file);
  $date = $tmp[2];
  $time = $tmp[3];
  $date_time = "$date\.$time";
  if ( $date_time >= $date_time1 && $date_time <= $date_time2) {
    # run nesob_dump
    $dat_file = $cdf_file;
    $dat_file =~ s/cdf/dat/g;
    my $cmd = "$exe_fname -v $variable_list $cdf_file > $dat_file";
    print "processing $cdf_file\n";
    system($cmd);
  }
} # end foreach
closedir(CDF);
