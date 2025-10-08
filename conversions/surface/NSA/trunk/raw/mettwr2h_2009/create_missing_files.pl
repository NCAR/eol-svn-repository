#!/usr/bin/perl

use Getopt::Std;
use File::Basename;
use Time::Local;
use Time::localtime;

#**************************************
# script to create an empty file if it
# is missing
#**************************************

if ($#ARGV < 0 ) {
  print "Usage: create_missing_files.pl\n";
  print "\t-b: date begin (YYYYMMDD)\n";
  print "\t-e: date end (YYYYMMDD)\n";
  print "\t-s: station(C1 or C2)\n";
  exit();
}

my %option;
getopt("bes", \%option);

my $date_begin = $option{'b'};
my $date_end = $option{'e'};
my $station = $option{'s'};

if (!$date_begin) {
  print "Begin date missing\n";
  exit();
}

if (!$date_end ) {
  print "End date missing\n";
  exit();
}

if ( $date_end < $date_begin ) {
  print "Invalid date range..\n";
  exit();
}

# the time_offset array
my $num_data_points = 1440;
my @arr = (0..$num_data_points-1);
my (@time_offset, $value);
foreach $element (@arr) {
  $value = $element*60;
  push(@time_offset, $value);
} # end foreach
my $time_offset = join(", ", @time_offset);

my ($C1, $C2);
my %hash;
$hash{C1}{lat} = 71.323;
$hash{C1}{lon} = -156.609;
$hash{C1}{alt} = 8;
$hash{C1}{name} = "mettwr4h";
$hash{C1}{in_dir} = "/Users/snorman/CEOP/version2/data_processing/other/NSA/raw/mettwr4h";

$hash{C2}{lat} = 70.4718 ;
$hash{C2}{lon} = -157.407 ;
$hash{C2}{alt} = 20 ;
$hash{C2}{name} = "mettwr2h";
$hash{C2}{in_dir} = "/Users/snorman/CEOP/version2/data_processing/other/NSA/raw/mettwr2h";

my $date_ref = fetch_list_of_dates($date_begin, $date_end);

my ($cdf_fname, $base_time);
my ($year,$month,$day);
foreach $date (@$date_ref) {
  $date =~ /(\d{4})(\d{2})(\d{2})/;
  $year = $1;
  $month= $2;
  $day = $3;
  $base_time = timegm(0,0,0,$day,$month-1,$year-1900);
  #foreach $station (keys(%hash)) {
    my $fname = "nsa$hash{$station}{name}$station.b1.$date.000000.dat";
    my $full_path = "$hash{$station}{in_dir}/$fname";
    if ( !-e $full_path ) {
      print "creating $full_path\n";
      open(DAT, ">$full_path") || die "cannot open $full_path";
      print "adding $full_path..\n";
      print DAT "netcdf $fname\n";
      print DAT "data:\n";
      print DAT "base_time: $base_time ;\n";
      print DAT "time_offset: $time_offset ;\n";
      print DAT "lat: $hash{$station}{lat};\n";
      print DAT "lon: $hash{$station}{lon};\n";
      print DAT "alt: $hash{$station}{alt};\n";
      print DAT "}\n";
      close(DAT);
    }
  #} # end foreach
} # end foreach

sub fetch_list_of_dates {

  # get a list of consecutive dates where the start
  # is the begin date and the end is the end date
  # as defined in the configuration

  my $date_begin = shift;
  my $date_end = shift;

  $date_begin =~ /(\d{4})(\d{2})(\d{2})/;
  my $year1 = $1;
  my $month1 = $2;
  my $day1 = $3;
  my $date1_epoch = timegm(0,0,0,$day1,$month1-1, $year1-1900);
  $date_end =~ /(\d{4})(\d{2})(\d{2})/;
  my $year2 = $1;
  my $month2 = $2;
  my $day2 = $3;
  my $date2_epoch = timegm(0,0,0,$day2, $month2-1, $year2-1900);

  my @list_of_dates;

  my (@ts, @date_arr, $year, $month, $day, @arr, $date);
  for ($i=$date1_epoch; $i<= $date2_epoch; $i+=86400) {
    @ts = gmtime($i);
    $year = $ts[5]+1900;
    $month = $ts[4]+1;
    $day = $ts[3];
    $date = sprintf ("%4d%02d%02d", $year,$month,$day);
    push(@list_of_dates, $date);
  }

  return \@list_of_dates;

}


