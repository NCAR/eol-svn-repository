#!/usr/bin/perl -I/net/work/CEOP/version2/data_processing/other/SGP

# script to create a plot that compares
# the raw data (output from nesobdump) to
# the converted data

use Getopt::Std;
use Time::Local;
use File::Basename;
use strict;
require "conversion.constants.twr.pl";

if ( $#ARGV < 0 ) {
  print "USAGE: create_plot.pl\n";
  print "\t-n: name of network (gndrad, mettwr2h, mettwr4h..etc)\n";
  print "\t-p: parameter name (as found in raw data file)\n";
  print "\t-d: directory where the raw (.dat) files reside\n";
  print "\t-o: name of output file (converted data)\n";
  print "\t-t: time interval (minutes)\n";
  print "\t-i: image directory (where the png files will reside)\n";
  exit();
}

#---------------------------------
# fetch the options
#---------------------------------
my %option = {};
getopt("npdoti:", \%option);
my $network = $option{n};
my $param_name = $option{p};
my $raw_data_dir = $option{d};
my $converted_fname = $option{o};
my $time_interval = $option{t};
my $image_dir = $option{i};

# default time interval
$time_interval = 30 if (!$time_interval);

my ($date_begin, $date_end);
# make sure the converted data file exists
if ( !-e $converted_fname ) {
  print STDERR "ERROR: $converted_fname doesn't exist!!\n";
  exit();
} else {
  # pull out the begin/end date from the output data file
  my $fname_only = basename($converted_fname);
  $fname_only =~ /(\w+)\_(\w+)\_(\w+)\_(\w+)\_(\w+)\_(\w+)\.(\w+)/;
  $date_begin = $5;
  $date_end = $6;
} # endif

# make sure the parameter name is valid
my $params = &params();
my $param_ref = $params->{$network};
my $key;
if ( !$param_ref ) {
  print STDERR "ERROR: $network is not a valid network\n";
  print STDERR "Your choices are:\n";
  foreach $key (keys(%$params)) {
    print "\t$key\n";
  } # end foreach
  exit();
}

# make sure the converted filename exists 
if ( !-e $converted_fname ) {
  print STDERR "ERROR: $converted_fname does not exist!!\n";
  exit();
}

# make sure the output image directory exists
# and is a directory
if ( !-e $image_dir ) {
  print STDERR "ERROR: $image_dir doesn't exist!!\n";
  exit();
}
if ( !-d $image_dir ) {
  print STDERR "ERROR: $image_dir isn't a directory!!\n";
  exit();
}

# get the station from the converted file
my $station = fetch_station($converted_fname);
my $stations = &station();
my $full_station_name = $stations->{$station};

# make sure the network for the
#---------------------------------
# first, create a file that prints a column
# from the raw files
#---------------------------------
# read the files in the raw data directory
# and print the output to a file
my $raw_out_fname = "$param_name.$full_station_name.$date_begin\_$date_end.raw.out";
open(OUT, ">$raw_out_fname") || die "cannot open $raw_out_fname";
close(OUT);
my ($file, $command);
opendir(DIR, $raw_data_dir) || die "cannot open $raw_data_dir";
my @list_of_files = sort(grep(/.dat$/, readdir(DIR)));
my ($file_timestamp);
my $begin_timestamp = convert_timestamp_to_epoch($date_begin."0000");
my $end_timestamp = convert_timestamp_to_epoch($date_end."2359");
print "$station: $param_name\n";
foreach $file (@list_of_files) {
  $file_timestamp = fetch_basetime( "$raw_data_dir/$file" );
  next if ( $file_timestamp < $begin_timestamp || $file_timestamp > $end_timestamp );
  #print "\tprocessing $file..\n";
  $command = "./print_column.raw.pl -i $raw_data_dir/$file -t $time_interval ";
  $command .= "-f $param_name >> $raw_out_fname";
  system($command);

  # make sure the station matches the converted file station
  if ( $file !~ /$station/ ) {
    print STDERR "WARNING: station $station not found in $file\n";
  } # endif
} # end foreaech
closedir(DIR);

#---------------------------------
# next, create a file that prints a column
# from the converted file
#---------------------------------
#my $converted_out_fname = "$param_name.$full_station_name.converted.out";
my $converted_out_fname = "$param_name.$full_station_name.$date_begin\_$date_end.converted.out";
open(OUT, ">$converted_out_fname") || die "cannot open $converted_out_fname";
close(OUT);
my $category = $param_ref->{$param_name}->{category};
my $height = $param_ref->{$param_name}->{height};
$command = "./print_twr_column.converted.pl -i $converted_fname -n $category -h $height >> $converted_out_fname";
system($command);

# finally, create the gnuplot script and run it :-)
my $script_fname = "$param_name.$full_station_name.$date_begin\_$date_end.gnuplot";
open(GNUPLOT, ">$script_fname") || die "cannot open $script_fname";
print GNUPLOT "set timefmt '\%Y\/\%m\/\%d'\n";
print GNUPLOT "set xdata time\n";
#print GNUPLOT "set xlabel '$title' 0,0\n";
print GNUPLOT "set xlabel 'Date' 0,0\n";
print GNUPLOT "set xrange ['".format_date($date_begin)."':'".format_date($date_end)."']\n";
print GNUPLOT "set format x '\%m\/\%d'\n";
print GNUPLOT "set timefmt '\%Y\/\%m\/\%d \%H:\%M'\n";
print GNUPLOT "set ylabel '$category ($param_name)'\n";
print GNUPLOT "set yrange [*:*]\n";
print GNUPLOT "set terminal png\n";
print GNUPLOT "set size 1.5,1.5\n";
print GNUPLOT "set output '$image_dir/$param_name.$full_station_name.$date_begin\_$date_end.png'\n";
#print GNUPLOT "set output '\/net\/www\/homes\/snorman\/ceop\/$param_name.$full_station_name.$date_begin\_$date_end.png'\n";
my $title = fetch_plot_title($date_begin, $date_end, $param_name, $category);
print GNUPLOT "set title '$title'\n";
print GNUPLOT "set datafile missing '-999.99'\n";
print GNUPLOT "plot '$raw_out_fname' using 1:3 title 'raw' with points,\\\n";
print GNUPLOT "'$converted_out_fname' using 1:7 title 'converted' with lines\n";
close(GNUPLOT);

# create the plot
system("gnuplot $script_fname");

# finally, create the html :-)
#************************************
sub fetch_station {

  # parse out the station from the converted filename
  my $fname = shift;

  # make sure it's a converted file, not a raw data file
  if ( $fname =~ /.dat$/ ) {
    print STDERR "ERROR: must be a converted data file, not a raw data file\n";
  } # endif

  my @tmp = split(/\_/, $fname);
  my $station = $tmp[2];

  return $station;

} # end fetch_station
#************************************
sub convert_timestamp_to_epoch {

  # convert a date/time stamp into
  # epoch seconds..input format = YYYYMMDDHHMM
  my $date_time = shift;

  $date_time =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
  my $year = $1;
  my $month = $2;
  my $day = $3;
  my $hour = $4;
  my $min = $5;

  return timegm(0, $min, $hour, $day, $month-1, $year-1900);

}
#************************************
sub fetch_basetime {

  # parse the base time from the specified file
  my $fname = shift;
  my $line;
  open(IN, $fname) || die "cannot open $fname";
  while ($line = <IN>) {
    if ( $line =~ /base_time/) {
      chop($line);
      $line =~ s/\s//g;
      $line =~ /(\w+)(\:)(\d+)(\;)/;
      return $3;
    } # endif
  } # endwhile

  close(IN);

  print STDERR "ERROR: couldn't find base_time for $fname\n";
  exit();


}
#************************************
sub fetch_plot_title {

  my $date_begin = shift;
  my $date_end = shift;
  my $param_name = shift;
  my $category = shift;

  $date_begin = format_date($date_begin);
  $date_end = format_date($date_end);

  return "$param_name ($category) $date_begin - $date_end";

}
#************************************
sub format_date {

  # format the date from YYYYMMDD to YYYY/MM/DD
  my $date = shift;
  $date =~ /(\d{4})(\d{2})(\d{2})/;
  return "$1/$2/$3";

}
