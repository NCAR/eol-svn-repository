#!/usr/bin/perl -I/net/work/CEOP/version2/data_processing/other/NSA/susans_playground

# wrapper script to run create_plot.pl on each parameter
# and then generate the html
# Susan Stringer 07/2009

#./create_plot.pl -n mettwr4h -p AtmPress -d ../../raw/mettwr4h -o ../../out/final/2005-2007/twr/run2/Other_NSA_C1_Barrow_20050101_20051231.twr -t 30 -i /net/www/homes/snorman/ceop

#my $params = &params();
#my @keys = keys (%{$params->{$network}});
#print "testit: $#keys\n";exit();
#print "xxx: ".$params->{$network}->{AtmPress}->{station}."\n";exit();

use Getopt::Std;
use Time::Local;
use strict;
require "conversion.constants.twr.pl";

if ( $#ARGV < 0 ) {
  print "USAGE: create_plots.pl\n";
  print "\t-n: name of network (gndrad, mettwr2h, mettwr4h..etc)\n";
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
getopt("ndoti:", \%option);
my $network = $option{n};
my $raw_data_dir = $option{d};
my $converted_fname = $option{o};
my $time_interval = $option{t};
my $image_dir = $option{i};

#---------------------------------
# the create_plot script
#---------------------------------
my $create_plot_script = "./create_plot.pl";
my $create_html_script = "./create_html.pl";

# now loop through the parameters and run create_plot.pl 
my $params = &params();
my @keys = keys (%{$params->{$network}});
my ($parameter, $station);
foreach $parameter (@keys) {
  # process each data parameter
  next if $parameter =~ /^qc/;
  next if ($parameter eq 'site_id' );
  next if ($parameter eq 'snsor_ht' );
  next if ($parameter eq 'cse_id' );
  next if ($parameter eq 'lat' );
  next if ($parameter eq 'lon' );
  next if ($parameter eq 'alt' );
  next if ($parameter eq 'time_offset' );
  next if ($parameter eq 'station_id' );
  # get the station...it should be the same for all parameters
  $station = $params->{$network}->{$parameter}->{station};
  # run the command to create the plots
  my $cmd = "$create_plot_script -n $network -p $parameter -d $raw_data_dir -o $converted_fname -i $image_dir";
#  print "running $cmd\n";
  system($cmd);
} # end foreach

# now, assemble the options for the html script
my $project_date = &project_dates();
my $date_begin = $project_date->{begin};
$date_begin =~ s/\///g;
my $date_end = $project_date->{end};
$date_end =~ s/\///g;
# get the year (for the output html filename)
$date_begin =~ /(\d{4})(\d{4})/;
my $year = $1;
my $html_fname = "$station.$year.html";
# finally, run the command that creates the composite html file!
my $cmd = "$create_html_script -s $station -p $image_dir -b $date_begin -e $date_end -o $image_dir/$html_fname";
#print "running $cmd\n";
system($cmd);
