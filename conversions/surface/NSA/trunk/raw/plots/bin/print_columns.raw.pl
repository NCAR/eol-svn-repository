#!/usr/bin/perl -I /net/work/CEOP/version2/data_processing/other/NSA/susans_playground

use Getopt::Std;
use Time::gmtime;
use Data::Point;
use Data::Parameter;
use Data::Flag;
require "conversion.constants.pl";
#require "./read_data.constants.pl";
use strict;

# script to read in an output file from nesobdump
# (.dat) and reformat it into a file format so it
# can be plotted with gnuplot
# SJS 07/2008

if ( $#ARGV < 0 ) {
   # no arguments are specified so show the options
   print "print_column.raw.pl\n";
   print "\t-i: name of input file (.dat)\n";
   print "\t-o: name of output file\n";
   print "\t-t: time interval in minutes\n";
   print "\t-f: name of field to output\n";
   exit();
}

# fetch the options
my %option = {};
getopt("iotf:", \%option);
my $in_fname = $option{i};
my $out_fname = $option{o};
my $time_int = $option{t};
my $output_param = $option{f};

# make sure that the user specified the input file
if ( !$in_fname) {
   print "Error: input file option (-i) not set..\n";
   exit();
} # endif

# construct the default output filename
my (@tmp, @full_path, $ext);
if ( !$out_fname ) {
   @full_path = split(/\//, $in_fname);
   $out_fname = $full_path[$#full_path];
   $out_fname =~ s/\.dat$/\.gp/g;
} # endif

# the default time interval
if ( !$time_int ) {
   $time_int = 30;
} # endif

# if no field entered, then default to 'all'
if ( !$output_param ) {
  print "ERROR: must specify a parameter name to print..\n";
  exit();
}

if ( !-e $in_fname ) {
  print "ERROR: $in_fname doesn't exist";
  exit();
}

my ($line, @parameter, $name, $values, $base_time, $timestamp);
my ($network, $station, $region, $param_ref);
open(IN, $in_fname) || die "cannot open $in_fname";

while ( $line = <IN> ) {
  chop($line);
  $line =~ s/\s*//g;
  $line =~ s/\;//g;
  $line =~ s/\}$//g;

  # $name is the parameter name and $values is
  # a comma separated list of values
  ($name, $values) = split(/\:/, $line);
  
  next if $line eq '';
  if ( $line =~ /netcdf/) {
    $line =~ s/^netcdf//g;
    $line =~ /(\w+)(gndrad|mettwr2h|mettwr4h|skyrad)(\w\d)\.\w+/;
    # the name of the array that corresponds to the constants file
    $region = $1;	# ie: nsa, twp..etc..
    $network = $2;	# ie: gndrad, mettwr2h, mettwr4h...etc..
    $station = $3;	# id: C1, C2, C3..etc..
    # reference to the hashes containing field metadata
    $param_ref = &params();	
  } elsif ( $line =~ /^base_time/) {
    # get the base time
    $base_time = $values;
  } elsif ( $line =~ /^time_offset/ ) {
    # fetch a reference to an array of timestamps
    $timestamp = fetch_timestamp($base_time, $line);
  } elsif ( !($line =~ /^(netcdf|data|base_time|time_offset)/) ) {
    # add a Data::Parameter object to an array
    if ( $#$timestamp > -1 ) {
      push(@parameter, create_parameter($param_ref->{$network}, $timestamp, $line));
    } else {
      die "ERROR: could't find any timestamps!\n";
    } # endif
  } # endif
} # end while

close(IN);

# finally, print out the parameter value(s)
if ( $#parameter > -1 ) {
  print_parameter($time_int, $output_param, $timestamp, \@parameter)
} else {
  print "ERROR: no parameters to print!!!\n";
  exit();
} # endif
#********************************************
sub print_parameter {

  # print out the parameter with the specified name

  # the time interval
  my $time_interval = shift;

  # the name of the parameter to print
  my $name_to_find = shift;

  # a reference to the timestamp array
  my $timestamp = shift;

  # a reference to a list of parameter objects
  my $param_list = shift;

  # find the parameter by name
  my $param;
  my $parameter = fetch_parameter($param_list, $output_param);
  if ( !$parameter ) {
    print "ERROR: can't find $output_param\n";
    print "Available parameters for $in_fname are:\n";
    foreach $param (@$param_list) { print $param->name()."\n"; }
    exit();
  }

  # first, find the closest timestamp to minutes='00';
  # return the minute from a timstamp
  my ($i, $j);
  printf ("%s%9s %8s %10s\n", "#", "date", "time", $output_param); 
  for ($i=0; $i <= $#$timestamp; $i++) {
    my $tm = gmtime($timestamp->[$i]);
    my $min = sprintf("%02d",$tm->min);
    if ( $min eq '00' || $min eq $time_int) {
      my $value = $parameter->data_ref->{$timestamp->[$i]}->value();
      #printf ("%02d/%02d/%4d %02d:%02d:%02d ",
      #        $tm->mon+1, $tm->mday, $tm->year+1900,
      #	      $tm->hour, $tm->min, $tm->sec);
      printf ("%4d/%02d/%02d %02d:%02d ",
              $tm->year+1900, $tm->mon+1, $tm->mday, 
      	      $tm->hour, $tm->min);
      printf ("   %7.2f\n", $value);
    }
  } # end for
  exit();

}
#********************************************
sub epoch_to_gmtime {

  my $timestamp = shift;
  my $tm = gmtime($timestamp);
  my $date = sprintf("%02d/%02d/%4d", $tm->mon+1, $tm->mday, $tm->year+1900);
  my $time = sprintf("%02d:%02d:%02d", $tm->hour, $tm->min, $tm->sec);
  return "$date $time";

}
#********************************************
sub create_parameter {

  # return a Data::Parameter object for the  
  # parameter name is found in the specified line 
  # of data ($line variable)

  my $param_ref = shift;

  # reference to an array of time stamps for each data point
  my $timestamp = shift;

  # line as found in the data file
  my $line = shift;

  # split out the name and values from the line of data
  my ($param_name, $param_values) = split(/\:/, $line);

  # the metadata located in the constants file
  my $category = $param_ref->{$param_name}->{category};
  my $height = $param_ref->{$param_name}->{height};

  # now, create a parameter object
  my $parameter = Data::Parameter->new($param_name, {}, $category, $height);

  # fetch the data points
  my @data_points = split(/\,/, $param_values);
  my ($i, $ts, $value);
  my $flag = Data::Flag->new("U");

  # loop through the data points and add them
  # to the parameter object
  for ($i=0; $i <= $#$timestamp; $i++) {
    $ts = $timestamp->[$i];
    $value = $data_points[$i];
    $parameter->add_datapoint(Data::Point->new($value, $ts, $flag));
  } # end for

  # finally, return the parameter 
  return $parameter; 

}
#********************************************
sub fetch_parameter {

  # find the parameter with the specified name
  my $param_ref = shift;
  my $name_to_find = shift;
  my ($name, $parameter);
  foreach $parameter (@$param_ref) {
    my $param_name = $parameter->name();
    if ( $param_name eq $name_to_find ) {
      return $parameter;
    } # endif
  } # end foreach

  return 0;

}
#********************************************
sub fetch_timestamp {

  my $base_time = shift;
  my $line = shift;

  # return a reference to an array of timestamps
  my $ts = [];
  my ($i);

  # split the line into the parameter name 
  # and a list of values (separated by a ,)
  my ($name, $values) = split(/\:/, $line);

  # get the values
  my @list_of_values = split(/\,/, $values);

  # now add the base time to the value to get
  # the timestamp.
  for ($i=0; $i <= $#list_of_values; $i++) {
    $ts->[$i] = $base_time + $list_of_values[$i];
  } # end for

  for ( $i=0; $i <= $#$ts; $i++ ) {
    my $date_time = epoch_to_gmtime($ts->[$i]);
    printf ("%d = %s\n", $ts->[$i], $date_time);
  }

  return $ts;

}
#********************************************
sub fetch_index {
   
  # return the index of the specified parameter name
  my $ref = shift;
  my $name = shift;
  my @arr = @$ref;
  my ($i, $param_name);
   
  for ( $i=0; $i <= $#arr; $i++ ) {
    $param_name = $arr[$i]->name();
    if ( $name eq $param_name ) {
      # yea...found the parameter name!
      return $i;
    } # endif
  } # end for
   
  # name not found..
  return -1;
   
} # end fetch_index

