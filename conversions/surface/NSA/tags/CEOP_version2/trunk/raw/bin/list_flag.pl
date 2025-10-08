#!/usr/bin/perl

use Getopt::Std;
use Time::gmtime;
use Data::Point;
use Data::Parameter;
require "./read_data.constants.pl";
use strict;

# script to list the value and flags for a specified
# file and parameter
# SJS 07/2008

if ( $#ARGV < 0 ) {
   # no arguments are specified so show the options
   print "read_data.pl\n";
   print "\t-i: name of input file (.dat)\n";
   print "\t-p: name of parameter\n";
   print "\t-t: time interval in minutes\n";
   exit();
}

# fetch the options
my %option = {};
getopt("ipt:", \%option);
my $in_fname = $option{i};
my $param_name = $option{p};
my $qc_param_name = "qc_$param_name";
my $time_int = $option{t};

# make sure that the user specified the input file
if ( !$in_fname) {
   print "Error: input file option (-i) not set..\n";
   exit();
} # endif

# the default time interval
if ( !$time_int ) {
   $time_int = 30;
} # endif

# convert the time interval to seconds since
# the data is in seconds
$time_int *= 60;

my ($field_name, $field_value, %data, $point, @tmp);
my ($data_point, $timestamp, %parameter);
my ($station, $sta, $value);

my $fname;
if ( -e $in_fname ) {

   open(IN, $in_fname) || die "cannot open $in_fname";

   while ( <IN> ) {
      chop;
      if ( /^netcdf/ ) {

         # get the station 
         s/netcdf //g;
	 my ($network, $id, $date, $time) = split(/\./, $_);
	 @tmp = split(//, $network);
	 $station = join("", splice(@tmp, $#tmp-1, $#tmp));
	
      } elsif ( !/^netcdf/ && !/^data:/ && !/}$/ ) {

          # get the parameter name and value
          ($field_name, $field_value) = split(/:/, $_);

	  # get rid of any spaces
	  $field_value =~ s/\s+//g;
	  # get rid of the trailing ';'
	  $field_value =~ s/;$//g;

          # get the mapping between the parameter names   
	  # and the type (see ./read_data.constants.pl)
	  #my $type = &get_type($field_name);

	  # set this to 'unknown' so the qc field 
	  # can be processed
	  my $type = "unknown";

	  if ( defined $type ) {

	     #print "processing field_name: $field_name\n";

	     # this is a valid data field to plot
	     my @data_value = split(/,/, $field_value);

	     # add the Parameter object to the parameter array
	     my $p = Data::Parameter->new($field_name, [], $type);
	     $parameter{$field_name} = $p;

	     # add the Data::Point objects to the Data::Parameter object
             foreach $value (@data_value) {
	        my $data_point = Data::Point->new($value, $timestamp, "U");
	        #print "adding ".$data_point->value()." to ".$p->name()."\n";
	        $p->add_datapoint( $data_point );
	     } # end foreach 

	  } # endif

      } # end if

   } # end while
   close(IN);

   # a reference to an array of Data::Point objects
   # for the 'time_offset' field so we can calculate
   # the date/time
   # the constant values
   my $base_time = $parameter{'base_time'}->data_ref()->[0]->value(); # base_time

   # a reference to an array of Data::Point objects for the time_offset field
   my $data_ref = $parameter{'time_offset'}->data_ref();

   # the number of Data::Point objects
   my $num_data_points = $#{$data_ref}+1;

   # convert the epoch seconds to date/time
   my ($i, $value, @datetime, $epoch_time, $tm);
   my ($key, $year, $month, $day, $hour, $min);
   for ( $i=0; $i < $num_data_points; $i++ ) {
      $value = $data_ref->[$i]->value();
      $epoch_time = $base_time + $value;
      $tm = gmtime($epoch_time);
      $year = $tm->year + 1900;
      $month = $tm->mon+1;
      $day = $tm->mday;
      $hour = $tm->hour;
      $min = $tm->min;
      $datetime[$i] = sprintf ("%4d/%02d/%02d %02d:%02d", $year,$month,$day,$hour,$min);
      print "$value = $datetime[$i]\n";
   } # end for

   for ($i=0; $i <= $#datetime; $i++) {
      print "test: $datetime[$i]\n";
   }
   exit();

   # finally, print out the values/flags for the specified parameter
   printf ("%20s %10s %4s\n", "DATE/TIME", "VALUE", "FLAG");
   my ($value, $flag);
   for ($i=0; $i < $num_data_points; $i++ ) {
      $value = $parameter{$param_name}->data_ref()->[$i]->value();
      $flag = $parameter{$qc_param_name}->data_ref()->[$i]->value();
      printf ("%20s", $datetime[$i]);
      printf ("%10.2f", $value);
      printf (" %4s", $flag);
      print "\n";
   } # end for

} # end if
