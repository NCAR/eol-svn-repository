#!/usr/bin/perl

use Getopt::Std;
use Time::gmtime;
use Data::Point;
use Data::Parameter;
use Data::Flag;
require "./read_data.constants.pl";
use strict;

# script to read in an output file from nesobdump
# (.dat) and reformat it into a file format so it
# can be plotted with gnuplot
# SJS 07/2008

if ( $#ARGV < 0 ) {
   # no arguments are specified so show the options
   print "read_data.pl\n";
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
  $output_param = 'all';
}

# convert the time interval to seconds since
# the data is in seconds
$time_int *= 60;

my ($field_name, $field_value, %data, $point, @tmp);
my ($data_point, $timestamp, %parameter);
my ($station, $sta, $value, $base_time);

# read the input file if it exists
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
	  my $type = &get_type($field_name);

	  if ( defined $type ) {

	     print "processing field_name: $field_name\n";

	     # this is a valid data field to plot
	     my @data_value = split(/,/, $field_value);

	     # add the Parameter object to the parameter array
	     #my $p = Data::Parameter->new($field_name, \@data_value, $type);
	     #my $p = Data::Parameter->new($field_name, [], $type);
	     my $p = Data::Parameter->new($field_name, {}, $type);
	     $parameter{$field_name} = $p;

	     # add the Data::Point objects to the Data::Parameter object
	     if ( $type eq 'base_time' ) {
	       $base_time = $data_value[0];
	     } else {
               foreach $value (@data_value) {
  	         $timestamp = $base_time + $value if ( $type eq 'time_offset');
                 my $flag = Data::Flag->new("U");
  	         my $data_point = Data::Point->new($value, $timestamp, $flag);
  	         #print "adding ".$data_point->value()." to ".$p->name()."\n";
  	         $p->add_datapoint( $data_point );
  	       } # end foreach 
	     } # endif

	  } # endif

      } # end if

   } # end while
   close(IN);

   # a reference to an array of Data::Point objects
   # for the 'time_offset' field so we can calculate
   # the date/time
   my $data_ref = $parameter{'time_offset'}->data_ref();

   # the constant values
   #my $base_time = $parameter{'base_time'}->data_ref()->[0]->value(); # base_time
   my $lat = $parameter{'lat'}->data_ref()->[0]->value(); # latitude
   my $lon = $parameter{'lon'}->data_ref()->[0]->value(); # longitude
   my $alt = $parameter{'alt'}->data_ref()->[0]->value(); # altitude

   # a reference to an array of Data::Point objects for the time_offset field
   my $data_ref = $parameter{'time_offset'}->data_ref();

   # the number of Data::Point objects
   my $num_data_points = $#{$data_ref}+1;

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
   } # end for

   # open the output file
   #open(OUT, ">>$out_fname") || die "cannot open $out_fname for writing..";

   # first, print the header
   my ($column_name, @columns);
   my @columns = sort(keys(%parameter) );
   # add the datetime as the first column
   #unshift(@columns, "date_time");
   #unshift(@columns, "station");
   print ("#");
   printf ("%19s", "station");
   printf ("%20s", "date_time");
   foreach $column_name (@columns) {
      printf ("%20s", $column_name);
   } # end foreach
   print "\n";

   my ($i, $j);
   for ( $i=0; $i < $num_data_points; $i++ ) {
      my $time_offset = $parameter{'time_offset'}->data_ref()->[$i]->value();
      if ( ($time_offset % $time_int) == 0 ) {
         printf ("%20s", $station);
         printf ("%20s", $datetime[$i]);
         foreach $column_name (@columns) {
            $j = $i;
            $j = 0 if ( $column_name eq 'base_time'); 
            $j = 0 if ( $column_name eq 'lat'); 
   	    $j = 0 if ( $column_name eq 'lon');
            $j = 0 if ( $column_name eq 'alt');
            printf ("%20s", $parameter{$column_name}->data_ref()->[$j]->value());
         } # end foreach
         print "\n";
      } # end if
   } # end for

   #close(OUT);
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

