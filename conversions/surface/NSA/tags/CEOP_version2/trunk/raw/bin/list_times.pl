#!/usr/bin/perl

use Getopt::Std;
use Time::gmtime;
use Data::Point;
use Data::Parameter;
use strict;

#***************************
# script to read in an output file from nesobdump
# (.dat) and list the data times
# SJS 09/2008
#***************************

if ( $#ARGV < 0 ) {
   # no arguments are specified so show the options
   print "list_times.pl\n";
   print "\t-i: name of input file (.dat)\n";
   print "\t-o: name of output file\n";
   exit();
}

# fetch the options
my %option;
getopt("io:", \%option);
my $in_fname = $option{i};
my $out_fname = $option{o};

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
   $out_fname =~ s/\.dat$/\.out/g;
} # endif

my ($field_name, $field_value, %data, $point, @tmp);
my ($data_point, $timestamp, %parameter);
my ($station, $sta, $value);


# read the input file if it exists
if ( -e $in_fname ) {
   open(IN, $in_fname) || die "cannot open $in_fname";
   while ( <IN> ) {
      chop;
      #if ( !/^netcdf/ && !/^data:/ && !/}$/ ) {
      if ( /time_offset/ || /base_time/ ) {

          # get the parameter name and value
          ($field_name, $field_value) = split(/:/, $_);

	  # get rid of any spaces
	  $field_value =~ s/\s+//g;
	  # get rid of the trailing ';'
	  $field_value =~ s/;$//g;

	  # this is a valid data field to plot
	  my @data_value = split(/,/, $field_value);

	  # add the Parameter object to the parameter array
	  #my $p = Data::Parameter->new($field_name, \@data_value, $type);
	  my $p = Data::Parameter->new($field_name, [], "time_offset");
	  $parameter{$field_name} = $p;

	  # add the Data::Point objects to the Data::Parameter object
          foreach $value (@data_value) {
	    my $data_point = Data::Point->new($value, $timestamp, "U");
	    #print "adding ".$data_point->value()." to ".$p->name()."\n";
	    $p->add_datapoint( $data_point );
	  } # end foreach 

      } # end if

   } # end while
   close(IN);

   # a reference to an array of Data::Point objects
   # for the 'time_offset' field so we can calculate
   # the date/time
   my $data_ref = $parameter{'time_offset'}->data_ref();

   # the constant values
   my $base_time = $parameter{'base_time'}->data_ref()->[0]->value(); # base_time
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

   # get the name of the file
   my @tmp = split(/\//, $in_fname);
   my $fname_only = $tmp[$#tmp];
   open(OUT, ">$out_fname");
   print OUT "netcdf filename: $fname_only:\n";
   print OUT "number of data points: $num_data_points\n";
   for ( $i=0; $i < $num_data_points; $i++ ) {
      printf OUT ("%20s\n", $datetime[$i]);
   } # end for
   close(OUT);

}
#********************************************
