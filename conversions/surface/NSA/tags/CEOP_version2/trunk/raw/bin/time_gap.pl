#!/usr/bin/perl

use Getopt::Std;
use Time::gmtime;
use strict;

# script to detect any time gaps in a .dat file

if ( $#ARGV < 0 ) {
   # no arguments are specified so show the options
   print "time_gap.pl\n";
   print "\t-i: name of input file (.dat)\n";
   exit();
}

# fetch the options
my %option = {};
getopt("i:", \%option);
my $in_fname = $option{i};

# make sure that the user specified the input file
if ( !$in_fname) {
   print "Error: input file option (-i) not set..\n";
   exit();
} # endif

my $time_interval = 60;  # time interval in data (seconds)
my ($field_name, $field_value, @time_offset, $base_time, $i);
my ( $value, $epoch_time, $tm, $year, $month, $day, $hour, $min, @datetime );

# read the input file if it exists
if ( -e $in_fname ) {

   my @tmp = split(/\//, $in_fname);
   my $fname_only = $tmp[$#tmp];

   open(IN, $in_fname) || die "cannot open $in_fname";

   while ( <IN> ) {

      chop;
      ($field_name, $field_value) = split(/:/, $_);
      # get rid of any spaces
      $field_value =~ s/\s+//g;
      # get rid of the trailing ';'
      $field_value =~ s/;$//g;

      if ( $field_name eq "base_time" ) {

         # the base time
         $base_time = $field_value;

      } elsif ( $field_name eq "time_offset" ) {

	  # stuff the values into an array
	  my @time_offset = split(/,/, $field_value);
	  #print "$in_fname: there are $#time_offset elements for $field_name..\n";
	  
	  # now, calculate the timestamp for each element
	  for ( $i=0; $i <= $#time_offset; $i++ ) { 
             $value = $time_offset[$i];
             $epoch_time = $base_time + $value;
             $tm = gmtime($epoch_time);
             $year = $tm->year + 1900;
             $month = $tm->mon+1;
             $day = $tm->mday;
             $hour = $tm->hour;
             $min = $tm->min;
             $datetime[$i] = sprintf ("%4d/%02d/%02d %02d:%02d", $year,$month,$day,$hour,$min);
	  } # end for

          # now, check to see if there is a time gap
          for ($i=0; $i < $#time_offset; $i++ ) {
             my $diff = $time_offset[$i+1]-$time_offset[$i];
	     if ( $diff != $time_interval ) {
	        print "$fname_only: there is a time gap between $datetime[$i] and $datetime[$i+1]\n";
	     } # endif
          } # end for
      
      } # end if

   } # end while

   close(IN);



}
