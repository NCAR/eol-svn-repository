#!/usr/bin/perl

use strict;
use DQR::Flag;
use Getopt::Std;

if ( $#ARGV < 0 ) {
   print "USAGE: read_dqr.pl\n";
   print "\t-i: input dqr file name\n";
   exit();
} 

# fetch the options
my %option = {};
getopts("i:", \%option);
my $dqr_fname = $option{i};

# make sure the filename exists!
die "$dqr_fname doesn't exist" if ( !-e $dqr_fname);
#
# read the dqr file and return a reference 
# to an array of DQR::Flag objects
#my $flag_ref = &read_dqr( $dqr_fname );
my $flag_ref = &sort_by_date( &read_dqr($dqr_fname) );

my $i;
my ($station, $parameter, $value, $date_begin, $date_end, $time_begin, $time_end);
my $count = $#{$flag_ref};
foreach $i (@$flag_ref) {
   print $i->date_begin()."\n";
   #$station = $i->station();
   #print "station: $station\n";
}

#************************
# subroutine to read the DQR file and return
# a reference to an array of DQR::Flag objects
sub read_dqr {

   my $dqr_fname = shift;
   my ($station, $parameter, $flag, $date_begin);
   my ($date_end, $time_begin, $time_end);
   my (@flag_arr);
   open(DQR, "$dqr_fname") || die "cannot read $dqr_fname";

   # read the dqr file
   my $flag_obj = 0;
   while (<DQR> ) {
      chop;
      if ( !/^Station/ ) {
         ($station, $parameter, $flag, $date_begin,
	  $date_end, $time_begin, $time_end) =
	  split(/\s+/, $_);
	  # create the flag object
	  $flag_obj = DQR::Flag->new($station, $parameter, $flag, 
                                     $date_begin, $date_end, $time_begin, 
                                     $time_end);
          push(@flag_arr, $flag_obj);
      } # end if

   } # end while

   return \@flag_arr;

   close(DQR);

} # end read_dqr
#************************
sub sort_by_date {

   my $arr_ref = shift;
   my $hash_ref = {};

   my ($i, $date, $time, $date_time, $key, $value, @arr);
   foreach $i (@$arr_ref) {
      $date = $i->date_begin();
      $time = $i->time_begin();
      $date_time = "$date $time";
      $hash_ref->{$date_time} = $i;
   } # end foreach

   my $count = 0;
   foreach $key (sort(keys %$hash_ref)) {
      $arr[$count++] = $hash_ref->{$key};
   } # end foreach

   return \@arr;

}
#************************
sub sort_by_station {
}
#************************
sub list_of_stations {

   my $arr_ref = shift;
   my (%hash, $i, $station);

   foreach $flag (@$arr_ref) {
      $station = $flag->station();
      $hash{$station} = "";
   } # end foreach

   

}
