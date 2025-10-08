#!/usr/bin/perl

use DQR::Flag;
use DQR::FlagList;
use strict;

my @dqr_fname;
push(@dqr_fname,"/net/work/CEOP/2005-2007/CPPA/NSA/raw/NSA_GNDRAD_flagging.txt");
push(@dqr_fname,"/net/work/CEOP/2005-2007/CPPA/NSA/raw/NSA_METTWR_flagging.txt");
push(@dqr_fname,"/net/work/CEOP/2005-2007/CPPA/NSA/raw/NSA_SKYRAD_flagging.txt");

my $flag_list = DQR::FlagList->new();

# read the dqr file 
my ($dqr_file, $flag_ref, $flag_count, $i, $flag);
foreach $dqr_file (@dqr_fname) {
   $flag_ref = undef;
   print "processing $dqr_file..\n";
   $flag_ref = &read_dqr( $dqr_file );
   $flag_count = $#{$flag_ref} + 1;
   foreach ($i = 0; $i <= $#{$flag_ref}; $i++) {
      $flag_list->add_flag( $flag_ref->[$i] );
      #print "count is ".$flag_list->count()."\n";
   }
} # end foreach

#my $station_list_ref = $flag_list->station_list();
#foreach $i (@$station_list_ref) {
#   print "station = $i\n";
#}

# make this global variable so it can
# be accessed by the get_flag subroutine
$flag_ref = $flag_list->sort();
my @sorted_arr = @$flag_ref;

my %flag_value;
my ($station, $ts1, $ts2, $ts, $value, $parameter);
my ($p, $d1, $t1, $d2, $t2);
my $id = "C2";
my $p = "T2m_AVG";
my $d = "02/15/2005";
my $t = "2255";
my $ts = DQR::Flag->convert_to_epoch( $d, $t);
my $p = "T2m_AVG";
my $f;
foreach $flag (@sorted_arr) {
   print "looking for flag..\n";
   $station = $flag->station();
   $value = $flag->value();
   $parameter = $flag->parameter();
   $d1 = $flag->date_begin();
   $t1 = $flag->time_begin();
   $d2 = $flag->date_end();
   $t2 = $flag->time_end();
   $ts1 = $flag->convert_to_epoch( $d1, $t1 );
   $ts2 = $flag->convert_to_epoch( $d2, $t2 );
   $f = "";
   if ( $ts >= $ts1 && $ts <= $ts2 ) {
      if ( $id eq $station && $p eq $parameter ) {
         print "setting flag to $value\n";
         $f = $value;
	 last;
      } # endif
   } # endif
   #$flag_value{$station}{$parameter}{$ts1}{$ts2} = $value;
}

print "here i am $f\n";

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

