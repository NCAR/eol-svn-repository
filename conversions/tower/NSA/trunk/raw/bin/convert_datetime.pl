#!/usr/bin/perl -w

#---------------------------------------------------------
# convert_DQR_datetime.pl
#
# Utility to read the text files which Scot put together
# from the ARM DQR reports, and change the date style from
# "mm/dd/yyyy  hhmm" to yyyymmdd.hhmm and in a line ready
# to be read by the "make_flag_code.pl" script. Lines 
# without dates are output unchanged.
#
# Note: Tabs must be changed to spaces in the input file
#       before conversion.
#       
#       Run this s/w by feeding it filenames from a
#       shell script e.g. run_datetime.sh 
#
# 1 April 04, ds
#
# rev 25 Oct 05, ds
#   added arguments to command line for filenames
#
# rev 29 May 08, ss
#   updated to reflect changes to the input file format
#---------------------------------------------------------

if (@ARGV) {                             # read in filenames on command line
    $infile = $ARGV[0];
    $outfile = "../code_frag/$ARGV[1]";
} else {
    print "\n\tSyntax: $0 <input filename> <output filename>\n\n";
    exit (1);
}

print "\n  output will be put into $outfile\n\n";

open (INFILE, $infile) || die "Can't open $infile";
open (OUTFILE, ">$outfile") || die "Can't open $outfile";

my $prev_station_id = "";

my @list_of_flags;
my @sorted_list_of_flags;
while ( <INFILE> ) {
   chop;
   if ( !/^Station/ ) {
     push(@list_of_flags, $_);
   } # endif
} # end while

@sorted_list_of_flags = sort(@list_of_flags);

foreach $i (sort(@list_of_flags)) {
   my ($station_id, $parameter, $flag, $begin_date, $begin_time,
       $end_date, $end_time, $dqr_id) = split(/\s+/, $i);
   # convert the date/time format
   $begin_datetime = &date_time( $begin_date, $begin_time );
   $end_datetime = &date_time( $end_date, $end_time );

   # now print the station id
   if ( $station_id ne $prev_station_id ) {
      print OUTFILE "\n$station_id\n";
   } # endif
   $prev_station_id = $station_id;

   # the parameter and flag
   print OUTFILE "\n$parameter ($flag)\n";

   # the datetime boundaries
   print OUTFILE "   if ( \$datetime >= $begin_datetime && \$datetime <= $end_datetime ) ||\n";
} # end foreach
#exit();
while ( <INFILE> ) {
   chop;
   my ($station_id, $parameter, $flag, $begin_date, $begin_time,
       $end_date, $end_time, $dqr_id) = split(/\s+/);
   if ( $begin_date =~ /\d+/ ) {
      # convert the date/time format
      $begin_datetime = &date_time( $begin_date, $begin_time );
      $end_datetime = &date_time( $end_date, $end_time );

      # now print the station id
      if ( $station_id ne $prev_station_id ) {
         print OUTFILE "\n$station_id\n";
      } # endif
      $prev_station_id = $station_id;

      # the parameter and flag
      print OUTFILE "\n$parameter ($flag)\n";

      # the datetime boundaries
      print OUTFILE "   if ( \$datetime >= $begin_datetime && \$datetime <= $end_datetime ) ||\n";
   } # endif

} # endwhile

#***** subroutines *****
sub date_time() {

   $date = $_[0];
   $time = $_[1];

   # parse out the month, day and year
   $date =~ /(\d{2})\/(\d{2})\/(\d{4})/;

   # return the datetime in the format:
   # YYYYMMDD.hhmm
   return "$3$1$2.$time";

}
close INFILE;
close OUTFILE;

