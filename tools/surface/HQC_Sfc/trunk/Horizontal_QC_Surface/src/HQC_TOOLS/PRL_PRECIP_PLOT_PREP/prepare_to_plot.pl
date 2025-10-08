#!/usr/bin/perl 

#--------------------------------------------------------------
# prepare_to_plot.pl - This perl script/program reads Daily PQCF records
#   and prepares the data for gnuplot plot. Puts data in
#   sequential order with day dates.
# 
# 99 Dec lec
#   Created.
#--------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nprepare_to_plot.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV.plotdat\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].plotdat") || die "Can NOT open output file for reading";

#--------------------------------------
# Count negative zeroes that are found.
#--------------------------------------
$rec_count = 0;

#--------------------------------------
# For the open file, read all records.
# Fix and write to output file.
#--------------------------------------
while ($line = <INFILE>) 
   { 
   $rec_count++;

   if ($debug)
      {
      printf "\nRead line: $line\n";
      }

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------

   $date_YYYYDD = substr ($line, 0, 7);
   $day = 1;

   for ($i=0;$i<31;$i++)
      {
      $precip[$i] = substr( $line, 62+$i*15, 7);

      if ($debug)
         {
         printf "precip: $precip[$i]\n";
         }

      printf OUTFILE "%-s/%d %7.2f\n", 

      $date_YYYYDD, $i+1, $precip[$i];
      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "prepare_to_plot.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
