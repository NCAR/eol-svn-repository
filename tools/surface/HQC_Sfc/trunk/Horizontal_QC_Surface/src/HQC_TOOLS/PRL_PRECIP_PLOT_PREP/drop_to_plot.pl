#!/usr/bin/perl 

#--------------------------------------------------------------
# drop_to_plot.pl - This perl script/program reads Daily PQCF records
#   and prepares the data for gnuplot plot. Puts data in
#   sequential order with day dates. Drops all estimated values.
# 
# 99 Dec lec
#   Created.
# 99 Dec 20 lec
#   If estimated, then put out 0.00. - test
#--------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ndrop_to_plot.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV.plotdat\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].plotdat") || die "Can NOT open output file for reading";

$rec_count = 0;

$est_count = 0;
$tot_est = 0;

$noest_count = 0;
$tot_noest = 0;

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

   print "$line:xxx$linexxx\n";

   for ($i=0;$i<31;$i++)
      {
      $QC_flag = substr( $line, 72+$i*15, 2);

      print "QC flag:", $QC_flag;

      $precip[$i] = substr( $line, 62+$i*15, 7);
 
      printf "precip: $precip[$i]\n";

      if ($QC_flag ne 'E ')
         {
         printf OUTFILE "%-s/%d %7.2f\n", $date_YYYYDD, $i+1, $precip[$i];

         $noest_count++;
         $tot_noest = $tot_noest + $precip[$i];
         }
      else
         {
         $est_count++; 
         $tot_est = $tot_est + $precip[$i];
         printf "QC flag was E - Drop from output: $QC_flag\n";
         }
      }
   }  # while data in file

print "Total number precip values read:", $rec_count,"\n";
print "Average Estimated value:", $tot_est/$est_count,"\n";
print "Average NON-Estimated value:", $tot_noest/$noest_count,"\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "drop_to_plot.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
