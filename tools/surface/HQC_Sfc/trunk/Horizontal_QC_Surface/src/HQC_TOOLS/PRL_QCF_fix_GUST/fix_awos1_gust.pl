#!/usr/bin/perl 

#--------------------------------------------------------------
# fix_awos1_gust.pl - This perl script/program reads QCF records
#     and fixes the GUST flag versus GUST value on AWOS1 records
#     only. The original AWOS1 conversion s/w should have 
#     comments explaining and fixing this problem. For AWOS1
#     records (only), the conversion s/w can output an 'M' flag
#     with a non-missing value. Another problem that was fixed
#     by another script/program - Same conversion s/w always
#     set a 'G' flag in the gust indicator even when the value
#     for gusts was 0.00 or -999.99 (missing).
# 
# 95 Mar lec
#   Created.
#--------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nFIX_AWOS1_GUST.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be GUST_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 4, 10);
print "Output file name is: GUST_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">GUST_$YYMMDD") || die "Can NOT open output file for reading";

#---------------------------------------------------------
# For the open file, read all records, determine if
# station has 0.00 elevation. Fix and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $stn  = substr( $line, 30, 6); # "AWOS1 "
   $gust = substr( $line, 174,  7);
   $gust_flag = substr( $line, 182, 1);

   if ($debug)
      {  
      printf "\nRead line: $line";
      printf "stn=: %-s\n", $stn; 
      printf "gust=: %f\n", $gust; 
      printf "gust_flag=: xxx%sxxx\n", $gust_flag; 
      }  

   if ($stn eq "AWOS1 ")
      {
      if ($debug)
         {
         printf "Found AWOS1 station.\n";
         }

      if (($gust >= 0.00) && ($gust_flag eq "M"))
         {
         $gust_flag = "U";
         }

      $line_seg1 = substr( $line,   0,  181);
      $line_seg2 = substr( $line, 183,   74);

      if ($debug)
         {
         printf "%-s %1s%-s", $line_seg1,$gust_flag,$line_seg2;
         }
 
      printf OUTFILE "%-s %1s%-s", $line_seg1,$gust_flag,$line_seg2;

      } # AWOS1 
   else
      {
      print OUTFILE "$line"; # write all other input lines to output
      }
   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "FIX_AWOS1_GUST.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
