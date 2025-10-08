#!/usr/bin/perl 

#--------------------------------------------------------------
# fix_neg_zero.pl - This perl script/program reads QCF records
#   and determines if the current rec contains a negative
#   zero value. If so, the negative zero is converted to 
#   a positive zero or just 0.00. This s/w checks the temp,
#   and dew point and precip slots for negative values.
#   If a negative value is found in the precip slot, a 
#   warning is issued, but the value is NOT changed.
# 
# 99 Feb lec
#   Created.
#--------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nfix_neg_zero.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be V95_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 6, 10);
print "Output file name is: ZERO_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">ZERO_$YYMMDD") || die "Can NOT open output file for reading";

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

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $stn  = substr( $line, 30, 14);

   $temp = substr( $line, 122,  7);
   $dew_pt = substr( $line, 132,  7);
   $precip = substr( $line, 162,  7);

   $line_seg1 = substr( $line,   0, 122);
   $line_seg2 = substr( $line, 129,   3); #Temp Flag
   $line_seg3 = substr( $line, 139,  23);
   $line_seg4 = substr( $line, 169,  87);


   if ($debug)
      {  
      printf "\nRead line: $line\n";

      printf "line_seg1: xxx%-sxxx\n", $line_seg1;
      printf "line_seg2: xxx%-sxxx\n", $line_seg2;
      printf "line_seg3: xxx%-sxxx\n", $line_seg3;
      printf "line_seg4: xxx%-sxxx\n", $line_seg4;

      printf "\nstn=: xxx%-sxxx\n", $stn;
      printf "temp=: %-s\n", $temp; 
      printf "dew_pt=: %-s\n", $dew_pt; 
      printf "precip=: %-s\n", $precip; 
      }  

   if ($temp eq "  -0.00")
      {
   #   if ($debug)
   #      {
         printf "Found station with -0.00 temp ($temp) at rec $rec_count.\n";
   #      }
      $temp = 0.00;
      }

   if ($dew_pt eq "  -0.00")
      {
#      if ($debug)
#         {
         printf "Found station with -0.00 dew_pt ($dew_pt) at rec $rec_count.\n";
#         }
      $dew_pt = 0.00;
      }

   if ($precip eq "  -0.00")
      {
      printf "Error:  Found station with -0.00 PRECIP ($precip) at rec $rec_count. Value NOT changed.\n";
      }


   printf OUTFILE "%-s%7.2f%-s%7.2f%-s%7.2f%-s", $line_seg1,$temp,$line_seg2,
        $dew_pt,  $line_seg3, $precip, $line_seg4;


   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "fix_neg_zero.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
