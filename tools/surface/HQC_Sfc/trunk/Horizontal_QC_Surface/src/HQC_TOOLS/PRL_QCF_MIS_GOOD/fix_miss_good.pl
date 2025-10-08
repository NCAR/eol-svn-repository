#!/usr/bin/perl 

#--------------------------------------------------------------
# fix_miss_good.pl - This perl script/program reads QCF records
#   and determines if the current rec contains a missing stn pressure
#   for the DATSAV2 data with a mismatch flag other than an I.
#   An I value is correct for DATSAV2 only since the stn pressure
#   is computed from the altimeter and elev values.
# 
# 99 May lec
#   Created.
#--------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nfix_miss_good.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be nomis_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 0, 10);
print "Output file name is: nomis_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">nomis_$YYMMDD") || die "Can NOT open output file for reading";

#--------------------------------------
# Count "-999.99 G", etc. that are found.
#--------------------------------------
$rec_count = 0;
$count_to_I = 0;

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
   $stn  = substr( $line, 30, 7);

   $stnpress = substr( $line, 92,  7);
   $stnflag = substr($line, 100, 1);

   $line_seg1 = substr( $line,   0, 100);
   $line_seg2 = substr( $line, 100,   1); #Temp Flag
   $line_seg3 = substr( $line, 101,  156);

   if ($debug)
      {  
      printf "\nRead line: $line\n";

      printf "line_seg1: xxx%-sxxx\n", $line_seg1;
      printf "line_seg2: xxx%-sxxx\n", $line_seg2;
      printf "line_seg3: xxx%-sxxx\n", $line_seg3;

      printf "\nstn=: xxx%-sxxx\n", $stn;
      printf "stnpress=: xxx%-sxxx\n", $stnpress; 
      printf "stnflag=: xxx%-sxxx\n", $stnflag; 
      }  

   if ($stnpress eq "-999.99"  &&
        ($stnflag ne "I" && $stnflag ne "M" && $stnflag ne "N" && $stnflag ne "X" && $stnflag ne "C"))
      {
      printf "Found MISMATCH for $stn between $stnpress stn_press and $stnflag flag at rec $rec_count.\n";
      }


   if ($stn eq "DATSAV2" and $stnpress eq "-999.99" and $stnflag ne "I")
      {
      printf "Found DATSAV2 stn with $stnpress stn press with $stnflag flag at rec $rec_count. Reset to I.\n";
 
      $line_seg2 = "I";

      $count_to_I++;

      if ($debug)
         {
         printf "line_seg2 =: xxx%-sxxx\n", $line_seg2;
         }

      } #if DATSAV2
   else
      {
      if ($debug)
         {
         printf "Do not reset stnpress flag.\n";
         }
      }

   printf OUTFILE "%-s%-s%-s", $line_seg1,$line_seg2,$line_seg3;

   }  # while data in file

printf "Total Number of Flags reset to I: %d\n", $count_to_I;

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "fix_miss_good.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
