#!/usr/bin/perl 

#-------------------------------------------------------
# reset_sfc_occ.pl - This perl script/program reads 
#   sfc qcf recs and resets the occ values to zero.
#
# 98 Feb lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nreset_sfc_occ.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
#----------------------------------------
print "Processing input file: $ARGV[0]\n";
print "Output file name is: $ARGV[0].occ\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].occ") || die "Can NOT open output file for reading";

$lines_read = 0;
$occ_reset_ct = 0;

#-----------------------------------------------------------
# For the open file, read all records, reset occ as required.
#-----------------------------------------------------------
while ($line = <INFILE>) 
   { 
   $lines_read++;

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $begin_line = substr ($line,  0,  80);  # sfc qcf
   $end_line   = substr ($line, 83, 173); 
   $occ = substr ($line,  81,  2);

   if ($debug)
      {  
      printf "Read line:\n$line\n";
      printf "occ=: xxx%sxxx\n", $occ;
      }  

   if ($occ != 0)  # Reset occ
      { 
      if ($debug)
         {
         printf "WARNING: $occ resetting occ to ZERO at line $line_read.\n";
         }

      $occ = 0;
      $occ_reset++;
      } # reset values

   #----------------------------
   # Print every line to output.
   #----------------------------
   if ($debug)
      {  
      printf "%-s%3s%-s\n", $begin_line,$occ,$end_line;
      }  
 
   printf OUTFILE "%-s%3s%-s", $begin_line,$occ,$end_line;

   }  # while data in file

   print "Number of lines read: $lines_read.\n";
   print "Number of occ reset to ZERO: $occ_reset.\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "reset_sfc_occ.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
