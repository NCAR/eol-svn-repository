#!/usr/bin/perl 

#-------------------------------------------------------
# correct_elev.pl - This perl script/program reads
#   COOP sfc recs and changes elev of -9999.90 to -999.99.
# 97 Dec lec
#   Created.
#-------------------------------------------------------*/

$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncorrect_elev.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].corr\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].corr") || die "Can NOT open output file for reading";

$lines_read = 0;

$corr = 0;

#-----------------------------------------------------------
# For the open file, read all records, reset some corr values
#-----------------------------------------------------------
while ($line = <INFILE>) 
   { 
   $lines_read++;

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $elev = substr ($line,  66, 8);
   $begin_line = substr ($line,  0, 66);
   $end_line   = substr ($line, 74, 81); # COOP sfc recs are 153 chars long.
 
   if ($debug)
      {  
      printf "\n---------\n";
      printf "Read line:\n$line\n";
      printf "elev=: xxx%sxxx\n", $elev;
      printf "begin_line=: xxx%sxxx\n", $begin_line;
      printf "end_line=: xxx%sxxx\n", $end_line;
      }  

   #-------------------------------
   # Begin COOP prcp corr settings.
   #-------------------------------
   if ($elev eq "-9999.90")
      {
      $elev = "-999.99";
      $corr++;
      printf "WARNING: line $lines_read has been corrected.\n";
      }

   printf OUTFILE "%-s%1s%-s", $begin_line,$elev,$end_line;

   if ($debug)
      {
      printf "%-s%1s%-s", $begin_line,$elev,$end_line;
      }

   }  # while data in file


   print "Number of lines corrected: $corr \n";
   print "Total number of lines read: $lines_read\n";
   
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "correct_elev.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
