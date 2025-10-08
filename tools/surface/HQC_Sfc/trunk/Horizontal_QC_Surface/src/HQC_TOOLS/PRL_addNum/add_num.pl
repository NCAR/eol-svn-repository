#!/usr/bin/perl 

#-------------------------------------------------------
# add_num.pl - This perl script/program reads records
#   and places incrementing numbers (starting with 1)
#   in front of each line.
# 
# 99 Aug lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nadd_num.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are xxxx.
# Output names will be 
# num_xxxx. 
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: num_$ARGV[0]\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">num_$ARGV[0]") || die "Can NOT open  output file for reading";

$number = 0;

#---------------------------------------------------------------------
# For the open file, read all records, put num in front and write out.
#---------------------------------------------------------------------
while ($line = <INFILE>) 
   { 
   $number++;

   printf OUTFILE "%-s %-s", $number, $line;

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "add_num.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
