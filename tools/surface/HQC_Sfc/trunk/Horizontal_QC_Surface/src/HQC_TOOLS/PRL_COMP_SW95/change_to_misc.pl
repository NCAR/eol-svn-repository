#!/usr/bin/perl 

#-------------------------------------------------------
# change_to_misc.pl - This perl script/program reads stn
#   lst recs and changes the requested (ABRFC) recs to
#   be misc (ellaneous) records. Used to change all TCH
#   and MESO ABRFC station recs to be misc recs.
# 
# 97 Sept lec
#   Created.
#-------------------------------------------------------*/
$debug = 1;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nchange_to_misc.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].misc\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].misc") || die "Can NOT open output file for reading";


#---------------------------------------------------------
# For the open file, read all records, determine if in
# MESO or TCH and change frequency to no set schedule.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in AOI.
   #----------------------------------
#
#  Following used to change stationCD.out file
#
#   $network = substr( $line, 54,   4);  
#   $part1   = substr( $line, 0,  147);
#   $freq    = substr( $line, 147, 15);
#   $part2   = substr( $line, 162, 12);
#
#  Following used to change station.out file
#
   $network = substr( $line, 60,   4);  
   $part1   = substr( $line, 0,  153);
   $freq    = substr( $line, 153, 15);
   $part2   = substr( $line, 168, 12);
 
   if ($debug)
      {  
      printf "\n---------\n";
      printf "Read line: $line\n";
      printf "network=: %s\n", $network; 
      printf "part1=: xxx%sxxx\n", $part1; 
      printf "freq=: xxx%sxxx\n", $freq; 
      printf "part2=: xxx%sxxx\n", $part2; 
      }  

   if ($network eq "MESO" || $network eq "TCH ")
      {
      if ($debug)
         {
         printf "Change to misc station!!!\n";
         }

      #--------------------------------
      # Change freq to no set schedule.
      #--------------------------------
      $freq = "no set schedule";

      } # record is MESO or TCH

   print OUTFILE "$part1$freq$part2\n";

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "change_to_misc.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
