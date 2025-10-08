#!/usr/bin/perl 

#-------------------------------------------------------
# change_to_misc2.pl - This perl script/program reads stn
#   lst recs and changes the requested (ABRFC) recs to
#   be misc2 (ellaneous) records. Sets freq to "No set
#   schedule" for specific set of stations that were
#   stripped from the 15m or hourly precip composites.
# 
# 97 Sept lec
#   Created.
#-------------------------------------------------------*/
$debug = 1;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nchange_to_misc2.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].misc2\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].misc2") || die "Can NOT open output file for reading";

$matches_found = 0;

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
   $network = substr( $line, 54,   4);  
   $part1   = substr( $line, 0,  147);
   $freq    = substr( $line, 147, 15);
   $part2   = substr( $line, 162, 12);
   $name    = substr( $line, 54,  25);
#
#  Following used to change station.out file
#  (Must use Name to change this file since
#   ID is NOT in the file.
#
#   $network = substr( $line, 60,   4);  
#   $part1   = substr( $line, 0,  153);
#   $freq    = substr( $line, 153, 15);
#   $part2   = substr( $line, 168, 12);
#   $name    = substr( $line, 60,  25);

   if ($debug)
      {  
      printf "\n---------\n";
      printf "Read line: $line\n";
      printf "network=: %s\n", $network; 
      printf "part1=: xxx%sxxx\n", $part1; 
      printf "freq=: xxx%sxxx\n", $freq; 
      printf "part2=: xxx%sxxx\n", $part2; 
      printf "name=: xxx%sxxx\n", $name; 
      }  

   if ($name eq "NNET - GRAPEVINE DAM     " ||
       $name eq "NNET - LEWISVILLE DAM    " ||
       $name eq "NNET - OUACHITA RVR AT CA" ||
       $name eq "DCP - DENISON 4NNW - DAM " ||
       $name eq "DCP - DEQUEEN 4NW        " ||
       $name eq "DCP - NARROWS DAM        " ||
       $name eq "NNET - LK OUACHITA BLAKEL" ||
       $name eq "NNET - PECOS RIVER BLO SU" ||
       $name eq "DCP - WISTER 2S - DAM POT" ||
       $name eq "DCP - NIMROD 5W - DAM FOU" ||
       $name eq "DCP - BOONEVILLE 2S (& 3S" ||
       $name eq "DCP - BLUE MOUNTAIN DAM T" ||
       $name eq "DCP - EUFAULA DAM CANADIA" ||
       $name eq "DCP - OVERHOLSER LAKE BEL" ||
       $name eq "DCP - GREERS FERRY DAM   " ||
       $name eq "DCP - TENKILLER FERRY DAM" ||
       $name eq "DCP - L & D 17 / OKAY 3W " ||
       $name eq "DCP - KEYSTONE DAM ARKANS" ||
       $name eq "NNET - WHITE RIVER NR NOR" ||
       $name eq "NNET - WHITE RVR NR BULL " ||
       $name eq "NNET - KINGS RIVER AT BER" ||
       $name eq "DCP - GREAT SALT PLAINS D" ||
       $name eq "DCP - GRENOLA 1N         " ||
       $name eq "ROSA - BEAUMONT          " ||
       $name eq "DCP - FALL RIVER 4NW - DA" ||
       $name eq "DCP - TORONTO 4SE - DAM V" ||
       $name eq "DCP - ARLINGTON          " ||
       $name eq "NNET - POM LAKE-HUNDRED T" ||
       $name eq "NNET - HILLSDALE LK-B BUL" ||
       $name eq "DCP - CRESTED BUTTE      " ||
       $name eq "NNET - CLINTON LAKE-WAKAR" ||
       $name eq "NNET - MILFORD LAKE - REP" ||
       $name eq "NNET - PERRY LK-DELAWARE " ||
       $name eq "NNET - TUTTLE CR LK-B BLU" ||
       $name eq "NNET - INTERCANYON       " ||
       $name eq "NNET - EVERGREEN         " ||
       $name eq "NNET - GOLDEN 3S         " ||
       $name eq "NNET - LAWSON CLEAR CREEK"   )
      {
      if ($debug)
         {
         printf "Change to misc2 station!!!\n";
         }

      $matches_found++;

      #--------------------------------
      # Change freq to no set schedule.
      #--------------------------------
      $freq = "no set schedule";

      } # record is one to change

   print OUTFILE "$part1$freq$part2\n";

   }  # while data in file

print "Total Matches Found $matches_found\n";


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "change_to_misc2.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
