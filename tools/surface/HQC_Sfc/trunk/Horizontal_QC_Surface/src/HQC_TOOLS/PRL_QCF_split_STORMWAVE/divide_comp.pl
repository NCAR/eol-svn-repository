#!/usr/bin/perl 

#-------------------------------------------------------
# divide_comp.pl - This perl script/program reads QCF records
#   and determines if the record should be written to
#   the STORMWAVE 20min or 5min composites.
# 
# 95 Feb lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nDIVIDE_COMP.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be SW20_YYMMDD.qcf
# and SW5_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 4, 10);
print "20MIN composite Output file name is: SW20_$YYMMDD\n";
print "5MIN composite Output file name is: SW5_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE20, ">SW20_$YYMMDD") || die "Can NOT open 20MIN output file for reading";
open (OUTFILE5, ">SW5_$YYMMDD") || die "Can NOT open 5MIN output file for reading";

#-------------------------------------------
# Create a list of all possible 20 and 5min
# networks. Note from the following network
# naming conventions, all the 20min nets
# have the number 20 in the same place.
# So, really don't even need these list,
# just search for '20' in a specific record
# location.
#-------------------------------------------
@min5_networks = ('ASOS5     ',
                  'OKMESO5   ',
                  'AWOSA05   ',
                  'ARMSFC1   ',
                  'AWOS1     ');

@min20_networks = ('AWOSQ20   ', 
                   'AWOSH20   ',
                   'AWOSA20   ');

#---------------------------------------------------------
# For the open file, read all records, determine if in
# which STORMWAVE output file to write record to.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # All lines must be written to one
   # of the output files. 
   #----------------------------------
   $network = substr( $line, 30, 10);
   $network_num = substr( $line, 35, 2);

   if ($debug)
      {  
      printf "Read line: $line";
      printf "network=: xxx%sxxx\n", $network; 
      printf "network_num=: xxx%sxxx\n", $network_num; 
      }  

   if ( $network_num == "20")
      {
      if ($debug)
         {
         printf "Write line to 20MIN file.\n\n";
         }

      print OUTFILE20 "$line";

      } # record is from 20MIN network
   else
      {
      if ($debug)
         {
         printf "Write line to 5MIN file.\n\n";
         }

      print OUTFILE5 "$line";

      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "DIVIDE_COMP.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
