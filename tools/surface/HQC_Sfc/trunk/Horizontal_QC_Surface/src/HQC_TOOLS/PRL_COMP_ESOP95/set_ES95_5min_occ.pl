#!/usr/bin/perl 

#-------------------------------------------------------
# set_occ.pl - This perl script/program reads QCF records
#   and determines if the station in the record should
#   have it's occurance set. If so, the occurance is 
#   set according to a given (hardcoded) list of occurance
#   settings.
# 
# 96 Aug lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
$overwrite = 0;

if ($overwite)
   {
   print "ORIGINAL INPUT FILES WILL BE OVERWRITTEN!\n";
   }
else
   {
   print "Original input files will be retained.\n";
   }
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nSET_OCC.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are xxx_YYMMDD.qcf.
# Intermediate output names will be 
# occ_YYMMDD.qcf. The intermediate
# output files are then "moved" into
# the original input file names....
# in other words: THE ORIGINAL INPUT
# FILES ARE OVERWRITTEN IF THE FLAG
# "overwrite" IS SET TO 1!!!!!! NOTE
# that the overwrite capability is not
# yet fully implemented or tested.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 4, 10);
print "Intermediate Output file name is: occ_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">occ_$YYMMDD") || die "Can NOT open INTERMEDIATE output file for reading";


$num_occs_to_set = 1; # number of stns in occ_settings - 1.

#---------------------------------------------------------
# For the open file, read all records, determine 
# which records should have their occs set. Write all
# records to the output file. Output file sizes should
# be identical.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $stn_info   = substr ($line, 30,  53);
   $begin_line = substr ($line,  0,  82);
   $end_line   = substr ($line, 83, 173);


   if ($debug)
      {  
      printf "\nRead line: $line";
      printf "stn_info=: xxx%sxxx\n", $stn_info; 
      }  

   $occ = 0;

# 
# Use following check for ESOP95 5MIN composite. 
# 
   if ( $stn_info eq "ASOS5      MLC               34.88000   -95.78000   0" )
      {
      if ($debug)
         {
         printf "This stn need occurance set.\n";
         }

      #------------------------
      # Set this station's occ. 
      #------------------------
      $occ = 1;
      }

   if ($debug)
      {
      printf "%-s%1s%-s\n", $begin_line,$occ,$end_line;
      }
 
   printf OUTFILE "%-s%1s%-s", $begin_line,$occ,$end_line;

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "SET_OCC.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
