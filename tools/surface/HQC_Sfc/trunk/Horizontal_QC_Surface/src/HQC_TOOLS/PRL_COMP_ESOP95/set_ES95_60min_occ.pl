#!/usr/bin/perl 

#-------------------------------------------------------
# set_occ.pl - This perl script/program reads QCF records
#   and determines if the station in the record should
#   have it's occurance set. If so, the occurance is 
#   set according to a given (hardcoded) list of occurance
#   settings.
# 
# 95 Aug lec
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

$YYMMDD = substr( $ARGV[0], 9, 10);
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

   $occ = substr ($line,  82,  1);

#------------------------------------------------------------------
# Use following check for ESOP95 HRLY composite.  There are 101 stations
# in the ESOP95 hourly composite that need their occurances set to 1.
#------------------------------------------------------------------
   if ( $stn_info eq "NCDC       LFK               31.23333   -94.75000   0"  ||
        $stn_info eq "NCDC       SJT               31.36667  -100.50000   0"  ||
        $stn_info eq "NCDC       ESF               31.40000   -92.30000   0"  ||
        $stn_info eq "NCDC       ELP               31.80000  -106.40000   0"  ||
        $stn_info eq "NCDC       ABI               32.41667   -99.68333   0"  ||
        $stn_info eq "NCDC       SHV               32.46667   -93.81667   0"  ||
        $stn_info eq "NCDC       ELD               33.21667   -92.80000   0"  ||
        $stn_info eq "NCDC       ROW               33.30000  -104.53333   0"  ||
        $stn_info eq "NCDC       LBB               33.65000  -101.81667   0"  ||
        $stn_info eq "NCDC       SPS               33.96667   -98.48333   0"  ||
        $stn_info eq "NCDC       LIT               34.73333   -92.23333   0"  ||
        $stn_info eq "AWOSH20    ADH               34.80000   -96.67000   0"  ||
        $stn_info eq "ASOSH      MLC               34.88000   -95.78000   0"  ||
        $stn_info eq "WPDN       Purcell           34.97972   -97.51862   0"  ||
        $stn_info eq "NCDC       HBR               35.00000   -99.05000   0"  ||
        $stn_info eq "NCDC       ABQ               35.05000  -106.61667   0"  ||
        $stn_info eq "NCDC       AMA               35.23333  -101.70000   0"  ||
        $stn_info eq "NCDC       FSM               35.33333   -94.36667   0"  ||
        $stn_info eq "NCDC       CSM               35.35000   -99.20000   0"  ||
        $stn_info eq "NCDC       OKC               35.40000   -97.60000   0"  ||
        $stn_info eq "NCDC       PWA               35.53333   -97.63333   0"  ||
        $stn_info eq "NCDC       DHT               36.01667  -102.55000   0"  ||
        $stn_info eq "NCDC       TUL               36.20000   -95.90000   0"  ||
        $stn_info eq "NCDC       HRO               36.26667   -93.15000   0"  ||
        $stn_info eq "NCDC       GAG               36.30000   -99.76667   0"  ||
        $stn_info eq "NCDC       SKX               36.45000  -105.66667   0"  ||
        $stn_info eq "NCDC       PNC               36.73333   -97.10000   0"  ||
        $stn_info eq "NCDC       BVO               36.75000   -96.00000   0"  ||
        $stn_info eq "NCDC       SGF               37.23333   -93.38333   0"  ||
        $stn_info eq "NCDC       ALS               37.45000  -105.86667   0"  ||
        $stn_info eq "NCDC       CNU               37.66667   -95.48333   0"  ||
        $stn_info eq "NCDC       DDC               37.76667   -99.96667   0"  ||
        $stn_info eq "NCDC       LHX               38.05000  -103.51667   0"  ||
        $stn_info eq "NCDC       PUB               38.28333  -104.51667   0"  ||
        $stn_info eq "NCDC       SLN               38.80000   -97.65000   0"  ||
        $stn_info eq "NCDC       OJC               38.85000   -94.73333   0"  ||
        $stn_info eq "NCDC       FOE               38.95000   -95.66667   0"  ||
        $stn_info eq "NCDC       MHK               39.15000   -96.66667   0"  ||
        $stn_info eq "NCDC       MCI               39.31667   -94.71667   0"  ||
        $stn_info eq "NCDC       GLD               39.36667  -101.70000   0"  ||
        $stn_info eq "NCDC       HLC               39.38333   -99.83333   0"  ||
        $stn_info eq "NCDC       CNK               39.55000   -97.65000   0"  ||
        $stn_info eq "NCDC       DEN               39.76667  -104.86667   0"  ||
        $stn_info eq "HPLAINS    101_St. Joseph    39.77000   -94.92000   0"     )
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
