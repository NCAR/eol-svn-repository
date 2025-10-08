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

$YYMMDD = substr( $ARGV[0], 7, 10);
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
# Use following check for SW HRLY composite.  There are 101 stations
# in the SW hourly composite that need their occurances set to 1.
# There is one station (MLC) whose occurance must be set to 2.
#------------------------------------------------------------------
   if ( $stn_info eq "NCDC       SAT               29.53333   -98.46667   0"  ||
        $stn_info eq "NCDC       LCH               30.11667   -93.21667   0"  ||
        $stn_info eq "NCDC       LFT               30.20000   -91.98333   0"  ||
        $stn_info eq "NCDC       DRO               37.15000  -107.75000   0"  ||
        $stn_info eq "NCDC       AUS               30.28333   -97.70000   0"  ||
        $stn_info eq "NCDC       TLH               30.38333   -84.36667   0"  ||
        $stn_info eq "NCDC       BTR               30.53333   -91.13333   0"  ||
        $stn_info eq "NCDC       MOB               30.68333   -88.25000   0"  ||
        $stn_info eq "NCDC       LFK               31.23333   -94.75000   0"  ||
        $stn_info eq "NCDC       SJT               31.36667  -100.50000   0"  ||
        $stn_info eq "NCDC       ESF               31.40000   -92.30000   0"  ||
        $stn_info eq "NCDC       ELP               31.80000  -106.40000   0"  ||
        $stn_info eq "NCDC       MGM               32.30000   -86.40000   0"  ||
        $stn_info eq "NCDC       JAN               32.31667   -90.08333   0"  ||
        $stn_info eq "NCDC       ABI               32.41667   -99.68333   0"  ||
        $stn_info eq "NCDC       SHV               32.46667   -93.81667   0"  ||
        $stn_info eq "NCDC       ELD               33.21667   -92.80000   0"  ||
        $stn_info eq "NCDC       TCL               33.23333   -87.61667   0"  ||
        $stn_info eq "NCDC       ROW               33.30000  -104.53333   0"  ||
        $stn_info eq "NCDC       BHM               33.56667   -86.75000   0"  ||
        $stn_info eq "NCDC       LBB               33.65000  -101.81667   0"  ||
        $stn_info eq "NCDC       SPS               33.96667   -98.48333   0"  ||
        $stn_info eq "NCDC       TUP               34.26667   -88.76667   0"  ||
        $stn_info eq "NCDC       HSV               34.65000   -86.76667   0"  ||
        $stn_info eq "NCDC       LIT               34.73333   -92.23333   0"  ||
        $stn_info eq "NCDC       MSL               34.75000   -87.61667   0"  ||
        $stn_info eq "AWOSH20    ADH               34.80000   -96.67000   0"  ||
        $stn_info eq "ASOSH      MLC               34.88000   -95.78000   0"  ||
        $stn_info eq "WPDN       Purcell           34.97972   -97.51862   0"  ||
        $stn_info eq "NCDC       HBR               35.00000   -99.05000   0"  ||
        $stn_info eq "NCDC       ABQ               35.05000  -106.61667   0"  ||
        $stn_info eq "NCDC       FSM               35.33333   -94.36667   0"  ||
        $stn_info eq "NCDC       CSM               35.35000   -99.20000   0"  ||
        $stn_info eq "NCDC       OKC               35.40000   -97.60000   0"  ||
        $stn_info eq "NCDC       PWA               35.53333   -97.63333   0"  ||
        $stn_info eq "NCDC       MKL               35.60000   -88.91667   0"  ||
        $stn_info eq "NCDC       TUL               36.20000   -95.90000   0"  ||
        $stn_info eq "NCDC       HRO               36.26667   -93.15000   0"  ||
        $stn_info eq "NCDC       GAG               36.30000   -99.76667   0"  ||
        $stn_info eq "NCDC       SKX               36.45000  -105.66667   0"  ||
        $stn_info eq "NCDC       PNC               36.73333   -97.10000   0"  ||
        $stn_info eq "NCDC       BVO               36.75000   -96.00000   0"  ||
        $stn_info eq "NCDC       BWG               36.96667   -86.43333   0"  ||
        $stn_info eq "NCDC       LBL               37.05000  -100.96667   0"  ||
        $stn_info eq "NCDC       LOZ               37.08333   -84.06667   0"  ||
        $stn_info eq "NCDC       CEZ               37.30000  -108.63333   0"  ||
        $stn_info eq "NCDC       ALS               37.45000  -105.86667   0"  ||
        $stn_info eq "NCDC       CNU               37.66667   -95.48333   0"  ||
        $stn_info eq "NCDC       DDC               37.76667   -99.96667   0"  ||
        $stn_info eq "NCDC       LHX               38.05000  -103.51667   0"  ||
        $stn_info eq "NCDC       PUB               38.28333  -104.51667   0"  ||
        $stn_info eq "NCDC       MTJ               38.50000  -107.88333   0"  ||
        $stn_info eq "NCDC       SLN               38.80000   -97.65000   0"  ||
        $stn_info eq "NCDC       OJC               38.85000   -94.73333   0"  ||
        $stn_info eq "NCDC       FOE               38.95000   -95.66667   0"  ||
        $stn_info eq "NCDC       MHK               39.15000   -96.66667   0"  ||
        $stn_info eq "NCDC       MCI               39.31667   -94.71667   0"  ||
        $stn_info eq "NCDC       GLD               39.36667  -101.70000   0"  ||
        $stn_info eq "NCDC       HLC               39.38333   -99.83333   0"  ||
        $stn_info eq "NCDC       HUF               39.45000   -87.30000   0"  ||
        $stn_info eq "NCDC       CNK               39.55000   -97.65000   0"  ||
        $stn_info eq "NCDC       DEN               39.76667  -104.86667   0"  ||
        $stn_info eq "HPLAINS    101_St. Joseph    39.77000   -94.92000   0"  ||
        $stn_info eq "NCDC       DAY               39.90000   -84.20000   0"  ||
        $stn_info eq "NCDC       DNV               40.20000   -87.60000   0"  ||
        $stn_info eq "NCDC       MCK               40.21667  -100.58333   0"  ||
        $stn_info eq "NCDC       VEL               40.45000  -109.51667   0"  ||
        $stn_info eq "NCDC       CAG               40.50000  -107.53333   0"  ||
        $stn_info eq "NCDC       PIA               40.66667   -89.68333   0"  ||
        $stn_info eq "NCDC       LNK               40.85000   -96.75000   0"  ||
        $stn_info eq "NCDC       GRI               40.96667   -98.31667   0"  ||
        $stn_info eq "NCDC       FWA               41.00000   -85.20000   0"  ||
        $stn_info eq "NCDC       SNY               41.10000  -102.98333   0"  ||
        $stn_info eq "HPLAINS    051_Mead Turf F   41.13000   -96.50000   0"  ||
        $stn_info eq "NCDC       OMA               41.30000   -95.90000   0"  ||
        $stn_info eq "NCDC       SBN               41.70000   -86.31667   0"  ||
        $stn_info eq "AWOS1      Clinton           41.83200   -90.32800   0"  ||
        $stn_info eq "NCDC       BFF               41.86667  -103.60000   0"  ||
        $stn_info eq "NCDC       AIA               42.05000  -102.80000   0"  ||
        $stn_info eq "AWOS1      Boone             42.05200   -93.84800   0"  ||
        $stn_info eq "NCDC       BEH               42.13333   -86.43333   0"  ||
        $stn_info eq "NCDC       RFD               42.20000   -89.10000   0"  ||
        $stn_info eq "NCDC       SUX               42.40000   -96.38333   0"  ||
        $stn_info eq "NCDC       DBQ               42.40000   -90.70000   0"  ||
        $stn_info eq "NCDC       ALO               42.55000   -92.40000   0"  ||
        $stn_info eq "NCDC       VTN               42.86667  -100.55000   0"  ||
        $stn_info eq "NCDC       GRR               42.88333   -85.51667   0"  ||
        $stn_info eq "NCDC       CPR               42.91667  -106.46667   0"  ||
        $stn_info eq "NCDC       MKE               42.95000   -87.90000   0"  ||
        $stn_info eq "NCDC       MSN               43.13333   -89.33333   0"  ||
        $stn_info eq "NCDC       EST               43.40000   -94.75000   0"  ||
        $stn_info eq "NCDC       AEL               43.68333   -93.36667   0"  ||
        $stn_info eq "NCDC       RAP               44.05000  -103.06667   0"  ||
        $stn_info eq "NCDC       MML               44.45000   -95.81667   0"  ||
        $stn_info eq "NCDC       GRB               44.48333   -88.13333   0"  ||
        $stn_info eq "NCDC       COD               44.51667  -109.01667   0"  ||
        $stn_info eq "HPLAINS    081_Watertown     44.92000   -97.15000   0"  ||
        $stn_info eq "NCDC       MIC               45.06667   -93.35000   0"  ||
        $stn_info eq "NCDC       ABR               45.45000   -98.43333   0"  ||
        $stn_info eq "NCDC       STC               45.55000   -94.06667   0"  ||
        $stn_info eq "NCDC       BIL               45.80000  -108.53333   0"  ||
        $stn_info eq "NCDC       AXN               45.86667   -95.38333   0" )
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

   if ( $stn_info eq "NCDC       MLC               34.88333   -95.78333   0")
      {
      if ($debug)
         {
         printf "Set MLC occurance to 2.\n";
         }
      $occ = 2;
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
