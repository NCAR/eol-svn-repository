#!/usr/bin/perl 

#------------------------------------------------------------
# set_LKICE_specials_occ.pl - This perl script/program reads QCF
#   records and determines if the station in the record should
#   have it's occurance set. If so, the occurance is 
#   set according to a given (hardcoded) list of occurance
#   settings.
# 
# 99 Mar lec
#   Created.
#------------------------------------------------------------
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
printf "\nset_LKICE_specials_occ.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are LK60_YYMMDD.qcf.
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

$YYMMDD = substr( $ARGV[0], 5, 10);
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

   #-------------------------------------------------
   # Use following check for LakeICE 20MIN composite.
   #-------------------------------------------------
if ( $stn_info eq "DATSAV2    999999:WNDG       37.50000   -74.40000   0" ||
     $stn_info eq "DATSAV2    999999:PWT        40.30000   -73.20000   0" ||
     $stn_info eq "DATSAV2    999999:45EO       43.40000   -79.40000   0" ||
     $stn_info eq "DATSAV2    999999:WE80       47.40000   -89.00000   0" ||
     $stn_info eq "DATSAV2    724507:KCNU       37.66667   -95.48333   0" ||
     $stn_info eq "DATSAV2    724504:K3KM       37.75000   -97.21667   0" ||
     $stn_info eq "DATSAV2    724336:KMDH       37.78333   -89.25000   0" ||
     $stn_info eq "DATSAV2    724115:KHSP       37.95000   -79.83333   0" ||
     $stn_info eq "DATSAV2    724635:KLHX       38.05000  -103.51667   0" ||
     $stn_info eq "DATSAV2    724456:KVIH       38.13333   -91.76667   0" ||
     $stn_info eq "DATSAV2    724233:KFFT       38.18333   -84.90000   0" ||
     $stn_info eq "DATSAV2    724033:KEZF       38.26667   -77.45000   0" ||
     $stn_info eq "DATSAV2    724250:KHTS       38.36667   -82.55000   0" ||
     $stn_info eq "DATSAV2    725314:KCPS       38.56667   -90.15000   0" ||
     $stn_info eq "DATSAV2    724450:KCOU       38.81667   -92.21667   0" ||
     $stn_info eq "DATSAV2    724170:KEKN       38.88333   -79.85000   0" ||
     $stn_info eq "DATSAV2    724580:KCNK       39.55000   -97.65000   0" ||
     $stn_info eq "DATSAV2    724176:KMGW       39.65000   -79.91667   0" ||
     $stn_info eq "DATSAV2    725515:KBIE       40.30000   -96.75000   0" ||
     $stn_info eq "DATSAV2    725127:KJST       40.31667   -78.83333   0" ||
     $stn_info eq "DATSAV2    725320:KPIA       40.66667   -89.68333   0" ||
     $stn_info eq "DATSAV2    726498:KFFL       41.05000   -91.98333   0" ||
     $stn_info eq "DATSAV2    725037:KHPN       41.06667   -73.70000   0" ||
     $stn_info eq "DATSAV2    725610:KSNY       41.10000  -102.98333   0" ||
     $stn_info eq "DATSAV2    725454:KAWG       41.28333   -91.66667   0" ||
     $stn_info eq "DATSAV2    725440:KMLI       41.45000   -90.51667   0" ||
     $stn_info eq "DATSAV2    725560:KOFK       41.98333   -97.43333   0" ||
     $stn_info eq "DATSAV2    725486:KBNW       42.05000   -93.85000   0" ||
     $stn_info eq "DATSAV2    725260:KERI       42.08333   -80.18333   0" ||
     $stn_info eq "DATSAV2    725480:KALO       42.55000   -92.40000   0" ||
     $stn_info eq "DATSAV2    725378:KOZW       42.63333   -83.98333   0" ||
     $stn_info eq "DATSAV2    725636:KCDR       42.83333  -103.10000   0" ||
     $stn_info eq "DATSAV2    726370:KFNT       42.96667   -83.75000   0" ||
     $stn_info eq "DATSAV2    726509:KUNU       43.43333   -88.70000   0" ||
     $stn_info eq "DATSAV2    726503:KDLL       43.51667   -89.76667   0" ||
     $stn_info eq "DATSAV2    726379:KMBS       43.53333   -84.08333   0" ||
     $stn_info eq "DATSAV2    726589:KAEL       43.68333   -93.36667   0" ||
     $stn_info eq "DATSAV2    726384:KCAD       44.28333   -85.41667   0" ||
     $stn_info eq "DATSAV2    726567:KULM       44.31667   -94.50000   0" ||
     $stn_info eq "DATSAV2    726686:KPIR       44.38333  -100.28333   0" ||
     $stn_info eq "DATSAV2    726170:KBTV       44.46667   -73.15000   0" ||
     $stn_info eq "DATSAV2    726574:KMFI       44.63333   -90.18333   0" ||
     $stn_info eq "DATSAV2    726465:KCWA       44.78333   -89.66667   0" ||
     $stn_info eq "DATSAV2    726575:KMIC       45.06667   -93.35000   0" ||
     $stn_info eq "DATSAV2    726578:KLXL       45.95000   -94.35000   0" ||
     $stn_info eq "DATSAV2    726555:KBRD       46.40000   -94.13333   0" ||
     $stn_info eq "DATSAV2    727340:KY62       46.46667   -84.36667   0" ||
     $stn_info eq "DATSAV2    727504:KAIT       46.55000   -93.68333   0" ||
     $stn_info eq "DATSAV2    727645:KDIK       46.80000  -102.80000   0" ||
     $stn_info eq "DATSAV2    727444:KTWM       47.05000   -91.75000   0" ||
     $stn_info eq "DATSAV2    727455:KHIB       47.38333   -92.83333   0" ||
     $stn_info eq "DATSAV2    716420:KYLD       47.81667   -83.35000   0" ||
     $stn_info eq "DATSAV2    727576:KGFK       47.95000   -97.18333   0" ||
     $stn_info eq "DATSAV2    994090:PILM4      48.21667   -88.36667   0" ||
     $stn_info eq "DATSAV2    718550:CYDN       51.10000  -100.05000   0" ||
     $stn_info eq "AWOSH20    HCDa              44.86000   -94.38000   0")
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
printf "set_LKICE_specials_occ.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
