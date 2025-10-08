#!/usr/bin/perl 

#-------------------------------------------------------
# set_SWpcp_occ.pl - This perl script/program reads PQCF
#   and determines if the station in the record should
#   have it's occurance set. If so, the occurance is 
#   set according to a given (hardcoded) list of occurance
#   settings.
# 
# 96 Sep lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nset_SWpcp_occ.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are YYMMDD.pqc.
# Output names will be occ_YYMMDD.pqc.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 0, 10);
print "Intermediate Output file name is: occ_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">occ_$YYMMDD") || die "Can NOT open output file for writing";

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
   $stn_info   = substr ($line, 18,  53);
   $begin_line = substr ($line,  0,  70);
##   $end_line   = substr ($line, 71, 300); # Hourly recs are 359 chars long

  $end_line   = substr ($line, 71, 1160); # 15 min recs are 1223 chars long

   if ($debug)
      {  
      printf "\nRead line: $line";
      printf "stn_info=: xxx%sxxx\n", $stn_info; 
      }  

   $occ = substr ($line,  70,  1);

#------------------------------------------------------------
# Use following check for SW 15min and Hrly precip composite.
#------------------------------------------------------------
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
        $stn_info eq "NCDC       AXN               45.86667   -95.38333   0"  ||
        $stn_info eq "Rec rainga 41941703          31.60000   -97.21667   0"  ||
        $stn_info eq "Rec rainga 41589001          31.95000  -102.18333   0"  ||
        $stn_info eq "Rec rainga 22577609          32.33333   -88.75000   0"  ||
        $stn_info eq "Rec rainga 41328403          32.81667   -97.35000   0"  ||
        $stn_info eq "Rec rainga 41224403          32.85000   -96.85000   0"  ||
        $stn_info eq "Rec rainga 41224203          32.90000   -97.03333   0"  ||
        $stn_info eq "ABRFC/DCP  TXKT2             33.30000   -94.16666   0"  ||
        $stn_info eq "ABRFC/TCH  TXAT2             33.41667   -94.08334   0"  ||
        $stn_info eq "ABRFC/TCH  NWBT2             33.45000   -94.41666   0"  ||
        $stn_info eq "ABRFC/FSS  TXK               33.45000   -94.00000   0"  ||
        $stn_info eq "ABRFC/OBS  DPTT2             33.51667   -95.31667   0"  ||
        $stn_info eq "Rec rainga 03483907          33.68333   -93.96667   0"  ||
        $stn_info eq "ABRFC/TCH  FORA4             33.73333   -94.40000   0"  ||
        $stn_info eq "ABRFC/TCH  BTAT2             33.76667   -97.60000   0"  ||
        $stn_info eq "ABRFC/OBS  DMNT2             33.80000  -100.51667   0"  ||
        $stn_info eq "ABRFC/DCP  WICT2             33.91667   -98.53333   0"  ||
        $stn_info eq "ABRFC/TCH  NSVA4             33.95000   -93.86667   0"  ||
        $stn_info eq "ABRFC/TCH  HUGO2             34.00000   -95.51667   0"  ||
        $stn_info eq "ABRFC/TCH  KGNO2             34.01667   -96.71667   0"  ||
        $stn_info eq "ABRFC/DCP  DQDA4             34.10000   -94.38333   0"  ||
        $stn_info eq "ABRFC/OBS  DROA4             34.15000   -94.10000   0"  ||
        $stn_info eq "ABRFC/OBS  ARMO2             34.20000   -97.15000   0"  ||
        $stn_info eq "ABRFC/LARC PBOA4             34.21667   -92.01667   0"  ||
        $stn_info eq "ABRFC/TCH  ATRO2             34.25000   -95.63333   0"  ||
        $stn_info eq "ABRFC/OBS  BKNO2             34.25000   -94.78333   0"  ||
        $stn_info eq "Rec rainga 41169802          34.43333  -100.28333   0"  ||
        $stn_info eq "ABRFC/TCH  LEHO2             34.46667   -96.21667   0"  ||
        $stn_info eq "ABRFC/OBSR MIOA4             34.53333   -93.59972   0"  ||
        $stn_info eq "ABRFC/TCH  MENA4             34.56667   -94.26667   0"  ||
        $stn_info eq "ABRFC/TCH  ALGO2             34.58333   -99.33334   0"  ||
        $stn_info eq "ABRFC/TCH  TKAO2             34.61666   -95.28333   0"  ||
        $stn_info eq "ABRFC/DCP  MTNO2             34.73333   -98.98333   0"  ||
        $stn_info eq "ABRFC/OBS  HERT2             34.81667  -102.40000   0"  ||
        $stn_info eq "ABRFC/TCH  BENO2             34.85000   -95.08334   0"  ||
        $stn_info eq "ABRFC/OBSR WDOA4             34.90000   -94.10000   0"  ||
        $stn_info eq "ABRFC/OBS  NROA4             34.95000   -93.16666   0"  ||
        $stn_info eq "ABRFC/FSS  HBR               35.00000   -99.05000   0"  ||
        $stn_info eq "Rec rainga 40165601          35.03333   -85.20000   0"  ||
        $stn_info eq "ABRFC/LARC CHKO2             35.05000   -97.91666   0"  ||
        $stn_info eq "Rec rainga 40595404          35.05000   -90.00000   0"  ||
        $stn_info eq "ABRFC/OBS  BMOA4             35.10000   -93.65000   0"  ||
        $stn_info eq "Rec rainga 41021101          35.23333  -101.70000   0"  ||
        $stn_info eq "ABRFC/OBS  STGO2             35.25000   -95.11667   0"  ||
        $stn_info eq "ABRFC/TCH  ELKO2             35.38334   -99.40000   0"  ||
        $stn_info eq "ABRFC/TCH  SWEO2             35.41667   -99.91666   0"  ||
        $stn_info eq "ABRFC/TCH  OMHO2             35.43333   -96.30000   0"  ||
        $stn_info eq "ABRFC/TCH  CKVA4             35.53333   -93.40000   0"  ||
        $stn_info eq "ABRFC/OBS  PAPT2             35.56667  -100.96667   0"  ||
        $stn_info eq "ABRFC/FSS  LVS               35.65000  -105.15000   0"  ||
        $stn_info eq "ABRFC/LARC BTAA4             35.71667   -92.46667   0"  ||
        $stn_info eq "ABRFC/TCH  ROYN5             35.95000  -104.20000   0"  ||
        $stn_info eq "ABRFC/FSS  DHT               36.01667  -102.55000   0"  ||
        $stn_info eq "ABRFC/TCH  TGAO2             36.03333   -98.96667   0"  ||
        $stn_info eq "ABRFC/LARC OILO2             36.06667   -96.56667   0"  ||
        $stn_info eq "ABRFC/OBS  FYVA4             36.10000   -94.16666   0"  ||
        $stn_info eq "Rec rainga 40640203          36.11667   -86.68333   0"  ||
        $stn_info eq "ABRFC/TCH  MSHO2             36.15000   -97.61667   0"  ||
        $stn_info eq "ABRFC/OBS  ROSO2             36.16667   -95.01667   0"  ||
        $stn_info eq "ABRFC/TCH  CVEO2             36.28333   -96.55000   0"  ||
        $stn_info eq "ABRFC/OBS  PWNO2             36.40000   -96.81667   0"  ||
        $stn_info eq "ABRFC/TCH  PRYO2             36.40000   -95.30000   0"  ||
        $stn_info eq "Rec rainga 29188703          36.45000  -103.15000   0"  ||
        $stn_info eq "ABRFC/OBS  EGNN5             36.55000  -105.26667   0"  ||
        $stn_info eq "ABRFC/TCH  BRNO2             36.56667   -96.16666   0"  ||
        $stn_info eq "ABRFC/TCH  WAYO2             36.58333   -98.86667   0"  ||
        $stn_info eq "ABRFC/OBS  GDWO2             36.60000  -101.61667   0"  ||
        $stn_info eq "ABRFC/LARC PWHO2             36.66667   -96.35000   0"  ||
        $stn_info eq "ABRFC/TCH  NOWO2             36.70000   -95.63333   0"  ||
        $stn_info eq "ABRFC/FSS  PNC               36.73333   -97.10000   0"  ||
        $stn_info eq "ABRFC/ROSA GLNM7             36.80000   -93.46667   0"  ||
        $stn_info eq "ABRFC/ROSA SENM7             36.85000   -94.61667   0"  ||
        $stn_info eq "ABRFC/OBS  RRTN5             36.91667  -104.43333   0"  ||
        $stn_info eq "Rec rainga 14243207          37.00000  -101.88333   0"  ||
        $stn_info eq "ABRFC/ROSA KWAK1             37.03333   -98.46667   0"  ||
        $stn_info eq "ABRFC/OBS  ENGK1             37.05000  -100.00000   0"  ||
        $stn_info eq "ABRFC/OBS  CDWK1             37.05000   -97.61667   0"  ||
        $stn_info eq "ABRFC/OBS  ARKK1             37.06667   -97.03333   0"  ||
        $stn_info eq "ABRFC/ARMD TRDC2             37.16667  -104.48333   0"  ||
        $stn_info eq "ABRFC/ROSA CMBK1             37.16667   -94.85000   0"  ||
        $stn_info eq "ABRFC/ROSA MLRM7             37.21667   -93.81667   0"  ||
        $stn_info eq "Rec rainga 23797604          37.23333   -93.38333   0"  ||
        $stn_info eq "ABRFC/FSS  TAD               37.25000  -104.33334   0"  ||
        $stn_info eq "ABRFC/DCP  BIGK1             37.26667   -95.46667   0"  ||
        $stn_info eq "ABRFC/OBS  KIMC2             37.45000  -103.31667   0"  ||
        $stn_info eq "ABRFC/ROSA SUBK1             37.48333  -100.85000   0"  ||
        $stn_info eq "ABRFC/NNET STZM7             37.70000   -93.76667   0"  ||
        $stn_info eq "ABRFC/WSO  ELDK1             37.81667   -96.83334   0"  ||
        $stn_info eq "ABRFC/ROSA UNTK1             37.85000   -94.96667   0"  ||
        $stn_info eq "ABRFC/OBS  IOAK1             37.91667   -95.43333   0"  ||
        $stn_info eq "ABRFC/FCWO GCK               37.93333  -100.71667   0"  ||
        $stn_info eq "ABRFC/ROSA GESK1             37.98333  -100.81667   0"  ||
        $stn_info eq "ABRFC/OBS  ROCC2             38.03333  -103.70000   0"  ||
        $stn_info eq "Rec rainga 12273807          38.05000   -87.53333   0"  ||
        $stn_info eq "ABRFC/OBS  LHX               38.05000  -103.51667   0"  ||
        $stn_info eq "ABRFC/DCP  JMCC2             38.06667  -102.93333   0"  ||
        $stn_info eq "ABRFC/OBS  CHWC2             38.10000  -103.50000   0"  ||
        $stn_info eq "Rec rainga 15495402          38.18333   -85.73333   0"  ||
        $stn_info eq "ABRFC/DCP  CGRC2             38.30000  -105.48333   0"  ||
        $stn_info eq "ABRFC/DCP  MABK1             38.36666   -97.08334   0"  ||
        $stn_info eq "ABRFC/ROSA TRBK1             38.46667  -101.76667   0"  ||
        $stn_info eq "ABRFC/OBS  ODYC2             38.51667  -103.70000   0"  ||
        $stn_info eq "Rec rainga 05366202          38.55000  -106.91667   0"  ||
        $stn_info eq "ABRFC/ROSA HEAK1             38.60000  -100.61667   0"  ||
        $stn_info eq "Rec rainga 14612806          38.61667   -95.28333   0"  ||
        $stn_info eq "ABRFC/OBS  GTAK1             38.65000   -98.95000   0"  ||
        $stn_info eq "ABRFC/OBS  FTNC2             38.68333  -104.70000   0"  ||
        $stn_info eq "Rec rainga 23745502          38.75000   -90.36667   0"  ||
        $stn_info eq "ABRFC/DCP  CRBC2             38.81667  -106.61667   0"  ||
        $stn_info eq "Rec rainga 05177801          38.81667  -104.71667   0"  ||
        $stn_info eq "Rec rainga 23179102          38.81667   -92.21667   0"  ||
        $stn_info eq "ABRFC/OBS  LGRC2             38.91667  -105.48333   0"  ||
        $stn_info eq "ABRFC/OBS  DVDC2             38.93333  -105.15000   0"  ||
        $stn_info eq "ABRFC/NNET WLSK1             38.96667   -98.48333   0"  ||
        $stn_info eq "ABRFC/ROSA ANTC2             39.00000  -105.88333   0"  ||
        $stn_info eq "Rec rainga 14816706          39.06667   -95.63333   0"  ||
        $stn_info eq "ABRFC/DCP  TWTC2             39.08333  -106.53333   0"  ||
        $stn_info eq "Rec rainga 05348802          39.10000  -108.55000   0"  ||
        $stn_info eq "ABRFC/OBSR ASPC2             39.18306  -106.83305   0"  ||
        $stn_info eq "ABRFC/WSO  LIC               39.18333  -103.70000   0"  ||
        $stn_info eq "ABRFC/DCP  LXVC2             39.23333  -106.31667   0"  ||
        $stn_info eq "ABRFC/OBS  SGLC2             39.25000  -106.36667   0"  ||
        $stn_info eq "Rec rainga 23435801          39.31667   -94.71667   0"  ||
        $stn_info eq "ABRFC/OBS  MDRC2             39.36666  -106.75000   0"  ||
        $stn_info eq "ABRFC/SNTL CPMC2             39.48333  -106.16666   0"  ||
        $stn_info eq "Rec rainga 05703102          39.53333  -107.80000   0"  ||
        $stn_info eq "Rec rainga 05632604          39.53333  -104.65000   0"  ||
        $stn_info eq "ABRFC/NNET CRYC2             39.56667  -104.85000   0"  ||
        $stn_info eq "Rec rainga 12425905          39.73333   -86.26667   0"  ||
        $stn_info eq "Rec rainga 11817906          39.85000   -89.68333   0"  ||
        $stn_info eq "Rec rainga 05010903          40.15000  -103.15000   0"  ||
        $stn_info eq "Rec rainga 13458508          40.61667   -93.95000   0"  ||
        $stn_info eq "Rec rainga 25479506          40.85000   -96.75000   0"  ||
        $stn_info eq "Rec rainga 13638909          41.10000   -92.45000   0"  ||
        $stn_info eq "Rec rainga 25606507          41.13333  -100.68333   0"  ||
        $stn_info eq "Rec rainga 48167508          41.15000  -104.81667   0"  ||
        $stn_info eq "Rec rainga 25625506          41.30000   -95.90000   0"  ||
        $stn_info eq "Rec rainga 13220305          41.53333   -93.65000   0"  ||
        $stn_info eq "Rec rainga 48753310          41.80000  -107.20000   0"  ||
        $stn_info eq "Rec rainga 25766501          41.86667  -103.60000   0"  ||
        $stn_info eq "Rec rainga 25599503          41.98333   -97.43333   0"  ||
        $stn_info eq "Rec rainga 11154902          42.00000   -87.88333   0"  ||
        $stn_info eq "Rec rainga 48539009          42.81667  -108.73333   0"  ||
        $stn_info eq "Rec rainga 13784401          43.16667   -95.15000   0"  ||
        $stn_info eq "Rec rainga 20571205          43.16667   -86.23333   0"  ||
        $stn_info eq "Rec rainga 39766709          43.56667   -96.73333   0"  ||
        $stn_info eq "Rec rainga 21917007          43.65000   -95.58333   0"  ||
        $stn_info eq "Rec rainga 21700409          43.91667   -92.50000   0"  ||
        $stn_info eq "Rec rainga 39107607          44.31667   -96.76667   0"  ||
        $stn_info eq "Rec rainga 39412707          44.38333   -98.21667   0"  ||
        $stn_info eq "Rec rainga 48815505          44.76667  -106.96667   0"  ||
        $stn_info eq "Rec rainga 21543506          44.88333   -93.21667   0" )
      {
      if ($debug)
         {
         printf "This stn need occurance set to 1.\n";
         }

      #------------------------
      # Set this station's occ. 
      #------------------------
      $occ = 1;
      }

   if ( $stn_info eq "NCDC       MLC               34.88333   -95.78333   0"  ||
        $stn_info eq "Rec rainga 16507807          30.11667   -93.21667   0"  ||
        $stn_info eq "Rec rainga 41042807          30.28333   -97.70000   0"  ||
        $stn_info eq "Rec rainga 16054906          30.53333   -91.13333   0"  ||
        $stn_info eq "Rec rainga 01547808          30.68333   -88.25000   0"  ||
        $stn_info eq "Rec rainga 41794306          31.36667  -100.50000   0"  ||
        $stn_info eq "Rec rainga 41941903          31.61667   -97.21667   0"  ||
        $stn_info eq "Rec rainga 41279705          31.80000  -106.40000   0"  ||
        $stn_info eq "Rec rainga 01555006          32.30000   -86.40000   0"  ||
        $stn_info eq "Rec rainga 22447205          32.31667   -90.08333   0"  ||
        $stn_info eq "Rec rainga 41001602          32.41667   -99.68333   0"  ||
        $stn_info eq "Rec rainga 16844001          32.46667   -93.81667   0"  ||
        $stn_info eq "Rec rainga 29761007          33.30000  -104.53333   0"  ||
        $stn_info eq "Rec rainga 01083102          33.56667   -86.75000   0"  ||
        $stn_info eq "Rec rainga 41541101          33.65000  -101.81667   0"  ||
        $stn_info eq "ABRFC/WSO  SPS               33.96667   -98.48333   0"  ||
        $stn_info eq "ABRFC/OBS  DQOA4             34.10000   -94.38333   0"  ||
        $stn_info eq "Rec rainga 22900303          34.26667   -88.76667   0"  ||
        $stn_info eq "ABRFC/FCWS CDS               34.43333  -100.28333   0"  ||
        $stn_info eq "Rec rainga 01406401          34.65000   -86.76667   0"  ||
        $stn_info eq "Rec rainga 29023405          35.05000  -106.61667   0"  ||
        $stn_info eq "ABRFC/DCP  BMTA4             35.10000   -93.65000   0"  ||
        $stn_info eq "ABRFC/WSO  AMA               35.23333  -101.70000   0"  ||
        $stn_info eq "Rec rainga 03257404          35.33333   -94.36667   0"  ||
        $stn_info eq "Rec rainga 34666105          35.40000   -97.60000   0"  ||
        $stn_info eq "ABRFC/FSS  PWA               35.53333   -97.63333   0"  ||
        $stn_info eq "Rec rainga 34899203          36.20000   -95.90000   0"  ||
        $stn_info eq "ABRFC/FSS  HRO               36.26667   -93.15000   0"  ||
        $stn_info eq "ABRFC/FSS  GAG               36.30000   -99.76667   0"  ||
        $stn_info eq "ABRFC/AMOS CAO               36.45000  -103.15000   0"  ||
        $stn_info eq "ABRFC/OBS  BVO               36.75000   -96.00000   0"  ||
        $stn_info eq "ABRFC/WSO  SGF               37.23333   -93.38333   0"  ||
        $stn_info eq "Rec rainga 05013005          37.45000  -105.86667   0"  ||
        $stn_info eq "ABRFC/FSS  CNU               37.66667   -95.48333   0"  ||
        $stn_info eq "Rec rainga 14216407          37.76667   -99.96667   0"  ||
        $stn_info eq "Rec rainga 05674001          38.28333  -104.51667   0"  ||
        $stn_info eq "ABRFC/OBS  GUC               38.55000  -106.91666   0"  ||
        $stn_info eq "Rec rainga 14716005          38.80000   -97.65000   0"  ||
        $stn_info eq "ABRFC/DCP  TRBC2             38.81667  -106.61667   0"  ||
        $stn_info eq "ABRFC/WSO  COS               38.81667  -104.71667   0"  ||
        $stn_info eq "ABRFC/WSFO TOP               39.06667   -95.63333   0"  ||
        $stn_info eq "ABRFC/SA   GJT               39.10000  -108.54972   0"  ||
        $stn_info eq "ABRFC/DCP  LXV               39.23333  -106.31667   0"  ||
        $stn_info eq "ABRFC/DCP  PRKC2             39.53333  -104.65000   0"  ||
        $stn_info eq "Rec rainga 14176702          39.55000   -97.65000   0"  ||
        $stn_info eq "ABRFC/WSFO DEN               39.76667  -104.86667   0"  ||
        $stn_info eq "Rec rainga 11671104          40.66667   -89.68333   0"  ||
        $stn_info eq "Rec rainga 25339505          40.96667   -98.31667   0"  ||
        $stn_info eq "Rec rainga 12303703          41.00000   -85.20000   0"  ||
        $stn_info eq "Rec rainga 12818702          41.70000   -86.31667   0"  ||
        $stn_info eq "Rec rainga 11738201          42.20000   -89.10000   0"  ||
        $stn_info eq "Rec rainga 13236703          42.40000   -90.70000   0"  ||
        $stn_info eq "Rec rainga 13770804          42.40000   -96.38333   0"  ||
        $stn_info eq "Rec rainga 25876002          42.86667  -100.55000   0"  ||
        $stn_info eq "Rec rainga 20333308          42.88333   -85.51667   0"  ||
        $stn_info eq "Rec rainga 48157008          42.91667  -106.46667   0"  ||
        $stn_info eq "Rec rainga 47547909          42.95000   -87.90000   0"  ||
        $stn_info eq "Rec rainga 47496108          43.13333   -89.33333   0"  ||
        $stn_info eq "Rec rainga 39693705          44.05000  -103.06667   0"  ||
        $stn_info eq "Rec rainga 47326906          44.48333   -88.13333   0"  ||
        $stn_info eq "Rec rainga 39893207          44.91667   -97.15000   0")
      {
      if ($debug)
         {
         printf "This stn need occurance set to 2.\n";
         }

      $occ = 2;
      }

   if ( $stn_info eq "Rec rainga 34566406          34.88333   -95.78333   0"  ||
        $stn_info eq "ABRFC/NNET ABQ               35.05000  -106.61667   0"  ||
        $stn_info eq "ABRFC/WSO  FSM               35.33333   -94.36667   0"  ||
        $stn_info eq "ABRFC/WSMO OKC               35.40000   -97.60000   0"  ||
        $stn_info eq "ABRFC/WSO  TUL               36.20000   -95.90000   0"  ||
        $stn_info eq "ABRFC/FSS  ALS               37.45000  -105.86667   0"  ||
        $stn_info eq "ABRFC/NNET DDC               37.76667   -99.96667   0"  ||
        $stn_info eq "ABRFC/WSO  PUB               38.28333  -104.51667   0"  ||
        $stn_info eq "ABRFC/FSS  SLN               38.80000   -97.65000   0")
      {
      if ($debug)
         {
         printf "This stn need occurance set to 3.\n";
         }
      $occ = 3;
      }

   if ($debug)
      {
      printf "%-s%1s%-s\n", $begin_line,$occ,$end_line;
      }
 
   printf OUTFILE "%-s%1s%-s", $begin_line,$occ,$end_line;

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "set_SWpcp_occ.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
