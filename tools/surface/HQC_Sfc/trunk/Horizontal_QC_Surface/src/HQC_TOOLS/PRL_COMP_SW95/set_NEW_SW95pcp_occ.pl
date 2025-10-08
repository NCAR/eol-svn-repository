#!/usr/bin/perl 

#-------------------------------------------------------
# set_NEW_SW95pcp_occ.pl - This perl script/program reads 
#   pqcf recs and changes the occ values in the requested 
#   recs to the indicated occ values.
#   Note that there approx. 1178 stns with occ setting,
#   In the stn lists for SW there are 1451 occs to
#   set, the difference is that about 300 are either
#   ABRFC - no set schedule OR soundings. Both of these
#   data types don't have occs to set in the actual data.
#
# 98 Jan lec
#   Created. Should be final. Note that this s/w was
#   actually only used to set the occ in the daily precip
#   since the other comps where done long time ago and
#   other scripts were used to set their occs.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nset_NEW_SW95pcp_occ.pl began at $year/$mon/$mday $hour:$min:$sec\n";
print "\nWARNING: S/W must be set differently to find MLC as ASOSH in hly and ASOS5 in 15min.\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].occ\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].occ") || die "Can NOT open output file for reading";

$lines_read = 0;
$occ1_ct = 0;
$occ1_71onlyct = 0;
$occ2_ct = 0;
$occ2_71onlyct = 0;
$occ3_ct = 0;
$occ3_71onlyct = 0;
$occ4_ct = 0;
$occ5_ct = 0;

$ABRFCocc = 0;
$SAOocc = 0;
$HPLAINSocc = 0;
$RecRaingaocc = 0;
$COOPocc = 0;
$otherocc = 0;

$occzero = 0;

#-----------------------------------------------------------
# For the open file, read all records, reset some occ values
# and set some to new values.
#-----------------------------------------------------------
while ($line = <INFILE>) 
   { 
   $lines_read++;

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
## $begin_line = substr ($line,  0,  70);  # for 15m and hly prcp.
   $begin_line = substr ($line,  0,  60);  # daily

## $end_line   = substr ($line, 71, 300);  # Hourly recs are 359 chars long
## $end_line   = substr ($line, 71, 1160); # 15 min recs are 1223 chars long
   $end_line   = substr ($line, 61, 470);  # daily
 
# Following set good for daily prcp recs only
   $id    = substr ( $line, 19, 17);
   $latlon = substr ( $line, 36, 24);
   $platform  = substr ( $line, 8, 5);
   $occ = substr ($line,  60,  1);

#-------------------------------------------------
# Following set good for 15m or hly recs of precip
#   $id    = substr ( $line, 29, 17);
#   $latlon = substr ( $line, 46, 24);
#   $platform  = substr ( $line, 18, 5);
#   $occ = substr ($line,  70,  1);
#-------------------------------------------------

   $stn = $id . $latlon;

   if ($debug)
      {  
      printf "\n---------\n";
      printf "Read line:\n$line\n";
      printf "id=: xxx%sxxx\n", $id;
      printf "occ=: xxx%sxxx\n", $occ;
      printf "latlon=: xxx%sxxx\n", $latlon;
      printf "platform=: xxx%sxxx\n", $platform;
      printf "stn=: xxx%sxxx\n", $stn;
      }  

   #---------------------------------------------
   # Note for ABRFC (71) for STORMWAVE - only occ
   # set to 1. No occs set to any higher value.
   #---------------------------------------------
   if ( $platform eq "ABRFC") 
      {  
      if ( $stn eq "BIGK1             37.26667   -95.46667   " || #1  71 hourly
           $stn eq "SYDO2             34.73333   -98.98333   " || #1  71 hourly 
           $stn eq "MLBK1             38.36666   -97.08334   " || #1  71 hourly
           $stn eq "LXVC2             39.23333  -106.31667   " || #1  71 15 minut
           $stn eq "CRYC2             39.56667  -104.85000   " ) #1  71 15 minut
           {
           if ($occ != 0 && $occ != 1)
              {
              printf "WARNING: $occ reset to 1 for $stn.\n";
              }

            $occ = 1;
            $occ1_ct++;
            $occ1_71onlyct++;

            $ABRFCocc++;

            if ($debug)
               {
               printf "--- Set occ to 1 for type 71 - ABRFC. ---\n";
               }
           }
        } # Type 71 = ABRFC


   if ( $platform eq "NCDC ") 
      {
      if ( $stn eq "GRI               40.96667   -98.31667   " || #1  82 hourly
           $stn eq "LFT               30.20000   -91.98333   " || #1  82 hourly
           $stn eq "HLC               39.38333   -99.83333   " || #1  82 hourly
           $stn eq "GRR               42.88333   -85.51667   " || #1  82 hourly
           $stn eq "CEZ               37.30000  -108.63333   " || #1  82 hourly
           $stn eq "DDC               37.76667   -99.96667   " || #1  82 hourly
           $stn eq "CNK               39.55000   -97.65000   " || #1  82 hourly
           $stn eq "ALS               37.45000  -105.86667   " || #1  82 hourly
           $stn eq "BHM               33.56667   -86.75000   " || #1  82 hourly
           $stn eq "HRO               36.26667   -93.15000   " || #1  82 hourly
           $stn eq "TUP               34.26667   -88.76667   " || #1  82 hourly
           $stn eq "SKX               36.45000  -105.66667   " || #1  82 hourly
           $stn eq "SBN               41.70000   -86.31667   " || #1  82 hourly
           $stn eq "PUB               38.28333  -104.51667   " || #1  82 hourly
           $stn eq "ABR               45.45000   -98.43333   " || #1  82 hourly
           $stn eq "AEL               43.68333   -93.36667   " || #1  82 hourly
           $stn eq "STC               45.55000   -94.06667   " || #1  82 hourly
           $stn eq "SNY               41.10000  -102.98333   " || #1  82 hourly
           $stn eq "VEL               40.45000  -109.51667   " || #1  82 hourly
           $stn eq "LIT               34.73333   -92.23333   " || #1  82 hourly
           $stn eq "ALO               42.55000   -92.40000   " || #1  82 hourly
           $stn eq "TCL               33.23333   -87.61667   " || #1  82 hourly
           $stn eq "PWA               35.53333   -97.63333   " || #1  82 hourly
           $stn eq "SAT               29.53333   -98.46667   " || #1  82 hourly
           $stn eq "DEN               39.76667  -104.86667   " || #1  82 hourly
           $stn eq "PNC               36.73333   -97.10000   " || #1  82 hourly
           $stn eq "DBQ               42.40000   -90.70000   " || #1  82 hourly
           $stn eq "MTJ               38.50000  -107.88333   " || #1  82 hourly
           $stn eq "OJC               38.85000   -94.73333   " || #1  82 hourly
           $stn eq "GLD               39.36667  -101.70000   " || #1  82 hourly
           $stn eq "MHK               39.15000   -96.66667   " || #1  82 hourly
           $stn eq "ESF               31.40000   -92.30000   " || #1  82 hourly
           $stn eq "LOZ               37.08333   -84.06667   " || #1  82 hourly
           $stn eq "LCH               30.11667   -93.21667   " || #1  82 hourly
           $stn eq "LBL               37.05000  -100.96667   " || #1  82 hourly
           $stn eq "LFK               31.23333   -94.75000   " || #1  82 hourly
           $stn eq "HSV               34.65000   -86.76667   " || #1  82 hourly
           $stn eq "HUF               39.45000   -87.30000   " || #1  82 hourly
           $stn eq "HBR               35.00000   -99.05000   " || #1  82 hourly
           $stn eq "CPR               42.91667  -106.46667   " || #1  82 hourly
           $stn eq "MCI               39.31667   -94.71667   " || #1  82 hourly
           $stn eq "GRB               44.48333   -88.13333   " || #1  82 hourly
           $stn eq "MSN               43.13333   -89.33333   " || #1  82 hourly
           $stn eq "MGM               32.30000   -86.40000   " || #1  82 hourly
           $stn eq "GAG               36.30000   -99.76667   " || #1  82 hourly
           $stn eq "LBB               33.65000  -101.81667   " || #1  82 hourly
           $stn eq "PIA               40.66667   -89.68333   " || #1  82 hourly
           $stn eq "VTN               42.86667  -100.55000   " || #1  82 hourly
           $stn eq "EST               43.40000   -94.75000   " || #1  82 hourly
           $stn eq "ELP               31.80000  -106.40000   " || #1  82 hourly
           $stn eq "MSL               34.75000   -87.61667   " || #1  82 hourly
           $stn eq "DRO               37.15000  -107.75000   " || #1  82 hourly
           $stn eq "OKC               35.40000   -97.60000   " || #1  82 hourly
           $stn eq "CNU               37.66667   -95.48333   " || #1  82 hourly
           $stn eq "TLH               30.38333   -84.36667   " || #1  82 hourly
           $stn eq "TUL               36.20000   -95.90000   " || #1  82 hourly
           $stn eq "CAG               40.50000  -107.53333   " || #1  82 hourly
           $stn eq "BFF               41.86667  -103.60000   " || #1  82 hourly
           $stn eq "BVO               36.75000   -96.00000   " || #1  82 hourly
           $stn eq "BIL               45.80000  -108.53333   " || #1  82 hourly
           $stn eq "ROW               33.30000  -104.53333   " || #1  82 hourly
           $stn eq "BEH               42.13333   -86.43333   " || #1  82 hourly
           $stn eq "AXN               45.86667   -95.38333   " || #1  82 hourly
           $stn eq "AUS               30.28333   -97.70000   " || #1  82 hourly
           $stn eq "SHV               32.46667   -93.81667   " || #1  82 hourly
           $stn eq "SJT               31.36667  -100.50000   " || #1  82 hourly
           $stn eq "SLN               38.80000   -97.65000   " || #1  82 hourly
           $stn eq "SPS               33.96667   -98.48333   " || #1  82 hourly
           $stn eq "ABI               32.41667   -99.68333   " || #1  82 hourly
           $stn eq "ABQ               35.05000  -106.61667   " || #1  82 hourly
           $stn eq "SUX               42.40000   -96.38333   " || #1  82 hourly
           $stn eq "MIC               45.06667   -93.35000   " || #1  82 hourly
           $stn eq "JAN               32.31667   -90.08333   " || #1  82 hourly
           $stn eq "FWA               41.00000   -85.20000   " || #1  82 hourly
           $stn eq "AIA               42.05000  -102.80000   " || #1  82 hourly
           $stn eq "LHX               38.05000  -103.51667   " || #1  82 hourly
           $stn eq "MKL               35.60000   -88.91667   " || #1  82 hourly
           $stn eq "LNK               40.85000   -96.75000   " || #1  82 hourly
           $stn eq "FOE               38.95000   -95.66667   " || #1  82 hourly
           $stn eq "MKE               42.95000   -87.90000   " || #1  82 hourly
           $stn eq "RFD               42.20000   -89.10000   " || #1  82 hourly
           $stn eq "RAP               44.05000  -103.06667   " || #1  82 hourly
           $stn eq "BTR               30.53333   -91.13333   " || #1  82 hourly
           $stn eq "BWG               36.96667   -86.43333   " || #1  82 hourly
           $stn eq "COD               44.51667  -109.01667   " || #1  82 hourly
           $stn eq "CSM               35.35000   -99.20000   " || #1  82 hourly
           $stn eq "OMA               41.30000   -95.90000   " || #1  82 hourly
           $stn eq "DAY               39.90000   -84.20000   " || #1  82 hourly
           $stn eq "ELD               33.21667   -92.80000   " || #1  82 hourly
           $stn eq "DNV               40.20000   -87.60000   " || #1  82 hourly
           $stn eq "MOB               30.68333   -88.25000   " || #1  82 hourly
           $stn eq "MML               44.45000   -95.81667   " || #1  82 hourly
           $stn eq "FSM               35.33333   -94.36667   " || #1  82 hourly
           $stn eq "MCK               40.21667  -100.58333   " )  #1  82 hourly
           {
            $occ = 1;
            $occ1_ct++;
            $SAOocc++;      
 
            if ($debug)  
               {
               printf "--- Set occ to 1 for type 82 - SAO. ---\n";
               }
           }
        } # Type  82 = NCDC SAO


   if ( $platform eq "NCDC ") 
      {  
         if ( $stn eq "MLC               34.88333   -95.78333   ") #2  82 hourly
           {
            $occ = 2;
            $occ2_ct++;
            $SAOocc++;
 
            if ($debug)
               {
               printf "--- Set occ to 2 for type 82 - SAO. ---\n";
               }
           }
        } # Type 82 = SAO


    if ($platform eq "Rec r") 
       {
       if ( $stn eq "22577609          32.33333   -88.75000   " || #1  46 hourly
            $stn eq "48167508          41.15000  -104.81667   " || #1  46 hourly
            $stn eq "48539009          42.81667  -108.73333   " || #1  46 hourly
            $stn eq "48815505          44.76667  -106.96667   " || #1  46 hourly
            $stn eq "23179102          38.81667   -92.21667   " || #1  46 hourly
            $stn eq "13220305          41.53333   -93.65000   " || #1  46 hourly
            $stn eq "23745502          38.75000   -90.36667   " || #1  46 hourly
            $stn eq "23797604          37.23333   -93.38333   " || #1  46 hourly
            $stn eq "25599503          41.98333   -97.43333   " || #1  46 hourly
            $stn eq "25606507          41.13333  -100.68333   " || #1  46 hourly
            $stn eq "11154902          42.00000   -87.88333   " || #1  46 hourly
            $stn eq "12425905          39.73333   -86.26667   " || #1  46 hourly
            $stn eq "21700409          43.91667   -92.50000   " || #1  46 hourly
            $stn eq "41589001          31.95000  -102.18333   " || #1  46 hourly
            $stn eq "21543506          44.88333   -93.21667   " || #1  46 hourly
            $stn eq "05348802          39.10000  -108.55000   " || #1  46 hourly
            $stn eq "20571205          43.16667   -86.23333   " || #1  46 hourly
            $stn eq "05177801          38.81667  -104.71667   " || #1  46 hourly
            $stn eq "12273807          38.05000   -87.53333   " || #1  46 hourly
            $stn eq "41224203          32.90000   -97.03333   " || #1  46 hourly
            $stn eq "14816706          39.06667   -95.63333   " || #1  46 hourly
            $stn eq "40595404          35.05000   -90.00000   " || #1  46 hourly
            $stn eq "41021101          35.23333  -101.70000   " || #1  46 hourly
            $stn eq "40640203          36.11667   -86.68333   " || #1  46 hourly
            $stn eq "40165601          35.03333   -85.20000   " || #1  46 hourly
            $stn eq "11817906          39.85000   -89.68333   " || #1  46 hourly
            $stn eq "15495402          38.18333   -85.73333   " || #1  46 hourly
            $stn eq "39766709          43.56667   -96.73333   " || #1  46 hourly
            $stn eq "39412707          44.38333   -98.21667   " || #1  46 hourly
            $stn eq "48753310          41.80000  -107.20000   " || #1  46 15 minut
            $stn eq "13458508          40.61667   -93.95000   " || #1  46 15 minut
            $stn eq "13638909          41.10000   -92.45000   " || #1  46 15 minut
            $stn eq "05703102          39.53333  -107.80000   " || #1  46 15 minut
            $stn eq "13784401          43.16667   -95.15000   " || #1  46 15 minut
            $stn eq "25625506          41.30000   -95.90000   " || #1  46 15 minut
            $stn eq "29188703          36.45000  -103.15000   " || #1  46 15 minut
            $stn eq "14243207          37.00000  -101.88333   " || #1  46 15 minut
            $stn eq "05010903          40.15000  -103.15000   " || #1  46 15 minut
            $stn eq "41941703          31.60000   -97.21667   " || #1  46 15 minut
            $stn eq "14612806          38.61667   -95.28333   " || #1  46 15 minut
            $stn eq "21917007          43.65000   -95.58333   " || #1  46 15 minut
            $stn eq "05366202          38.55000  -106.91667   " || #1  46 15 minut
            $stn eq "41169802          34.43333  -100.28333   " || #1  46 15 minut
            $stn eq "39107607          44.31667   -96.76667   " || #1  46 15 minut
            $stn eq "41224403          32.85000   -96.85000   " || #1  46 15 minut
            $stn eq "41328403          32.81667   -97.35000   "  )  #1  46 15 minut
         {
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn.\n"; 
            }  

         $occ = 1;
         $occ1_ct++;
         $RecRaingaocc++;

         if ($debug)  
             { 
             printf "--- Set occ to 1 for type 46 = Rec Rainga. ---\n";
             }
         }
       } # Type46  - Rec rainga
    
   
    if ($platform eq "AWOS2") # AWOS20
       { 
       if ($stn eq "ADH               34.80000   -96.67000   " ) #1  41 20 minut
         {  
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn (AWOS20).\n"; 
            }  

         $occ = 1; 
         $occ1_ct++;
         $otherocc++;

         if ($debug)  
           { 
           printf "--- Set occ to 1 for type 41. (ADH)---\n"; 
           }
         } 
       } # Type 41


    if ($platform eq "AWOS1")
       { 
        if ($stn eq "Boone             42.05200   -93.84800   " || #1  41 1 minute Iowa
            $stn eq "Clinton           41.83200   -90.32800   " ) #1  41 1 minute Iowa
         {
         if ($occ != 0 && $occ != 1)
            {
            printf "WARNING: $occ reset to 1 for $stn (Iowa Awos).\n";
            }
 
         $occ = 1;
         $occ1_ct++;
         $otherocc++;
 
         if ($debug)
           {
           printf "--- Set occ to 1 for type 41. (ADH)---\n";
           }
         }
       } # Type  Iowa Awos


    if ($platform eq "ASOSH")
       {  
       if ($stn eq "MLC               34.88000   -95.78000   " ) #1  40 5 minute
         {  
         if ($occ != 0 && $occ != 1) 
           { 
           printf "WARNING: $occ reset to 1 for $stn. (ASOSH or ASOS5)\n"; 
           }  

         $occ = 1;  
         $occ1_ct++; 
         $otherocc++;

         if ($debug)  
            { 
            printf "--- Set occ to 1 for type 40. (ASOS5 or ASOSH MLC) ---\n";  
            }
         }  
       } # Type 40


    if ($platform eq "WPDN ")
       {  
       if ($stn eq "Purcell           34.97972   -97.51862   " ) #1   1 6 minute
         {  
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn. (WPDN)\n"; 
            }  

         $occ = 1;   
         $occ1_ct++;  
         $otherocc++;

         if ($debug)  
             { 
             printf "--- Set occ to 1 for type 1 = WPDN. (Purcell)---\n";  
             }
         }   
       } # Type 1


    if ($platform eq "HPLAI") # HPLAINS
       {    
       if ($stn eq "101_St. Joseph    39.77000   -94.92000   " || #1  16 hourly
           $stn eq "081_Watertown     44.92000   -97.15000   " || #1  16 hourly
           $stn eq "051_Mead Turf F   41.13000   -96.50000   " ) #1  16 hourly 
         {
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn. (HPLAINS)\n"; 
            }  

         $occ = 1;
         $occ1_ct++;
         $HPLAINSocc++;

         if ($debug)  
           { 
           printf "--- Set occ to 1 for type 16. (101)---\n";
           }
         }
       } # Type 16


    #------------------------------------------------------
    #  Set appropriate occs to 2 for specific stns/plaform
    #  combos.
    #------------------------------------------------------
    if ($platform eq "Rec r") # Rec rainga
       { 
       if ($stn eq "41541101          33.65000  -101.81667   " ||  #2  46 hourly
            $stn eq "25876002          42.86667  -100.55000   " ||  #2  46 hourly
            $stn eq "12818702          41.70000   -86.31667   " ||  #2  46 hourly
            $stn eq "48157008          42.91667  -106.46667   " ||  #2  46 hourly
            $stn eq "05674001          38.28333  -104.51667   " ||  #2  46 hourly
            $stn eq "11671104          40.66667   -89.68333   " ||  #2  46 hourly
            $stn eq "47547909          42.95000   -87.90000   " ||  #2  46 hourly
            $stn eq "14176702          39.55000   -97.65000   " ||  #2  46 hourly
            $stn eq "25479506          40.85000   -96.75000   " ||  #2  46 hourly
            $stn eq "39693705          44.05000  -103.06667   " ||  #2  46 hourly
            $stn eq "25766501          41.86667  -103.60000   " ||  #2  46 hourly
            $stn eq "13770804          42.40000   -96.38333   " ||  #2  46 hourly
            $stn eq "01406401          34.65000   -86.76667   " ||  #2  46 hourly
            $stn eq "41941903          31.61667   -97.21667   " ||  #2  46 hourly
            $stn eq "20333308          42.88333   -85.51667   " ||  #2  46 hourly
            $stn eq "12303703          41.00000   -85.20000   " ||  #2  46 hourly
            $stn eq "29023405          35.05000  -106.61667   " ||  #2  46 hourly
            $stn eq "01547808          30.68333   -88.25000   " ||  #2  46 hourly
            $stn eq "34899203          36.20000   -95.90000   " ||  #2  46 hourly
            $stn eq "16844001          32.46667   -93.81667   " ||  #2  46 hourly
            $stn eq "41001602          32.41667   -99.68333   " ||  #2  46 hourly
            $stn eq "41042807          30.28333   -97.70000   " ||  #2  46 hourly
            $stn eq "41794306          31.36667  -100.50000   " ||  #2  46 hourly
            $stn eq "34666105          35.40000   -97.60000   " ||  #2  46 hourly
            $stn eq "05013005          37.45000  -105.86667   " ||  #2  46 hourly
            $stn eq "22447205          32.31667   -90.08333   " ||  #2  46 hourly
            $stn eq "47326906          44.48333   -88.13333   " ||  #2  46 hourly
            $stn eq "16507807          30.11667   -93.21667   " ||  #2  46 hourly
            $stn eq "11738201          42.20000   -89.10000   " ||  #2  46 hourly
            $stn eq "03257404          35.33333   -94.36667   " ||  #2  46 hourly
            $stn eq "13236703          42.40000   -90.70000   " ||  #2  46 hourly
            $stn eq "41279705          31.80000  -106.40000   " ||  #2  46 hourly
            $stn eq "16054906          30.53333   -91.13333   " ||  #2  46 hourly
            $stn eq "47496108          43.13333   -89.33333   " ||  #2  46 hourly
            $stn eq "01555006          32.30000   -86.40000   " ||  #2  46 hourly
            $stn eq "22900303          34.26667   -88.76667   " ||  #2  46 hourly
            $stn eq "23435801          39.31667   -94.71667   " ||  #2  46 hourly
            $stn eq "25339505          40.96667   -98.31667   " ||  #2  46 hourly
            $stn eq "14216407          37.76667   -99.96667   " ||  #2  46 hourly
            $stn eq "13870603          42.55000   -92.40000   " ||  #2  46 hourly
            $stn eq "14716005          38.80000   -97.65000   " ||  #2  46 15 minut
            $stn eq "29761007          33.30000  -104.53333   " ||  #2  46 15 minut
            $stn eq "01083102          33.56667   -86.75000   " ||  #2  46 15 minut
            $stn eq "39893207          44.91667   -97.15000   " )  #2  46 15 minut
         {
         if ($occ != 0 && $occ != 2) 
            { 
            printf "WARNING: $occ reset to 2 for $stn.\n"; 
            }  

         $occ = 2;
         $occ2_ct++;
         $RecRaingaocc++;

         if ($debug)  
           { 
           printf "--- Set occ to 2 for type 46 - Rec rainga. ---\n";
           }
         }
       } # Type 46 - Rec Rainga


    if ($platform eq "Rec r") # Rec rainga
       { 
       if ($stn eq "34566406          34.88333   -95.78333   " )  #3  46 15 minut
         {
         if ($occ != 0 && $occ != 3)  
            {  
            printf "WARNING: $occ reset to 3 for $stn.\n";  
            }   

         $occ = 3;
         $occ3_ct++;
         $RecRaingaocc++;

         if ($debug)  
           { 
           printf "--- Set occ to 3 for type 46 - Rec rainga. ---\n";
           }
         }
       } # Type 46 - Rec rainga


    #-------------------------------
    # Begin COOP prcp occ settings.
    #-------------------------------
    if ($platform eq "COOP ") # COOP precip only
       {
       if ($stn eq "010008            31.58333   -85.28333   " || #1   2 daily
           $stn eq "010252            31.30000   -86.53333   " || #1   2 daily
           $stn eq "010402            31.16667   -87.48333   " || #1   2 daily
           $stn eq "013519            31.85000   -86.65000   " || #1   2 daily
           $stn eq "030064            35.90000   -91.08333   " || #1   2 daily
           $stn eq "033704            34.70000   -93.05000   " || #1   2 daily
           $stn eq "128036            38.66667   -86.80000   " || #1   2 daily
           $stn eq "226718            30.40000   -88.48333   " || #1   2 daily
           $stn eq "475120            44.65000   -90.13333   " || #1   2 daily
           $stn eq "475335            44.88333   -91.93333   " || #1   2 daily
           $stn eq "227132            31.96667   -91.00000   " || #1   2 daily
           $stn eq "227220            31.15000   -89.40000   " || #1   2 daily
           $stn eq "476718            43.51667   -89.43333   " || #1   2 daily
           $stn eq "478267            44.86667   -87.33333   " || #1   2 daily
           $stn eq "128784            40.21667   -86.11667   " || #1   2 daily
           $stn eq "227444            31.30000   -88.90000   " || #1   2 daily
           $stn eq "478589            44.00000   -91.43333   " || #1   2 daily
           $stn eq "478968            44.91667   -89.61667   " || #1   2 daily
           $stn eq "227467            34.73333   -88.95000   " || #1   2 daily
           $stn eq "481000            43.41667  -108.18333   " || #1   2 daily
           $stn eq "481165            44.35000  -106.68333   " || #1   2 daily
           $stn eq "227560            32.90000   -90.88333   " || #1   2 daily
           $stn eq "227815            34.40000   -89.80000   " || #1   2 daily
           $stn eq "034185            33.36667   -93.56667   " || #1   2 daily
           $stn eq "128967            37.80000   -87.98333   " || #1   2 daily
           $stn eq "227840            30.63333   -89.05000   " || #1   2 daily
           $stn eq "482725            43.41667  -104.95000   " || #1   2 daily
           $stn eq "228053            31.86667   -88.70000   " || #1   2 daily
           $stn eq "485415            41.31667  -105.68333   " || #1   2 daily
           $stn eq "128999            41.51667   -87.03333   " || #1   2 daily
           $stn eq "228374            33.46667   -88.78333   " || #1   2 daily
           $stn eq "486120            41.90000  -106.20000   " || #1   2 daily
           $stn eq "486395            44.26667  -104.95000   " || #1   2 daily
           $stn eq "228445            33.43333   -90.91667   " || #1   2 daily
           $stn eq "486660            43.85000  -104.21667   " || #1   2 daily
           $stn eq "487200            41.63333  -104.48333   " || #1   2 daily
           $stn eq "054770            38.08333  -102.61667   " || #1   2 daily
           $stn eq "129424            40.41667   -86.93333   " || #1   2 daily
           $stn eq "487240            41.16667  -104.15000   " || #1   2 daily
           $stn eq "129430            40.46667   -87.00000   " || #1   2 daily
           $stn eq "229079            34.38333   -89.53333   " || #1   2 daily
           $stn eq "488626            44.56667  -106.90000   " || #1   2 daily
           $stn eq "229860            32.90000   -90.38333   " || #1   2 daily
           $stn eq "488852            44.06667  -107.41667   " || #1   2 daily
           $stn eq "488858            43.81667  -107.36667   " || #1   2 daily
           $stn eq "030220            34.15000   -93.05000   " || #1   2 daily
           $stn eq "054834            38.06667  -103.21667   " || #1   2 daily
           $stn eq "130364            41.41667   -95.00000   " || #1   2 daily
           $stn eq "230022            37.08333   -89.90000   " || #1   2 daily
           $stn eq "488875            43.65000  -108.20000   " || #1   2 daily
           $stn eq "488995            42.08333  -104.21667   " || #1   2 daily
           $stn eq "230164            36.65000   -94.43333   " || #1   2 daily
           $stn eq "489615            42.11667  -104.95000   " || #1   2 daily
           $stn eq "489770            44.01667  -107.96667   " || #1   2 daily
           $stn eq "131060            40.81667   -91.16667   " || #1   2 daily
           $stn eq "230204            38.20000   -94.03333   " || #1   2 daily
           $stn eq "489785            43.96667  -107.96667   " || #1   2 daily
           $stn eq "230657            37.08333   -93.55000   " || #1   2 daily
           $stn eq "131063            40.78333   -91.11667   " || #1   2 daily
           $stn eq "231289            37.23333   -89.56667   " || #1   2 daily
           $stn eq "231600            39.53333   -91.63333   " || #1   2 daily
           $stn eq "131257            42.30000   -91.01667   " || #1   2 daily
           $stn eq "231640            39.36667   -90.90000   " || #1   2 daily
           $stn eq "231674            37.11667   -90.78333   " || #1   2 daily
           $stn eq "034548            33.31667   -93.23333   " || #1   2 daily
           $stn eq "131314            41.88333   -91.70000   " || #1   2 daily
           $stn eq "231711            38.40000   -93.76667   " || #1   2 daily
           $stn eq "131354            40.73333   -92.86667   " || #1   2 daily
           $stn eq "232240            36.98333   -94.35000   " || #1   2 daily
           $stn eq "055531            37.20000  -108.48333   " || #1   2 daily
           $stn eq "131651            40.68333   -93.50000   " || #1   2 daily
           $stn eq "232302            36.76667   -92.25000   " || #1   2 daily
           $stn eq "232318            40.48333   -92.36667   " || #1   2 daily
           $stn eq "132203            41.53333   -93.65000   " || #1   2 daily
           $stn eq "232568            38.86667   -94.03333   " || #1   2 daily
           $stn eq "232809            37.78333   -90.40000   " || #1   2 daily
           $stn eq "013645            34.10000   -87.98333   " || #1   2 daily
           $stn eq "030326            35.30000   -91.38333   " || #1   2 daily
           $stn eq "055734            39.10000  -104.86667   " || #1   2 daily
           $stn eq "233079            38.85000   -91.95000   " || #1   2 daily
           $stn eq "233369            40.48333   -94.41667   " || #1   2 daily
           $stn eq "233601            39.71667   -91.36667   " || #1   2 daily
           $stn eq "055922            40.60000  -103.85000   " || #1   2 daily
           $stn eq "233999            36.05000   -90.11667   " || #1   2 daily
           $stn eq "234271            38.58333   -92.18333   " || #1   2 daily
           $stn eq "132977            43.28333   -93.63333   " || #1   2 daily
           $stn eq "234315            37.15000   -94.50000   " || #1   2 daily
           $stn eq "299330            35.81667  -104.93333   " || #1   2 daily
           $stn eq "056023            40.70000  -104.78333   " || #1   2 daily
           $stn eq "133473            41.71667   -92.73333   " || #1   2 daily
           $stn eq "234359            39.11667   -94.60000   " || #1   2 daily
           $stn eq "134038            42.40000   -95.51667   " || #1   2 daily
           $stn eq "234544            40.21667   -92.58333   " || #1   2 daily
           $stn eq "234694            38.20000   -92.61667   " || #1   2 daily
           $stn eq "056131            38.21667  -103.71667   " || #1   2 daily
           $stn eq "134101            41.65000   -91.53333   " || #1   2 daily
           $stn eq "234705            37.50000   -94.26667   " || #1   2 daily
           $stn eq "234825            37.66667   -92.65000   " || #1   2 daily
           $stn eq "134381            40.40000   -91.36667   " || #1   2 daily
           $stn eq "234919            37.55000   -91.90000   " || #1   2 daily
           $stn eq "235027            37.38333   -93.95000   " || #1   2 daily
           $stn eq "030586            36.31667   -94.21667   " || #1   2 daily
           $stn eq "035018            35.65000   -94.15000   " || #1   2 daily
           $stn eq "134502            41.33333   -93.11667   " || #1   2 daily
           $stn eq "235050            39.73333   -92.48333   " || #1   2 daily
           $stn eq "235298            39.11667   -93.21667   " || #1   2 daily
           $stn eq "134585            40.61667   -93.95000   " || #1   2 daily
           $stn eq "235307            37.33333   -92.90000   " || #1   2 daily
           $stn eq "235671            39.40000   -92.43333   " || #1   2 daily
           $stn eq "056203            38.01667  -107.66667   " || #1   2 daily
           $stn eq "235834            37.15000   -92.26667   " || #1   2 daily
           $stn eq "235862            37.06667   -93.88333   " || #1   2 daily
           $stn eq "134963            41.86667   -90.93333   " || #1   2 daily
           $stn eq "235987            37.85000   -94.40000   " || #1   2 daily
           $stn eq "236012            39.01667   -92.76667   " || #1   2 daily
           $stn eq "035186            35.60000   -91.28333   " || #1   2 daily
           $stn eq "135198            42.06667   -92.93333   " || #1   2 daily
           $stn eq "236357            39.96667   -95.13333   " || #1   2 daily
           $stn eq "236402            38.05000   -93.70000   " || #1   2 daily
           $stn eq "135235            43.15000   -93.33333   " || #1   2 daily
           $stn eq "236460            36.66667   -93.11667   " || #1   2 daily
           $stn eq "236563            40.03333   -94.13333   " || #1   2 daily
           $stn eq "135769            40.68333   -94.30000   " || #1   2 daily
           $stn eq "236678            36.95000   -94.00000   " || #1   2 daily
           $stn eq "236777            37.91667   -93.31667   " || #1   2 daily
           $stn eq "135796            40.95000   -91.55000   " || #1   2 daily
           $stn eq "237300            38.38333   -91.40000   " || #1   2 daily
           $stn eq "237452            38.63333   -90.20000   " || #1   2 daily
           $stn eq "010430            32.60000   -85.50000   " || #1   2 daily
           $stn eq "013655            34.05000   -86.76667   " || #1   2 daily
           $stn eq "035320            34.83333   -92.26667   " || #1   2 daily
           $stn eq "056765            38.26667  -104.71667   " || #1   2 daily
           $stn eq "137664            43.45000   -95.71667   " || #1   2 daily
           $stn eq "237506            37.63333   -91.53333   " || #1   2 daily
           $stn eq "056977            37.71667  -105.31667   " || #1   2 daily
           $stn eq "137700            43.05000   -96.15000   " || #1   2 daily
           $stn eq "237813            40.28333   -95.08333   " || #1   2 daily
           $stn eq "237862            39.38333   -94.55000   " || #1   2 daily
           $stn eq "138688            41.28333   -91.68333   " || #1   2 daily
           $stn eq "238003            40.20000   -94.55000   " || #1   2 daily
           $stn eq "238043            38.00000   -91.36667   " || #1   2 daily
           $stn eq "238051            39.96667   -91.88333   " || #1   2 daily
           $stn eq "238082            37.70000   -93.78333   " || #1   2 daily
           $stn eq "238171            38.18333   -91.13333   " || #1   2 daily
           $stn eq "238223            38.96667   -93.41667   " || #1   2 daily
           $stn eq "238466            38.25000   -93.36667   " || #1   2 daily
           $stn eq "238524            38.95000   -94.40000   " || #1   2 daily
           $stn eq "035354            35.78333   -94.41667   " || #1   2 daily
           $stn eq "057167            38.03333  -103.70000   " || #1   2 daily
           $stn eq "238614            38.11667   -91.76667   " || #1   2 daily
           $stn eq "238620            38.20000   -91.98333   " || #1   2 daily
           $stn eq "238664            37.25000   -94.56667   " || #1   2 daily
           $stn eq "238700            36.93333   -90.28333   " || #1   2 daily
           $stn eq "057287            38.83333  -104.08333   " || #1   2 daily
           $stn eq "138806            42.46667   -93.80000   " || #1   2 daily
           $stn eq "238880            36.73333   -91.83333   " || #1   2 daily
           $stn eq "250245            41.60000   -99.83333   " || #1   2 daily
           $stn eq "140135            38.46667   -99.55000   " || #1   2 daily
           $stn eq "250622            40.30000   -96.75000   " || #1   2 daily
           $stn eq "250760            40.05000  -101.53333   " || #1   2 daily
           $stn eq "035358            34.60000   -93.76667   " || #1   2 daily
           $stn eq "057296            40.71667  -105.71667   " || #1   2 daily
           $stn eq "140264            37.15000   -98.08333   " || #1   2 daily
           $stn eq "250865            41.06667  -102.08333   " || #1   2 daily
           $stn eq "251145            41.66667  -103.10000   " || #1   2 daily
           $stn eq "252020            40.61667   -96.95000   " || #1   2 daily
           $stn eq "252100            40.66667  -100.50000   " || #1   2 daily
           $stn eq "057309            38.85000  -104.98333   " || #1   2 daily
           $stn eq "140365            37.20000   -99.76667   " || #1   2 daily
           $stn eq "253037            40.10000   -98.96667   " || #1   2 daily
           $stn eq "253185            41.45000   -97.76667   " || #1   2 daily
           $stn eq "140424            37.43333   -96.76667   " || #1   2 daily
           $stn eq "030814            34.31667   -94.43333   " || #1   2 daily
           $stn eq "035591            34.80000   -94.00000   " || #1   2 daily
           $stn eq "057370            38.53333  -106.01667   " || #1   2 daily
           $stn eq "140441            39.68333  -100.96667   " || #1   2 daily
           $stn eq "253690            40.51667  -101.01667   " || #1   2 daily
           $stn eq "253735            40.16667   -97.58333   " || #1   2 daily
           $stn eq "140447            37.66667   -96.98333   " || #1   2 daily
           $stn eq "057430            37.18333  -105.40000   " || #1   2 daily
           $stn eq "140620            38.26667   -99.75000   " || #1   2 daily
           $stn eq "140865            38.51667   -99.20000   " || #1   2 daily
           $stn eq "057557            39.55000  -103.35000   " || #1   2 daily
           $stn eq "140957            39.06667   -94.88333   " || #1   2 daily
           $stn eq "141141            38.20000   -99.53333   " || #1   2 daily
           $stn eq "256720            42.20000   -97.53333   " || #1   2 daily
           $stn eq "057572            37.71667  -105.23333   " || #1   2 daily
           $stn eq "141173            38.08333   -96.88333   " || #1   2 daily
           $stn eq "141202            38.65000   -96.25000   " || #1   2 daily
           $stn eq "258395            40.66667   -96.18333   " || #1   2 daily
           $stn eq "030936            34.88333   -91.18333   " || #1   2 daily
           $stn eq "035760            34.58333   -93.90000   " || #1   2 daily
           $stn eq "141351            38.05000   -96.63333   " || #1   2 daily
           $stn eq "259510            40.86667   -97.60000   " || #1   2 daily
           $stn eq "290041            36.23333  -106.43333   " || #1   2 daily
           $stn eq "290199            32.88333  -105.95000   " || #1   2 daily
           $stn eq "141383            38.80000   -99.71667   " || #1   2 daily
           $stn eq "290417            31.95000  -108.81667   " || #1   2 daily
           $stn eq "141425            38.80000   -96.23333   " || #1   2 daily
           $stn eq "290600            32.76667  -104.38333   " || #1   2 daily
           $stn eq "290640            34.08333  -107.61667   " || #1   2 daily
           $stn eq "036008            35.30000   -93.88333   " || #1   2 daily
           $stn eq "141452            38.33333   -98.31667   " || #1   2 daily
           $stn eq "290818            33.41667  -108.11667   " || #1   2 daily
           $stn eq "291286            32.90000  -107.30000   " || #1   2 daily
           $stn eq "291469            32.41667  -104.23333   " || #1   2 daily
           $stn eq "291475            32.33333  -104.26667   " || #1   2 daily
           $stn eq "058434            37.26222  -104.33833   " || #1   2 daily
           $stn eq "141522            37.81667  -100.35000   " || #1   2 daily
           $stn eq "291515            33.63333  -105.88333   " || #1   2 daily
           $stn eq "291664            36.91667  -106.58333   " || #1   2 daily
           $stn eq "141536            38.51667   -98.53333   " || #1   2 daily
           $stn eq "291982            35.63333  -106.31667   " || #1   2 daily
           $stn eq "010063            34.21667   -87.16667   " || #1   2 daily
           $stn eq "010272            33.58333   -85.85000   " || #1   2 daily
           $stn eq "031140            33.30000   -92.48333   " || #1   2 daily
           $stn eq "036928            35.30000   -93.65000   " || #1   2 daily
           $stn eq "058436            37.15000  -104.55000   " || #1   2 daily
           $stn eq "141704            37.26667   -99.33333   " || #1   2 daily
           $stn eq "292030            35.40000  -104.18333   " || #1   2 daily
           $stn eq "292100            35.23333  -106.60000   " || #1   2 daily
           $stn eq "292207            33.51667  -103.35000   " || #1   2 daily
           $stn eq "292241            36.03333  -106.96667   " || #1   2 daily
           $stn eq "058501            39.10000  -106.35000   " || #1   2 daily
           $stn eq "141730            38.90000  -100.11667   " || #1   2 daily
           $stn eq "292250            35.08333  -107.51667   " || #1   2 daily
           $stn eq "292436            32.25000  -107.73333   " || #1   2 daily
           $stn eq "292837            36.60000  -106.73333   " || #1   2 daily
           $stn eq "293142            36.70000  -108.25000   " || #1   2 daily
           $stn eq "058781            37.63333  -104.78333   " || #1   2 daily
           $stn eq "293265            32.80000  -108.15000   " || #1   2 daily
           $stn eq "293422            35.51667  -108.78333   " || #1   2 daily
           $stn eq "293488            35.90000  -105.43333   " || #1   2 daily
           $stn eq "293682            35.16667  -107.90000   " || #1   2 daily
           $stn eq "059096            40.03333  -106.21667   " || #1   2 daily
           $stn eq "141867            38.68333   -96.51667   " || #1   2 daily
           $stn eq "294009            32.93333  -107.56667   " || #1   2 daily
           $stn eq "294398            35.95000  -107.08333   " || #1   2 daily
           $stn eq "142135            38.56667   -96.76667   " || #1   2 daily
           $stn eq "294426            32.61667  -106.73333   " || #1   2 daily
           $stn eq "294719            35.03333  -107.36667   " || #1   2 daily
           $stn eq "031152            33.60000   -92.81667   " || #1   2 daily
           $stn eq "059216            37.01667  -104.48333   " || #1   2 daily
           $stn eq "294742            36.98333  -104.36667   " || #1   2 daily
           $stn eq "294862            35.53333  -105.20000   " || #1   2 daily
           $stn eq "295370            32.81667  -103.70000   " || #1   2 daily
           $stn eq "081986            30.78333   -86.51667   " || #1   2 daily
           $stn eq "295490            36.56667  -104.56667   " || #1   2 daily
           $stn eq "295754            32.93333  -108.01667   " || #1   2 daily
           $stn eq "142430            37.28333   -95.80000   " || #1   2 daily
           $stn eq "296275            36.18333  -105.05000   " || #1   2 daily
           $stn eq "296435            32.38333  -106.10000   " || #1   2 daily
           $stn eq "038052            35.25000   -90.80000   " || #1   2 daily
           $stn eq "296659            32.65000  -103.38333   " || #1   2 daily
           $stn eq "296812            34.50000  -107.90000   " || #1   2 daily
           $stn eq "297094            34.41667  -105.88333   " || #1   2 daily
           $stn eq "297254            34.15000  -105.00000   " || #1   2 daily
           $stn eq "086240            30.51667   -86.50000   " || #1   2 daily
           $stn eq "142470            38.41667   -96.83333   " || #1   2 daily
           $stn eq "297323            36.70000  -105.40000   " || #1   2 daily
           $stn eq "142548            38.43333   -96.20000   " || #1   2 daily
           $stn eq "297423            33.75000  -107.21667   " || #1   2 daily
           $stn eq "015112            32.70000   -87.26667   " || #1   2 daily
           $stn eq "015478            30.68333   -88.25000   " || #1   2 daily
           $stn eq "086842            30.21667   -85.60000   " || #1   2 daily
           $stn eq "142574            38.90000   -97.11667   " || #1   2 daily
           $stn eq "142622            37.81667   -96.28333   " || #1   2 daily
           $stn eq "298085            35.61667  -105.98333   " || #1   2 daily
           $stn eq "298387            34.08333  -106.88333   " || #1   2 daily
           $stn eq "086997            30.46667   -87.20000   " || #1   2 daily
           $stn eq "298501            36.36667  -104.58333   " || #1   2 daily
           $stn eq "298518            35.16667  -105.96667   " || #1   2 daily
           $stn eq "142773            38.25000   -96.93333   " || #1   2 daily
           $stn eq "298535            32.28333  -106.75000   " || #1   2 daily
           $stn eq "031666            34.43333   -94.41667   " || #1   2 daily
           $stn eq "091640            33.60000   -85.08333   " || #1   2 daily
           $stn eq "299031            35.80000  -107.18333   " || #1   2 daily
           $stn eq "299156            35.20000  -103.68333   " || #1   2 daily
           $stn eq "142835            37.85000   -94.70000   " || #1   2 daily
           $stn eq "142945            37.46667   -95.35000   " || #1   2 daily
           $stn eq "299569            32.38333  -103.80000   " || #1   2 daily
           $stn eq "094941            34.65000   -85.35000   " || #1   2 daily
           $stn eq "299686            32.78333  -106.18333   " || #1   2 daily
           $stn eq "299897            35.06667  -108.83333   " || #1   2 daily
           $stn eq "340017            34.78333   -96.68333   " || #1   2 daily
           $stn eq "031750            34.70000   -92.45000   " || #1   2 daily
           $stn eq "050114            40.16667  -103.21667   " || #1   2 daily
           $stn eq "094949            33.05000   -85.01667   " || #1   2 daily
           $stn eq "143008            38.28333   -95.23333   " || #1   2 daily
           $stn eq "340193            36.80000   -98.65000   " || #1   2 daily
           $stn eq "340200            35.16667   -97.88333   " || #1   2 daily
           $stn eq "340364            34.78333   -96.06667   " || #1   2 daily
           $stn eq "099291            32.86667   -85.18333   " || #1   2 daily
           $stn eq "143248            37.36667   -96.45000   " || #1   2 daily
           $stn eq "340394            34.45000   -96.06667   " || #1   2 daily
           $stn eq "143257            38.10000   -95.88333   " || #1   2 daily
           $stn eq "340631            35.75000   -96.08333   " || #1   2 daily
           $stn eq "110330            40.23333   -90.93333   " || #1   2 daily
           $stn eq "143441            38.33333   -95.38333   " || #1   2 daily
           $stn eq "340755            36.53333   -97.45000   " || #1   2 daily
           $stn eq "340782            35.98333   -95.88333   " || #1   2 daily
           $stn eq "143594            38.66667   -96.95000   " || #1   2 daily
           $stn eq "340818            36.81667   -97.23333   " || #1   2 daily
           $stn eq "340908            36.73333  -102.48333   " || #1   2 daily
           $stn eq "143665            39.36667   -99.83333   " || #1   2 daily
           $stn eq "340980            34.08333   -95.88333   " || #1   2 daily
           $stn eq "341144            35.83333   -96.38333   " || #1   2 daily
           $stn eq "143667            38.35000   -97.20000   " || #1   2 daily
           $stn eq "341162            34.05000   -94.73333   " || #1   2 daily
           $stn eq "341168            34.13333   -94.70000   " || #1   2 daily
           $stn eq "031900            35.83333   -93.20000   " || #1   2 daily
           $stn eq "032148            33.88333   -91.48333   " || #1   2 daily
           $stn eq "110510            38.50000   -89.85000   " || #1   2 daily
           $stn eq "111302            38.06667   -88.18333   " || #1   2 daily
           $stn eq "143810            39.66667   -95.51667   " || #1   2 daily
           $stn eq "341243            36.83333   -99.61667   " || #1   2 daily
           $stn eq "341256            36.70000   -96.73333   " || #1   2 daily
           $stn eq "341391            34.96667   -96.25000   " || #1   2 daily
           $stn eq "341445            36.08333   -98.60000   " || #1   2 daily
           $stn eq "143822            37.51667   -96.20000   " || #1   2 daily
           $stn eq "341499            34.15000   -94.63333   " || #1   2 daily
           $stn eq "143847            38.10000   -98.65000   " || #1   2 daily
           $stn eq "341620            36.31667   -98.98333   " || #1   2 daily
           $stn eq "341684            35.70000   -96.88333   " || #1   2 daily
           $stn eq "143855            37.18333  -101.35000   " || #1   2 daily
           $stn eq "143930            37.93333   -98.03333   " || #1   2 daily
           $stn eq "341706            34.45000   -98.61667   " || #1   2 daily
           $stn eq "341711            35.46667   -95.51667   " || #1   2 daily
           $stn eq "341717            36.48333   -95.41667   " || #1   2 daily
           $stn eq "341745            34.50000   -96.96667   " || #1   2 daily
           $stn eq "143954            37.25000   -95.70000   " || #1   2 daily
           $stn eq "341828            36.31667   -95.58333   " || #1   2 daily
           $stn eq "143974            38.23333   -97.78333   " || #1   2 daily
           $stn eq "342039            35.35000   -98.68333   " || #1   2 daily
           $stn eq "342125            35.28333   -98.98333   " || #1   2 daily
           $stn eq "050304            38.85000  -102.16667   " || #1   2 daily
           $stn eq "111577            41.73333   -87.76667   " || #1   2 daily
           $stn eq "143997            39.66667   -98.35000   " || #1   2 daily
           $stn eq "342242            35.95000   -97.61667   " || #1   2 daily
           $stn eq "342318            35.98333   -96.76667   " || #1   2 daily
           $stn eq "342354            34.55000   -95.68333   " || #1   2 daily
           $stn eq "342678            34.01667   -96.38333   " || #1   2 daily
           $stn eq "111664            38.51667   -88.40000   " || #1   2 daily
           $stn eq "144087            38.25000   -99.96667   " || #1   2 daily
           $stn eq "342818            35.55000   -97.96667   " || #1   2 daily
           $stn eq "342849            35.38333   -99.40000   " || #1   2 daily
           $stn eq "144104            38.25000   -95.75000   " || #1   2 daily
           $stn eq "342912            36.41667   -97.86667   " || #1   2 daily
           $stn eq "342944            35.20000   -99.80000   " || #1   2 daily
           $stn eq "112193            39.83333   -89.01667   " || #1   2 daily
           $stn eq "144178            38.60000   -97.95000   " || #1   2 daily
           $stn eq "343065            34.95000   -94.90000   " || #1   2 daily
           $stn eq "343304            36.55000   -99.58333   " || #1   2 daily
           $stn eq "144421            38.35000   -94.75000   " || #1   2 daily
           $stn eq "343353            34.40000   -99.01667   " || #1   2 daily
           $stn eq "343358            36.76667   -99.11667   " || #1   2 daily
           $stn eq "112353            37.43333   -88.66667   " || #1   2 daily
           $stn eq "144464            37.93333  -101.25000   " || #1   2 daily
           $stn eq "343497            35.63333   -98.31667   " || #1   2 daily
           $stn eq "144530            38.18333   -99.10000   " || #1   2 daily
           $stn eq "032300            33.21667   -92.80000   " || #1   2 daily
           $stn eq "050776            37.43333  -105.51667   " || #1   2 daily
           $stn eq "112687            39.13333   -88.53333   " || #1   2 daily
           $stn eq "144608            38.41667   -95.85000   " || #1   2 daily
           $stn eq "343884            35.20000   -95.88333   " || #1   2 daily
           $stn eq "343956            35.81667   -95.68333   " || #1   2 daily
           $stn eq "144675            38.08333   -95.63333   " || #1   2 daily
           $stn eq "344017            34.33333   -94.65000   " || #1   2 daily
           $stn eq "344019            36.53333   -98.26667   " || #1   2 daily
           $stn eq "112923            40.73333   -88.51667   " || #1   2 daily
           $stn eq "144735            38.56667   -97.66667   " || #1   2 daily
           $stn eq "344055            36.10000   -97.83333   " || #1   2 daily
           $stn eq "144821            38.65000   -99.18333   " || #1   2 daily
           $stn eq "344235            35.08333   -96.40000   " || #1   2 daily
           $stn eq "344289            36.41667   -96.38333   " || #1   2 daily
           $stn eq "050834            39.63333  -102.18333   " || #1   2 daily
           $stn eq "113109            38.68333   -88.56667   " || #1   2 daily
           $stn eq "144972            39.20000   -96.58333   " || #1   2 daily
           $stn eq "344393            36.91667   -96.10000   " || #1   2 daily
           $stn eq "145039            38.38333   -97.08333   " || #1   2 daily
           $stn eq "344567            36.43333   -94.80000   " || #1   2 daily
           $stn eq "344573            36.71667   -97.80000   " || #1   2 daily
           $stn eq "113262            42.30000   -89.60000   " || #1   2 daily
           $stn eq "145063            39.83333   -96.63333   " || #1   2 daily
           $stn eq "344766            36.90000  -102.96667   " || #1   2 daily
           $stn eq "145069            38.18333   -96.56667   " || #1   2 daily
           $stn eq "344861            35.85000   -97.90000   " || #1   2 daily
           $stn eq "032356            36.41667   -93.78333   " || #1   2 daily
           $stn eq "050895            38.45000  -102.45000   " || #1   2 daily
           $stn eq "113369            41.35000   -88.43333   " || #1   2 daily
           $stn eq "145171            37.28333  -100.33333   " || #1   2 daily
           $stn eq "344915            34.96667   -96.75000   " || #1   2 daily
           $stn eq "145173            37.28333   -98.58333   " || #1   2 daily
           $stn eq "345013            36.68333   -97.56667   " || #1   2 daily
           $stn eq "113666            39.35000   -90.21667   " || #1   2 daily
           $stn eq "145210            38.50000   -95.70000   " || #1   2 daily
           $stn eq "345045            36.70000   -99.90000   " || #1   2 daily
           $stn eq "145306            39.08333   -96.88333   " || #1   2 daily
           $stn eq "345118            36.85000   -95.63333   " || #1   2 daily
           $stn eq "345216            34.81667   -97.65000   " || #1   2 daily
           $stn eq "050909            39.48333  -106.03333   " || #1   2 daily
           $stn eq "113879            37.73333   -88.51667   " || #1   2 daily
           $stn eq "145463            37.91667   -95.18333   " || #1   2 daily
           $stn eq "345247            34.26667   -97.60000   " || #1   2 daily
           $stn eq "345329            35.36667   -98.33333   " || #1   2 daily
           $stn eq "145483            39.35000  -100.03333   " || #1   2 daily
           $stn eq "345437            35.76667   -94.73333   " || #1   2 daily
           $stn eq "345522            36.16667   -96.43333   " || #1   2 daily
           $stn eq "114198            40.46667   -87.66667   " || #1   2 daily
           $stn eq "145536            37.18333   -95.45000   " || #1   2 daily
           $stn eq "345540            36.25000   -96.68333   " || #1   2 daily
           $stn eq "345563            33.93333   -97.11667   " || #1   2 daily
           $stn eq "145692            38.45000   -99.90000   " || #1   2 daily
           $stn eq "010140            32.23333   -87.41667   " || #1   2 daily
           $stn eq "010369            33.28333   -85.80000   " || #1   2 daily
           $stn eq "011099            34.98333   -85.81667   " || #1   2 daily
           $stn eq "032366            36.08333   -91.61667   " || #1   2 daily
           $stn eq "051071            38.85000  -106.13333   " || #1   2 daily
           $stn eq "114317            39.13333   -87.66667   " || #1   2 daily
           $stn eq "145744            38.03333   -97.38333   " || #1   2 daily
           $stn eq "345693            35.15000   -94.96667   " || #1   2 daily
           $stn eq "145768            38.96667   -97.46667   " || #1   2 daily
           $stn eq "345713            34.31667   -95.86667   " || #1   2 daily
           $stn eq "345768            36.80000   -97.73333   " || #1   2 daily
           $stn eq "114355            41.41667   -91.01667   " || #1   2 daily
           $stn eq "145787            39.81667  -100.23333   " || #1   2 daily
           $stn eq "345779            35.50000   -96.98333   " || #1   2 daily
           $stn eq "474937            43.21667   -91.10000   " || #1   2 daily
           $stn eq "346130            35.76667   -95.33333   " || #1   2 daily
           $stn eq "474404            43.56667   -90.63333   " || #1   2 daily
           $stn eq "474370            43.86667   -91.25000   " || #1   2 daily
           $stn eq "473756            43.45000   -88.63333   " || #1   2 daily
           $stn eq "473453            43.31667   -88.40000   " || #1   2 daily
           $stn eq "145852            39.81667   -99.93333   " || #1   2 daily
           $stn eq "473038            43.56667   -91.23333   " || #1   2 daily
           $stn eq "472447            44.73333   -89.75000   " || #1   2 daily
           $stn eq "472428            44.86667   -91.48333   " || #1   2 daily
           $stn eq "471913            42.61667   -90.43333   " || #1   2 daily
           $stn eq "471676            44.61667   -88.75000   " || #1   2 daily
           $stn eq "471667            42.56667   -88.83333   " || #1   2 daily
           $stn eq "471568            44.03333   -88.15000   " || #1   2 daily
           $stn eq "471416            43.05000   -89.46667   " || #1   2 daily
           $stn eq "346139            36.23333   -99.16667   " || #1   2 daily
           $stn eq "470308            43.30000   -89.35000   " || #1   2 daily
           $stn eq "051179            39.75000  -104.13333   " || #1   2 daily
           $stn eq "470045            42.61667   -89.06667   " || #1   2 daily
           $stn eq "114442            39.73333   -90.20000   " || #1   2 daily
           $stn eq "419893            33.01667   -99.05000   " || #1   2 daily
           $stn eq "419830            31.78333  -103.20000   " || #1   2 daily
           $stn eq "419715            31.85000   -97.36667   " || #1   2 daily
           $stn eq "145920            37.81667   -99.56667   " || #1   2 daily
           $stn eq "419491            30.33333   -96.15000   " || #1   2 daily
           $stn eq "146115            37.16667   -95.10000   " || #1   2 daily
           $stn eq "346678            35.60000   -95.46667   " || #1   2 daily
           $stn eq "114603            41.13333   -87.88333   " || #1   2 daily
           $stn eq "419346            34.08333   -99.30000   " || #1   2 daily
           $stn eq "419286            33.48333   -97.16667   " || #1   2 daily
           $stn eq "419270            30.56667  -104.48333   " || #1   2 daily
           $stn eq "419191            34.40000  -100.90000   " || #1   2 daily
           $stn eq "419175            34.53333  -101.76667   " || #1   2 daily
           $stn eq "419163            33.76667   -99.83333   " || #1   2 daily
           $stn eq "418942            33.41667   -94.08333   " || #1   2 daily
           $stn eq "418743            33.15000   -95.63333   " || #1   2 daily
           $stn eq "418646            31.03333   -97.53333   " || #1   2 daily
           $stn eq "418523            36.18333  -101.18333   " || #1   2 daily
           $stn eq "418305            31.18333  -105.31667   " || #1   2 daily
           $stn eq "418252            30.70000  -101.83333   " || #1   2 daily
           $stn eq "146128            38.61667   -95.28333   " || #1   2 daily
           $stn eq "346751            36.35000   -98.46667   " || #1   2 daily
           $stn eq "417936            31.06667   -94.10000   " || #1   2 daily 
           $stn eq "417336            34.25000   -99.68333   " || #1   2 daily 
           $stn eq "417300            31.96667   -98.50000   " || #1   2 daily 
           $stn eq "417274            31.71667   -98.51667   " || #1   2 daily 
           $stn eq "417074            33.18333  -102.83333   " || #1   2 daily 
           $stn eq "417066            32.93333   -94.96667   " || #1   2 daily 
           $stn eq "417060            33.60000  -100.53333   " || #1   2 daily 
           $stn eq "416794            33.66667   -95.56667   " || #1   2 daily 
           $stn eq "346926            34.73333   -97.28333   " || #1   2 daily 
           $stn eq "416757            31.78333   -95.60000   " || #1   2 daily 
           $stn eq "416740            34.01667  -100.30000   " || #1   2 daily 
           $stn eq "346940            36.35000   -96.80000   " || #1   2 daily 
           $stn eq "051268            37.01667  -102.56667   " || #1   2 daily 
           $stn eq "114710            41.25000   -89.90000   " || #1   2 daily 
           $stn eq "416210            31.95000   -96.70000   " || #1   2 daily 
           $stn eq "416177            31.61667   -94.65000   " || #1   2 daily 
           $stn eq "416108            33.16667   -95.00000   " || #1   2 daily 
           $stn eq "416104            30.66667  -104.00000   " || #1   2 daily 
           $stn eq "415958            32.78333   -98.06667   " || #1   2 daily 
           $stn eq "146333            39.11667   -95.41667   " || #1   2 daily 
           $stn eq "347003            35.96667   -97.03333   " || #1   2 daily 
           $stn eq "347012            36.28333   -97.30000   " || #1   2 daily 
           $stn eq "415875            35.70000  -100.63333   " || #1   2 daily 
           $stn eq "146374            39.73333   -99.31667   " || #1   2 daily 
           $stn eq "347068            35.65000   -97.75000   " || #1   2 daily 
           $stn eq "114805            41.03333   -89.40000   " || #1   2 daily 
           $stn eq "415424            31.23333   -94.75000   " || #1   2 daily 
           $stn eq "146414            37.35000   -94.63333   " || #1   2 daily 
           $stn eq "415348            32.35000   -94.65000   " || #1   2 daily 
           $stn eq "347254            35.05000   -94.61667   " || #1   2 daily 
           $stn eq "347264            35.48333   -96.70000   " || #1   2 daily 
           $stn eq "347327            34.96667   -97.43333   " || #1   2 daily 
           $stn eq "415094            33.03333   -96.48333   " || #1   2 daily 
           $stn eq "415018            31.05000   -98.18333   " || #1   2 daily
           $stn eq "414982            33.75000   -99.15000   " || #1   2 daily
           $stn eq "414866            32.13333   -97.48333   " || #1   2 daily
           $stn eq "414792            31.06667   -97.73333   " || #1   2 daily
           $stn eq "114957            38.73333   -87.68333   " || #1   2 daily
           $stn eq "414517            33.23333   -98.15000   " || #1   2 daily
           $stn eq "414375            30.05000   -99.51667   " || #1   2 daily
           $stn eq "414278            31.85000   -99.56667   " || #1   2 daily
           $stn eq "146685            38.65000   -99.90000   " || #1   2 daily
           $stn eq "414137            31.98333   -98.03333   " || #1   2 daily
           $stn eq "347390            36.50000   -96.73333   " || #1   2 daily
           $stn eq "347394            36.60000   -95.91667   " || #1   2 daily
           $stn eq "146725            38.55000   -95.95000   " || #1   2 daily
           $stn eq "413686            30.70000   -97.33333   " || #1   2 daily
           $stn eq "347505            36.46667   -97.16667   " || #1   2 daily
           $stn eq "413546            32.73333   -94.98333   " || #1   2 daily
           $stn eq "347565            35.16667   -99.36667   " || #1   2 daily
           $stn eq "115413            40.20000   -89.73333   " || #1   2 daily
           $stn eq "146979            37.81667   -96.61667   " || #1   2 daily
           $stn eq "347579            35.65000   -99.91667   " || #1   2 daily
           $stn eq "347727            34.85000   -99.01667   " || #1   2 daily
           $stn eq "413133            32.51667   -96.66667   " || #1   2 daily
           $stn eq "147046            38.86667   -98.81667   " || #1   2 daily
           $stn eq "347952            35.30000   -99.61667   " || #1   2 daily
           $stn eq "348016            34.98333   -98.75000   " || #1   2 daily
           $stn eq "412715            32.40000   -98.85000   " || #1   2 daily
           $stn eq "051547            39.65000  -104.85000   " || #1   2 daily
           $stn eq "115768            40.91667   -90.63333   " || #1   2 daily
           $stn eq "412464            34.58333  -102.31667   " || #1   2 daily
           $stn eq "348042            35.23333   -96.66667   " || #1   2 daily
           $stn eq "348110            35.35000   -96.90000   " || #1   2 daily
           $stn eq "348416            35.25000   -94.61667   " || #1   2 daily
           $stn eq "348479            35.26667   -97.20000   " || #1   2 daily
           $stn eq "115841            39.41667   -89.46667   " || #1   2 daily
           $stn eq "147397            38.90000  -101.75000   " || #1   2 daily
           $stn eq "348501            36.11667   -97.10000   " || #1   2 daily
           $stn eq "348563            35.75000   -96.65000   " || #1   2 daily
           $stn eq "148114            37.93333   -96.30000   " || #1   2 daily
           $stn eq "412096            32.53333   -97.61667   " || #1   2 daily
           $stn eq "412086            31.76667   -97.83333   " || #1   2 daily
           $stn eq "412082            31.38333  -102.33333   " || #1   2 daily
           $stn eq "411956            30.33333   -95.48333   " || #1   2 daily
           $stn eq "411761            34.93333  -100.88333   " || #1   2 daily
           $stn eq "348677            35.93333   -94.96667   " || #1   2 daily
           $stn eq "051564            38.81667  -102.35000   " || #1   2 daily
           $stn eq "411430            34.98333  -101.93333   " || #1   2 daily
           $stn eq "411246            32.55000   -97.31667   " || #1   2 daily
           $stn eq "411068            30.88333   -97.93333   " || #1   2 daily
           $stn eq "411017            31.11667   -99.33333   " || #1   2 daily
           $stn eq "115888            38.40000   -87.75000   " || #1   2 daily
           $stn eq "410958            35.65000  -101.45000   " || #1   2 daily
           $stn eq "348751            35.20000   -96.95000   " || #1   2 daily
           $stn eq "410738            30.75000   -98.01667   " || #1   2 daily
           $stn eq "348815            35.75000   -98.75000   " || #1   2 daily
           $stn eq "410691            32.65000   -97.45000   " || #1   2 daily
           $stn eq "410518            32.26667   -96.63333   " || #1   2 daily
           $stn eq "410509            30.13333   -98.81667   " || #1   2 daily
           $stn eq "349017            36.81667  -100.86667   " || #1   2 daily
           $stn eq "116159            38.91667   -88.11667   " || #1   2 daily
           $stn eq "410248            32.31667  -102.53333   " || #1   2 daily
           $stn eq "349023            34.61667   -95.28333   " || #1   2 daily
           $stn eq "349032            34.50000   -97.53333   " || #1   2 daily
           $stn eq "410174            30.36667  -103.66667   " || #1   2 daily
           $stn eq "349118            34.00000   -95.15000   " || #1   2 daily
           $stn eq "349172            36.15000   -99.28333   " || #1   2 daily
           $stn eq "410012            33.83333  -101.85000   " || #1   2 daily
           $stn eq "409492            36.08333   -87.86667   " || #1   2 daily
           $stn eq "409219            36.40000   -89.05000   " || #1   2 daily
           $stn eq "408108            35.15000   -88.31667   " || #1   2 daily
           $stn eq "408065            36.45000   -89.31667   " || #1   2 daily
           $stn eq "407359            36.58333   -86.53333   " || #1   2 daily
           $stn eq "051713            38.43333  -106.76667   " || #1   2 daily
           $stn eq "406402            36.11667   -86.68333   " || #1   2 daily
           $stn eq "406162            35.25000   -85.83333   " || #1   2 daily
           $stn eq "116490            42.00000   -89.28333   " || #1   2 daily
           $stn eq "148245            37.81667   -99.06667   " || #1   2 daily
           $stn eq "405089            35.25000   -87.35000   " || #1   2 daily
           $stn eq "349203            36.66667   -95.13333   " || #1   2 daily
           $stn eq "349212            34.91667   -99.91667   " || #1   2 daily
           $stn eq "148250            39.78333   -95.05000   " || #1   2 daily
           $stn eq "402489            36.06667   -87.38333   " || #1   2 daily
           $stn eq "349247            35.96667   -95.36667   " || #1   2 daily
           $stn eq "349278            34.35000   -98.30000   " || #1   2 daily
           $stn eq "116610            39.63333   -87.70000   " || #1   2 daily
           $stn eq "401480            36.25000   -85.95000   " || #1   2 daily
           $stn eq "400876            35.26667   -88.98333   " || #1   2 daily
           $stn eq "399347            43.55000  -103.48333   " || #1   2 daily
           $stn eq "349298            36.91667   -95.80000   " || #1   2 daily
           $stn eq "349364            35.85000   -98.41667   " || #1   2 daily
           $stn eq "398622            42.75000   -96.91667   " || #1   2 daily
           $stn eq "397992            44.25000   -99.45000   " || #1   2 daily
           $stn eq "148341            39.35000   -95.45000   " || #1   2 daily
           $stn eq "349395            34.16667   -98.00000   " || #1   2 daily
           $stn eq "349399            34.23333   -98.05000   " || #1   2 daily
           $stn eq "148802            38.80000   -96.73333   " || #1   2 daily
           $stn eq "349445            35.48333   -95.20000   " || #1   2 daily
           $stn eq "396636            44.55000  -102.18333   " || #1   2 daily
           $stn eq "349575            35.15000   -96.48333   " || #1   2 daily
           $stn eq "396574            43.06667   -98.53333   " || #1   2 daily
           $stn eq "349629            34.73333   -98.71667   " || #1   2 daily
           $stn eq "396427            44.06667  -103.48333   " || #1   2 daily
           $stn eq "396304            43.40000  -103.26667   " || #1   2 daily
           $stn eq "396170            44.45000  -100.41667   " || #1   2 daily
           $stn eq "148964            37.23333   -96.96667   " || #1   2 daily
           $stn eq "395620            43.30000  -100.66667   " || #1   2 daily
           $stn eq "349634            34.95000   -95.16667   " || #1   2 daily
           $stn eq "395481            43.23333   -97.58333   " || #1   2 daily
           $stn eq "349668            35.05000   -99.50000   " || #1   2 daily
           $stn eq "394184            43.75000  -101.95000   " || #1   2 daily
           $stn eq "349760            36.43333   -99.38333   " || #1   2 daily
           $stn eq "391076            44.31667   -96.76667   " || #1   2 daily
           $stn eq "394037            44.01667   -97.51667   " || #1   2 daily
           $stn eq "012124            32.83333   -85.73333   " || #1   2 daily
           $stn eq "018178            31.91667   -87.73333   " || #1   2 daily
           $stn eq "051959            38.86667  -106.96667   " || #1   2 daily
           $stn eq "116837            39.61667   -90.80000   " || #1   2 daily
           $stn eq "151227            37.53333   -87.26667   " || #1   2 daily
           $stn eq "391972            43.96667  -101.86667   " || #1   2 daily
           $stn eq "392557            43.30000  -103.81667   " || #1   2 daily
           $stn eq "392565            43.63333  -103.91667   " || #1   2 daily
           $stn eq "117072            39.93333   -91.20000   " || #1   2 daily
           $stn eq "153430            37.25000   -85.50000   " || #1   2 daily
           $stn eq "153435            37.25000   -85.50000   " || #1   2 daily
           $stn eq "117150            40.31667   -88.16667   " || #1   2 daily
           $stn eq "153929            37.53333   -85.73333   " || #1   2 daily
           $stn eq "394766            44.06667   -99.46667   " || #1   2 daily
           $stn eq "395544            44.53333  -101.56667   " || #1   2 daily
           $stn eq "117187            38.03333   -88.98333   " || #1   2 daily
           $stn eq "395891            43.88333  -100.70000   " || #1   2 daily
           $stn eq "154955            38.28333   -85.80000   " || #1   2 daily
           $stn eq "051964            37.98333  -105.68333   " || #1   2 daily
           $stn eq "155067            37.35000   -87.51667   " || #1   2 daily
           $stn eq "156091            37.76667   -87.15000   " || #1   2 daily
           $stn eq "396552            44.06667  -101.65000   " || #1   2 daily
           $stn eq "396597            44.38333  -100.28333   " || #1   2 daily
           $stn eq "156110            37.06667   -88.76667   " || #1   2 daily
           $stn eq "156580            37.11667   -87.86667   " || #1   2 daily
           $stn eq "117391            41.51667   -90.56667   " || #1   2 daily
           $stn eq "157215            36.73333   -86.21667   " || #1   2 daily
           $stn eq "397882            44.50000  -103.86667   " || #1   2 daily
           $stn eq "158070            36.81667   -85.71667   " || #1   2 daily
           $stn eq "118020            37.16667   -88.43333   " || #1   2 daily
           $stn eq "158824            37.18333   -86.63333   " || #1   2 daily
           $stn eq "160098            31.31667   -92.46667   " || #1   2 daily
           $stn eq "018209            32.88333   -86.70000   " || #1   2 daily
           $stn eq "032810            34.11667   -94.23333   " || #1   2 daily
           $stn eq "160104            31.40000   -92.30000   " || #1   2 daily
           $stn eq "160548            30.41667   -91.13333   " || #1   2 daily
           $stn eq "401663            36.31667   -87.21667   " || #1   2 daily
           $stn eq "402197            35.95000   -85.08333   " || #1   2 daily
           $stn eq "402685            36.01667   -89.40000   " || #1   2 daily
           $stn eq "404556            35.60000   -88.91667   " || #1   2 daily
           $stn eq "404561            35.61667   -88.83333   " || #1   2 daily
           $stn eq "052286            40.23333  -108.96667   " || #1   2 daily
           $stn eq "118389            39.51667   -88.63333   " || #1   2 daily
           $stn eq "161246            30.38333   -91.26667   " || #1   2 daily
           $stn eq "161287            30.95000   -92.16667   " || #1   2 daily
           $stn eq "118740            40.10000   -88.23333   " || #1   2 daily
           $stn eq "161411            32.51667   -92.33333   " || #1   2 daily
           $stn eq "161899            30.80000   -90.96667   " || #1   2 daily
           $stn eq "032946            36.33333   -93.43333   " || #1   2 daily
           $stn eq "052790            39.63778  -105.31556   " || #1   2 daily
           $stn eq "118781            38.96667   -89.06667   " || #1   2 daily
           $stn eq "161979            32.16667   -92.10000   " || #1   2 daily
           $stn eq "162534            30.06667   -91.03333   " || #1   2 daily
           $stn eq "120177            40.10000   -85.71667   " || #1   2 daily
           $stn eq "164030            30.50000   -90.36667   " || #1   2 daily
           $stn eq "164696            31.66667   -92.20000   " || #1   2 daily
           $stn eq "052932            39.31667  -103.08333   " || #1   2 daily
           $stn eq "120331            40.30000   -87.18333   " || #1   2 daily
           $stn eq "164700            30.20000   -92.66667   " || #1   2 daily
           $stn eq "164739            31.48333   -91.85000   " || #1   2 daily
           $stn eq "120830            40.75000   -85.16667   " || #1   2 daily
           $stn eq "165021            30.21667   -92.06667   " || #1   2 daily
           $stn eq "410708            33.58333  -100.03333   " || #1   2 daily
           $stn eq "010390            34.76667   -86.95000   " || #1   2 daily
           $stn eq "012172            30.25000   -88.08333   " || #1   2 daily
           $stn eq "018323            31.78333   -85.95000   " || #1   2 daily
           $stn eq "032962            35.23333   -92.36667   " || #1   2 daily
           $stn eq "052965            38.88333  -105.28333   " || #1   2 daily
           $stn eq "121256            37.90000   -86.63333   " || #1   2 daily
           $stn eq "410779            31.20000  -101.46667   " || #1   2 daily
           $stn eq "411000            35.53333  -102.25000   " || #1   2 daily
           $stn eq "121739            41.15000   -85.48333   " || #1   2 daily
           $stn eq "165287            31.05000   -93.28333   " || #1   2 daily
           $stn eq "165620            30.36667   -91.16667   " || #1   2 daily
           $stn eq "053002            38.68333  -104.76667   " || #1   2 daily
           $stn eq "121814            38.21667   -86.11667   " || #1   2 daily
           $stn eq "166244            32.58333   -93.28333   " || #1   2 daily
           $stn eq "166303            32.51667   -92.05000   " || #1   2 daily
           $stn eq "121873            39.96667   -86.93333   " || #1   2 daily
           $stn eq "166314            32.53333   -92.06667   " || #1   2 daily
           $stn eq "166667            30.03333   -90.03333   " || #1   2 daily
           $stn eq "032976            35.21667   -94.25000   " || #1   2 daily
           $stn eq "053005            40.58333  -105.08333   " || #1   2 daily
           $stn eq "122309            38.45000   -86.70000   " || #1   2 daily
           $stn eq "167738            32.41667   -93.63333   " || #1   2 daily
           $stn eq "169357            31.58333   -91.46667   " || #1   2 daily
           $stn eq "412404            33.20000   -97.10000   " || #1   2 daily
           $stn eq "412462            30.35000   -96.83333   " || #1   2 daily
           $stn eq "122825            40.25000   -85.15000   " || #1   2 daily
           $stn eq "169803            31.93333   -92.68333   " || #1   2 daily
           $stn eq "412633            33.81667   -98.93333   " || #1   2 daily
           $stn eq "169806            32.10000   -91.71667   " || #1   2 daily
           $stn eq "200710            42.13333   -86.43333   " || #1   2 daily
           $stn eq "412818            34.03333   -98.91667   " || #1   2 daily
           $stn eq "413005            31.46667   -98.16667   " || #1   2 daily
           $stn eq "018380            33.23333   -87.61667   " || #1   2 daily
           $stn eq "053500            40.18333  -105.86667   " || #1   2 daily
           $stn eq "204320            43.20000   -85.76667   " || #1   2 daily
           $stn eq "413214            33.96667  -101.33333   " || #1   2 daily
           $stn eq "413247            33.46667   -97.56667   " || #1   2 daily
           $stn eq "205567            43.46667   -86.41667   " || #1   2 daily
           $stn eq "413507            30.68333   -97.71667   " || #1   2 daily
           $stn eq "413646            32.23333   -98.66667   " || #1   2 daily
           $stn eq "123082            40.31667   -86.50000   " || #1   2 daily
           $stn eq "208251            44.73333   -85.58333   " || #1   2 daily
           $stn eq "414093            33.81667   -98.20000   " || #1   2 daily
           $stn eq "211263            44.71667   -96.28333   " || #1   2 daily
           $stn eq "123104            38.86667   -87.30000   " || #1   2 daily
           $stn eq "213962            44.91667   -94.36667   " || #1   2 daily
           $stn eq "214418            43.86667   -91.30000   " || #1   2 daily
           $stn eq "053553            40.41667  -104.70000   " || #1   2 daily
           $stn eq "123418            41.56667   -85.83333   " || #1   2 daily
           $stn eq "214884            44.98333   -93.18333   " || #1   2 daily
           $stn eq "414570            33.25000  -100.56667   " || #1   2 daily
           $stn eq "214937            43.66667   -96.20000   " || #1   2 daily
           $stn eq "124181            40.85000   -85.50000   " || #1   2 daily
           $stn eq "215435            44.88333   -93.21667   " || #1   2 daily
           $stn eq "415193            30.41667   -97.01667   " || #1   2 daily
           $stn eq "053656            38.68333  -105.38333   " || #1   2 daily
           $stn eq "216822            44.61667   -92.61667   " || #1   2 daily
           $stn eq "415247            36.23333  -100.26667   " || #1   2 daily
           $stn eq "216835            44.55000   -95.08333   " || #1   2 daily
           $stn eq "415596            30.30000  -104.01667   " || #1   2 daily
           $stn eq "415658            34.01667  -100.83333   " || #1   2 daily
           $stn eq "415770            35.23333  -100.60000   " || #1   2 daily
           $stn eq "012675            31.38333   -85.90000   " || #1   2 daily
           $stn eq "018517            33.80000   -88.11667   " || #1   2 daily
           $stn eq "033428            33.71667   -93.55000   " || #1   2 daily
           $stn eq "124286            39.76667   -86.18333   " || #1   2 daily
           $stn eq "217184            43.83333   -91.76667   " || #1   2 daily
           $stn eq "415878            33.80000   -95.50000   " || #1   2 daily
           $stn eq "217907            44.25000   -94.98333   " || #1   2 daily
           $stn eq "124372            38.38333   -86.93333   " || #1   2 daily
           $stn eq "218323            44.23333   -95.61667   " || #1   2 daily
           $stn eq "218450            44.98333   -93.18333   " || #1   2 daily
           $stn eq "053828            38.45000  -103.15000   " || #1   2 daily
           $stn eq "124730            41.65000   -85.41667   " || #1   2 daily
           $stn eq "416335            31.98333   -95.13333   " || #1   2 daily
           $stn eq "416641            33.43333   -98.78333   " || #1   2 daily
           $stn eq "124782            41.51667   -86.26667   " || #1   2 daily 
           $stn eq "220021            33.83333   -88.51667   " || #1   2 daily
           $stn eq "220237            34.75000   -90.13333   " || #1   2 daily
           $stn eq "033466            34.51667   -93.05000   " || #1   2 daily
           $stn eq "054076            38.05000  -102.11667   " || #1   2 daily
           $stn eq "124837            41.60000   -86.71667   " || #1   2 daily
           $stn eq "220955            34.66667   -88.56667   " || #1   2 daily
           $stn eq "221094            31.55000   -90.45000   " || #1   2 daily
           $stn eq "125337            40.56667   -85.66667   " || #1   2 daily
           $stn eq "221262            34.86667   -89.68333   " || #1   2 daily
           $stn eq "221707            34.20000   -90.56667   " || #1   2 daily
           $stn eq "054082            40.58333  -102.30000   " || #1   2 daily
           $stn eq "125407            39.40000   -86.45000   " || #1   2 daily
           $stn eq "221743            33.80000   -90.71667   " || #1   2 daily
           $stn eq "221852            31.63333   -89.56667   " || #1   2 daily
           $stn eq "126304            39.55000   -86.10000   " || #1   2 daily
           $stn eq "221962            34.91667   -88.51667   " || #1   2 daily
           $stn eq "222658            32.80000   -89.33333   " || #1   2 daily
           $stn eq "018673            32.78333   -87.83333   " || #1   2 daily
           $stn eq "033544            36.06667   -93.75000   " || #1   2 daily
           $stn eq "054172            39.13333  -103.46667   " || #1   2 daily
           $stn eq "126580            38.88333   -86.55000   " || #1   2 daily
           $stn eq "222773            34.15000   -89.91667   " || #1   2 daily
           $stn eq "222896            33.55000   -89.23333   " || #1   2 daily
           $stn eq "126697            38.40000   -86.11667   " || #1   2 daily
           $stn eq "223107            32.31667   -89.48333   " || #1   2 daily
           $stn eq "223627            33.50000   -90.08333   " || #1   2 daily
           $stn eq "126864            40.75000   -86.05000   " || #1   2 daily
           $stn eq "224001            34.61667   -89.18333   " || #1   2 daily
           $stn eq "419565            34.83333  -100.21667   " || #1   2 daily
           $stn eq "224173            34.81667   -89.43333   " || #1   2 daily
           $stn eq "127069            40.41667   -85.00000   " || #1   2 daily
           $stn eq "224265            33.91667   -89.00000   " || #1   2 daily
           $stn eq "470124            44.33333   -91.93333   " || #1   2 daily
           $stn eq "033600            36.01667   -93.18333   " || #1   2 daily
           $stn eq "054388            38.06667  -102.91667   " || #1   2 daily
           $stn eq "127125            38.35000   -87.58333   " || #1   2 daily
           $stn eq "470855            44.28333   -90.85000   " || #1   2 daily
           $stn eq "224966            31.15000   -88.55000   " || #1   2 daily
           $stn eq "127298            40.93333   -87.15000   " || #1   2 daily
           $stn eq "225062            33.13333   -90.06667   " || #1   2 daily
           $stn eq "225247            33.13333   -89.06667   " || #1   2 daily
           $stn eq "054444            38.73333  -103.53333   " || #1   2 daily
           $stn eq "127482            41.06667   -86.21667   " || #1   2 daily
           $stn eq "225361            33.10000   -88.53333   " || #1   2 daily
           $stn eq "225614            31.23333   -90.46667   " || #1   2 daily
           $stn eq "127999            39.51667   -85.78333   " || #1   2 daily
           $stn eq "225704            31.46667   -90.88333   " || #1   2 daily
           $stn eq "474546            42.83333   -90.78333   " )  #1   2 daily
         {
         if ($occ != 0)
            {
            printf "WARNING: $occ reset for COOP $stn to 1.\n";
            }

         $occ = 1;
         $occ1_ct++;
         $COOPocc++;
 
         if ($debug)
           {
           printf "--- Set occ for type COOP = 1---\n";
           }
         }


#------------
# COOP occ = 2
#------------
       if ( $stn eq "481675            41.15000  -104.81667   " || #2   2 daily
            $stn eq "347309            36.40000   -95.30000   " || #2   2 daily
            $stn eq "055018            39.18333  -103.70000   " || #2   2 daily
            $stn eq "298596            34.60000  -104.38333   " || #2   2 daily
            $stn eq "412415            33.51667   -95.31667   " || #2   2 daily
            $stn eq "142432            37.00000  -101.88333   " || #2   2 daily
            $stn eq "035200            34.95000   -93.16667   " || #2   2 daily
            $stn eq "349404            36.58333   -98.86667   " || #2   2 daily
            $stn eq "148259            39.25000   -96.60000   " || #2   2 daily
            $stn eq "148946            38.96667   -98.48333   " || #2   2 daily
            $stn eq "058184            38.81667  -106.61667   " || #2   2 daily
            $stn eq "057031            39.53333  -107.80000   " || #2   2 daily
            $stn eq "054538            37.45000  -103.31667   " || #2   2 daily
            $stn eq "142686            37.65000   -96.08333   " || #2   2 daily
            $stn eq "294856            35.65000  -105.15000   " || #2   2 daily
            $stn eq "415192            33.06667   -97.01667   " || #2   2 daily
            $stn eq "297638            35.95000  -104.20000   " || #2   2 daily
            $stn eq "237455            38.75000   -90.36667   " || #2   2 daily
            $stn eq "142975            37.93333  -100.71667   " || #2   2 daily
            $stn eq "142980            37.98333  -100.81667   " || #2   2 daily
            $stn eq "416270            33.45000   -94.41667   " || #2   2 daily
            $stn eq "058064            39.25000  -106.36667   " || #2   2 daily
            $stn eq "141233            37.05000   -97.61667   " || #2   2 daily
            $stn eq "340179            34.58333   -99.33333   " || #2   2 daily
            $stn eq "058429            37.16667  -104.48333   " || #2   2 daily
            $stn eq "142401            37.81667   -96.83333   " || #2   2 daily
            $stn eq "255995            41.98333   -97.43333   " || #2   2 daily
            $stn eq "036920            34.46667   -91.41667   " || #2   2 daily
            $stn eq "165026            30.20000   -91.98333   " || #2   2 daily
            $stn eq "137844            43.16667   -95.15000   " || #2   2 daily
            $stn eq "343628            36.60000  -101.61667   " || #2   2 daily
            $stn eq "148235            38.46667  -101.76667   " || #2   2 daily
            $stn eq "148830            37.65000   -97.43333   " || #2   2 daily
            $stn eq "397667            43.56667   -96.73333   " || #2   2 daily
            $stn eq "237656            36.85000   -94.61667   " || #2   2 daily
            $stn eq "154954            38.18333   -85.73333   " || #2   2 daily
            $stn eq "412240            36.01667  -102.55000   " || #2   2 daily
            $stn eq "412242            32.90000   -97.03333   " || #2   2 daily
            $stn eq "122738            38.05000   -87.53333   " || #2   2 daily
            $stn eq "256065            41.13333  -100.68333   " || #2   2 daily
            $stn eq "219170            43.65000   -95.58333   " || #2   2 daily
            $stn eq "124259            39.73333   -86.26667   " || #2   2 daily
            $stn eq "050372            39.18333  -106.83333   " || #2   2 daily
            $stn eq "344865            34.01667   -96.71667   " || #2   2 daily
            $stn eq "416776            35.56667  -100.96667   " || #2   2 daily
            $stn eq "225776            32.33333   -88.75000   " || #2   2 daily
            $stn eq "414257            33.58333   -95.90000   " || #2   2 daily
            $stn eq "413691            32.96667   -97.05000   " || #2   2 daily
            $stn eq "487533            41.80000  -107.20000   " || #2   2 daily
            $stn eq "341750            35.05000   -97.91667   " || #2   2 daily
            $stn eq "148191            37.75000   -95.93333   " || #2   2 daily
            $stn eq "346935            36.66667   -96.35000   " || #2   2 daily
            $stn eq "143984            37.91667   -95.43333   " || #2   2 daily
            $stn eq "346638            35.43333   -96.30000   " || #2   2 daily
            $stn eq "419916            33.30000   -94.16667   " || #2   2 daily
            $stn eq "341544            34.25000   -94.78333   " || #2   2 daily
            $stn eq "488155            44.76667  -106.96667   " || #2   2 daily
            $stn eq "032444            36.10000   -94.16667   " || #2   2 daily
            $stn eq "032978            35.51667   -92.00000   " || #2   2 daily
            $stn eq "051539            38.10000  -103.50000   " || #2   2 daily
            $stn eq "231791            38.81667   -92.21667   " || #2   2 daily
            $stn eq "217004            43.91667   -92.50000   " || #2   2 daily
            $stn eq "413284            32.81667   -97.35000   " || #2   2 daily
            $stn eq "346485            36.70000   -95.63333   " || #2   2 daily
            $stn eq "032544            33.73333   -94.40000   " || #2   2 daily
            $stn eq "412394            33.81667   -96.56667   " || #2   2 daily
            $stn eq "344975            35.30000   -95.36667   " || #2   2 daily
            $stn eq "143686            38.66667   -94.90000   " || #2   2 daily
            $stn eq "344384            34.00000   -95.51667   " || #2   2 daily
            $stn eq "015749            34.75000   -87.61667   " || #2   2 daily
            $stn eq "051778            38.81667  -104.71667   " || #2   2 daily
            $stn eq "348708            36.03333   -98.96667   " || #2   2 daily
            $stn eq "118179            39.85000   -89.68333   " || #2   2 daily
            $stn eq "394127            44.38333   -98.21667   " || #2   2 daily
            $stn eq "150909            36.96667   -86.43333   " || #2   2 daily
            $stn eq "116711            40.66667   -89.68333   " || #2   2 daily
            $stn eq "405954            35.05000   -90.00000   " || #2   2 daily
            $stn eq "412244            32.85000   -96.85000   " || #2   2 daily
            $stn eq "053063            38.68333  -104.70000   " || #2   2 daily
            $stn eq "412621            33.80000  -100.51667   " || #2   2 daily
            $stn eq "414098            34.81667  -102.40000   " || #2   2 daily
            $stn eq "415890            31.95000  -102.18333   " || #2   2 daily
            $stn eq "146498            38.65000   -95.56667   " || #2   2 daily
            $stn eq "054293            39.56667  -105.21667   " || #2   2 daily
            $stn eq "111549            42.00000   -87.88333   " || #2   2 daily
            $stn eq "050263            39.00000  -105.88333   " || #2   2 daily
            $stn eq "345589            36.15000   -97.61667   " || #2   2 daily
            $stn eq "345108            34.46667   -96.21667   " || #2   2 daily
            $stn eq "340292            34.20000   -97.15000   " ||  #2   2 daily
            $stn eq "344812            36.15000   -96.25000   " ||  #2   2 daily
            $stn eq "485390            42.81667  -108.73333   " ||  #2   2 daily
            $stn eq "143153            39.36667  -101.70000   " ||  #2   2 daily
            $stn eq "233094            36.80000   -93.46667   " ||  #2   2 daily
            $stn eq "343740            36.75000   -98.13333   " ||  #2   2 daily
            $stn eq "056136            38.51667  -103.70000   " ||  #2   2 daily
            $stn eq "054742            38.91667  -105.48333   " ||  #2   2 daily
            $stn eq "050109            40.15000  -103.15000   " ||  #2   2 daily
            $stn eq "297279            36.91667  -104.43333   " ||  #2   2 daily
            $stn eq "034900            33.60000   -91.80000   " ||  #2   2 daily
            $stn eq "340535            36.56667   -96.16667   " ||  #2   2 daily
            $stn eq "030616            36.43333   -93.61667   " ||  #2   2 daily
            $stn eq "030764            34.56667   -93.20000   " ||  #2   2 daily
            $stn eq "141612            38.93333   -95.33333   " ||  #2   2 daily
            $stn eq "141740            37.16667   -94.85000   " ||  #2   2 daily
            $stn eq "140313            37.06667   -97.03333   "  )  #2   2 daily
         {
         if ($occ != 0) 
            {
            printf "WARNING: $occ reset for COOP $stn to 2.\n";
            }
 
         $occ = 2;
         $occ2_ct++;
         $COOPocc++;
 
        if ($debug)
           {
           printf "--- Set occ for type COOP = 2---\n";
           }
        } # COOP occ = 2



#------------
# COOP occ =3 
#------------
       if ($stn eq "419417            31.60000   -97.21667   " ||  #3   2 daily
           $stn eq "344204            35.00000   -99.05000   " ||  #3   2 daily
           $stn eq "229003            34.26667   -88.76667   " ||  #3   2 daily
           $stn eq "132367            42.40000   -90.70000   " ||  #3   2 daily
           $stn eq "347201            36.73333   -97.10000   " ||  #3   2 daily
           $stn eq "034248            34.73333   -92.23333   " ||  #3   2 daily
           $stn eq "340548            36.75000   -96.00000   " ||  #3   2 daily
           $stn eq "411698            34.43333  -100.28333   " ||  #3   2 daily
           $stn eq "475479            42.95000   -87.90000   " ||  #3   2 daily
           $stn eq "474961            43.13333   -89.33333   " ||  #3   2 daily
           $stn eq "015550            32.30000   -86.40000   " ||  #3   2 daily
           $stn eq "035754            34.21667   -92.01667   " ||  #3   2 daily
           $stn eq "410211            35.23333  -101.70000   " ||  #3   2 daily
           $stn eq "123037            41.00000   -85.20000   " ||  #3   2 daily
           $stn eq "037488            34.90000   -94.10000   " ||  #3   2 daily
           $stn eq "034988            34.53333   -93.60000   " ||  #3   2 daily
           $stn eq "033165            36.26667   -93.15000   " ||  #3   2 daily
           $stn eq "203333            42.88333   -85.51667   " ||  #3   2 daily
           $stn eq "257665            41.86667  -103.60000   " ||  #3   2 daily
           $stn eq "141427            37.66667   -95.48333   " ||  #3   2 daily
           $stn eq "254795            40.85000   -96.75000   " ||  #3   2 daily
           $stn eq "054885            39.23333  -106.31667   " ||  #3   2 daily
           $stn eq "297610            33.30000  -104.53333   " ||  #3   2 daily
           $stn eq "343407            36.30000   -99.76667   " ||  #3   2 daily
           $stn eq "056326            39.53333  -104.65000   " ||  #3   2 daily
           $stn eq "396937            44.05000  -103.06667   " ||  #3   2 daily
           $stn eq "148167            39.06667   -95.63333   " ||  #3   2 daily
           $stn eq "398932            44.91667   -97.15000   " ||  #3   2 daily
           $stn eq "346661            35.40000   -97.60000   " ||  #3   2 daily
           $stn eq "237976            37.23333   -93.38333   " ||  #3   2 daily
           $stn eq "234358            39.31667   -94.71667   " ||  #3   2 daily
           $stn eq "128187            41.70000   -86.31667   " ||  #3   2 daily
           $stn eq "253395            40.96667   -98.31667   " ||  #3   2 daily
           $stn eq "117382            42.20000   -89.10000   " ||  #3   2 daily
           $stn eq "141767            39.55000   -97.65000   " ||  #3   2 daily
           $stn eq "410428            30.28333   -97.70000   " ||  #3   2 daily
           $stn eq "137708            42.40000   -96.38333   " ||  #3   2 daily
           $stn eq "147160            38.80000   -97.65000   " ||  #3   2 daily
           $stn eq "160549            30.53333   -91.13333   " ||  #3   2 daily
           $stn eq "054720            38.05000  -103.51667   " ||  #3   2 daily
           $stn eq "138706            42.55000   -92.40000   " ||  #3   2 daily
           $stn eq "258760            42.86667  -100.55000   " ||  #3   2 daily
           $stn eq "010831            33.56667   -86.75000   " ||  #3   2 daily
           $stn eq "410016            32.41667   -99.68333   " ||  #3   2 daily
           $stn eq "481570            42.91667  -106.46667   " ||  #3   2 daily
           $stn eq "224472            32.31667   -90.08333   " ||  #3   2 daily
           $stn eq "417943            31.36667  -100.50000   " ||  #3   2 daily
           $stn eq "052220            39.76667  -104.86667   " ||  #3   2 daily
           $stn eq "256255            41.30000   -95.90000   " ||  #3   2 daily NEW
           $stn eq "165078            30.11667   -93.21667   "  )  #3   2 daily
         {
         if ($occ != 0) 
            {
            printf "WARNING: $occ reset for COOP $stn to 3 .\n";
            }
 
         $occ = 3;
         $occ3_ct++;
         $COOPocc++;

         if ($debug)
           {
           printf "--- Set occ for type COOP = 3---\n";
           }
         }  # COOP occ 3. 

#------------
# COOP occ =4
#------------
       if ($stn eq "142164            37.76667   -99.96667   " ||  #4   2 daily
           $stn eq "344672            36.20000   -94.78333   " ||  #4   2 daily
           $stn eq "412797            31.80000  -106.40000   " ||  #4   2 daily
           $stn eq "345664            34.88333   -95.78333   " ||  #4   2 daily
           $stn eq "032574            35.33333   -94.36667   " ||  #4   2 daily NEW
           $stn eq "050130            37.45000  -105.86667   " )   #4   2 daily
         {
         if ($occ != 0)
            {
            printf "WARNING: $occ reset for COOP $stn to 4.\n";
            }

         $occ = 4;
         $occ4_ct++;
         $COOPocc++;

         if ($debug)
           {
           printf "--- Set occ for type COOP = 4---\n";
           }

         } # Set COOP occ to 4 

#------------
# COOP occ =5
#------------
       if ($stn eq "168440            32.46667   -93.81667   " )  #5   2 daily
         {
         if ($occ != 0)
            {
            printf "WARNING: $occ reset for COOP $stn to 5.\n"; 
            }

         $occ = 5;
         $occ5_ct++;
         $COOPocc++;

         if ($debug)
           {
           printf "--- Set occ for type COOP = 5---$stn\n";
           }

         } # Set COOP occ to 5


       } # Plaform Type 2 - COOP (END)


   #----------------------------
   # Print every line to output.
   #----------------------------
   if ($debug)
      {  
      printf "%-s%1s%-s\n", $begin_line,$occ,$end_line;
      }  
 
   printf OUTFILE "%-s%1s%-s", $begin_line,$occ,$end_line;

   }  # while data in file

   print "\nNumber of occ reset to ZERO: $occzero.\n";

   print "\nNumber of stn matches to ensure occ is 1: $occ1_ct with $occ1_71onlyct type 71.\n";
   print "Number of stn matches to ensure occ is 2: $occ2_ct with $occ2_71onlyct type 71.\n";
   print "Number of stn matches to ensure occ is 3: $occ3_ct with $occ3_71onlyct type 71.\n";

   print "\nNumber of ABRFC occ set: $ABRFCocc.\n";
   print "Number of HPLAINS occ set: $HPLAINSocc.\n";
   print "Number of SAO occ set: $SAOocc.\n";
   print "Number of Rec Rainga occ set: $RecRaingaocc.\n";
   print "Number of COOP occ set: $COOPocc.\n";
   print "Number of otherocc set: $otherocc.\n";

   $total_spct = $ABRFCocc + $HPLAINSocc + $RecRaingaocc + $COOPocc + $SAOocc + $otherocc;
   $total_ct = $occ1_ct+$occ2_ct+$occ3_ct+$occ4_ct + $occ5_ct;

   if ($total_spct != $total_ct)
      {
      print "\nWARNING: Failure comparing total counts! $total_spct ne $total_ct\n";
      }

   print "\nTotal number of matching stns found (occ to 1,2,3,4,5): $total_ct\n";
   print "Total number of lines read: $lines_read\n";
   
   print "\n(Note that actual number of records reset may not equal number of stn matches.\n";
   print " Some stns may already have their occurrance values set properly.)\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "set_NEW_SW95pcp_occ.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
