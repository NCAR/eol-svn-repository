#!/usr/bin/perl 

#-------------------------------------------------------
# set_NEW_ES95pcp_occ.pl - This perl script/program reads 
#   pqcf recs and changes the occ values in the requested 
#   (mostly ABRFC) recs to the indicated occ values.
#   !!!This version sets ES9595 (all but COOP) occs!!!!
#   Note that there approx. 407 stns with occ setting,
#   not counting COOP.  15min and hourly precip 
#   composites ONLY. Note that most of the occ that
#   need to be set are in the ABRFC misc comp where
#   we don't have an occ to set. The misc comp is raw
#   data.
#
# 97 Oct lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nset_NEW_ES95pcp_occ.pl began at $year/$mon/$mday $hour:$min:$sec\n";
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

$ABRFCocc = 0;
$SAOocc = 0;
$HPLAINSocc = 0;
$RecRaingaocc = 0;
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
   $begin_line = substr ($line,  0,  70);
   $end_line   = substr ($line, 71, 300); # Hourly recs are 359 chars long
## $end_line   = substr ($line, 71, 1160); # 15 min recs are 1223 chars long
 
   $id    = substr ( $line, 29, 17);
   $latlon = substr ( $line, 46, 24);
   $platform  = substr ( $line, 18, 5);
   $occ = substr ($line,  70,  1);

   $stn = $id . $latlon;

   if ($debug)
      {  
      printf "\n---------\n";
      printf "Read line: $line\n";
      printf "id=: xxx%sxxx\n", $id;
      printf "occ=: xxx%sxxx\n", $occ;
      printf "latlon=: xxx%sxxx\n", $latlon;
      printf "platform=: xxx%sxxx\n", $platform;
      printf "stn=: xxx%sxxx\n", $stn;
#     printf "part1=: xxx%sxxx\n", $part1; 
#     printf "part2=: xxx%sxxx\n", $part2; 
      }  

   #---------------------------------------------------
   # Note there are only 11 ABRFC stations that affect
   # any precip composites. There are hundreds of ABRFC
   # stns that have occ values set are all in the MISC
   # composite where we don't have an occ position to
   # set. There are a few more that are Rec rainga
   # stns that need their occs set back to zero.
   #---------------------------------------------------
   if ($platform eq "Rec r")  #Rec rainga = NCDC precip
      { 
      if ($stn eq "05632604          39.53333  -104.65000   " ||
          $stn eq "03483907          33.68333   -93.96667   " ||
          $stn eq "23435801          39.31667   -94.71667   " ||
          $stn eq "41972902          33.96667   -98.48333   " ||
          $stn eq "41021101          35.23333  -101.70000   " ||
          $stn eq "23797604          37.23333   -93.38333   " ||
          $stn eq "05732001          37.93333  -104.93333   " ||
          $stn eq "34566406          34.88333   -95.78333   " )
         {
         printf "WARNING: $occ resetting occ to ZERO for $stn.\n";
         $occ = 0;
         $occzero++;
         }
      } # reset values


   if ( $platform eq "ABRFC")
      {  
      if ( $stn eq "SYDO2             34.73333   -98.98333   " ||   # hourly comp
           $stn eq "CRLO2             35.01000   -99.90000   " ||   # hourly comp
           $stn eq "CRYC2             39.56667  -104.85000   "    ) # 15 min comp
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


   if ( $platform eq "ABRFC")
      {  
      if ( $stn eq "MLBK1             38.36666   -97.08334   " || # hourly comp
           $stn eq "MSCT2             33.26667   -95.91666   " || # 15min comp
           $stn eq "DARN5             33.35000  -104.85000   " || # 15min comp
           $stn eq "BIGK1             37.26667   -95.46667   " || # hourly comp
           $stn eq "FRSO2             34.26667   -95.91666   " || # hourly comp
           $stn eq "MBLT2             33.76000   -99.14000   " || # hourly comp
           $stn eq "RPDO2             36.54389  -101.08055   " ) # 15min comp
           {
            $occ = 2;
            $occ2_ct++;
            $occ2_71onlyct++;

            $ABRFCocc++;

            if ($debug)
               {
               printf "--- Set occ to 2 for type 71 - ABRFC. ---\n";
               } 
           }    
        } # Type 71 = ABRFC


   if ( $platform eq "ABRFC")
      {   
      if ( $stn eq "BMRA4             35.10000   -93.65000   ") # hourly comp
           { 
            $occ = 3; 
            $occ3_ct++; 
            $occ3_71onlyct++; 
 
            $ABRFCocc++; 
 
            if ($debug) 
               { 
               printf "--- Set occ to 3 for type 71 - ABRFC. ---\n"; 
               }  
           }     
        } # Type 71 = ABRFC 

#--------------------------------------------------------------
#  Believe that all NCDC SAO precip has been previously removed
#  since it produces dups with the NCDC recording raingauge
#  precip. The following left in, just in case.
#--------------------------------------------------------------
   if ( $platform eq "NCDC ")
      {
      if ( $stn eq "HLC               39.38333   -99.83333   " ||
           $stn eq "DHT               36.01667  -102.55000   " ||
           $stn eq "ALS               37.45000  -105.86667   " ||
           $stn eq "CNK               39.55000   -97.65000   " ||
           $stn eq "GLD               39.36667  -101.70000   " ||
           $stn eq "FSM               35.33333   -94.36667   " ||
           $stn eq "PWA               35.53333   -97.63333   " ||
           $stn eq "ESF               31.40000   -92.30000   " ||
           $stn eq "MCI               39.31667   -94.71667   " ||
           $stn eq "SKX               36.45000  -105.66667   " ||
           $stn eq "HRO               36.26667   -93.15000   " ||
           $stn eq "ELD               33.21667   -92.80000   " ||
           $stn eq "SHV               32.46667   -93.81667   " ||
           $stn eq "OJC               38.85000   -94.73333   " ||
           $stn eq "PNC               36.73333   -97.10000   " ||
           $stn eq "OKC               35.40000   -97.60000   " ||
           $stn eq "SPS               33.96667   -98.48333   " ||
           $stn eq "ABI               32.41667   -99.68333   " ||
           $stn eq "BVO               36.75000   -96.00000   " ||
           $stn eq "FOE               38.95000   -95.66667   " ||
           $stn eq "SLN               38.80000   -97.65000   " ||
           $stn eq "LHX               38.05000  -103.51667   " ||
           $stn eq "TUL               36.20000   -95.90000   " ||
           $stn eq "HBR               35.00000   -99.05000   " ||
           $stn eq "DEN               39.76667  -104.86667   " ||
           $stn eq "ROW               33.30000  -104.53333   " ||
           $stn eq "AMA               35.23333  -101.70000   " ||
           $stn eq "LIT               34.73333   -92.23333   " ||
           $stn eq "MHK               39.15000   -96.66667   " ||
           $stn eq "LFK               31.23333   -94.75000   " ||
           $stn eq "SGF               37.23333   -93.38333   " ||
           $stn eq "ELP               31.80000  -106.40000   " ||
           $stn eq "GAG               36.30000   -99.76667   " ||
           $stn eq "LBB               33.65000  -101.81667   " ||
           $stn eq "CSM               35.35000   -99.20000   " ||
           $stn eq "ABQ               35.05000  -106.61667   " ||
           $stn eq "SJT               31.36667  -100.50000   " ||
           $stn eq "PUB               38.28333  -104.51667   " ||
           $stn eq "DDC               37.76667   -99.96667   "  )
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
         if ( $stn eq "CNU               37.66667   -95.48333   "  )
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
       if ($stn eq "41589001          31.95000  -102.18333   " ||
           $stn eq "14243207          37.00000  -101.88333   " ||
           $stn eq "03490009          33.60000   -91.80000   " ||
           $stn eq "05366202          38.55000  -106.91667   " ||
           $stn eq "14612806          38.61667   -95.28333   " ||
           $stn eq "05177801          38.81667  -104.71667   " ||
           $stn eq "23179102          38.81667   -92.21667   " ||
           $stn eq "41224403          32.85000   -96.85000   " ||
           $stn eq "14816706          39.06667   -95.63333   " ||
           $stn eq "41169802          34.43333  -100.28333   " ||
           $stn eq "41224203          32.90000   -97.03333   " ||
           $stn eq "41941703          31.60000   -97.21667   " ||
           $stn eq "41328403          32.81667   -97.35000   " ||
           $stn eq "29188703          36.45000  -103.15000   "  )
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
    
   
    if ($platform eq "AWOS2") # Believe this is AWOS20, but not in 15m/hly ES95pcp - so unsure.
       { 
       if ($stn eq "ADH               34.80000   -96.67000   " )    # AWOS
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


    if ($platform eq "ASOSH") # ASOSH in hly and ASOS5 in 15m!!
       {  
       if ($stn eq "MLC               34.88000   -95.78000   " )
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
       if ($stn eq "Purcell           34.97972   -97.51862   " )
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


    if ($platform eq "HPLAI") # HPLAINS - done
       {    
       if ($stn eq "101_St. Joseph    39.77000   -94.92000   " )
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
       if ($stn eq "41279705          31.80000  -106.40000   " ||
           $stn eq "41972902          33.96667   -98.48333   " ||
           $stn eq "34566406          34.88333   -95.78333   " ||
           $stn eq "29761007          33.30000  -104.53333   " ||
           $stn eq "16844001          32.46667   -93.81667   " ||
           $stn eq "29023405          35.05000  -106.61667   " ||
           $stn eq "05013005          37.45000  -105.86667   " ||
           $stn eq "03257404          35.33333   -94.36667   " ||
           $stn eq "14216407          37.76667   -99.96667   " ||
           $stn eq "05674001          38.28333  -104.51667   " ||
           $stn eq "34666105          35.40000   -97.60000   " ||
           $stn eq "41794306          31.36667  -100.50000   " ||
           $stn eq "14716005          38.80000   -97.65000   " ||
           $stn eq "41941903          31.61667   -97.21667   " ||
           $stn eq "41541101          33.65000  -101.81667   " ||
           $stn eq "23435801          39.31667   -94.71667   " ||
           $stn eq "14176702          39.55000   -97.65000   " ||
           $stn eq "34899203          36.20000   -95.90000   " ||
           $stn eq "41001602          32.41667   -99.68333   "  )
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
       if ($stn eq "23797604          37.23333   -93.38333   " ||    #currently both have occ 1
           $stn eq "41021101          35.23333  -101.70000   "  ) 
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
           printf "--- Set occ to 3 for type 46 - Rec rainga. (34566406)---\n";
           }
         }
       } # Type 46 - Rec rainga

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
   print "Number of otherocc set: $otherocc.\n";

   $total_spct = $ABRFCocc + $HPLAINSocc + $RecRaingaocc + $SAOocc + $otherocc;
   $total_ct = $occ1_ct+$occ2_ct+$occ3_ct;

   if ($total_spct != $total_ct)
      {
      print "\nWARNING: Failure comparing total counts! $total_spct ne $total_ct\n";
      }

   print "\nTotal number of matching stns found (occ to 1,2 or 3): $total_ct\n";
   print "Total number of lines read: $lines_read\n";
   
   print "\n(Note that actual number of records reset may not equal number of stn matches.\n";
   print " Some stns may already have their occurrance values set properly.)\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "set_NEW_ES95pcp_occ.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
