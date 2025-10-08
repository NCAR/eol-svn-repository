#!/usr/bin/perl 

#-------------------------------------------------------
# set_NEW_SWpcp_occ.pl - This perl script/program reads 
#   pqcf recs and changes the occ values in the requested 
#   (mostly ABRFC) recs to the indicated occ values.
#   !!!This version sets SW95 (all but COOP) occs!!!!
#   Note that there approx. 485 stns with occ setting,
#   not counting COOP.  15min and hourly precip 
#   composites ONLY.
#
# 97 Oct lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nset_NEW_SWpcp_occ.pl began at $year/$mon/$mday $hour:$min:$sec\n";

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
## $end_line   = substr ($line, 71, 300); # Hourly recs are 359 chars long
   $end_line   = substr ($line, 71, 1160); # 15 min recs are 1223 chars long
 
   $id    = substr ( $line, 29, 12);
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

   #------------------------------------------------
   # Note there are only 5 ABRFC stations that affect
   # any precip composites. The other 211 ABRFC stns
   # that have occ values set are all in the MISC
   # composite where we don't have an occ position to
   # set. There are a few more that are Rec rainga
   # stns that need their occs set back to zero.
   #------------------------------------------------
   if ($platform eq "Rec r")  #Rec rainga = NCDC precip
      { 
      if ($stn eq "05632604     39.53333  -104.65000   " ||
          $stn eq "03483907     33.68333   -93.96667   " ||
          $stn eq "05732001     37.93333  -104.93333   "  )
         {
         printf "WARNING: $occ resetting occ to ZERO for $stn.\n";
         $occ = 0;
         $occzero++;
         }
      }

   if ( $platform eq "ABRFC")
      {  
      if ( $stn eq "SYDO2        34.73333   -98.98333   " ||  # hourly comp
           $stn eq "BIGK1        37.26667   -95.46667   " ||  # hourly comp
           $stn eq "MLBK1        38.36666   -97.08334   " ||  # hourly comp
           $stn eq "LXVC2        39.23333  -106.31667   " ||  # 15 min comp
           $stn eq "CRYC2        39.56667  -104.85000   "    ) # 15 min comp
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


    if ($platform eq "Rec r")  #Rec rainga = NCDC precip
       {
       if ($stn eq "41941703     31.60000   -97.21667   " ||
           $stn eq "41589001     31.95000  -102.18333   " ||
           $stn eq "22577609     32.33333   -88.75000   " ||
           $stn eq "41328403     32.81667   -97.35000   " ||
           $stn eq "41224403     32.85000   -96.85000   " ||
           $stn eq "41224203     32.90000   -97.03333   " ||
           $stn eq "41169802     34.43333  -100.28333   " ||
           $stn eq "40165601     35.03333   -85.20000   " ||
           $stn eq "40595404     35.05000   -90.00000   " ||
           $stn eq "41021101     35.23333  -101.70000   " ||
           $stn eq "40640203     36.11667   -86.68333   " ||
           $stn eq "29188703     36.45000  -103.15000   " ||
           $stn eq "14243207     37.00000  -101.88333   " ||
           $stn eq "23797604     37.23333   -93.38333   " ||
           $stn eq "12273807     38.05000   -87.53333   " ||
           $stn eq "15495402     38.18333   -85.73333   " ||
           $stn eq "05366202     38.55000  -106.91667   " ||
           $stn eq "14612806     38.61667   -95.28333   " ||
           $stn eq "23745502     38.75000   -90.36667   " ||
           $stn eq "05177801     38.81667  -104.71667   " || 
           $stn eq "23179102     38.81667   -92.21667   " || 
           $stn eq "14816706     39.06667   -95.63333   " || 
           $stn eq "05348802     39.10000  -108.55000   " || 
           $stn eq "05703102     39.53333  -107.80000   " ||
           $stn eq "12425905     39.73333   -86.26667   " ||
           $stn eq "11817906     39.85000   -89.68333   " ||
           $stn eq "05010903     40.15000  -103.15000   " ||
           $stn eq "13458508     40.61667   -93.95000   " ||
           $stn eq "13638909     41.10000   -92.45000   " ||
           $stn eq "25606507     41.13333  -100.68333   " ||
           $stn eq "48167508     41.15000  -104.81667   " ||
           $stn eq "25625506     41.30000   -95.90000   " ||
           $stn eq "13220305     41.53333   -93.65000   " ||
           $stn eq "48753310     41.80000  -107.20000   " ||
           $stn eq "25599503     41.98333   -97.43333   " ||
           $stn eq "11154902     42.00000   -87.88333   " ||
           $stn eq "48539009     42.81667  -108.73333   " || 
           $stn eq "13784401     43.16667   -95.15000   " ||
           $stn eq "20571205     43.16667   -86.23333   " ||
           $stn eq "39766709     43.56667   -96.73333   " ||
           $stn eq "21917007     43.65000   -95.58333   " ||
           $stn eq "21700409     43.91667   -92.50000   " ||
           $stn eq "39107607     44.31667   -96.76667   " ||
           $stn eq "39412707     44.38333   -98.21667   " ||
           $stn eq "48815505     44.76667  -106.96667   " ||
           $stn eq "21543506     44.88333   -93.21667   "  )
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
       
 
    if ($stn eq "ADH          34.80000   -96.67000   ")
       {
       printf "WARNING: Found ADH station - how set occ? See this code.\n";
       }

    if ($platform eq "AWOS1")
       { 
       if ($stn eq "ADH          34.80000   -96.67000   " ||    # AWOS - but may not be AWOS1???
           $stn eq "Clinton      41.83200   -90.32800   " ||    # AWOS1
           $stn eq "Boone        42.05200   -93.84800   "  )    # AWOS1
         {  
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn.\n"; 
            }  

         $occ = 1; 
         $occ1_ct++;
         $otherocc++;

         if ($debug)  
           { 
           printf "--- Set occ to 1 for type 41. (ADH,Clinton,Boone)---\n"; 
           }
         } 
       } # Type 41

    if ($platform eq "ASOSH")
       {  
       if ($stn eq "MLC          34.88000   -95.78000   " )
         {  
         if ($occ != 0 && $occ != 1) 
           { 
           printf "WARNING: $occ reset to 1 for $stn.\n"; 
           }  

         $occ = 1;  
         $occ1_ct++; 
         $otherocc++;

         if ($debug)  
            { 
            printf "--- Set occ to 1 for type 40. (ASOSH MLC) ---\n";  
            }
         }  
       } # Type 40


    if ($platform eq "WPDN ")
       {  
       if ($stn eq "Purcell      34.97972   -97.51862   " )
         {  
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn.\n"; 
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
       if ($stn eq "101_St. Jose 39.77000   -94.92000   " ||
           $stn eq "051_Mead Tur 41.13000   -96.50000   " ||
           $stn eq "081_Watertow 44.92000   -97.15000   " )
         {
         if ($occ != 0 && $occ != 1) 
            { 
            printf "WARNING: $occ reset to 1 for $stn.\n"; 
            }  

         $occ = 1;
         $occ1_ct++;
         $HPLAINSocc++;

         if ($debug)  
           { 
           printf "--- Set occ to 1 for type 16. (101,051,081)---\n";
           }
         }
       } # Type 16


    #------------------------------------------------------
    #  Set appropriate occs to 2 for specific stns/plaform
    #  combos.
    #------------------------------------------------------
    if ($platform eq "Rec r") # Rec rainga
       { 
       if ($stn eq "16507807     30.11667   -93.21667   " ||
           $stn eq "41042807     30.28333   -97.70000   " ||
           $stn eq "16054906     30.53333   -91.13333   " ||
           $stn eq "01547808     30.68333   -88.25000   " ||
           $stn eq "41794306     31.36667  -100.50000   " ||
           $stn eq "41941903     31.61667   -97.21667   " ||
           $stn eq "41279705     31.80000  -106.40000   " ||
           $stn eq "01555006     32.30000   -86.40000   " ||
           $stn eq "22447205     32.31667   -90.08333   " ||
           $stn eq "41001602     32.41667   -99.68333   " ||
           $stn eq "16844001     32.46667   -93.81667   " ||
           $stn eq "29761007     33.30000  -104.53333   " ||
           $stn eq "01083102     33.56667   -86.75000   " ||
           $stn eq "41541101     33.65000  -101.81667   " ||
           $stn eq "22900303     34.26667   -88.76667   " ||
           $stn eq "01406401     34.65000   -86.76667   " ||
           $stn eq "29023405     35.05000  -106.61667   " ||
           $stn eq "03257404     35.33333   -94.36667   " ||
           $stn eq "34666105     35.40000   -97.60000   " ||
           $stn eq "34899203     36.20000   -95.90000   " ||
           $stn eq "05013005     37.45000  -105.86667   " ||
           $stn eq "14216407     37.76667   -99.96667   " ||
           $stn eq "05674001     38.28333  -104.51667   " ||
           $stn eq "14716005     38.80000   -97.65000   " ||
           $stn eq "23435801     39.31667   -94.71667   " ||    # already 1??
           $stn eq "14176702     39.55000   -97.65000   " ||
           $stn eq "11671104     40.66667   -89.68333   " ||
           $stn eq "25479506     40.85000   -96.75000   " ||
           $stn eq "25339505     40.96667   -98.31667   " ||
           $stn eq "12303703     41.00000   -85.20000   " ||
           $stn eq "12818702     41.70000   -86.31667   " ||
           $stn eq "25766501     41.86667  -103.60000   " ||
           $stn eq "11738201     42.20000   -89.10000   " ||
           $stn eq "13770804     42.40000   -96.38333   " ||
           $stn eq "13236703     42.40000   -90.70000   " ||
           $stn eq "13870603     42.55000   -92.40000   " ||
           $stn eq "25876002     42.86667  -100.55000   " ||
           $stn eq "20333308     42.88333   -85.51667   " ||
           $stn eq "48157008     42.91667  -106.46667   " ||
           $stn eq "47547909     42.95000   -87.90000   " ||
           $stn eq "47496108     43.13333   -89.33333   " ||
           $stn eq "39693705     44.05000  -103.06667   " ||
           $stn eq "47326906     44.48333   -88.13333   " ||
           $stn eq "39893207     44.91667   -97.15000   "   )
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


    #------------------------------------------------------
    #  Set appropriate occs to 3 for specific stns/plaform
    #  combos.
    #------------------------------------------------------
    if ($platform eq "Rec r") # Rec rainga
       { 
       if ($stn eq "34566406     34.88333   -95.78333   " )
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

   print "\nNumber of ABRFC occ checked: $ABRFCocc.\n";
   print "Number of HPLAINS occ checked: $HPLAINSocc.\n";
   print "Number of Rec Rainga occ checked: $RecRaingaocc.\n";
   print "Number of otherocc checked: $otherocc.\n";

   $total_spct = $ABRFCocc + $HPLAINSocc + $RecRaingaocc + $otherocc;
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
printf "set_NEW_SWpcp_occ.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
