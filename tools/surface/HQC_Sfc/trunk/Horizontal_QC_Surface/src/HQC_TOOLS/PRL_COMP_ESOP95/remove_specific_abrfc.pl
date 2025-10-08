#!/usr/bin/perl 

#-----------------------------------------------------------------
# remove_specific_abrfc.pl - This perl script/program reads PQCF
#   and strips only specific ABRFC pcp stations from the file.
#   This script was used to remove (along with remove_ALL_abrfc.pl)
#   to redo the 1995 ESOP 15min and hrly precip composites.
# 
# 97 Sep/Oct lec
#   Created.
#----------------------------------------------------------------*/
$debug = 0;

print "Processing ESOP data\n";

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----remove_specific_abrfc.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are ABRFCxx_YYMMDD.
# Output names will be rmvsp_YYMMDD.pqc.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 8, 6);
print "Output file name is: rmvsp_$YYMMDD.pqc\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">rmvsp_$YYMMDD.pqc") || die "Can NOT open output file for writing";


$number_lines_read = 0;
$number_lines_dropped = 0;
$number_lines_printed = 0;

#----------------------------------------------------------
# For the open file, read all records, write only
# specific ABRFC and non-ABRFC records to the output file.
#----------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $number_lines_read++;

   $network_match  = substr ($line, 18, 10);
   $station_ID     = substr ($line, 29, 5);

##  $end_line   = substr ($line, 71, 300); # Hourly recs are 359 chars long
##  $end_line   = substr ($line, 71, 1160); # 15 min recs are 1223 chars long

   if ($debug)
      {  
##    printf "\nRead line: $line";
      printf "network_match=: xxx%sxxx\n", $network_match; 
      }  

#------------------------------------------------------------
# Use following check for ES 15min and Hrly precip composite.
# Completely strip TCH and MESO networks. Also, strip 
# individual stations that are duplicated in the NCDC (rec
# rainga) precip data. All stations will still be present
# in the ABRFC MISC composite. Last 3 stations are outside
# the ESOP95 AOI.
#------------------------------------------------------------
   if ( $network_match ne "ABRFC/TCH " &&
        $network_match ne "ABRFC/MESO" &&
        $station_ID ne "GPVT2" &&
        $station_ID ne "LEWT2" &&
        $station_ID ne "CAMA4" &&
        $station_ID ne "AHDA4" &&
        $station_ID ne "DSNT2" &&
        $station_ID ne "DQTA4" &&
        $station_ID ne "DQNA4" &&
        $station_ID ne "DIEA4" &&
        $station_ID ne "NADA4" &&
        $station_ID ne "BMDA4" &&
        $station_ID ne "PSDN5" &&
        $station_ID ne "WSLO2" &&
        $station_ID ne "NMLA4" &&
        $station_ID ne "BONA4" &&
        $station_ID ne "EUFO2" &&
        $station_ID ne "OLBO2" &&
        $station_ID ne "GRRA4" &&
        $station_ID ne "TENO2" &&
        $station_ID ne "WAGO2" &&
        $station_ID ne "GIBO2" &&
        $station_ID ne "KEYO2" &&
        $station_ID ne "NFDA4" &&
        $station_ID ne "BSGA4" &&
        $station_ID ne "BRYA4" &&
        $station_ID ne "GSPO2" &&
        $station_ID ne "GRNK1" &&
        $station_ID ne "BUMK1" &&
        $station_ID ne "FLLK1" &&
        $station_ID ne "TRLK1" &&
        $station_ID ne "ARLK1" &&
        $station_ID ne "PLKK1" &&
        $station_ID ne "HILK1" &&
        $station_ID ne "CBUC2" &&
        $station_ID ne "CLIK1" &&
        $station_ID ne "MLFK1" &&
        $station_ID ne "PRRK1" &&
        $station_ID ne "MTTK1" &&
        $station_ID ne "ITRC2" &&
        $station_ID ne "EVGC2" &&
        $station_ID ne "GOLC2" &&
        $station_ID ne "IDOC2" &&
        $station_ID ne "KRMC2" &&   # Last 3 are out of ES95 AOI
        $station_ID ne "PGSC2" &&
        $station_ID ne "PSPC2" )
      {  
      if ($debug)
         {
         printf "Print this line! network: xxx%sxxx xxx%sxxx\n", $network_match, $station_ID;
         }

      $number_lines_printed++;

      printf OUTFILE "%-s", $line;

      }  
   else
      {
      $number_lines_dropped++;

      if ($debug)
         {
         printf "Don't print this ABRFC line! network: xxx%sxxx\n", $network_match;
         }
      }   

   }  # while data in file

printf ("Number of records read: %d\n", $number_lines_read);
printf ("Number of ABRFC records dropped: %d\n", $number_lines_dropped);
printf ("Number of records printed to output file: %d\n", $number_lines_printed);


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----remove_specific_abrfc.pl ended at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
