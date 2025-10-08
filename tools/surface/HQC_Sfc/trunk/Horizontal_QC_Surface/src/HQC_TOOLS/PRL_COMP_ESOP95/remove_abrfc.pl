#!/usr/bin/perl 

#-------------------------------------------------------
# remove_abrfc.pl - This perl script/program reads PQCF
#   and strips all the ABRFC data from the file.
# 
# 97 Sep lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----remove_abrfc.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are xxxx_YYMMDD.pqc.
# Output names will be rmv_YYMMDD.pqc.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 5, 6);
print "Output file name is: rmv_$YYMMDD.pqc\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">rmv_$YYMMDD.pqc") || die "Can NOT open output file for writing";


$number_lines_read = 0;
$number_lines_dropped = 0;
$number_lines_printed = 0;

#-------------------------------------------------
# For the open file, read all records, write only
# non-ABRFC records to the output file.
#-------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $number_lines_read++;

   $network_match  = substr ($line, 18, 5);

##  $end_line   = substr ($line, 71, 300); # Hourly recs are 359 chars long
##  $end_line   = substr ($line, 71, 1160); # 15 min recs are 1223 chars long

   if ($debug)
      {  
##    printf "\nRead line: $line";
      printf "network_match=: xxx%sxxx\n", $network_match; 
      }  

#------------------------------------------------------------
# Use following check for SW 15min and Hrly precip composite.
#------------------------------------------------------------
   if ( $network_match ne "ABRFC")
      {
      if ($debug)
         {
         printf "Print this line! network: xxx%sxxx\n", $network_match;
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
printf ("Number of Non_ABRFC records printed: %d\n", $number_lines_printed);


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----remove_abrfc.pl ended at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
