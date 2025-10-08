#!/usr/bin/perl 

#-------------------------------------------------------
# remove_KP59.pl - This perl script/program reads PQCF
#   and strips all the KP59 data from the file.
# 
# 99 Mar lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----remove_KP59.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are solar_YYMMDD.
# Output names will be rmv_YYMMDD.0qc.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 6, 6);
print "Output file name is: rmv_$YYMMDD.0qc\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">rmv_$YYMMDD.0qc") || die "Can NOT open output file for writing";


$number_lines_read = 0;
$number_lines_dropped = 0;
$number_lines_printed = 0;

#-------------------------------------------------
# For the open file, read all records, write only
# non-KP59 records to the output file.
#-------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $number_lines_read++;

   $ID_match  = substr ($line, 41, 11);

   if ($debug)
      {  
##    printf "\nRead line: $line";
      printf "ID_match=: xxx%sxxx\n", $network_match; 
      }  

   if ( $ID_match ne "725387:KP59")
      {
      if ($debug)
         {
         printf "Print this line! network: xxx%sxxx\n", $ID_match;
         }

      $number_lines_printed++;

      printf OUTFILE "%-s", $line;

      }
   else
      {
      $number_lines_dropped++;

      if ($debug)
         {
         printf "Don't print this KP59 line! network: xxx%sxxx\n", $ID_match;
         }
      }

   }  # while data in file

printf ("Number of records read: %d\n", $number_lines_read);
printf ("Number of KP59 records dropped: %d\n", $number_lines_dropped);
printf ("Number of Non_KP59 records printed: %d\n", $number_lines_printed);


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----remove_KP59.pl ended at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
