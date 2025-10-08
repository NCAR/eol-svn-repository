#!/usr/bin/perl 

#-------------------------------------------------------
# remove_IFLOWS.pl - This perl script/program reads Daily
#   PQCF and strips all the IFLOWS data from a TVA
#   daily precip file.
# 
# 99 Dec lec
#   Created.
#-------------------------------------------------------*/
$debug = 1;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----remove_IFLOWS.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are YYMMDD.
# Output names will be rmv_YYMMDD.dqc.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: $ARGV[0].noIFLOWS\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$ARGV[0].noIFLOWS") || die "Can NOT open output file for writing";


$number_lines_read = 0;
$number_lines_dropped = 0;
$number_lines_printed = 0;

#-------------------------------------------------
# For the open file, read all records, write only
# non-IFLOWS records to the output file.
#-------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $number_lines_read++;

   $ID_number  = substr ($line, 23, 4);

   if ($debug)
      {  
      printf "\nRead line: $line";
      printf "ID_number=: xxx%sxxx\n", $ID_number; 
      }  

   #---------------------------------------------------
   # All IFLOWS ID's are numbers > 1000. This code
   # works for TVA since all TVA numbers are < 1000.
   # Not sure whether or not =1000 is TVA or IFLOWS, so
   # put out a warning.
   #---------------------------------------------------
   if ( $ID_number < 1000)
      {
      if ($debug)
         {
         printf "Found TVA stn: xxx%sxxx\n", $ID_number;
         }

      $number_lines_printed++;

      printf OUTFILE "%-s", $line;

      }
   else
      {
      if ( $ID_number == 1000)
         {
         if ($debug)
            {
            printf "Found stn with ID == 1000 - DROPPING REC!: xxx%sxxx\n", $ID_number;
            }
         }

      $number_lines_dropped++;

      if ($debug)
         {
         printf "Don't print this IFLOWS line! network: xxx%sxxx\n", $ID_number;
         }
      }

   }  # while data in file

printf ("Number of records read: %d\n", $number_lines_read);
printf ("Number of IFLOWS records dropped: %d\n", $number_lines_dropped);
printf ("Number of Non_IFLOWS records printed: %d\n", $number_lines_printed);


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----remove_IFLOWS.pl ended at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
