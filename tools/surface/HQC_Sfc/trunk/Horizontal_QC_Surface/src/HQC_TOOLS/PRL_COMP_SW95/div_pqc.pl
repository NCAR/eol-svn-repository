#!/usr/bin/perl 

#-------------------------------------------------------------
# div_pqc.pl - This perl script/program reads PQCF
#   and divides the input data into 3 output files.
# 
# 97 Oct lec
#   Created.
#------------------------------------------------------------*/
$debug = 0;
   
#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\n-----div_pqc.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$INFILENAME = $ARGV[0];
print "Output file name is: $INFILENAME.out1\n";
print "Output file name is: $INFILENAME.out2\n";
print "Output file name is: $INFILENAME.out3\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE1, ">$INFILENAME.out1") || die "Can NOT open output file for writing";
open (OUTFILE2, ">$INFILENAME.out2") || die "Can NOT open output file for writing";
open (OUTFILE3, ">$INFILENAME.out3") || die "Can NOT open output file for writing";


$number_lines_read = 0;

#-------------------------------------
# For the open file, read all records.
#-------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $number_lines_read++;

   if ($debug)
      {  
      printf "\nRead line: $line";
      }  

   if ( $number_lines_read < 38405)
      {
      printf OUTFILE1 "%-s", $line;
      }
   else
      {
      if ( $number_lines_read < 59863)
         {
         printf OUTFILE2 "%-s", $line;
         }  
      else
         {
         printf OUTFILE3 "%-s", $line;
         }  
      }
   }  # while data in file

printf ("Number of records read: %d\n", $number_lines_read);

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "-----div_pqc.pl ended at $year/$mon/$mday $hour:$min:$sec\n\n";

close(INFILE);
close(OUTFILE);
