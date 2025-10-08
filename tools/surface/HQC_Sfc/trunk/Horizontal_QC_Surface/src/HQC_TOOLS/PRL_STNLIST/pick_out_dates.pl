#!/usr/bin/perl 

#-------------------------------------------------------
# pick_out_dates.pl - This perl script/program reads recs
#   and searches for a match with specific dates.
#   Records with the specified dates are the only recs
#   written to the output file.
# 
# 95 Jul lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\npick_out_dates.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$INPUTFILE = substr( $ARGV[0], 0, 7);
print "Output file name is: $INPUTFILE.out\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">$INPUTFILE.out") || die "Can NOT open output file for reading";


#---------------------------------------------------------
# For the open file, read all records
#---------------------------------------------------------
$prev_stn = "jjj\0";

while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in AOI.
   #----------------------------------
   $date = substr( $line, 4, 6);
   $stn = substr( $line, 0, 3);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "date=: %s\n", $date; 
      printf "prev_stn, stn=: %s %s\n", $prev_stn, $stn; 
      }

   if ($date == 950401 || $date == 950402 || 
       $date == 950628 || $date == 950629 || $date == 950630 || 
       $date == 950930)
      {
      if ($debug)
         {
         printf "Write line to output file.\n";
         print "xxx", $line, "xxx"; # writes to stdout
         }

      print OUTFILE "$line";

      }
   else
      {
      if ($debug)
         {
         printf "Record NOT a match.\n";
         }
      }

   $prev_stn = $stn;

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "pick_out_dates.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
