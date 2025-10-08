#!/usr/bin/perl 

#-----------------------------------------------------------------
# cut_to_LakeICE.pl - This perl script/program reads PQCF records
#   and determines if the record falls within the hardcoded
#   area of interest (LakeICE). If the record
#   is within the LakeICE, it will be written to an output
#   file.  All other records are just dropped. Only works on
#   precip records.
# 
# 99 Jan LEC 
#   Created.
#-----------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncut_to_LakeICE.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are YYMMDD.0qc.
# Output names should be LK_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 0, 10);
print "Output file name is: LK_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">LK_$YYMMDD") || die "Can NOT open output file for reading";


$lines_read = 0;
$lines_cut = 0;

#---------------------------------------------------------
# For the open file, read all records, determine if in
# LakeICE and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   $lines_read++;

   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in LakeICE.
   #----------------------------------
   $lat = substr( $line, 59, 8);
   $lon = substr( $line, 69, 10);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "lat=: %f\n", $lat; 
      printf "lon=: %f\n", $lon; 
      }  

   if ($lat >=37.00   && $lat <=55.00 &&
       $lon >=-104.00 && $lon <=-73.00 )
      {
      if ($debug)
         {
         printf "Write line toLK file.\n";
         print "xxx", $line, "xxx"; # writes to stdout
         }

      print OUTFILE "$line";

      } # record in LK
   else
      {
      $lines_cut++;

      if ($debug)
         {
         printf "Record NOT in LakeICE.\n";
         }
      }

   }  # while data in file

printf "Number of lines read = $lines_read\n";
printf "Number of lines cut = $lines_cut\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "cut_to_LakeICE.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
