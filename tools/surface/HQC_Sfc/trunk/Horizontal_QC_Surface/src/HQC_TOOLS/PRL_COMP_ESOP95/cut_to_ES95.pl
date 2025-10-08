#!/usr/bin/perl 

#-------------------------------------------------------
# cut_to_ES95.pl - This perl script/program reads QCF records
#   and determines if the record falls within the ESOP 95
#   area of interest (AOI). If the record is within the 
#   AOI, it will be written to an output file. All other
#   records are just dropped.
# 
# 95 Jun lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncut_to_ES95.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be ESOP95_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 4, 10);
print "Output file name is: ESOP95_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">ESOP95_$YYMMDD") || die "Can NOT open output file for reading";


#---------------------------------------------------------
# For the open file, read all records, determine if in
# ESOP 95 AOI and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in AOI.
   #----------------------------------
   $lat = substr( $line, 57, 10);
   $lon = substr( $line, 69, 10);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "lat=: %f\n", $lat; 
      printf "lon=: %f\n", $lon; 
      }  

   if ($lat >=31.00   && $lat <=40.00 &&
       $lon >=-107.00 && $lon <=-91.00 )
      {
      if ($debug)
         {
         printf "Write line to ESOP95 file.\n";
         print "xxx", $line, "xxx"; # writes to stdout
         }

      print OUTFILE "$line";

      } # record in ESOP 95 AOI
   else
      {
      if ($debug)
         {
         printf "Record NOT in AOI.\n";
         }
      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "cut_to_ES95.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
