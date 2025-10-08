#!/usr/bin/perl 

#-------------------------------------------------------
# cut_to_ES95AOI.pl - This perl script/program reads QCF records
#   and determines if the record falls within the hardcoded
#   area of interest (ES95AOI). If the record
#   is within the ES95AOI, it will be written to an output
#   file.  All other records are just dropped.
# 
# 97 Sep lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncut_to_ES95AOI.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are rmvsp_YYMMDD.qcf.
# Output names should be ESsp_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 6, 10);
print "Output file name is: ESsp_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">ESsp_$YYMMDD") || die "Can NOT open output file for reading";

$recs_read = 0;
$recs_dropped = 0;
$recs_printed = 0;

#---------------------------------------------------------
# For the open file, read all records, determine if in
# ES95AOI and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in ES95AOI.
   #----------------------------------
   $recs_read++;

   $stn = substr( $line, 18,26);

   $lat = substr( $line, 46, 10);
   $lon = substr( $line, 57, 10);

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
         printf "Write line toES95 file.\n";
         print "xxx", $line, "xxx"; # writes to stdout
         }

      $recs_printed++;

      print OUTFILE "$line";

      } # record in ES95 ES95AOI
   else
      {
      $recs_dropped++;

      print "Dropping ($lat, $lon) station: $stn\n";

      if ($debug)
         {
         printf "Record NOT in ES95AOI.\n";
         }
      }

   }  # while data in file

printf "   Number of recs read: $recs_read\n";
printf "   Number of recs printed to output: $recs_printed\n";
printf "   Number of recs dropped: $recs_dropped\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "cut_to_ES95AOI.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
