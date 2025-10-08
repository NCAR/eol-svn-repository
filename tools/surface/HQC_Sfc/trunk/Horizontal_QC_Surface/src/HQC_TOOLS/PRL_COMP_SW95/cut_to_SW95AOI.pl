#!/usr/bin/perl 

#-----------------------------------------------------------------
# cut_to_SW95AOI.pl - This perl script/program reads PQCF records
#   and determines if the record falls within the hardcoded
#   area of interest (SW95AOI). If the record
#   is within the SW95AOI, it will be written to an output
#   file.  All other records are just dropped. Only works on
#   precip records.
# 
# 97 Sep lec
#   Created.
#-----------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncut_to_SW95AOI.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are rmvsp_YYMMDD.qcf.
# Output names should be SWsp_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 6, 10);
print "Output file name is: SWsp_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">SWsp_$YYMMDD") || die "Can NOT open output file for reading";


#---------------------------------------------------------
# For the open file, read all records, determine if in
# SW95AOI and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in SW95AOI.
   #----------------------------------
   $lat = substr( $line, 46, 10);
   $lon = substr( $line, 57, 10);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "lat=: %f\n", $lat; 
      printf "lon=: %f\n", $lon; 
      }  

   if ($lat >=30.00   && $lat <=45.00 &&
       $lon >=-109.00 && $lon <=-85.00 )
      {
      if ($debug)
         {
         printf "Write line toSW95 file.\n";
         print "xxx", $line, "xxx"; # writes to stdout
         }

      print OUTFILE "$line";

      } # record in SW95
   else
      {
      if ($debug)
         {
         printf "Record NOT in SW95AOI.\n";
         }
      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "cut_to_SW95AOI.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
