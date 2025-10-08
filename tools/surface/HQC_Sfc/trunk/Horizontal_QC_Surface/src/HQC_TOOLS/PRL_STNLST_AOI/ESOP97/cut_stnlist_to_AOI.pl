#!/usr/bin/perl 

#-------------------------------------------------------
# cut_stnlist_to_AOI.pl - This perl script/program reads
#   stnlist records and cuts out all records not within
#   the specified Area of Interest. This s/w assumes
#   that all the input files have the stns in the
#   same order.
# 
# 99 March lec
#   Created.
# 99 May lec
#   Updated for ESOP97 AOI.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncut_stnlist_to_AOI.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
#
# Input names are stationCD.out, 
#    station.out, stn_lst.out (in that order.
#
# Output names are aoi_*
#----------------------------------------
print "Processing input file: $ARGV[0], $ARGV[1], $ARGV[2]\n";

print "Output file name is: aoi_$ARGV[0], aoi_$ARGV[1], aoi_$ARGV[2]\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">aoi_$ARGV[0]") || die "Can NOT open output file for reading";

open (INFILE1, "$ARGV[1]") || die "Can NOT open input file1 for reading";
open (OUTFILE1, ">aoi_$ARGV[1]") || die "Can NOT open output file1 for reading";

open (INFILE2, "$ARGV[2]") || die "Can NOT open input file2 for reading";
open (OUTFILE2, ">aoi_$ARGV[2]") || die "Can NOT open output file2 for reading";


$num_recs_cut  = 0;
$num_recs_read = 0;

#---------------------------------------------------------
# For the open file, read all records, determine 
# drop all records/stns not in specified AOI.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   $line1 = <INFILE1>;
   $line2 = <INFILE2>;

   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $num_recs_read++;

   $lat = substr ($line, 22, 9); # CD file
   $lon = substr ($line, 33, 10); # CD file

   if ($debug)
      {  
      printf "\nRead line: $line";
      print "lat=: $lat\n";
      print "lon=: $lon\n";
      }  

#
# Let the Mobile stations pass through with missing lat/lons.
#
   if ( ($lat <= -99.99990 && $lon <= -999.99990) ||
        ( ($lat >= 37.0) && ($lat <= 50.0) && 
          ($lon >= -99.0) && ($lon <= -85.0) ) )
      {
      if ($debug)
         {
         printf "This rec is IN the AOI - keep it.\n";
         }

      printf OUTFILE "%-s", $line;
      printf OUTFILE1 "%-s", $line1;
      printf OUTFILE2 "%-s", $line2;

      }
   else
      {
      if ($debug)
         {
         printf "This rec is out of AOI - drop it.\n";
         }

      $num_recs_cut++;
      }

   }  # while data in file

print "Number records read: $num_recs_read\n";
print "Number records cut: $num_recs_cut\n";

print "WARNING: sequential numbers in files may now NOT be correct!\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "cut_stnlist_to_AOI.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(INFILE1);
close(INFILE2);

close(OUTFILE);
close(OUTFILE1);
close(OUTFILE2);
