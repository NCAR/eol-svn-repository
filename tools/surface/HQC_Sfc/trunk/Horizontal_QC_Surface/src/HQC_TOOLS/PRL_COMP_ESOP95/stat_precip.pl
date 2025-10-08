#!/usr/bin/perl 

#-------------------------------------------------------
# stat_precip.pl - This perl script/program reads records
#   from the finalprecip.stats file produced by the s/w
#   that summarizes the precip QC statistics (statPQCF).
# 
# 96 Apr lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nSTAT_PRECIP.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are finalprecip.stats files.
# Output names should be totalprcp.stats.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

print "Output file name is: totalprcp.stats\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">totalprcp.stats") || die "Can NOT open output file for reading";


#---------------------------------------------------------
# For the open file, read all records, pick out info of
# interest.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $line_part1 = substr( $line, 0, 15);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "line_part1=:xxx%sxxx\n",$line_part1; 
      }  

   if ( $line_part1 eq    "Total Bad value" )
      {
      $bad = substr( $line, 18, 6);
      $tot_bad = $tot_bad + $bad;
      } 
   elsif ( $line_part1 eq "Total dubious v" )
      {
      $dub = substr( $line, 22, 6);
      $tot_dub = $tot_dub + $dub; 
      }
   elsif ( $line_part1 eq "Total good plus" ) #Includes trace values.
      {  
      $good = substr( $line, 30, 6);  # was 29!!!
      $tot_good = $tot_good + $good; 
      }  
   elsif ( $line_part1 eq "Total missing v" )
      {  
      $miss = substr( $line, 41, 6);   # was 40
      $tot_miss = $tot_miss + $miss;
      }  
   elsif ( $line_part1 eq "Total number va" )
      {  
      $count = substr( $line, 31, 10); 
      $tot_ct = $tot_ct + $count; 
      }  

   if ($debug)
      {
      printf "count, miss, good, dub, bad: %d %d %d %d %d\n", 
           $tot_ct, $tot_miss, $tot_good, $tot_dub, $tot_bad;
      }

   }  # while data in file

printf OUTFILE "Tot_Count  Tot_miss  Tot_Good  Tot_Dub  Tot_Bad\n";
printf OUTFILE "%6d     %6d    %6d    %6d    %6d\n\n", 
                $tot_ct, $tot_miss, $tot_good, $tot_dub, $tot_bad;

printf OUTFILE "Prcnt_Miss  Prcnt_Good  Prcnt_flagged  Prcnt_Dub  Prcnt_Bad\n";
if ($tot_ct != 0)
   {
   printf OUTFILE "%f     %f     %f      %f    %f\n",
     $tot_miss/$tot_ct, $tot_good/$tot_ct, ($tot_dub+$tot_bad)/$tot_ct, 
     $tot_dub/$tot_ct, $tot_bad/$tot_ct;
   }


($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "STAT_PRECIP.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
