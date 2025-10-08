#!/usr/bin/perl 

#--------------------------------------------------------------
# fix_zero_elev.pl - This perl script/program reads QCF records
#   and determines if the current stn has an elevation
#   of zero. If the stn has an elev of zero (should
#   only occur for a few ASOS stations - found 18 stns)
#   the calculated sea level pressure (CSLP) is reset
#   to "-999.99 I". This is correct and proper, because
#   the cslp's in the data were inproperly computed with
#   0.00 elevations. The original conversion s/w should
#   have put out "-999.99 I".
# 
# 95 Mar lec
#   Created.
#--------------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nFIX_ZERO_ELEV.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be V95_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 6, 10);
print "Output file name is: ELV_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">ELV_$YYMMDD") || die "Can NOT open output file for reading";

$missing = "-999.99 I"; # define the new CSLP value and QC flag.

#---------------------------------------------------------
# For the open file, read all records, determine if
# station has 0.00 elevation. Fix and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   #----------------------------------
   $stn  = substr( $line, 30, 14);
   $elev = substr( $line, 84,  7);

   if ($debug)
      {  
      printf "\nRead line: $line";
      printf "stn=: %-s\n", $stn; 
      printf "elev=: %f\n", $elev; 
      }  

   if ($elev ==0.00)
      {
      if ($debug)
         {
         printf "Found station with 0.00 elevation.\n";
         }

      #-------------------------------------------------
      # Reset 0.00 elevations to proper values. Set
      # calculation sea level pressure to -999.99 with
      # a QC flag of I for insufficient data to compute.
      #-------------------------------------------------
      if    ($stn eq "ASOS5      ARA")
         {$elev = 16.00;}
      elsif ($stn eq "ASOS5      MOB")
         {$elev =   65.00;}
      elsif ($stn eq "ASOS5      ELP")
         {$elev = 1198.00;}
      elsif ($stn eq "ASOS5      TCS")
         {$elev = 1470.00;}
      elsif ($stn eq "ASOS5      ROW")
         {$elev = 1113.00;}
      elsif ($stn eq "ASOS5      RVS")
         {$elev =  201.00;}
      elsif ($stn eq "ASOS5      PAH")
         {$elev =  125.00;}
      elsif ($stn eq "ASOS5      CGI")
         {$elev =  103.00;}
      elsif ($stn eq "ASOS5      MDH")
         {$elev =  131.00;}
      elsif ($stn eq "ASOS5      SDF")
         {$elev =  148.00;}
      elsif ($stn eq "ASOS5      DAY")
         {$elev =  306.00;}
      elsif ($stn eq "ASOS5      FWA")
         {$elev =  252.00;}
      elsif ($stn eq "ASOS5      MLI")
         {$elev =  180.00;}
      elsif ($stn eq "ASOS5      OFK")
         {$elev =  471.00;}
      elsif ($stn eq "ASOS5      RIW")
         {$elev = 1699.00;}
      elsif ($stn eq "ASOS5      NED")
         {$elev =  622.00;}
      elsif ($stn eq "ASOS5      RAP")
         {$elev =  976.00;}
      elsif ($stn eq "ASOS5      GRB")
         {$elev =  208.00;}

      $line_seg1 = substr( $line,   0,  84);
      $line_seg2 = substr( $line,  91,  21);
      $line_seg3 = substr( $line, 121, 136);

      if ($debug)
         {
         printf "%-s%7.2f%-s%-s%-s", $line_seg1,$elev,$line_seg2,$missing,$line_seg3;
         }
 
      printf OUTFILE "%-s%7.2f%-s%-s%-s", $line_seg1,$elev,$line_seg2,$missing,$line_seg3;

      } # Zero elevation
   else
      {
      print OUTFILE "$line"; # write all other input lines to output
      }
   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "FIX_ZERO_ELEV.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
