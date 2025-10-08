#!/usr/bin/perl 

#------------------------------------------------------------
# divide_LakeICE_comp.pl - This perl script/program reads 
#   QCF records and determines if the record should be
#   written to the LakeICE hourly, 20min or 5min composites.
#   Note that all the data was HQC'd together then this
#   script is run to split the data into the final composites.
# 
# 98 Dec lec
#   Created.
#------------------------------------------------------------*/
$debug = 1;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ndivide_LakeICE_comp.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are utc_YYMMDD.qcf.
# Output names should be LK20_YYMMDD.qcf,
# LK5_YYMMDD.qcf and LK60_YYMMDD.qcf.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";

$YYMMDD = substr( $ARGV[0], 4, 10);
print "60MIN composite Output file name is: LK60_$YYMMDD\n";
print "20MIN composite Output file name is: LK20_$YYMMDD\n";
print "5MIN composite Output file name is: LK5_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE60, ">LK60_$YYMMDD") || die "Can NOT open 60MIN output file for reading";
open (OUTFILE20, ">LK20_$YYMMDD") || die "Can NOT open 20MIN output file for reading";
open (OUTFILE5, ">LK5_$YYMMDD") || die "Can NOT open 5MIN output file for reading";

#--------------------------------------------
# Create a list of all possible 60, 20 & 5min
# networks. Note from the following network
# naming conventions, all the 20min nets
# have the number 20 in the same place.
# So, really don't even need these list,
# just search for '20' in a specific record
# location.
#-------------------------------------------
@min5_networks  = ('ASOS5     ');

@min20_networks = ('AWOSQ20   ', 
                   'AWOSH20   ',
                   'AWOSA20   ');

@min60_networks = ('ASOSH     ',
                   'DATSAV2   ',
                   'GLERL     ',
                   'HPLAINS   ',
                   'ISWS/ICN  ',
                   'MI_AWSN   ',
                   'NPN       ',
                   'WI_AWON   ');
 

#------------------------------------------------
# For the open file, read all records, determine
# which LAKEICE output file to write record to.
#------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # All lines must be written to one
   # of the output files. 
   #----------------------------------
   $network = substr( $line, 30, 10);
   $network_num = substr( $line, 35, 2);

   if ($debug)
      {  
      printf "Read line: $line";
      printf "network=: xxx%sxxx\n", $network; 
      printf "network_num=: xxx%sxxx\n", $network_num; 
      }  

   if ( $network_num == "20")
      {
      if ($debug)
         {
         printf "Write line to 20MIN file.\n\n";
         }

      print OUTFILE20 "$line";

      } # record is from 20MIN network
   else
      {
      print "Either 5min or hourly\n";

      print "network: xxx$networkxxx\n";

      if ($network eq "ASOS5     ")
         {
         if ($debug)
            {
            printf "Write line to 5MIN file.\n\n";
         }

         print OUTFILE5 "$line";

         }
      else #Everything else is hourly
         {
         if ($debug)
            {
            printf "Write line to Hourly file.\n\n";
            }
 
         print OUTFILE60 "$line";
 
         }
      } # 20min?

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "divide_LakeICE_comp.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
