#!/usr/bin/perl 

#-------------------------------------------------------
# cutnetwork.pl - This perl script/program reads QCF records
#   and determines if the record falls is the specified 
#   network. If so, then the rec is written to the 
#   output file. Otherwise, the record is dropped.
# 
# 96 Apr lec
#   Created.
#-------------------------------------------------------*/
$debug = 1;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nCUTNETWORK.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: $ARGV[0]\n";
print "Requested network: $ARGV[1]xxx\n";

$YYMMDD = substr( $ARGV[0], 4, 10);
print "Output file name is: net_$YYMMDD\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILE, ">net_$YYMMDD") || die "Can NOT open output file for reading";


#---------------------------------------------------------
# For the open file, read all records, determine if 
# requested network and write to output file.
#---------------------------------------------------------
while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # Only write out lines in AOI.
   #----------------------------------
   $network = substr( $line, 30, 10);

   if (length($ARGV[1]) < length($network))
      {
      # Chop length of network to match 
      # length of input argument.
      #
      substr ($network, -1*(10-length($ARGV[1]))) = '';
      }

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "network=: xxx%-sxxx\n", $network; 
      printf "$ARGV[1]=: xxx%-sxxx\n", $ARGV[1]; 
      }  

   if ($network eq $ARGV[1])
      {
      if ($debug)
         {
         printf "Write line to output file.\n";
         print "xxx", $line, "xxx"; # writes to stdout
         }

      print OUTFILE "$line";

      } # record from requested network
   else
      {
      if ($debug)
         {
         printf "Record NOT from requested net.\n";
         }
      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "CUTNETWORK.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
