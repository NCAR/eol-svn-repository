#!/usr/bin/perl 

#-------------------------------------------------------
# divide_ASOS.pl - This perl script/program reads raw
#   ASOS data records. This s/w assumes that if there
#   are any (old) or original ASOS formatted records 
#   in a file, they occur first in that file. These
#   old type ASOS records are written to an output
#   file named *.old. Once a new or METAR ASOS record
#   is located, all subsequent data records are written
#   to the METAR output file named *.met
#
# WARNING: This s/w makes assumptions about the input
#          file name.
# WARNING: User may want to comment out some of the
#          print statements. And beware that the debug
#          variable is set to 0 to prevent numerous
#          screen messages.
#          
# 97 Mar LEC
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\nDIVIDE_ASOS.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#------------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#
# Input names are iii.hrly.yymmdd.00, 
# where iii is the ID name.
#
# Output names should be 
#   iii.hrly.yymmdd.old  - old format
#                          extracted data
#                          from input file.
#   iii.hrly.yymmdd.met  - METAR format
#                          extracted data
#                          from input file.
#-------------------------------------------
print "Processing input file: $ARGV[0]\n";

$NAME = substr( $ARGV[0], 0, 15);
print "OLD   format data in Output file name is: $NAME.old\n";
print "METAR format data in Output file name is: $NAME.met\n";

open (INFILE, "$ARGV[0]") || die "Can NOT open input file for reading";
open (OUTFILEold, ">$NAME.old") || die "Can NOT open OLD output file for writing";
open (OUTFILEmet, ">$NAME.met") || die "Can NOT open METAR output file for writing";


#---------------------------------------------------------
# For the open file, read all records, determine if have
# hit a METAR record. Once hit a METAR record, write all
# future records in that input file to the $NAME.met file.
# We assume that once a station switches from the old
# data format to METAR, the station never goes back the
# old format. Note that when a station goes to METAR, we
# expect to either the work METAR or the work SPECI in
# a specific column of each line.
#---------------------------------------------------------
$old_ct = 0;
$metar_ct = 0;

while ($line = <INFILE>) 
   { 
   #----------------------------------
   # Read a line from file. Parse it.
   # All lines must be written to one
   # of the output files. 
   #----------------------------------
   $rec_type = substr( $line, 19, 5);

   if ($debug)
      {  
      printf "Read line: $line";
      printf "rec_type=: xxx%sxxx\n", $rec_type; 
      }  

   if (( $rec_type eq "METAR") || ( $rec_type eq "SPECI"))
      {
      $metar_ct++;

      if ($debug)
         {
         printf "\nFound first METAR record. Write rest of lines to *.met file.\n\n";
         }

      print OUTFILEmet "$line";

     #--------------------------------------------
     # Write all subsequent records to METAR file.
     #--------------------------------------------
      while ($line = <INFILE>)
         {
         $metar_ct++;
         print OUTFILEmet "$line";
         } # while data in file

      } # record is METAR
   else
      {
      if ($debug)
         {
         printf "Old format record. Write line to *.old file.\n";
         }
      $old_ct++;

      print OUTFILEold "$line";

      } # record is OLD ASOS format

   }  # while data in file

printf "Found $old_ct OLD recs and $metar_ct METAR recs in $ARGV[0]\n";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "DIVIDE_ASOS.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILEold);
close(OUTFILEmet);
