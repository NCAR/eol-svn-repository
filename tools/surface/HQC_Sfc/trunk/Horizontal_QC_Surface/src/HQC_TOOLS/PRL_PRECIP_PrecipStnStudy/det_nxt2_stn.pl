#!/usr/bin/perl 

#-------------------------------------------------------
# det_nxt2_stn.pl - This perl script/program takes
#   a list of input stations and determines which of
#   those stations is NOT in a given station list.
#   (Hash table searching - appears to work and is
#   extremely fast.)
# 
# 98 Jul lec
#   Created.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ndet_nxt2_stn.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file.\n";

#------------------------------------------------------
# nwsli.uniq (or ESOP96_IDs) - given stn list
# stnids - list of IDs from dly and hly Sid Katz data.
# stns_not_in_list.out - list of ID's not in nwsli.uniq
# stns_totalUniq_in_nwsli.out - uniq IDs in nwsli.uniq
#------------------------------------------------------
open (INSTNFILE, "ESOP96_IDs") || die "Can NOT open file hash.inp for reading";
open (INCHECKFILE, "IDs_not_in_list.in") || die "Can NOT open input file stnids for reading";
open (OUTFILE, ">IDs_not_in_ESOP96.out") || die "Can NOT open output file for reading";
open (OUTFILE2, ">IDs_in_ESOP96.out") || die "Can NOT open output2 file for reading";

#---------------------------------
# Read in the given station list.
# Fill the associative array.
#---------------------------------
%stnIDs = ();

$p0ct = 0;
$p4ct = 0;
$p4Dash = 0;

while ($line = <INSTNFILE>)
   {
   chop ($line);

   @parts0 = $line;

   print "\nline:xxx", $line, "xxx\n";
   print "parts0[0]:xxx", $parts0[0], "xxx\n";
#
#-----------------------------------------------
#   Pick out the primary and secondary IDs and
#   put them into hash tables (assoc. arrays).
#-----------------------------------------------

#orig:   print OUTFILE2 $parts0[0],"\n";

   $p0ct++;

   $stnIDs{ $parts0[0] } = "filler";

   } # Fill associative array.


print "p0ct = ", $p0ct,"\n";

#----------------------------------
# Read a line from file. Parse it.
# Only write out lines if stn is
# NOT in given list.
#----------------------------------
$i = 0;
while ($line = <INCHECKFILE>) 
   { 
   $stn = substr( $line, 0, 9);
   chop ($stn);

   if ($debug)
      {  
      printf "Read line: $line\n";
      printf "stn=:xxx%sxxx\n", $stn; 
      }

   if ( length ( $stnIDs{ $stn } ) != 0 )
      {
      if ($debug)
         {
         printf "Found Station.\n";
         }
      print OUTFILE2 "Station in list: " , $stn, "\n"
      }
   else
      {
      if ($debug)
         {
         printf "Station NOT found.\n";
         }

      # Write to output file.
      print OUTFILE "Station not in list: " , $stn, "\n"

      }

   }  # while data in file

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "det_nxt2_stn.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILE);
