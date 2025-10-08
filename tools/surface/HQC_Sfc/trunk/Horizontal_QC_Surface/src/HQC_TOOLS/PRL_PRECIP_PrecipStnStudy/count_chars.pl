#!/usr/bin/perl 

#-------------------------------------------------------
# count_chars.pl - This perl script/program takes
#   a list of input stations and determines  the number
#   of chars in those stations. This is to verify of
#   the stations in the Sid Katz precip (dly & hly)
#   which have 4 chars. The ID's with 4 chars are
#   suppose to be suspect and probably due to 
#   transmission errors. 
# 
# 98 Jul lec
#   Created.
# 98 Aug lec
#   Update s/w to create list of all IDs that do ont
#   have 4 chars and do not begin with Xlatlon. The
#   Xlatlon IDs are called "stranger" IDs.
#-------------------------------------------------------*/
$debug = 0;

#-----------------------------
# First get and print the time
#-----------------------------
($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "\ncount_chars.pl began at $year/$mon/$mday $hour:$min:$sec\n";

#---------------------------------------
# Open the input and output files.
# Update to read name from input 
# line and create output file name.
#----------------------------------------
print "Processing input file: junk.txt\n";

open (INSTNFILE, "stns_not_in_list.out") || die "Can NOT open file stns_not_in_list.out for reading";

open (OUTFILE4CHARS,   ">count_4chars.out") || die "Can NOT open output file for writing";
open (OUTFILESTATS,    ">count_stats.out") || die "Can NOT open output file for writing";
open (OUTFILESTRANGER, ">count_stranger.out") || die "Can NOT open output file for writing";
open (OUTFILEMISS,     ">truly_missing_IDs.out") || die "Can NOT open output file for writing";

$ii = 0;

$smaller = 0;
$larger = 0;
$equal = 0;

$zero = 0;
$one = 0;
$two =0;
$three = 0;
$five = 0;
$six = 0;
$seven = 0;
$eight = 0;
$nineOrMore = 0;
$stranger = 0;

while ($line = <INSTNFILE>)
   {
   chop ($line);

   @parts = split (" ", $line);

   print "\nline:xxx", $line, "xxx\n";
   print "parts[4]:xxx", $parts[4], "xxx\n";

   $len = length ($parts[4]);

   print "length = ", $len, "\n";

   #-----------------------------
   # This 4 char IDs are junk 
   # due to transmission errors.
   # During data processing, they
   # will have to be stripped.
   #-----------------------------
   if ($len == 4)
      {
      print OUTFILE4CHARS $parts[4],"\n";
      $equal++;
      }

   if ($len < 4)
      {
      $smaller++;

      if ($len == 0) {$zero++;}
      if ($len == 1) {$one++;}
      if ($len == 2) {$two++;}
      if ($len == 3) {$three++;}

      print OUTFILEMISS $parts[4],"\n";
      }
 
   if ($len > 4) 
      { 
      $larger++;

      if ($len == 5) {$five++;  print OUTFILEMISS $parts[4],"\n";}
      if ($len == 6) {$six++;   print OUTFILEMISS $parts[4],"\n";}
      if ($len == 7) {$seven++; print OUTFILEMISS $parts[4],"\n";}

      if ($len == 8)
         {
         $eight++;

         $name = substr ($parts[4], 0, 1);

         print "parts[4]= xxx", $parts[4], "xxx\n";
         print "NAME(1st char)= xxx", $name, "xxx\n";
 
         if ($name eq "X")
            {
            $stranger++;

            print "Found a STRANGER ID - Don't print as missing.\n";
            print OUTFILESTRANGER $parts[4], "\n";
            }
         else
            {
            print OUTFILEMISS $parts[4],"\n";
            }

         } # == 8

      if ($len > 8) {$nineOrMore++; print OUTFILEMISS $parts[4],"\n";}

      } # > 4chars

   $ii++;

   } # while data in file

   print OUTFILESTATS "Number records read: ", $ii, "\n";
   print OUTFILESTATS "<4chars, >4chars, =4chars: ", $smaller," ", $larger," ", $equal,"\n";
   print OUTFILESTATS "#chars  Count\n";
   print OUTFILESTATS "-------------\n";
   print OUTFILESTATS "0\t", $zero,"\n","1\t", $one,"\n", "2\t",$two,"\n", 
         "3\t", $three,"\n", "4\t", $equal,"\n", "5\t", $five,"\n", 
         "6\t",$six,"\n", "7\t", $seven,"\n", "8\t", $eight,"\n", ">8\t",$nineOrMore, "\n\n";

   print OUTFILESTATS "Stranger IDs found =\n", $stranger;

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
printf "count_chars.pl ended at $year/$mon/$mday $hour:$min:$sec\n";

close(INFILE);
close(OUTFILESTATS);
close(OUTFILE4CHARS);
close(OUTFILEMISS);
close(OUTFILESTRANGER);
