#! /usr/bin/perl -w
#
##Module--------------------------------------------------------------------------
# <p>convertESCLocalToUTC.pl - This script is used to take sounding files in ESC
# format that incorrectly have local times in the file name and in the UTC and
# Nominal Release lines and convert all the release times and the time in the
# file name to UTC. Only the release times and the date/time in the output file
# name are changed.
#
# The release time lines in the ESC format are of the following form:
#      UTC Release Time (y,m,d,h,m,s):    2008, 08, 01, 08:30:39
#      Nominal Release Time (y,m,d,h,m,s):2008, 08, 01, 08:30:39
#
# A sample input file name is "Indonesia_Surabaya_201203311837.cls"
#
#  @author LE Cully
#  @version Based on code by L. Echo-Hawk for Japanese Naze ESC time
#           conversion from Local to UTC.  Note that for DYNAMO 
#           Indonesian Operational sites such as Surabay where the 
#           raw data are supplied in Local Time which is 7 hours off 
#           from UTC. That is, Local_Time -7 = UTC. Some Indonesian
#           sites are partial hours off from UTC. This code allows
#           the user to input the number or hours and minutes off the
#           data are off from UTC.
#
# @use      convertESCLocalToUTC.pl <ESC Sonde Input file> [Hours off from UTC] [Minutes off from UTC]
#
#           Note that the Hours/Minutes off from UTC can be positive or negative.
#           Year, month, and leap year (Feb 29 vs Feb 28) have been tested. 
#
# Examples:  
#           convertESCLocalToUTC.pl Indonesia_Surbaya_200808010830.cls  -7 0   (Local = UTC - 7.0 hrs)
#           convertESCLocalToUTC.pl Colombo_201112032305.cls  -5 -30   (Local = UTC - 5.5 hrs) 
#
#           Useful in a foreach loop script. Such as shown here:
#
#           for f in *.cls
#             do
#             echo "--------------------------------------------------------"
#             echo "convertESCLocalToUTC.pl $f -7 0"
#             convertESCLocalToUTC.pl $f -7 0
#           done
#           echo "All ESC Class file Local Times converted to UTC!"
#
# WARNING: Output files will be written to same dir as input files. Output
#          files will be same name as input files with ".utc" suffix. Specify
#          the complete path name for files not in same dir.
#
# WARNING: This code only works on sounding ESC formatted data files.
#
# WARNING: Some assumptions are made about underscores in the input file name.
#          Read that section below carefully.
#
# Note: Set $debug = 1 for minor debug output.
#
##Module--------------------------------------------------------------------------
use strict;
use warnings;
use Date::Calc qw(Add_Delta_DHMS);

if (@ARGV < 3) 
   {
   print "Incorrect number of input values:\n Example:: convertESCLocalToUTC.pl Indonesia_Surabaya_201203311837.cls -7 0\n";
   exit(0);
   }


my $debug = 0;

# --------------------------------------------------------------
# At this point, only offset the hour, but s/w below should
# handle offsets to years, days, minutes, and seconds, too.
# --------------------------------------------------------------
my $input_file_name   = $ARGV[0];    # name of file to process

my $days_offset   = 0;
my $hour_offset   = $ARGV[1];   # Number hours off from UTC = -7 for Indonesia
my $minute_offset = $ARGV[2];   # Number of Minutes off from UTC (e.g., Colombo, Sri Lanka is -5hrs 30mins off from UTC)
my $second_offset = 0;;

# -------------------------------------------------------------------------------
# BEWARE: include the complete path name for the input files or run
# this script in the area where the input files are located. 
# The output files will be written to the same dir but with ".utc" as a suffix.
# -------------------------------------------------------------------------------
print " -------\nProcessing::  $input_file_name\t Offset from UTC = $hour_offset $minute_offset\n";

open (my $FILE, $input_file_name) || die "Cant open $input_file_name for reading\n";

#----------------------------------------------
# parse the file name and determine the 
# UTC time to create the new output file name
#----------------------------------------------
my $year=0; my $month=0; my $day=0; 
my $hour=0; my $min=0;   my $sec=0;

if ($input_file_name =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
   { 
   ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5); 
   }

if ($debug) {print "   From Input File Name::  yr,mon,day,hr,min    :: $year, $month, $day, $hour, $min\n";}

#----------------------------------------------------
# Determine the UTC time. Using the Add_Delta_DHMS()
# avoids conversion to/from Epoch seconds.  Note
# that Add_Delta_DHMS() takes a full year value. That
# is, one that hasn't had 1900 subtracted from it. See
# http://docstore.mik.ua/orelly/perl/cookbook/ch03_05.htm
# for more on this fn.
#----------------------------------------------------
my $new_year  = 0; my $new_month = 0; my $new_day   = 0; 
my $new_hour  = 0; my $new_min   = 0; my $new_sec   = 0;

($new_year, $new_month, $new_day, $new_hour, $new_min, $new_sec) = 
    Add_Delta_DHMS( $year, $month, $day, $hour, $min, $sec,
                    $days_offset, $hour_offset, $minute_offset, $second_offset );

if ($debug) {print "   From Add_Delta_DHMS():: yr,mon,day,hr,min,sec:: $new_year, $new_month, $new_day, $new_hour, $new_min, $new_sec\n"; }

my $newDate = sprintf("%04d%02d%02d%02d%02d", $new_year, $new_month, $new_day, $new_hour, $new_min);


#----------------------------------------------------
# Form the output file name.
#----------------------------------------------------
# Parse the input file name by underscores. 
# Use this to form the output file name along with
# the UTC date/time.
#
# BEWARE: This is an assumption that the input file
#         names can be divided by underscores.
#----------------------------------------------------
my @fileNameWords;
@fileNameWords = split("_", $input_file_name);

if ($debug) {print "   fileNameWords = xxx @fileNameWords xxx\n";}
if ($debug) {print "   length (-1) of fileNameWords = xxx $#fileNameWords xxx\n"; }

my $i=0;
my $output_file_name="";

while ($i< $#fileNameWords)
   {
   $output_file_name = $output_file_name.$fileNameWords[$i]."_";
   $i++;
   }

if ($debug) {print "   Partial output_file_name= xxx $output_file_name xxx\n"; }

my $outfile = ""; 
$outfile = sprintf("%s%s.cls.utc", $output_file_name,$newDate);

print "  Final Output File Name:  $outfile\n";

my $OUT;
open ($OUT, ">".$outfile) || die "Can't open output file ($outfile) for writing\n";

# ---------------------------------------------
# Read all the lines in the input file but only 
# update the lines with the Release times. 
# ---------------------------------------------
my @lines = <$FILE>;
close($FILE);

my $index = 0;

foreach my $line (@lines)
   {
   if ($line =~ /UTC Release/)
      {
      # UTC Release Time (y,m,d,h,m,s):    2008, 08, 01, 08:30:39
      chomp($line);
      $sec = (split(":",$line))[3];
      my $new_release_time = sprintf("UTC Release Time (y,m,d,h,m,s):    %04d, %02d, %02d, %02d:%02d:%02d", 
                                       $new_year, $new_month, $new_day, $new_hour, $new_min, $sec); 
      print($OUT "$new_release_time\n");
      }
   elsif ($line =~ /Nominal Release/)
      {
      # Nominal Release Time (y,m,d,h,m,s):2008, 08, 01, 08:30:43
      my $new_nominal_time = sprintf("Nominal Release Time (y,m,d,h,m,s):%04d, %02d, %02d, %02d:%02d:%02d", 
                                     $new_year, $new_month, $new_day, $new_hour, $new_min, $sec);
      print($OUT "$new_nominal_time\n");
      }
   else
      {
      print($OUT $line);
      }
   } # foreach line

close $OUT;
