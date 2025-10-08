#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The preproc_raw_Meisei_data.pl script is used for pre-processing RAW Indonesian
# Meisei data. In particular, this s/w was used to cleanup "bad" times in the
# Indonesian Surabaya raw data. "Bad" times are times that are out of
# sequence (6:32 then 15:28) or there's more than a 10 second gap. This s/w
# also verifies that the date/time (to the hour) match the date/time in the
# first header record.  Beware that this code will warn and then strip records
# with duplicate times that are on sequential records.  This code also catches
# time that goes "backwards", i.e., negative times. The s/w marches through
# the data file records in order from the first rec to the last. Comparisons
# are done between two records only. If a "bad" time (difference) is calculated
# between the two records, the second record is dropped from the output file. 
#
# @author L.E. Cully September 2012
# @version DYNAMOC Created to preprocess Indonesia Surabaya RAW Sounde data.
#
# @use     preproc_raw_Meisei_data.pl F2011101306S7110237.CSV >& output.log
#
# This code assumes:
#    The raw data files have 8 header lines at the top of the file.
#    The data lines are comma delimited. See Meisei format description.
#    That the times are the first parameter on a data line and are in the
#       form hh:mm:ss (e.g., 06:22:49).
#    That the data are 1 second data.  This is the expected time increment.
#    That if there's a time gap in the data greater than 10 seconds, then
#       the second record in the comparison has a "bad" time.
#    That the time in the first data record is "valid" and should be used
#       as "truth" in the first time comparison.
#
# Note: The output file names are of the form: "[input_file_name].TimesFixed".
#
###Module------------------------------------------------------------------------
use strict;
use warnings;
use Date::Calc qw(Delta_DHMS);

if (@ARGV < 1) 
   {
   print "Incorrect number of input values:\n Example:: preproc_raw_Meisei_data.pl F2011101306S7110237.CSV\n";
   exit(0);
   }

printf "\npreproc_raw_Meisei_data.pl began on ";print scalar localtime;printf "\n";

my $debug = 0;

my $input_file_name  = $ARGV[0];    # name of file to process
my $output_file_name = $ARGV[0].".TimesFixed";

my $OUT;
open ($OUT, ">".$output_file_name) || die "Can't open output file ($output_file_name) for writing\n";

my $total_recs_processed = 0;
my $output_file_count= 0; 

# -------------------------------------------------------------------------------
# BEWARE: include the complete path name for the input files or run
# this script in the area where the input files are located. 
# The output files will be written to the same dir as the input files.
# -------------------------------------------------------------------------------
print "---------------------------------------------\nProcessing::  $input_file_name\n";
print "Final Output File Name:  $output_file_name\n\n";

open (my $FILE, $input_file_name) || die "Cant open $input_file_name for reading\n";


# ---------------------------------------------------
# Pull date and expected time out of file name.
# Output a warning if there's a mismatch 
# between file name time (hr) versus the time in 
# first header line of the file.
#
# Sample raw data file name: F2012033118S7111797.CSV
# whose form is  FYYYYMMDDhhSsiteID.CSV .
# ---------------------------------------------------
my $fileName_year  = substr ($input_file_name,1,4);
my $fileName_month = substr ($input_file_name,5,2);
my $fileName_day   = substr ($input_file_name,7,2);
my $fileName_hour  = substr ($input_file_name,9,2);

if ($debug) {print "   fileName: yr, mon, day, hr:: $fileName_year, $fileName_month, $fileName_day, $fileName_hour \n";}

# ---------------------------------------------
# Read all the lines in the input file but only 
# output lines with good times.  Strip records
# with "bad" times where there's a gap of more
# than 10 seconds or the time difference goes
# negative.
# ---------------------------------------------
my @file_lines = <$FILE>;
close($FILE);

# ---------------------------------------------
# Pull out the release time from first line
# of header. This should match the input
# file name. Use this as the first time
# to start time comparisons.  ASSUMPTION!!
# ---------------------------------------------
my $first_hdr_line = $file_lines[0];

my $hdr_year = 0; my $hdr_month = 0; my $hdr_day = 0;
my $hdr_hr = 0;   my $hdr_min = 0;   my $hdr_sec = 0; 

if ($first_hdr_line =~ /RS-06G/)
   {
   chomp($first_hdr_line);
   my @headerInfo = split(',', $first_hdr_line);

   #------------
   # Header Date
   #------------
   my $hdr_date = trim($headerInfo[4]); # YYYY/MM/DD

   if ($hdr_date =~ /(\d{4})\/(\d{2})\/(\d{2})/)
      {
      ($hdr_year, $hdr_month, $hdr_day) = ($1,$2,$3);
      if ($debug) { print "Date from Header:: $hdr_date  -> $hdr_year $hdr_month $hdr_day\n"; }
      }
   else
      { print "WARNING: No valid date found in header info!\n"; }

   #------------
   # Header Time 
   #------------
   my $hdr_time = trim($headerInfo[5]); # "HH:MM:SS"

   if ($hdr_time =~ /(\d{2})\:(\d{2})\:(\d{2})/) 
      {
      ($hdr_hr, $hdr_min, $hdr_sec) = ($1,$2,$3);
      if ($debug) { print "Time from Header:: $hdr_time  -> $hdr_hr  $hdr_min  $hdr_sec\n"; }
      }
   else
      { print "WARNING: No valid TIME found in header info!\n"; }

   } # Get date/time from first header line. This is the sonde release time.

#-----------------------------------------------------------
# Compare the file name date/time versus the header date/time.
# They should match!
#-----------------------------------------------------------
if ($fileName_year != $hdr_year || $fileName_month != $hdr_month ||
    $fileName_day != $hdr_day || $fileName_hour != $hdr_hr)
   {
   print "WARNING: Date/Time in File Name does NOT match Date/Time in first Header Record!\n";
   }

#****************************************************************
#----------------------------------------------------------------
# Go through all data records and strip records with "bad" times.
#----------------------------------------------------------------
#****************************************************************

###my $index = 0;
#
my $line1_index = 0;
my $line2_index = 1;
my $change_index1 = 1;
my $line1_written = 0;

my $fileLength = $#file_lines; # count starts at zero
my $total_lines_in_file = $#file_lines + 1;
my $data_lines_in_file = $total_lines_in_file - 7;

my $bad_time_found = 0;
my $bad_rec_time = 0;
my $number_of_bad_times_found = 0;

my $line1 = ""; 
my $time1=""; 
my @timeinfo1=""; 
my $hr1=0; 
my $min1=0; 
my $sec1=0;

my $line2 = ""; 
my $time2=""; 
my @timeinfo2=""; 
my $hr2=0; 
my $min2=0; 
my $sec2=0;

if ($debug) 
   {
   print "   Number of TOTAL lines in raw file = $total_lines_in_file\n";
   print "   Number of DATA lines in raw file = $data_lines_in_file\n";
   print "\nBegin for loop\n";
   }

while ($line1_index < $fileLength && $line2_index < $total_lines_in_file)
   {
   if ($debug) {print "--------------------------\nline1_index = $line1_index  line2_index = $line2_index\n";}

   if ($line1_index < 7)
      {
      #----------------------------------------------------------
      # Skip header lines and write to the output file untouched.
      # First 8 lines (0-7) are header records. 
      #----------------------------------------------------------
      if ($debug) {print "HDR Line - write to output file as is. file_lines line1_index = $line1_index.\n";}

      my $lineH = $file_lines[$line1_index];
      print($OUT $lineH);

      if ($debug) {print "lineH:: $lineH\n";}

      $line1_index++;
      $line2_index++;
      }
   else
      {
      if ($debug) {print "DATA Lines - check times. line1_index= $line1_index line2_index = $line2_index\n";}

      $bad_time_found = 0;

      #-----------------------------------------------------------------
      # Get data time from record. Date/Time must fall between
      # Fri Dec 13 20:45:52 1901 to Tue Jan 19 03:14:07 2038 (inclusive)
      # for these PERL "DMHS" calls to work. 
      #-----------------------------------------------------------------
      # Sample sequential date/times from raw data in the form: hh:mm:ss
      # 18:21:57    OR     06:54:25
      # 18:21:58           15:32:50
      # 18:21:59           06:54:27
      # 18:22:00           06:54:28
      #-----------------------------------------------------------------
      if ($change_index1)
         {
         $line1 = $file_lines[$line1_index];
         $time1 = (split(",",$line1))[0]; # e.g., 18:21:54
         @timeinfo1 = split(":",$time1);  # e.g., 18 21 54
         $hr1  = $timeinfo1[0]; # e.g., 18
         $min1 = $timeinfo1[1]; # e.g., 21
         $sec1 = $timeinfo1[2]; # e.g., 54

         if ($debug) {print "Line1: hr1, min1, sec1:  $hr1, $min1, $sec1\n";}
         }
      else
         {
         if ($debug) {print "Keeping same line1\n";}
         }

      $line2 = $file_lines[$line2_index];
      $time2 = (split(",",$line2))[0]; # e.g., 18:21:55
      @timeinfo2 = split(":",$time2);  # e.g., 18 21 55
      $hr2  = $timeinfo2[0]; 
      $min2 = $timeinfo2[1];
      $sec2 = $timeinfo2[2];

      if ($debug) 
         {
         ####print "Compare line1:: $line1\n";
         print "Line1: hr1, min1, sec1:  $hr1, $min1, $sec1\n";
         print "Compare line2:: $line2\n";
         print "Line2: hr2, min2, sec2:  $hr2, $min2, $sec2\n";
         }

      #------------------------------------------------------------------------
      # Compute the time difference between two sequential records.
      # Use the Delta_DHMS fn. The Delta_DHMS function returns a 
      # four-element list corresponding to the number of days, hours, 
      # minutes, and seconds between the two dates you give it.
      #
      # NOTE: Use the year from the input file name or the first
      #       header line. They should match or a warning is issued.
      #
      # Example of Delta_DHMS:
      #    @bree = (1981, 6, 16, 4, 35, 25);   # 16 Jun 1981, 4:35:25
      #    @nat  = (1973, 1, 18, 3, 45, 50);   # 18 Jan 1973, 3:45:50
      #    @diff = Delta_DHMS(@nat, @bree);
      #    print "Bree came $diff[0] days, $diff[1]:$diff[2]:$diff[3] after Nat\n";
      #    Bree came 3071 days, 0:49:35 after Nat
      #
      #------------------------------------------------------------------------
      my @dateTime1 = ($hdr_year, $hdr_month, $hdr_day, $hr1, $min1, $sec1);
      my @dateTime2 = ($hdr_year, $hdr_month, $hdr_day, $hr2, $min2, $sec2);

      my @dateTimeDiff = Delta_DHMS(@dateTime1, @dateTime2);

      if ($debug) {print "Difference between times:\n"; print "dateTimeDiff: @dateTimeDiff\n";}

      #--------------------------------------------------
      # If diff is negative, then time decreasing.
      # This is a bad time! (Days, Hrs, Mins, Secs)
      # Verified that fn returns negative values, properly
      # based on date/time. 
      #--------------------------------------------------
      if ($dateTimeDiff[0] < 0 || $dateTimeDiff[1] < 0 ||
          $dateTimeDiff[2] < 0 || $dateTimeDiff[3] < 0 )
         {
         $bad_time_found = 1;
         $bad_rec_time = $line2_index;
         $number_of_bad_times_found++;

         print "WARNING: Negative date/time difference (@dateTimeDiff) found at record $line1_index versus $line2_index. Time going backwards at record = $bad_rec_time !\n";
         if ($debug) {print "WARNING: Record at ($bad_rec_time) will be dropped.\n";}
         }
      elsif ($dateTimeDiff[0] > 0 || $dateTimeDiff[1] > 0 || $dateTimeDiff[2] > 0 ) # Diff Days, Hrs, or Mins > 0
         {
         #--------------------------------------------------
         # If diff value has any number Days, Hrs, or Mins.
         # This is a Very Large difference. Issue warningx
         # and drop record with bad time.
         # Note that the raw data is expected to be 
         # in 1 second time increments. If off by days, hrs,
         # or minutes, this is a very large (unacceptable)
         # gap in time.
         #
         # If all of those elements
         # are zero, then times are closer and do finer
         # check on number of seconds in diff.
         #--------------------------------------------------
         $bad_time_found = 1;
         $bad_rec_time = $line2_index;
         $number_of_bad_times_found++;

         print "WARNING: VERY LARGE date/time difference found. (Diff Days, Hrs, Mins:: $dateTimeDiff[0], $dateTimeDiff[1], $dateTimeDiff[2] at record = $bad_rec_time !\n";
         if ($debug) {print "WARNING: Record at ($bad_rec_time) will be dropped.\n";}
         }
      elsif ($dateTimeDiff[0] == 0 && $dateTimeDiff[1] == 0 && $dateTimeDiff[2] == 0 && $dateTimeDiff[3] == 0 )
         {
         #------------------------------------------
         # Same day, hour, minutes, and seconds.
         # Possibly we skipped a whole year, either
         # way, it's a bad time.
         #------------------------------------------
         $bad_time_found = 1;
         $bad_rec_time = $line2_index;
         $number_of_bad_times_found++;
       
         print "WARNING: DUPLICATE date/time. No difference found. (Diff Days, Hrs, Mins:: $dateTimeDiff[0], $dateTimeDiff[1], $dateTimeDiff[2] at record = $bad_rec_time !\n";
         if ($debug) {print "WARNING: Record at ($bad_rec_time) will be dropped.\n";}
         }
      else
         {
         #-------------------------------------------------------------
         # If difference exists, it's less than a day, hour, or minute.
         # Check for finer time difference.
         # If difference is of seconds few seconds, then that's OK so
         # output both recs and move forward one rec.
         #
         # If difference is >10 seconds, then bad time! Gap is too large.
         # Output first record. Drop the second record. Keep first time 
         # for next comparison.
         #-------------------------------------------------------------
         if ($dateTimeDiff[3] > 10) # check diff seconds
            {
            $bad_time_found = 1;
            $bad_rec_time = $line2_index;
            $number_of_bad_times_found++;

            print "WARNING: Time difference is too LARGE and is > 10 SECONDS: $dateTimeDiff[3] at record = $bad_rec_time !\n";
            if ($debug) {print "WARNING: Record at ($bad_rec_time) will be dropped.\n";}
            }
         else
            {
            if ($debug) {print "Good Time: Time difference ($dateTimeDiff[3] secs) < 10 seconds:. Keep both recs at $line1_index and $line2_index.\n"; }

            $bad_time_found = 0;
            $bad_rec_time = 0;
            }
         } # Positive date/time diff

      #----------------------------------------------------
      # Don't write out the recs with "bad times" which is 
      # the second record in the comparison. Only write out 
      # rec with good time....if not already written out.
      #----------------------------------------------------
      if (!$bad_time_found) # time is good!
         {
         #-------------------------------------------------------
         # Good time diff found, so both recs OK. Print first rec
         # and move forward one rec and begin next comparison.
         # The second line used in this comparison will be written
         # out during the next comparison.
         #-------------------------------------------------------
         if ($debug) { print "Both Good Record times. Write to output line1: $line1\n"; }
         
         print($OUT $line1); 

         $change_index1 = 1;
         $line1_written = 0;

         $line1_index = $line2_index;
         $line2_index++;
         } 
      else
         {
         #-------------------------------------------------------
         # Bad time diff found between line1 and line2. Assume (?)
         # line1 is OK and line2 is the problem. Write out line1
         # but drop line2 from the raw data. 
         #-------------------------------------------------------
         if ($debug)
            {
            print "WARNING: Found bad time at index = $bad_rec_time Write line1 to output but Do Not Write line2 Output File!\n";
            print "Line1 at $line1_index written to output: $line1\n";
            print "Line2 at $line2_index will NOT be written to output: $line2\n";
            }


         # --------------------------------------------------
         # Only write this line out once, not multiple times!
         # --------------------------------------------------
         if ($line1_written) 
            {
            print($OUT $line1); 
            $line1_written = 1; # If multiple bad recs, only output line1 once. 
            }

         #-------------------------------------------------------
         # Skip line2 with bad time. Retain time from line1 for
         # next comparison.
         #-------------------------------------------------------
         $change_index1 = 0;
         $line2_index++;

         if ($debug) { print "Keep line1 at same rec ($line1_index), but move line2 forward to rec $line2_index .\n"; }
         }

      } # Non-header line found = data record found

   $total_recs_processed++;

   } # while data in file


#--------------------------------------------------------
# Final cleanup at end of file. Write out the last record 
# if not bad time. 
#--------------------------------------------------------
if (!$bad_time_found)
   {
   if ($debug) { print "Post While Loop: Last rec has good time! Write out the last (line2) rec of file: $line2\n"; }
   print($OUT $line2);
   $total_recs_processed++;
   }
else
   {
   #--------------------------------------------------------------------
   # That last rec was bad, but in effect it was compared and processed.
   # So, don't forget to count it. And write out the line before the
   # last if not already written to output.
   #--------------------------------------------------------------------
   if ($debug) { print "Post While Loop: Ending with BAD comparison. Still need to output line1. First line in comparison: $line1\n"; }

   if (!$line1_written)
      {
      print($OUT $line1);
      $line1_written = 1; # don't need to set this since at end of file.
      }

   if ($line2_index == $total_lines_in_file)
      { $total_recs_processed++; }

   }

close $OUT;

print "\nTotal records from file size = $total_lines_in_file \n";
print "Expected number of data records to process  = $data_lines_in_file \n\n";

print "Total number of records processed = $total_recs_processed \n";
print "Total number of bad times found = $number_of_bad_times_found . These records were dropped!\n";
print "\nCompleted Processing::  $input_file_name \n---------------------------------------------\n";
printf "preproc_raw_Meisei_data.pl end on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
## @signature String trim(String line)
## <p>Remove all leading and trailing whitespace from the specified String.</p>
##
## @input $line The String to be trimmed.
## @output $line The trimmed String.
###------------------------------------------------------------------------------
sub trim {
   my ($line) = @_;
   return $line if (!defined($line));
   $line =~ s/^\s+//;
   $line =~ s/\s+$//;
   return $line;
} #trim()
