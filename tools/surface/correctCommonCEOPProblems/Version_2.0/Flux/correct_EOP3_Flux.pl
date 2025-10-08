#!/usr/bin/perl 

#-------------------------------------------------------
# correct_EOP3_Flux.pl - This perl program corrects some
#   common problems found in the CEOP EOP3 Flux data 
#   provided by the source in "almost" the right format.
#   This s/w only corrects the Flux format.
#-------------------------------------------------------
#   WARNING: Search for "HARDCODED" to find all fixed values.
#
#   WARNING: This s/w assumes the data are space delimited
#      and in a format similar to that described in the
#      CEOP EOP3 Approved Report 2003.
#
# 3 Feb 2004 lec
#   Created.
#-------------------------------------------------------
$debug = 1;

$number_lines_read = 0;

$neg_zeros_reset = 0;
$incorrect_missing_reset = 0;
$mismatched_missing_reset = 0;


printf "\ncorrect_EOP3_Flux.pl began on ";print scalar localtime; print "\n";

############################################################
#-----------------------------------------------------------
# parse_input_line() - subroutine to parse input 
#   Flux line as defined by EOP3 report.
#-----------------------------------------------------------
sub parse_input_line{
   my ($input_line) = @_;

   if ($debug) {print "Enter parse_input_line()\n";}

   #-------------------------------------------
   # Divide record by meta data versus data
   # Total line length is 160 including return.
   # HARDCODED Values below.
   #-------------------------------------------
   $meta_line = substr ($input_line, 0, 112);
   @meta_parts = split (' ', $meta_line); # This may not work if spaces in names!!

   if ($debug) {print "Meta line: xxx $meta_line xxx\n";}
   if ($debug) 
      {
      print "Meta parts: xxx @meta_parts xxx\n";

      for ($ii=0; $ii<=10; $ii++)
       {print "Meta_parts[$ii]: xxx $meta_parts[$ii] xxx\n";}
      }

   $data_line = substr ($input_line, 112, 250);
   @data_parts = split (' ', $data_line);

   if ($debug) {print "Data line: xxx $data_line xxx\n";}
   if ($debug)
      {
      print "Data parts: xxx @data_parts xxx\n";

      for ($ii=0; $ii<=8; $ii++)
       {print "Data_parts[$ii]: xxx $data_parts[$ii] xxx\n";}
      } 

   if ($debug) {print "Exit parse_input_line()\n";}
   } # parse_input_line()

############################################################
#---------------------------------------------------------
# correct_output_values() - subroutine to corrected output
#   Sfc/Met/Rad line as defined by EOP3 report.
# Do fix values computed from all missing. Reset to -999.99 M.
# Do fix Justification on names and float values. Left Justify names.
#    right justify floats.
# Do NOT reset -0.00 to 0.00.
# Do reset "-999.99 ?" to "-999.99 M".
# Do reset "999.99 M" to "-999.99 M".
#-----------------------------------------------------------
# Don't strip all missing recs nor resort as of 13 Jan 2004.
#-----------------------------------------------------------
sub correct_output_values {  
   my ($current_line) = @_; 

    if ($debug) {print "Enter correct_output_values()\n";}

    for ($jj=0; $jj<=7; $jj=$jj+2)
       {
       if ($debug) {print "first loop: jj = $jj. Data_parts[$jj]=$data_parts[$jj], Data_parts[$jj+1]=$data_parts[$jj+1]\n";}

       #-----------------------------------
       # Convert '999.99 M' to '-999.99 M'. 
       #-----------------------------------
       if ($data_parts[$jj] eq "999.99" && $data_parts[$jj+1] eq 'M')
          { 
          $incorrect_missing_reset++;

          print "Resetting 999.99 M ($data_parts[$jj] $data_parts[$jj+1]) to -999.99 M at $current_line. Total Found: $incorrect_missing_reset.\n";

          $data_parts[$jj] = -999.99; 
          }

       #-----------------------------------
       # Convert '-999.99 ?' to '-999.99 M'.
       #-----------------------------------  
       if ($data_parts[$jj] eq "-999.99" && $data_parts[$jj+1] ne 'M')
          { 
          $mismatched_missing_reset++;

          print "Resetting -999.99 $data_parts[$jj+1] ($data_parts[$jj] $data_parts[$jj+1]) to -999.99 M at $current_line. Total Found: $mismatched_missing_reset.\n";
          $data_parts[$jj+1] = 'M'; 
          }

       } # for data_parts

    if ($debug) {print "Exit correct_output_values()\n";}

   } # correct_output_values()


############################################################
#--------------------------------------------------
# write_output_line() - subroutine to writes output
#   Sfc/Met/Rad line as defined by EOP3 report.
#--------------------------------------------------
sub write_output_line {

    if ($debug) {print "Enter write_output_line()\n";}

    # Print Dates through elevation. First 10 elements of line.
    printf OUTFILE_JOSSCorrected ("%10s %5s %10s %5s %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f", 
    $meta_parts[0], $meta_parts[1], $meta_parts[2], $meta_parts[3], 
    $meta_parts[4], $meta_parts[5], $meta_parts[6], $meta_parts[7], 
    $meta_parts[8], $meta_parts[9], $meta_parts[10], $meta_parts[11]);

    if ($debug) {print "Meta data printed.\n";}

    #-------------------------------------------
    # Print Sensible Heat through Soil Heat Flux
    #-------------------------------------------
    for ($mm=0; $mm<=7; $mm=$mm+2)  # print 4 vals and 4 flags.
       {
       if ($debug) {print "first Data loop: mm = $mm\n";}

       printf OUTFILE_JOSSCorrected (" %8.2f %s", 
          $data_parts[$mm], $data_parts[$mm+1]);
       }

    printf OUTFILE_JOSSCorrected ("\n"); 
 
    if ($debug) {print "Exit write_output_line()\n";}
   } #write_output_line()


########################################################
#---------------------------------------
# Count number of lines in the INFILE
#---------------------------------------
open (INFILE, "<$ARGV[0]") ||
   die "Can NOT open input_file ($ARGV[0]) for reading";

$num_recs_in_file += tr/\n/\n/ while sysread (INFILE, $_, 2 ** 16);
close(INFILE);
print "There are $num_recs_in_file lines in $ARGV[0]\n";

open (INFILE, "<$ARGV[0]") ||
   die "Can NOT open input_file ($ARGV[0]) for reading";

open (OUTFILE_JOSSCorrected, ">$ARGV[0].JOSScorrected") ||
   die "Can NOT open $ARGV[0].JOSScorrected for writing.";


#----------------------------------
# Process all the data in the file.
#----------------------------------
while ($uncorrected_line = <INFILE>)
   {
   if ($debug){print "--------------\n"; print "Read line: $uncorrected_line\n";}

   $number_lines_read++; 

   #--------------------------------------
   # Parse into Meta and Data parts line.
   #--------------------------------------
   if ($debug){print "Call parse_input_line.";}
   parse_input_line($uncorrected_line);

   #------------------
   # Make Corrections.
   #------------------
   if ($debug){print "Call correct_output_values.";}
   correct_output_values ($number_lines_read);

   #----------------------------------
   # Write out line in correct format.
   #----------------------------------
   if ($debug){print "Call write_output_line.";}
   write_output_line;

   } # while data in file


close(INFILE);
close(OUTFILE_JOSSCorrected);

print "Total number lines Input File: $num_recs_in_file\n";
print "Total number lines processed: $number_lines_read\n";
print "Error Corrections Completed: \n";
print "   Negative Zeroes Reset: $neg_zeros_reset\n";
print "   Incorrect Missings  (999.99 M) Reset: $incorrect_missing_reset\n";
print "   Mismatched Missing (-999.99 ?) Reset: $mismatched_missing_reset\n";

printf "correct_EOP3_Flux.pl ended on ";print scalar localtime; printf "\n";
