#!/usr/bin/perl 

#------------------------------------------------------------
# correct_CEOP_EOP3_data.pl - This perl program corrects some
#   common problems found in the CEOP EOP3 SFC data 
#   provided by the source in "almost" the right format.
#   This s/w only corrects all 4 CEOP EOP3 formats.
#
#   To execute this s/w:
#
#      correct_CEOP_EOP3_data.pl [input_file] [data_type]
#
#   where data_type is one of SURFACE, FLUX, SOILS, or  TOWER.
#
#------------------------------------------------------------
#   WARNING: Search for "HARDCODED" to find all fixed values.
#
#   WARNING: This s/w assumes the data are space delimited
#      and in a format similar to that described in the
#      CEOP EOP3 Approved Report 2003.
#
# 3 Feb 2004 lec
#   Created.
# 19 Jan 2010 lec
#   Update all JOSS references to EOL.
#------------------------------------------------------------
$debug = 0;

$number_lines_read = 0;

$neg_zeros_reset = 0;
$incorrect_missing_reset = 0;
$mismatched_missing_reset = 0;

#-------------------------------------------------------------------
# Note that Number of meta and data parts is actually one more than
# shown since Perl arrays start at zero.
#-------------------------------------------------------------------
%position = (
   SURFACE =>{CHARS_IN_META=>107, NUM_META_PARTS=>9,  CHARS_IN_DATA=>256, NUM_DATA_PARTS=>37},
   FLUX    =>{CHARS_IN_META=>115, NUM_META_PARTS=>10, CHARS_IN_DATA=>256, NUM_DATA_PARTS=>7 },
   SOILS   =>{CHARS_IN_META=>115, NUM_META_PARTS=>10, CHARS_IN_DATA=>256, NUM_DATA_PARTS=>3 },
   TOWER   =>{CHARS_IN_META=>115, NUM_META_PARTS=>10, CHARS_IN_DATA=>256, NUM_DATA_PARTS=>17}
   );  


############################################################
#-----------------------------------------------------------
# parse_input_line() - subroutine to parse input 
#   Sfc/Met/Rad line as defined by EOP3 report.
#-----------------------------------------------------------
sub parse_input_line{
   my ($input_line) = @_;

   if ($debug) {print "Enter parse_input_line(). data_type = $data_type\n";}
   if ($debug) {print "\n";}

   #-------------------------------------------
   # Divide record by meta data versus data
   #-------------------------------------------
   $meta_line = substr ($input_line, 0, $position{$data_type}{CHARS_IN_META});
   @meta_parts = split (' ', $meta_line); # This may not work if spaces in names!!

   if ($debug) {print "position{data_type}{CHARS_IN_META}: xxx $position{$data_type}{CHARS_IN_META} xxx\n";}
   if ($debug) {print "Meta line: xxx $meta_line xxx\n";}
   if ($debug) 
      {
      print "Meta parts: xxx @meta_parts xxx\n";

      for ($ii=0; $ii<=$position{$data_type}{NUM_META_PARTS}; $ii++)
       {print "Meta_parts[$ii]: xxx $meta_parts[$ii] xxx\n";}
      }

   $data_line = substr ($input_line, $position{$data_type}{CHARS_IN_META}, 
                                     $position{$data_type}{CHARS_IN_DATA});

   @data_parts = split (' ', $data_line);

   if ($debug) {print "Data line: xxx $data_line xxx\n";}
   if ($debug)
      {
      print "Data parts: xxx @data_parts xxx\n";

      for ($ii=0; $ii<=$position{$data_type}{NUM_DATA_PARTS}; $ii++)
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

    $index  = $position{$data_type}{NUM_DATA_PARTS} + 1;

    for ($jj=0; $jj<=$index; $jj=$jj+2)
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
# write_output_sfc_line() - subroutine to writes output
#   Sfc/Met/Rad line as defined by EOP3 report.
#   HARDCODED VALUES.
#--------------------------------------------------
sub write_output_sfc_line {

    if ($debug) {print "Enter write_output_line()\n";}

    # Print Dates through elevation.
    printf OUTFILE_EOLCorrected ("%10s %5s %10s %5s %-10s %-15s %-15s %10.5f %11.5f %7.2f", 
    $meta_parts[0], $meta_parts[1], $meta_parts[2], $meta_parts[3], 
    $meta_parts[4], $meta_parts[5], $meta_parts[6], $meta_parts[7], 
    $meta_parts[8], $meta_parts[9], $meta_parts[10]);

    if ($debug) {print "Meta data printed.\n";}

    #-----------------------------
    # Print Data values and flags.
    #-----------------------------
#   printf OUTFILE_EOLCorrected (" %7.2f %s  %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %8.2f %s %8.2f %s %8.2f %s %8.2f %s %8.2f %s %8.2f %s %8.2f %s %8.2f %s\n",

    #---------------------------------------
    # Print Stn Pressure through Snow depth.
    #---------------------------------------
    for ($mm=0; $mm<=21; $mm=$mm+2)  # print 22 vals and flags. (HARDCODED)
       {
       if ($debug) {print "first Data loop: mm = $mm\n";}

       printf OUTFILE_EOLCorrected (" %7.2f %s", 
          $data_parts[$mm], $data_parts[$mm+1]);
       }
 
    #-----------------------------------------------
    # Print Incoming Shortwave through Outgoing PAR.
    # HARDCODED VALUES.
    #-----------------------------------------------
    for ($mm=22; $mm<=$position{$data_type}{NUM_DATA_PARTS}; $mm=$mm+2)  # print 16 vals and flags.
       {
       if ($debug) {print "second Data loop: mm = $mm\n";}
       printf OUTFILE_EOLCorrected (" %8.2f %s",
          $data_parts[$mm], $data_parts[$mm+1]);
       } 

    printf OUTFILE_EOLCorrected ("\n");

    if ($debug) {print "Exit write_output_sfc_line()\n";}
   } #write_output_sfc_line()

############################################################
#-------------------------------------------------------
# write_output_tower_line() - subroutine to writes output
#   Tower line as defined by EOP3 report.
#-------------------------------------------------------
sub write_output_tower_line {

    if ($debug) {print "Enter write_output_tower_line()\n";}

    #--------------------------
    # Print Meta data and data.
    #--------------------------
    printf OUTFILE_EOLCorrected ("%10s %5s %10s %5s %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s %7.2f %s\n",
    $meta_parts[0], $meta_parts[1], $meta_parts[2], $meta_parts[3],
    $meta_parts[4], $meta_parts[5], $meta_parts[6], $meta_parts[7],
    $meta_parts[8], $meta_parts[9], $meta_parts[10],
    $data_parts[0], $data_parts[1], $data_parts[2], $data_parts[3],
    $data_parts[4], $data_parts[5], $data_parts[6], $data_parts[7],
    $data_parts[8], $data_parts[9], $data_parts[10], $data_parts[11],
    $data_parts[12], $data_parts[13], $data_parts[14], $data_parts[15],
    $data_parts[16], $data_parts[17] );

    if ($debug) {print "Exit write_output_tower_line()\n";}
   } #write_output_tower_line()        



############################################################
#-------------------------------------------------------
# write_output_flux_line() - subroutine to writes output
#   Flux line as defined by EOP3 report.
#   HARDCODED VALUES.
#-------------------------------------------------------
sub write_output_flux_line {

    if ($debug) {print "Enter write_output_flux_line()\n";}

    # Print Dates through elevation.
    printf OUTFILE_EOLCorrected ("%10s %5s %10s %5s %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f %8.2f %s %8.2f %s %8.2f %s %8.2f %s\n",
    $meta_parts[0], $meta_parts[1], $meta_parts[2], $meta_parts[3],
    $meta_parts[4], $meta_parts[5], $meta_parts[6], $meta_parts[7],
    $meta_parts[8], $meta_parts[9], $meta_parts[10],
    $data_parts[0], $data_parts[1], $data_parts[2], $data_parts[3],
    $data_parts[4], $data_parts[5], $data_parts[6], $data_parts[7]);

    if ($debug) {print "Exit write_output_flux_line()\n";}
   } #write_output_flux_line()
                                 

############################################################
#-------------------------------------------------------
# write_output_soils_line() - subroutine to writes output
#   Soils line as defined by EOP3 report.
#-------------------------------------------------------
sub write_output_soils_line {

    if ($debug) {print "Enter write_output_soils_line()\n";}
    
    if ($debug) {print "data_parts[0,1,2,3]: $data_parts[0], $data_parts[1], $data_parts[2], $data_parts[3]\n";}

    # Print Meta data and data.
    printf OUTFILE_EOLCorrected ("%10s %5s %10s %5s %-10s %-15s %-15s %10.5f %11.5f %7.2f %7.2f %8.2f %s %8.2f %s\n",
    $meta_parts[0], $meta_parts[1], $meta_parts[2], $meta_parts[3],
    $meta_parts[4], $meta_parts[5], $meta_parts[6], $meta_parts[7],
    $meta_parts[8], $meta_parts[9], $meta_parts[10],
    $data_parts[0], $data_parts[1], $data_parts[2], $data_parts[3]);

    if ($debug) {print "Exit write_output_soils_line()\n";}
   } #write_output_soils_line()
                                


########################################################
# MAIN Processing
########################################################

#---------------------------------------
# Count number of lines in the INFILE
#---------------------------------------
printf "\ncorrect_CEOP_EOP3_data.pl began on ";print scalar localtime; print "\n";
open (INFILE, "<$ARGV[0]") ||
   die "Can NOT open input_file ($ARGV[0]) for reading";

$data_type = $ARGV[1]; 
print "Processing data file type of $data_type, $ARGV[0], $ARGV[1]\n";

if ($data_type ne "SURFACE" && $data_type ne "FLUX" && 
    $data_type ne "SOILS" && $data_type ne "TOWER")
   { 
   print "\nPROPER USEAGE: \n      correct_CEOP_EOP3_data.pl [input_file] [data_type] \n\n      where [data_type] is one of SURFACE, FLUX, SOILS, or  TOWER.\n\n";

   die "ERROR: Unknown Data Type ($data_type). Type must be one of SURFACE, FLUX, SOILS, or TOWER.\n"; 
   }  


$num_recs_in_file += tr/\n/\n/ while sysread (INFILE, $_, 2 ** 16);
close(INFILE);
print "There are $num_recs_in_file lines in $ARGV[0]\n";

open (INFILE, "<$ARGV[0]") ||
   die "Can NOT open input_file ($ARGV[0]) for reading";

open (OUTFILE_EOLCorrected, ">$ARGV[0].EOLcorrected") ||
   die "Can NOT open $ARGV[0].EOLcorrected for writing.";

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
   if ($debug){print "Call write_output_$data_type_line.";}
   if    ($data_type eq "SURFACE") { write_output_sfc_line; }
   elsif ($data_type eq "FLUX")    { write_output_flux_line; }
   elsif ($data_type eq "SOILS")   { write_output_soils_line; }  
   elsif ($data_type eq "TOWER")   { write_output_tower_line; }  
   else
      {print "ERROR: Unknown Data Type ($data_type)\n";}

   } # while data in file

close(INFILE);
close(OUTFILE_EOLCorrected);

print "Total number lines Input File: $num_recs_in_file\n";
print "Total number lines processed: $number_lines_read\n";
print "Error Corrections Completed: \n";
print "   Negative Zeroes Reset: $neg_zeros_reset\n";
print "   Incorrect Missings  (999.99 M) Reset: $incorrect_missing_reset\n";
print "   Mismatched Missing (-999.99 ?) Reset: $mismatched_missing_reset\n";

printf "correct_CEOP_EOP3_data.pl ended on ";print scalar localtime; printf "\n";
