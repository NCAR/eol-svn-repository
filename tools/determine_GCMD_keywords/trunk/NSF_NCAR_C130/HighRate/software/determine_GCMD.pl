#! /usr/bin/perl -w

#--------------------------------------------------------------------------------
# determine_GCMD.pl
#
# The Perl script runs ncdump -h commands and dumps the netCDF header for each
# input file.  It them determines a unique set of header/data parameters for
# the total set of input files. That unique set of parameters can then be translated
# by science staff to GCMD keywords that can be applied appropriately to the 
# input datasets.  
#
# WARNING: This s/w will only process files with *.nc or *.cdf suffices.  The first
#    seven chars of the input file names should be the CODIAC/Dataset ID where the
#    period has been changed to a dash (e.g., 534-001_RF05.cdf for dataset 534.001
#    whose original name was RF05.cdf). The input file names should be modified before 
#    running this software. 
#
# Execute: 
#    determine_GCMD.pl <input_data_dir> <output_dir> <platform_freq>
#
# Examples: 
#    determine_GCMD.pl ../input_data ../output_data  C130_LRT
#
# Input: 
#    <input_data_dir> - directory with netCDF data files to be processed. Files must
#    have either *.cdf or *.nc for their suffix.
#
#    <platform_freq> - a unique string that identifies the input data by platform and 
#    frequency, as needed. DO NOT include spaces in this string!
#
# Output: 
#    <output_data_dir>/*.hdr - Header info generated from "ncdump -h" from each input file.
#         Header files have the same name as input file name but with *.hdr.
#
# Notes and Assumptions:
#
# 1. The user should search for HARDCODED, ASSUMPTIONS, BEWARE, WARNING, and ERROR in this code. 
#    The user should also search for "exit" in this code so that they know all the possible
#    places/reasons why this code will stop executing.
#
# 2. ASSUMPTION: That the raw data are in expected netCDF format and the input files have either 
#    .nc or .cdf as their suffix.
# 
# 3. ASSUMPTION: Data is in netCDF3 form which implies that the following are the only
#    expected data_types: boolean, byte, char, double, enum1, enum2, enum4, float, int
#    long, opaque, sequence, short, string, structure. This info taken from
#    https://www.unidata.ucar.edu/software/netcdf-java/v4.3/v4.2/javadoc/  . 
#
#    NOTE: For netCDF4 and beyond see data_types at 
#    https://www.unidata.ucar.edu/software/netcdf/docs/data_type.html
#
# 4. ASSUMPTION: That the netCDF format and exact parmeter names are as expected as in
#    this example which is extracted from an EOL RAF aircraft (C130 LRT) data file.
#    Note that the exact "long_name", "units", "Category" values are searched for in
#    the lines (exact caps and all) so the data must be of this form:
#
#    float ADIFR(Time) ;
#            ADIFR:units = "mbar" ;
#                     ADIFR:long_name = "Vertical Differential Pressure, Radome" ;
#                     ADIFR:Category = "Uncorr\'d Raw" ;
#                     ADIFR:missing_value = -32767.f ;
#                     ADIFR:SampledRate = 25 ;
#                     ADIFR:OutputRate = 1 ;
#                     ADIFR:DataQuality = "Good" ;
#                     ADIFR:CalibrationCoefficients = 0.3645f, 5.2766f, -0.0167f ;
#
#    Note that the work "Category" is also found in some long_name header lines.
#    Also, long_name lines can have commas embedded in text. 
#
# 5. One Perl warning is issued but that is currently ignore as it does not appear
#    to impact the output. The warning is 'Variable "%ALL_InputFileDataTypes" will 
#    not stay shared at ./determine_GCMD.pl line 498.' 
#
# Created: L. Cully January 2020
#
# Updates: None.
#
#--------------------------------------------------------------------------------
package determine_GCMD;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
} else {
    use lib "/work/lib/perl/Utilities";
}


my $debug  = 0; # BEWARE: Generates output to screen
my $TotalFilesProcessed = 0;
my $OutputRec = 0;
my $parm;

# netCDF data_types for netCDF 3   - HARDCODED
my %netCDF3_dataTypes = (
   boolean => 1,
   byte => 1,
   char => 1,
   double => 1,
   enum1 => 1,
   enum2 => 1,
   enum4 => 1,
   float => 1,
   int => 1,
   long => 1,
   opaque => 1,
   sequence => 1,
   short => 1,
   string => 1,
   structure => 1,
   );


printf "\ndetermine_GCMD.pl began on ";print scalar localtime;printf "\n\n";
&main();
printf "\ndetermine_GCMD.pl ended on ";print scalar localtime;printf "\n";


#--------------------------------------------------------------
# void main()
# Run the scripts to determine uniq set of netCDF data_types
# in all files so they can be translated by science staff into
# GCMD keywords.
#--------------------------------------------------------------
sub main 
   {
   if ($debug) {print "Enter Main:: length ARGV = $#ARGV,  ARGV() = @ARGV\n";}

   if ($#ARGV < 2)
      { 
      print "Incorrect number of command line arguments!\n ARGV = @ARGV\n";
      print "Usage: determine_GCMD.pl <input_dir> <output_dir> <platform_freq>\n";
      exit(1);
      }

   my $INPUT_netCDF_DIR  = $ARGV[0];
   if ($debug) {print "INPUT_netCDF_DIR = $INPUT_netCDF_DIR \n";}

   my $OUTPUT_DIR  = $ARGV[1];
   if ($debug) {print "OUTPUT_DIR = $OUTPUT_DIR \n";}

   my $platform_freq = $ARGV[2];
   if ($debug) {print "platform_freq = $platform_freq \n";}

   #--------------------------------------
   # Read in the list of files to process.
   #--------------------------------------
   printf "Opening INPUT_netCDF_DIR:: $INPUT_netCDF_DIR\n";
   opendir(my $INPUT_DIR, $INPUT_netCDF_DIR) or die("Cannot open INPUT_netCDF_DIR\n");

   # WAS:   my @files = grep(/\.nc$/,readdir($INPUT_DIR)); 
   my @files = grep(/\.nc$/ || /\.cdf$/,readdir($INPUT_DIR));      # Process *.nc AND *.cdf 

   closedir($INPUT_DIR);

   ##my $output_uniq_data_types_ALL = sprintf("%s/%s", $OUTPUT_DIR, "All_uniq_data_types_parm.txt"); # HARDCODED
   my $output_uniq_data_types_ALL = sprintf("%s/%s%s%s", $OUTPUT_DIR, "All_uniq_data_types_",$platform_freq, "_parm.txt"); # HARDCODED
   open(OUTFILE_UNIQPARMS_ALL,">", $output_uniq_data_types_ALL) or die("Can't open file for writing: ".$output_uniq_data_types_ALL);


   #----------------------------------------------
   # Set up hashes to contain uniq set of data_types
   # for specifc input files and summary for 
   # all input files.
   #----------------------------------------------
   my %ALL_InputFileDataTypes = ();
   my %InputFileDataTypes = ();

   #-----------------------------------------------------------------
   # Process every netCDF file (*.nc or *.cdf) in the input directory.
   # Determine unique set of data_types for each file and for all
   # input files combined.
   #-----------------------------------------------------------------
   foreach my $file (sort(@files)) 
      {
      #----------------------------------------------------------------
      # Form and execute system command to dump netCDF file header
      # that includes list of parameters in the data.
      # That is, run "ncdump -h" executable on input files.
      #----------------------------------------------------------------
      $TotalFilesProcessed++;

      if ($debug) {print "-----------------------------\n";}
      printf "\nprocessing netCDF file: $file \n";

      #----------------------------------------------------------------
      # Create file name to dump header into [input_file_name].hdf
      #----------------------------------------------------------------
      my $netCDF_hdr_file = sprintf("%s/%s.hdr", $OUTPUT_DIR, $file);    # HARDCODE
      if ($debug) {print "Output HEADER  netCDF_hdr_file = $netCDF_hdr_file\n";}

      #----------------------------------------------------------------
      # Form the netCDF header dump command. Dump Header! HARDCODED
      #----------------------------------------------------------------
      my $cmdhdrdump = sprintf("/bin/ncdump -h %s/%s > %s", $INPUT_netCDF_DIR, $file, $netCDF_hdr_file );
      print "Execute system cmd:: $cmdhdrdump\n";
      system ($cmdhdrdump); # Dump Header from input netCDF file


      #----------------------------------------------------------------
      # Create and open file to write unique list of data_types into 
      # for specific input file [input_file_name].dataTypes
      #----------------------------------------------------------------
      my $output_uniq_data_types = sprintf("%s/%s.dataTypes", $OUTPUT_DIR, $file);  # HARDCODED
      print "Uniq data Types File Name = $output_uniq_data_types \n";
      open(OUTFILE_UNIQPARMS_FILE,">", $output_uniq_data_types) or die("Can't open file for writing: ".$output_uniq_data_types);

      if ($debug) {print "Specific output file for uniq data_types  = $output_uniq_data_types\n";}

      #----------------------------------------------
      # Set up hash for this file's unique data_types.
      #----------------------------------------------
      %{InputFileDataTypes} = (); # Clear hash for each input file

      # Open the Header Dump file for processing
      print "netCDF_hdr_file name = $netCDF_hdr_file\n";
      my $FILE_HDR_DUMP;
      open($FILE_HDR_DUMP,"<", $netCDF_hdr_file) or die("Can't open file for READING: ".$netCDF_hdr_file);

      #-----------------------------------------------------------
      # Process the data_type lines in the header dump and add
      # counts to hashes then write to output files for science
      # staff to review and translation to GCMD keywords. 
      #-----------------------------------------------------------
      my @lines = <$FILE_HDR_DUMP>;
      my $number_lines_in_file = $#lines+1;
      my $TotalRecProc = 0;

      my $long_name = "long_name = unknown";
      my $units = "units = unknown";
      my $category = "Category = unknown";
      my @fields;
      my @ALL_fields = ("long_name = unknown", "units = unknown", "Category = unknown",0); # long_name, units, category, totalNumFilesWithParm

      # --------------------------------------------
      # Loop through file lines in header dump file.
      # --------------------------------------------
      foreach my $line (@lines)
         {
         $TotalRecProc++;

         chomp ($line); #remove return \n
         if ($debug) {print "\nOrig Line: xxx $line xxx\n";}

         # -------------------------------------------------
         # If line is blank skip it else check for data_type
         # and count data_type if found.
         # -------------------------------------------------
         $line =~ s/^\s+|\s+$//g; #Strip white space from both ends of string

         if ($line =~ /^\s*$/)
            {
            if ($debug) {print "SKIP Blank Line: $line\n";}
            next;
            }
         else
            {
            # ---------------------------------------------------------
            # Pick off/determine if type line &/or what data_type is
            # if any of known data types then found and count
            # If ("float" - case insensitive is in line) then count 
            # in total hash and add in hashes
            # ---------------------------------------------------------
            my @record = split (/ /, $line); # split on space
            my $input_key = trim ($record[0]); # data_type found as first line element

            if ($debug) {print "   input_key = xxx $input_key xxx\n";}

            # ---------------------------------------------------------
            # If first line element matches a known netCDF3 data_type
            # then count in hashes.
            #
            # Also find next long_name to save off.
            # ---------------------------------------------------------
            if ( exists($netCDF3_dataTypes{$input_key}) )
               {
               if ($debug) {print "   *** FOUND VALID data_type = $input_key . Increment count in both hashes for this data_type.\n";}

               my $parm1 = trim ($record[1]); # parameter name found as second line element
               $parm = trim (split (/\(/, $parm1)); # pick off just the parm name

               if ($debug) {print "   *** parm1 = xxx $parm1 xxx  parm = xxx $parm xxx\n";}

               $long_name = "long_name = unknown,";
               $units = "units = unknown,";
               $category = "Category = unknown,";

               #------------------------------------------------
               # In an individual file, each parm should/must be
               # unique so there won't be any dups parmeters.
               # Form hash of arrays where for single input file 
               # there are three fields: 
               #    $fields[0] = $long_name; 
               #    $fields[1] = $units;
               #    $fields[2] = $category;
               #------------------------------------------------
               if (exists ($InputFileDataTypes{$parm}))
                  {
                  print "   WARNING: Duplicate Parm names ($parm) in input file ($file) already in hash.\n";
                  }
               else
                  {
                  if ($debug) {print "   NEW Parm ($parm) to add to input file hash. Set fields to unknown\n";}
                  $fields[0] = "long_name = unknown,";  # long_name
                  $fields[1] = "units = unknown,";  # units
                  $fields[2] = "Category = unknown,";  # category 

                  $InputFileDataTypes{$parm} = [@fields]; 
                  }


               #---------------------------------------------
               # Set parm values for Total/All files
               # Has 4 fields: 
               #   $ALL_fields[0] = $long_name; 
               #   $ALL_fields[1] = $units; 
               #   $ALL_fields[2] = $category;
               #   $ALL_fields[3] = $total_FilesHaveThisParm;
               #---------------------------------------------
               if ( exists ($ALL_InputFileDataTypes{$parm}) )
                  {
                  if ($debug) {print "   This Parm ($parm) exists in TOTAL/ALL files hash. Increment the count of file that have this parmeter.\n";}

                  if ($debug) {print "   BEFORE: Total Files Having $parm = $ALL_InputFileDataTypes{$parm}[3]\n";}

                  $ALL_InputFileDataTypes{$parm}[3]++;

                  if ($debug) {print "   AFTER: Total Files Having $parm = $ALL_InputFileDataTypes{$parm}[3]\n";}

                  }
               else
                  {
                  if ($debug) {print "   NEW Parm ($parm) to add to TOTAL/ALL files hash. Set fields to unknown\n";}
                  $ALL_fields[0] = "long_name = unknown,";  # long_name
                  $ALL_fields[1] = "units = unknown,";  # units
                  $ALL_fields[2] = "Category = unknown,";  # category
                  $ALL_fields[3] = 1;  # Total number input files with this parm. Count the first one!

                  $ALL_InputFileDataTypes{$parm} = [@ALL_fields];
                  }

               }
            else  # Unknown data_type (i.e.,  NOT any of known data_types: int, float, double, etc.)
               {
               if ($debug) {print "   NOT a valid data_type = $input_key . Skip if not long_name, units, category.\n";}

               my @line_parts = split(/:/, $line);

               # --------------------------------------------------------------------------------------
               # Assumes long_name, units, category records will follow data_type record for that parm.
               # --------------------------------------------------------------------------------------
               
               # ---------------------------------------------
               # Set Long Name if in line. Set in both hashes.
               # Note that "long_name" was not found other than
               # in the long_name header lines. 
               # ---------------------------------------------
               if (index($line, "long_name") != -1)
                 {
                 $long_name = $line_parts[1] . ",";
                 if ($debug) {print "   *** LINE: $line CONTAINS phrase xxx $long_name xxx. Set ($parm) long_name.\n";}

                 $InputFileDataTypes{$parm}[0] = $long_name; 
                 $ALL_InputFileDataTypes{$parm}[0] = $long_name; 

                 $long_name = "long_name = unknown,";
                 } # Set long_name

               # -----------------------------------------
               # Set Units if in line. Set in both hashes.
               # Note that "units" was not found other than
               # in the unit hdr lines.
               # -----------------------------------------
               if (index($line, "units") != -1)
                 {
                 $units = $line_parts[1] . ",";
                 if ($debug) {print "   *** LINE: $line CONTAINS phrase xxx $units xxx. Set ($parm) units.\n";}

                 $InputFileDataTypes{$parm}[1] = $units; 
                 $ALL_InputFileDataTypes{$parm}[1] = $units; 

                 $units = "units = unknown,"; 
                 } # Set units

               # --------------------------------------------
               # Set Category if in line. Set in both hashes.
               #
               # WARNING: "Category" was found in both the
               # Category and "long_name" lines so must search
               # for Category right after colon. 
               # --------------------------------------------
               if (index($line, ":Category") != -1)
                 {
                 $category = $line_parts[1] . ",";
                 if ($debug) {print "   *** LINE: $line CONTAINS phrase xxx $category xxx. Set ($parm) category.\n";}

                 $InputFileDataTypes{$parm}[2] = $category;
                 $ALL_InputFileDataTypes{$parm}[2] = $category;

                 $category = "Category = unknown,";
                 } # Set units

               } #unknown data_type or not a data_type record

            } # Add data_type &/or count to hashes

         } # end foreach all lines in the file

      if ($debug)
         {
         print "\n*****Total Lines Processed from Input File ($file): $TotalRecProc *****\n";
         print "\n*****Hash output individual Input File Data_type Hash for file= $file*****\n";
         print "\nInputFileDataTypes Hash Contents::\n   \n";
         for $parm ( sort keys %InputFileDataTypes ) { print "$parm: @{ $InputFileDataTypes{$parm} }\n"; }
         }

      print OUTFILE_UNIQPARMS_FILE  "-------Parms found in File: $file\n";
      for $parm ( sort keys %InputFileDataTypes ) { print OUTFILE_UNIQPARMS_FILE "$parm, @{ $InputFileDataTypes{$parm} }\n" }

      close (OUTFILE_UNIQPARMS_FILE);


      } # for each input file

   # ---------------------------------------------------
   # ---------------------------------------------------
   # All input files processed. Now print sorted output
   # to files. Sort by parm (same as by hash keys), by
   # category, and by count where count is the number of
   # files that contain that parameter.  Write output
   # to separate files into output directory. Output
   # file names will have the form: 
   #    All_uniq_data_types_xxxx.txt where xxxx is
   #    either parm, category, or count. 
   # ---------------------------------------------------
   # ----------------------------------------------------------
   # First print to output file total uniq list of data types
   # sorted by parm that were found in all input files combined. 
   #    All_uniq_data_types_parm.txt
   # -----------------------------------------------------------
   if ($debug)
     {
     # Print to screen/log
      print "-----------------------------------------------------\n";
      print "-------   Total Number of Files Processed: $TotalFilesProcessed -------\n";
      print "-----------------------------------------------------\n";
      print " Parm Short Name: Parm Long Name; Number Files have Parm\n";
      print "---------------------------------------------------------------------\n";
      for $parm ( sort keys %ALL_InputFileDataTypes ) { print "$parm: @{ $ALL_InputFileDataTypes{$parm} }\n"; } 
      } # Debug

   # --------------------------------------------------------------
   # Print (debug really) by "unnamed" array element of hash table
   # to test access.
   # --------------------------------------------------------------
   if ($debug)
      {
      foreach my $hash_key (keys %ALL_InputFileDataTypes)
         {
         print "Elements on Hash key::  $hash_key\n ";
         for my $i (0..$#{ $ALL_InputFileDataTypes{$hash_key} } )
            { print " Array[$i] xxx $ALL_InputFileDataTypes{ $hash_key }[$i] xxx "; }
         print "\n\n";
         }
      } # Debug


   # -------------------------------------------------
   # -------------------------------------------------
   # Print to output file All_uniq_data_types_parm.txt
   # SORTED BY PARAMETER (PARM). Write to output file:
   #
   #    All_uniq_data_types_parm.txt
   # -------------------------------------------------
   # -------------------------------------------------
#   print OUTFILE_UNIQPARMS_ALL  "----------------------------------------------------------------------\n";
#   print OUTFILE_UNIQPARMS_ALL  "------   Total Number of Input Files Processed: $TotalFilesProcessed ------\n";
#   print OUTFILE_UNIQPARMS_ALL  "----------------------------------------------------------------------\n";
#   print OUTFILE_UNIQPARMS_ALL  " SHORT Name, LONG Name, UNITS, CATEGORY,  NUMBER INPUT FILES with Parm\n";
#   print OUTFILE_UNIQPARMS_ALL  "----------------------------------------------------------------------\n";

   for $parm ( sort keys %ALL_InputFileDataTypes ) { print OUTFILE_UNIQPARMS_ALL "$parm, @{ $ALL_InputFileDataTypes{$parm} } , xxx,\n"; }


   # -----------------------------------------------------------
   # -----------------------------------------------------------
   # Next print to output file total uniq list of data types
   # SORTED BY COUNT that were found in all input files combined. 
   # Write to output file:
   #
   #    All_uniq_data_types_count.txt
   # -----------------------------------------------------------
   # -----------------------------------------------------------
   my $output_byCount = sprintf("%s/%s%s%s", $OUTPUT_DIR, "All_uniq_data_types_",$platform_freq,"_count.txt"); # HARDCODED
   open(OUTFILE_UNIQPARMS_COUNT,">", $output_byCount) or die("Can't open file for writing: ".$output_byCount);
 
#   print OUTFILE_UNIQPARMS_COUNT  "----------------------------------------------------------------------------------\n";
#   print OUTFILE_UNIQPARMS_COUNT  "-------------                Sorted by COUNT                  ----------------------\n";
#   print OUTFILE_UNIQPARMS_COUNT  "----------------------------------------------------------------------------------\n";
#   print OUTFILE_UNIQPARMS_COUNT  " NumberFilesWithParm      PARM      LONG NAME     UNITS     CATEGORY \n";
#   print OUTFILE_UNIQPARMS_COUNT  "----------------------------------------------------------------------------------\n";

   #--------------------------------------------------------
   # Sort hash records (i.e., parms) by COUNT which is the
   # total number files that a parm was found in. 
   #--------------------------------------------------------
   my @countSorted = sort countsort (keys %ALL_InputFileDataTypes);
   sub countsort {
     $ALL_InputFileDataTypes{$a}[3] <=> $ALL_InputFileDataTypes{$b}[3] # This does a numeric sort!
     } #end countsort

   for my $ii (0..$#countSorted)
      {
      if ($debug)
         { print  "countSorted[$ii], FilesWithParm = $ALL_InputFileDataTypes{$countSorted[$ii]}[3], Parm = $countSorted[$ii], $ALL_InputFileDataTypes{$countSorted[$ii]}[0] $ALL_InputFileDataTypes{$countSorted[$ii]}[1] $ALL_InputFileDataTypes{$countSorted[$ii]}[2]\n"; } # Debug

      print OUTFILE_UNIQPARMS_COUNT  "$ALL_InputFileDataTypes{$countSorted[$ii]}[3], $countSorted[$ii], $ALL_InputFileDataTypes{$countSorted[$ii]}[0] $ALL_InputFileDataTypes{$countSorted[$ii]}[1] $ALL_InputFileDataTypes{$countSorted[$ii]}[2] , xxx,\n";
      } # End Print Sorted by Count

   #--------------------------------------------------------
   #--------------------------------------------------------
   # Next print to output file total uniq list of data types
   # SORTED BY CATEGORY. This requires an alphabetical search
   # since the category is text. Simple "sort" on external
   # file works fine. Write to output file:
   #
   #     All_uniq_data_types_category.txt
   #--------------------------------------------------------
   #--------------------------------------------------------
   my $output_byCat = sprintf("%s/%s%s%s", $OUTPUT_DIR, "All_uniq_data_types_",$platform_freq,"_category.txt"); # HARDCODED
   open(OUTFILE_UNIQPARMS_CATEGORY,">", $output_byCat) or die("Can't open file for writing: ".$output_byCat);

###   my @countSorted = sort countsort (keys %ALL_InputFileDataTypes);  # See countedSorted above.

   for my $ii (0..$#countSorted)
      { print OUTFILE_UNIQPARMS_CATEGORY "$ALL_InputFileDataTypes{$countSorted[$ii]}[2] $countSorted[$ii], $ALL_InputFileDataTypes{$countSorted[$ii]}[0]  $ALL_InputFileDataTypes{$countSorted[$ii]}[1] $ALL_InputFileDataTypes{$countSorted[$ii]}[3] , xxx,\n"; }

   my $cmd_catSort = sprintf("sort $output_byCat > $output_byCat.sorted");

   print "\nExecute system cmd:: $cmd_catSort\n";
   system ($cmd_catSort); # Sort by Category 


   # Move the sorted file into the sorted by category file
   my $cmd_overwrite = sprintf("mv -f $output_byCat.sorted $output_byCat");

   print "\nExecute system cmd:: $cmd_overwrite\n";
   system ($cmd_overwrite); # Sort by Category


   # Write some general stats to screen
   print "\n-------   Total Number of Files Processed: $TotalFilesProcessed -------\n";

   my $size = keys %ALL_InputFileDataTypes;
   print "Size of Hash (equals number uniq parms found):  %ALL_InputFileDataTypes = $size\n";


   close (OUTFILE_UNIQPARMS_ALL); # sorted by parm
   close (OUTFILE_UNIQPARMS_COUNT); # sorted by count
   close (OUTFILE_UNIQPARMS_CATEGORY); # sorted by category
   } # main


##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all leading and trailing whitespace from the specified String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##------------------------------------------------------------------------------
sub trim {
    my ($line) = @_;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
} # trim()
