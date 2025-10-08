#!/usr/bin/perl 

#-------------------------------------------------------------------
# check_QCF_sfc.pl - This perl program verifies that the input file
#   is in the QCF (Quality Control Format) which is the standard
#   format for all UCAR/JOSS surface composite datasets. 
#   This s/w checks the metadata separate from the data.
#
# SHOULD THE freq and year BE AN INPUT?
#
#   Execute: check_QCF_sfc.pl "input_file.qcf" 0
#
#   Required Inputs: Two input parameters are required as shown below:
#
#      check_QCF_sfc.pl input_file_name info_level
#
#   where [input_file_name] is the name of the file to be checked
#   and [info_level] is either 0, 1, or 2. Level 0 produces minimal
#   information to be printed to screen. Level 1 produces more detailed
#   information to be printed to screen, and Level 2 produces the
#   maximum level of information and debug to be printed to screen.
#   The default info_level is 0.
#
#   E.g., check_QCF_sfc.pl BAMEX_20030625.qcf 2
#
#   This s/w also requires a "final_QCF_sort" file (with that exact name)
#   to be located in the directory in which it it executed. The final_QCF_sort
#   file is defined as:
#
#   # cosort specifications by LEC (Linda Cully) !!!!!!!!!!!!!!!1
#
#   Output:
#     This s/w prints Informational Messages including error messages
#     to the screen.  There are 3 types of ERROR messages issued by
#     this s/w (i.e., ERROR FILE, ERROR META, and ERROR DATA). These
#     are file, metadata, or data type error messages. WARNING messages
#     are also issued. If the information level is set to 2, then debug
#     type message are also issued.
#
#     This s/w creates various Intermediate Work Files that are not
#     all deleted if the info_level = 2.  Those intermediate files
#     are input.sort, input_sort.uniq, in1-70chars.sort, in1-70chars.uniq,
#     [input_file].diff, [input_file].sort, EXACT_DUP_records_[input_file], 
#     and INEXACT_DUP_records_[input_file]. Some intermediate files are
#     always deleted as soon as they are checked.
#
#     This s/w maintains and prints to the screen a record of the
#     maximum and minimum values found in the input file for each
#     parameter type and for all records/stns within the input file.
#
#     A [inputfilename]_stationCD.out file containing limited information on 
#     every station found in the input file is also created. 
#
#   This s/w performs the following checks and makes the 
#      following assumptions:
#
#   That the user will search for all instances of the word HARDCODED
#      and update these values accordingly.
#
#   That every line is the proper length including the return char.
#   That the file name contains the day data collected in YYYYMMDD form.
#   That the file has the correct suffix (0qc or qcf).
#   That the file is sorted correctly.
#   That the file does not contain exact duplicate records.
#   That the file does not contain in-exact duplicate records (i.e.,
#     dates/times/names/lat/lons match but possibly not data.)
#
#   This s/w assumes an exact beginning position and length for each
#     piece of metadata and each data parameter on each record.
#   That there are slashes in the date values.
#   That there are colons in the time values.
#   That there are decimal points (in the lat, lon, elev, and all data values)
#     in expected locations. This should check field justification.
#   That there is at least one space separating each element of the record.
#   Each metadata value and data parameter must be within specified
#     max/min values.
#   That the year values are either the $PROJECT_YEAR or $PROJECT_YEAR_NEXT.
#   That the Nominal and Actual Dates match the File Name Dates.
#   xxxThat the Nominal and Actual dates match on each record. - NOT IF SOLAR
#   xxxThat the Nominal minute values are either 00 or 30. - FREQ DEPENDENT
#   xxxxThat the Actual minute values are either 00 or 30. - FREQ DEPENDENT
#   xxxThat if the Actual times are >15 and <=45, the Nominal time is 30, else
#     the Nominal time is 00. There's some potential conflict in time checks.
#   xxxThat the Nominal Date/Times exactly match the Actual Date/Times 
#     for each record.
#   That the lat/lon/elev are constant for each station in the file.
#   This s/w keeps track (and prints) station information for every stn found
#      in the file. Helps verify if there are duplicate or junk stns.
#   That there are no leading spaces in the Network or StnID names. This
#      checks left justification of the name fields.
#   That there are no control chars in any record.
#   That flag values are only one of the following: U,G,D,B,C,M,N,X,E,I,T. T for pcp only.
#   That data values are not -0.00.
#   That if the data value is missing the data flag is either M,N,I,C only.
#   That if the data value is NOT missing the data flag is U, B, D, G, or T. T for pcp only.
#   This s/w issues a "Drop Record" warning if all data values on a
#      record are missing. 
#   This s/w tracks missing elev, lat, lon and issues warning messages.
#   xxxVerify that the Absolute value of the wind components are less than the wind speed. - WOULD HAVE TO COMPUTE WIND COMPONENTS
#
# -------------------------------------
# Consider adding the following checks:
# -------------------------------------
#  That there are no embedded spaces in the Network or StnID.
#  Keep track of Extremes for each parameter and for each station.
#
#
# WARNING: See HARDCODED sections.
# WARNING: This code is currently set up to run on one file at a time.
#     Need to create different interm file names to handle many files.
#
# Execute: check_QCF_sfc.pl solar_20030625.qcf 0
#--------------------------------------------------------------------
#
# Sep 2004 lec
#   Created Version 1.0.
#------------------------------------------------------------------------
# HARDCODED
#
# xxxBAMEX runs 19 May 2003 thru 6 July 2003
#-------------------------------------------------------
$PROJECT_YEAR = 2003;                   # HARDCODED
$PROJECT_YEAR_NEXT = $PROJECT_YEAR + 1; # HARDCODED

$RECORD_LENGTH = 260; # HARDCODED - Includes return for the QCF Surface format
$FILE_SUFFIX1 = "qcf"; # HARDCODED - Set for Quality Control Format (QCF) Surface.
$FILE_SUFFIX2 = "0qc"; # HARDCODED - Set for Quality Control Format (QCF) Surface.

$number_lines_in_file = 0;
$number_lines_processed = 0;

#------------------------
# Initialize Error Counts
#------------------------
$err_elev_missing = 0; $err_lat_missing = 0; $err_lon_missing = 0;

$total_completely_missing=0;

$err_file = 0; $err_meta = 0; $err_data = 0; $total_err = 0;
$total_warn = 0;

%stnInfo = (); # Retain uniq list of network/stns

##################################################
#----------------------------------------------
# write_stationCD_file() - subroutine
#   that creates CEOP_[inputfilename]_stationCD.out file and
#   writes a record for each stn found in data.
#
#DOES THIS MAKE sense because will run on seperate day files?
#----------------------------------------------
sub write_stationCD_file{

   #--------------------------
   # Print out stationCD file.
   #--------------------------
   open (OUTPUT_FILE, ">$ARGV[0]_stationCD.out") ||
       die "Can NOT open $ARGV[0]_stationCD.out file for writing";

   print "\n----------------------------\n";
   print "\nSee CEOP_$ARGV[0]_stationCD.out for info on following stations found in $ARGV[0]:\n";

   foreach $ID (sort keys %stnInfo)
      {
      print "   network:stnID= $ID   (Last Lat/Lon/Elev Found: $stnInfo{$ID}{LAT} $stnInfo{$ID}{LON} $stnInfo{$ID}{ELEV})\n";

      if ( $stnInfo{$ID}{ELEV} < -999.0)
         { $stnInfo{$ID}{ELEV} = "-9999.9" } # standard for printing missing elev, only.

      printf OUTPUT_FILE
        "%-15s %4d %10.5f %11.5f %3d %5d %-46s %-3s %-8s %-8s %-2s %-2s %-3s %6.2f %-1s %4d %-15s %9.1f %1s\n",
         $stnInfo{$ID}{STNID}, 999, $stnInfo{$ID}{LAT}, $stnInfo{$ID}{LON},
         0, 999, $ID, "(N)",
         $stnInfo{$ID}{BEGIN_DATE}, $stnInfo{$ID}{END_DATE}, "??", "??",
         "???", 999.99, "?", 999,
         "unknown", $stnInfo{$ID}{ELEV}, "f";
      }
   
   close (OUTPUT_FILE); 

   } # write_stationCD_file()


##################################################
#----------------------------------------------
# write_final_stats() - subroutine
#   that outputs final stats about input file.
#----------------------------------------------
sub write_final_stats{
                                    
   #--------------------------------------------------
   # Print Minimum and Maximum Extremes for each parm
   # for all records and stns found in the input file.
   # Print number of errors found for each parameter.
   #--------------------------------------------------
   print "\n----------------------------\n";
   print "Extreme Values per Parameter/All Stns/All Records in $ARGV[0]. (Number of Err per Parm) (MIN,MAX Limits)\n\n";

   foreach $parm (sort keys %parm_positions)
      {  
      if ($err_data != 0) # Don't divide by zero
         {
         printf "Parm: %-20s  Minimum_Extreme: %8.2f Maximum_Extreme: %8.2f Error_count: %7d (%5.1f%) (%7.2f, %7.2f)\n",
            $parm, $parm_positions{$parm}{MIN_EXT}, $parm_positions{$parm}{MAX_EXT},
            $parm_positions{$parm}{ERR_CT}, ($parm_positions{$parm}{ERR_CT}/$err_data)*100,
            $parm_positions{$parm}{MIN_VAL}, $parm_positions{$parm}{MAX_VAL};
         }
      else
         {
         printf "Parm: %-20s  Minimum_Extreme: %8.2f Maximum_Extreme: %8.2f Error_count: %7d (%5.1f%) (%7.2f, %7.2f)\n",
            $parm, $parm_positions{$parm}{MIN_EXT}, $parm_positions{$parm}{MAX_EXT},
            $parm_positions{$parm}{ERR_CT}, 0.0, $parm_positions{$parm}{MIN_VAL}, $parm_positions{$parm}{MAX_VAL};
         }                                         
      } # foreach parm


   #-----------------------------------------------
   # Print final counts of each flag for each parm.
   #-----------------------------------------------
   print "\n----------------------------\n";
   print "Counts per Flag Type Found per Parameter/All Stns/All Records in $ARGV[0]:\n\n";

   foreach $parm (sort keys %flag_count)
     { 
     printf "Parm: %-20s  M: %7d (%5.1f%) N: %7d (%5.1f%) C: %7d (%5.1f%) I: %7d (%5.1f%) X: %7d (%5.1f%) E: %7d (%5.1f%)  B: %7d (%5.1f%) D: %7d (%5.1f%) G: %7d (%5.1f%) U: %7d (%5.1f%) Unknown: %7d (%5.1f%)\n",
     $parm, 
     $flag_count{$parm}{M}, ($flag_count{$parm}{M}/$number_lines_processed)*100,
     $flag_count{$parm}{N}, ($flag_count{$parm}{N}/$number_lines_processed)*100,
     $flag_count{$parm}{C}, ($flag_count{$parm}{C}/$number_lines_processed)*100,
     $flag_count{$parm}{I}, ($flag_count{$parm}{I}/$number_lines_processed)*100,
     $flag_count{$parm}{X}, ($flag_count{$parm}{X}/$number_lines_processed)*100,
     $flag_count{$parm}{E}, ($flag_count{$parm}{E}/$number_lines_processed)*100,
     $flag_count{$parm}{B}, ($flag_count{$parm}{B}/$number_lines_processed)*100,
     $flag_count{$parm}{D}, ($flag_count{$parm}{D}/$number_lines_processed)*100,
     $flag_count{$parm}{G}, ($flag_count{$parm}{G}/$number_lines_processed)*100,
     $flag_count{$parm}{U}, ($flag_count{$parm}{U}/$number_lines_processed)*100,
     $flag_count{$parm}{UNKNOWN}, ($flag_count{$parm}{UNKNOWN}/$number_lines_processed)*100
     }

#  Version 2

   print "\n----------------------------\n";

   printf "Total Count Missing or Negative Elevations: $err_elev_missing\n";
   printf "Total Count Missing Latitudes:  $err_lat_missing\n";
   printf "Total Count Missing Longitudes: $err_lon_missing\n\n";

   printf "Total Count Completely Missing Records: $total_completely_missing\n";

   #------------------
   # Issue File Scores
   #------------------
   $total_meta = $number_lines_processed*9; # 9 meta/line FIX THIS - confirm
   $total_data = $number_lines_processed*20; #20  parms/line FIX THIS - confirm
   $total_err = $err_file + $err_meta + $err_data;   

   print "----------------------------\n\n";
   print "WARNING and ERROR Scores (0=perfect) for $ARGV[0].\n";

   print "   Total Warnings: $total_warn\n";
   print "   Total Errors: $total_err\n";

   print "   Error Scores by Count (Percent of Total Possible Errors for that Type)\n";

   if ( ($total_meta != 0) && ($total_data != 0) )
      {
      printf "      File: %10d  Metadata: %10d (%5.1f%) Data: %10d (%5.1f%)\n",
          $err_file, $err_meta, ($err_meta/$total_meta)*100, $err_data,
          ($err_data/$total_data)*100;
      }
   else
      {
      printf "      File: %10d  Metadata: %10d (%5.1f%) Data: %10d (%5.1f%)\n",
          $err_file, $err_meta, 0.0, $err_data, 0.0;
      }                                        

   } # write_final_stats()


##################################################
#----------------------------------------------
# check_general_file_info() - subroutine
#   that checks and prints general system info 
#   about the input file.
#----------------------------------------------
sub check_general_file_info {
   my ($input_file_name) = @_;                

   if ($debug1) {print "\nEnter check_general_file_info\n";}

   #------------------------------
   # Open input file. Count lines.
   #------------------------------
   open (INPUT_FILE, "<$input_file_name") ||
     die "Can NOT open first arg $input_file_name input file for reading";

   $line = `ls -al $input_file_name`;
   print "Processing File: $line\n";  

   @file_info = stat INPUT_FILE;

   $number_lines_in_file += tr/\n/\n/ while sysread(INPUT_FILE, $_, 2 ** 16);

   if ($debug1) {print "     Original Full file_info: @file_info\n";}
   if ($debug) 
      { print "   Original File size = $file_info[7], blksize = $file_info[11], blocks = $file_info[12], Number_of_recs = $number_lines_in_file\n";}
 
   $rec_size = $file_info[7]/$number_lines_in_file;

   if ($rec_size == $RECORD_LENGTH)
      {print "   Correct Record Length of $rec_size chars (inclds return) computed for each line in file.\n";}
   else
      {print "   ERROR FILE: Improper Record Length of $rec_size computed for $input_file_name. Should be $RECORD_LENGTH chars (inclds return)\n"; $err_file++;}

   close(INPUT_FILE);

   #-----------------------------------------
   # Check input file for correct sorting and
   # exact and inexact duplicate records.
   #-----------------------------------------
   system "cp $input_file_name input.dat";
   system "cosort final_QCF_sort";
   system "/usr/bin/diff input.dat input.sort > diffsort.tmp";

   system "mv input.sort $input_file_name.sort";
   system "mv diffsort.tmp $input_file_name.diff";


   #------------------------------------------------------
   # If file "diffsort.tmp" has any lines there are diffs
   # and this file was not sorted in the proper manner.
   #------------------------------------------------------
   open (DIFF_FILE, "<$input_file_name.diff") ||
     die "Can NOT open first arg $input_file_name.diff input file for reading";

   $number_lines_in_DIFF_file += tr/\n/\n/ while sysread(DIFF_FILE, $_, 2 ** 16);

   if ($debug)
      { 
      @file_info = stat DIFF_FILE;
      print "   Diff_file File size = $file_info[7], blksize = $file_info[11], blocks = $file_info[12]\n";
      }       

   close (DIFF_FILE);

   if ($number_lines_in_DIFF_file != 0) # Error raw input file not sorted!
      {
      print "   ERROR FILE: Raw Input File $input_file_name not sorted properly. See $input_file_name.sort and $input_file_name.diff\n";
      $err_file++;
      }  
   else
      { 
      print "   Correct File Sorting in $input_file_name.\n";
      system "/bin/rm $input_file_name.diff";
      }

   #----------------------
   # Check for Exact Dups!
   #----------------------
   system "uniq $input_file_name.sort > input.sort.uniq";
   system "diff $input_file_name.sort input.sort.uniq > EXACT_DUP_records_$input_file_name.out"; 

   open (DUP_FILE, "< EXACT_DUP_records_$input_file_name.out") ||
     die "Can NOT open first arg EXACT_DUP_records_$input_file_name.out input file for reading";

   if ($debug)
      {
      @file_info = stat DUP_FILE;
      print "   EXACT_Dup_file File size = $file_info[7], blksize = $file_info[11], blocks = $file_info[12]\n";
      }       

   $number_lines_in_DUP_file += tr/\n/\n/ while sysread(DUP_FILE, $_, 2 ** 16)
;
   close (DUP_FILE);          

   if ($number_lines_in_DUP_file != 0) 
      {print "   ERROR FILE: There are ($number_lines_in_DUP_file) EXACT DUPLICATE records in raw Input File $input_file_name. See file EXACT_DUP_records_$input_file_name.out file.\n"; $err_file++;}  
   else
      {
      print "   Correct No Exact Duplicate Records found in $input_file_name.\n";
      system "/bin/rm EXACT_DUP_records_$input_file_name.out";
      }     


   #-----------------------
   # Check for inexact Dups
   #-----------------------
   system "cut -c1-95 input.sort.uniq > in1-95chars.sort";
   system "uniq in1-95chars.sort > in1-95chars.uniq";
   system "diff in1-95chars.sort in1-95chars.uniq> INEXACT_DUP_records_$input_file_name.out"; 

   open (INEXDUP_FILE, "< INEXACT_DUP_records_$input_file_name.out") ||
     die "Can NOT open first arg INEXACT_DUP_records_$input_file_name.out input file for reading";

   if ($debug)
      {
      @file_info = stat INEXDUP_FILE;
      print "   INEXACT_Dup_file File size = $file_info[7], blksize = $file_info[11], blocks = $file_info[12]\n";
      }      

   $number_lines_in_INEXDUP_file += tr/\n/\n/ while sysread(INEXDUP_FILE, $_, 2 ** 16)
;
   close (INEXDUP_FILE);

   if ($number_lines_in_INEXDUP_file != 0)
      {print "   ERROR FILE: There are ($number_lines_in_INEXDUP_file) IN-EXACT DUPLICATE records in raw Input File $input_file_name. See file INEXACT_DUP_records_$input_file_name.out file.\n"; $err_file++;}     
   else
      {
      print "   Correct No IN-EXACT Duplicate Records found in $input_file_name.\n";
      system "/bin/rm INEXACT_DUP_records_$input_file_name.out";
      } 


   #-----------------------------------------------
   # Clean up. WARNING - This deletes interm files.
   #-----------------------------------------------
   system "/bin/rm input.dat input.sort.uniq in1-95chars.sort in1-95chars.uniq";

   if ($number_lines_in_DIFF_file == 0) # input file correctly sorted
      {system "/bin/rm $input_file_name.sort";}

   if ($debug1) {print "\nExit check_general_file_info\n\n";}

   } # check_general_file_info {}


####################################################
#---------------------------------------------------
#FIX THIS!!!!!!!
#
# parse_and_check_file_name() - subroutine 
#   that parses and checks the input file name.
#   Name should be of the form:
#    CSE_ReferenceSite_Station_BeginDate_EndDate.sfc
#
# (e.g., LBA_Pantanal_Pantanal_20020701_20020705.sfc)
#
# WARNING: HARDCODED values!
#---------------------------------------------------
sub parse_and_check_file_name {
   my ($input_file_name) = @_;    

   #------------------------------------
   # First cut off and check the suffix.
   # Should work if one "." proceeds
   # suffix, else name is incorrect.
   #------------------------------------
   @name_parts = split /\./, $input_file_name;
   $file_name_suffix    = $name_parts[1];

   if ($file_name_suffix eq $FILE_SUFFIX1 || 
       $file_name_suffix eq $FILE_SUFFIX2  )
      {
      if ($info == 1)
        {print "   Correct File Name Suffix $file_name_suffix.\n\n";}
      }
   else
      {print "   ERROR FILE: Improper File Name Suffix of $file_name_suffix instead of $FILE_SUFFIX1 or $FILE_SUFFIX2.\n"; $err_file++;}

   #--------------------------------------------
   # Pick the two dates off the end of the name.
   # Note that it is possible that the file 
   # name has underscores in the CSE, RefSite,
   # or Station name, but not in the dates.
   #--------------------------------------------
   @name_title = split /_/, $name_parts[0];
   
   $num_title_parts = scalar(@name_title);

   if ($debug1) {print "Name_title ", $num_name_title, " total elements.\n";} 

   if (scalar(@name_title) == 5 )
      {
      print "   File Name Structure - 5 elements before suffix: @name_parts\n";
      print "   File Name Structure - Assume name elements are single words.\n";

      $file_name_CSE        = $name_title[0];
      $file_name_Ref_Site   = $name_title[1];
      $file_name_Stn        = $name_title[2];
      $file_name_Begin_Date = $name_title[3];
      $file_name_End_Date   = $name_title[4];  
      }
   else
      {
       print "   WARNING: Possible Improper File Name Structure $ARGV[0] with $num_title_parts elements. Parse name using best guess.\n";
       $total_warn++;
 
      $file_name_Begin_Date = $name_title[$#name_title-1]; #Dates are last
      $file_name_End_Date   = $name_title[$#name_title];   #two elements

      #------------------------------------------------------
      # Attempt to separate the CSE and RefSite and StnID.
      # First element should be at least part of the CSE
      # and last element should be at least part of the StnID
      # As best first guess since many CSE's are single word,
      # take the first word as the CSE and divide the rest
      # to be the RefSite and StnID.
      #------------------------------------------------------
      # WARNING: THIS MAY NOT WORK IN ALL CASES!!!!! (HERE)
      #------------------------------------------------------

      $file_name_CSE = $name_title[0]; # first word
      $num_words = int(($num_title_parts-3)/2); # Give half to each RefSite and Stn

      if ($debug1) {print "File Name: num_words = $num_words\n";}

      #--------------------------------------------------
      # Split words equally between each RefSite and Stn.
      #--------------------------------------------------
      # If the number of words is even then this should
      # should split them evenly between the two. If uneven,
      # either the RefSite or the Stn has more words, so
      # guess that the Stn has more words.
      # May not be a very good guess?                             
      #-------------------------------------------------
      for ($i=1; $i<=$num_words; $i++)
         {
         $file_name_Ref_Site   = $file_name_Ref_Site.$name_title[$i];

         if ($num_words > $i)
            { $file_name_Ref_Site   = $file_name_Ref_Site."_"; } 

         if ($debug1) {print "Ref_site: i = $i\n";}
         }

      for ($ii=$num_words+1; $ii<$num_title_parts-2; $ii++)
         {
         $file_name_Stn = $file_name_Stn.$name_title[$ii];

         if (($num_title_parts-3) > $ii) 
            { $file_name_Stn   = $file_name_Stn."_"; }

         if ($debug1) {print "Stn_name: ii = $ii\n";}
         }

      } # More than 5 words in first part of name before suffix.


   print "          CSE = $file_name_CSE, Ref_Site = $file_name_Ref_Site, Stn = $file_name_Stn, \n          Begin_Date = $file_name_Begin_Date, End_Date = $file_name_End_Date, Suffix = $file_name_suffix\n";

   } #parse_and_check_file_name{}


##################################################
#-------------------------------------------------
# parse_record_metadata() - subroutine that parses/
#    defines the meta data in the QCF record.
#    This should be the same for every record.
#    Meta Data are in first 95 chars of record. 
#    Note that the spaces in the metadata section 
#    are defined in the routine that parses/defines
#    parameter routine.
#
# WARNING: HARDCODED values!
#-------------------------------------------------
sub parse_record_metadata {

   #------------------------------------------------------------------------
   # Verify that there are slashes and colons in the min required positions.
   #------------------------------------------------------------------------
   @slash_positions = (4, 7, 21, 24); # In the dates
   @colon_positions = (13, 30);       # In the times

   if ($debug) { print  "slash_positions:: @slash_positions\n"; }  
   if ($debug) { print  "colon_positions:: @colon_positions\n"; }  

   #---------------------------------------------------
   # Set up metadata hashes of hashes with detailed info.
   # Since the Network and StnID are chars they
   # are not included in same hash, but are currently
   # handled separately - NOTE
   #---------------------------------------------------
   %name_positions = (
      Network  =>{BEG_POS=>34,  LENGTH=>10},
      StnID    =>{BEG_POS=>45,  LENGTH=>15}
      );

   if ($debug)
      {
      print "\n\n";
      foreach $name (sort keys %name_positions)
         {
         print  "name_positions:: $name  ";
         foreach $info_set (sort keys %{ $name_positions{$name} } )
            { print "$info_set = $name_positions{$name}{$info_set} "; }
            print "\n";
         }
      } # debug  


   %meta_positions = (
      UTC_nominal_year    =>{BEG_POS=>0,   LENGTH=>4, MIN_VAL=>$PROJECT_YEAR, MAX_VAL=>$PROJECT_YEAR_NEXT},
      UTC_nominal_month   =>{BEG_POS=>5,   LENGTH=>2, MIN_VAL=>1,  MAX_VAL=>12},
      UTC_nominal_day     =>{BEG_POS=>8,   LENGTH=>2, MIN_VAL=>1,  MAX_VAL=>31},
      UTC_nominal_hour    =>{BEG_POS=>11,  LENGTH=>2, MIN_VAL=>0,  MAX_VAL=>23},
      UTC_nominal_minute  =>{BEG_POS=>14,  LENGTH=>2, MIN_VAL=>0,  MAX_VAL=>59},

      UTC_actual_year     =>{BEG_POS=>17,  LENGTH=>4, MIN_VAL=>$PROJECT_YEAR, MAX_VAL=>$PROJECT_YEAR_NEXT},
      UTC_actual_month    =>{BEG_POS=>22,  LENGTH=>2, MIN_VAL=>1,  MAX_VAL=>12},
      UTC_actual_day      =>{BEG_POS=>25,  LENGTH=>2, MIN_VAL=>1,  MAX_VAL=>31},
      UTC_actual_hour     =>{BEG_POS=>28,  LENGTH=>2, MIN_VAL=>0,  MAX_VAL=>23},
      UTC_actual_minute   =>{BEG_POS=>31,  LENGTH=>2, MIN_VAL=>0,  MAX_VAL=>59},
 
      LAT    =>{BEG_POS=>61,  LENGTH=>10, MIN_VAL=>-90.00,  MAX_VAL=>90.00},
      LON    =>{BEG_POS=>72,  LENGTH=>11, MIN_VAL=>-180.00, MAX_VAL=>180.00},
      OCC    =>{BEG_POS=>84,  LENGTH=>3,  MIN_VAL=>0,       MAX_VAL=>10},
      ELEV   =>{BEG_POS=>88,  LENGTH=>7,  MIN_VAL=>0.00,    MAX_VAL=>9000.00}
      );

   if ($debug)
      {
      print "\n\n";
      foreach $meta (sort keys %meta_positions)
         {
         print  "meta_positions:: $meta  ";
         foreach $info_set (sort keys %{ $meta_positions{$meta} } )
            { print "$info_set = $meta_positions{$meta}{$info_set} "; }
            print "\n";
         }
      } # debug     

   } #sub parse_record_metadata{}

##################################################
#-------------------------------------------------
# check_record_metadata() - subroutine that checks
#    the input CEOP record metadata.
# Save off uniq list of Network/StnIDs.
#-------------------------------------------------
sub check_record_metadata {    
   my ($input_file_name, $input_record, $number_lines_processed) = @_;

   @days_in_month = (31,28,31,30,31,30,31,31,30,31,30,31);

   @record_chars = split //, $input_record;    # Also done in check_record data

   #-------------------------------------------
   # Verify slashes in expected Date locations.
   #-------------------------------------------
   $err = 0;
   foreach $slash (@slash_positions)
      {
      if ($record_chars[$slash] ne '/')
         {
         print "      ERROR META: Non-Slash ($record_chars[$slash]) at char $slash should be SLASH at line $number_lines_processed in $input_file_name.\n";
         $err++; $err_meta++;
         }

      if ($debug)
         { print "Checking char ($record_chars[$slash]) at position $slash to verify is SLASH.\n";}
      }

   if ($err == 0 && $info == 1)
      {
      print "      Correct SLASHES in Date fields of record positions for record $number_lines_processed.\n";
      }     

   #-----------------------------------------
   # Verify colon in expected time locations.
   #-----------------------------------------
   $err = 0;
   foreach $colon (@colon_positions)
      {
      if ($record_chars[$colon] ne ':')
         {
         print "      ERROR META: Non-Colon ($record_chars[$colon]) at char $colon should be COLON at line $number_lines_processed in $input_file_name.\n";
         $err++; $err_meta++;
         }

      if ($debug)
         { print "Checking char ($record_chars[$colon]) at position $colon to verify is COLON.\n";}
      }               

   if ($err == 0 && $info == 1)
      {
      print "      Correct COLONS in Time Fields of record positions for record $number_lines_processed.\n";
      }  

   #---------------------------------------
   # Verify that each metadata value is
   # within the specified "general" limits.
   #---------------------------------------
   foreach $meta (sort keys %meta_positions)
      {
      $meta_value = substr ($input_record, $meta_positions{$meta}{BEG_POS},
                            $meta_positions{$meta}{LENGTH});

      if ($debug) {print "$meta = $meta_value, ";}

      if ($debug) {print "BEG_POS=$meta_positions{$meta}{BEG_POS}; LENGTH=$meta_positions{$meta}{LENGTH}; MAX_VAL=$meta_positions{$meta}{MAX_VAL}; MIN_VAL=$meta_positions{$meta}{MIN_VAL} \n";}
                                              
      #-------------------------------------------------------------------
      # Verify that meta values are within specified ranges for each piece
      # of metadata. Each value must be within the specified limits.
      # ** THE METADATA CAN NOT BE MISSING! **
      #-------------------------------------------------------------------
      if ( ($meta_value > $meta_positions{$meta}{MAX_VAL}) ||
           ($meta_value < $meta_positions{$meta}{MIN_VAL})   )
         {
         #-----------------------------------------------------------
         # Note that some meta data are only 2 chars and missing should
         # be impossible for date/time but maybe for lat/lon/elev.
         #-----------------------------------------------------------
         if ($meta_value < -999.90 && $meta ne ELEV && $meta ne LAT && $meta ne LON)
            {                                                      
            print "      ERROR META: $meta ($meta_value) outside expected range ($meta_positions{$meta}{MIN_VAL}, $meta_positions{$meta}{MAX_VAL}) at line $number_lines_processed in $input_file_name.\n";
            $err++; $err_meta++;
            }
         else # Missing Meta data
            {                                                      
            if ($meta eq ELEV)
               {
               if ($err_elev_missing == 0)
                  { print "      ERROR META: (1st Instance) $meta ($meta_value) is MISSING or NEGATIVE ($meta_positions{$meta}{MIN_VAL}, $meta_positions{$meta}{MAX_VAL}) at line $number_lines_processed in $input_file_name. Can NOT be Missing!\n"; }

               $err++; $err_meta++; $err_elev_missing++;
               }
            elsif ($meta eq LAT) 
               {
               if ($err_lat_missing == 0)
                  { print "      ERROR META: (1st Instance) $meta ($meta_value) is MISSING ($meta_positions{$meta}{MIN_VAL}, $meta_positions{$meta}{MAX_VAL}) at line $number_lines_processed in $input_file_name. Can NOT be Missing!\n"; }

               $err++; $err_meta++; $err_lat_missing++;
               }
            elsif ($meta eq LON)
               {
               if ($err_lon_missing == 0)
                  { print "      ERROR META: (1st Instance) $meta ($meta_value) is MISSING ($meta_positions{$meta}{MIN_VAL}, $meta_positions{$meta}{MAX_VAL}) at line $number_lines_processed in $input_file_name. Can NOT be Missing!\n"; }
               $err++; $err_meta++; $err_lon_missing++;
               }
            else # Meta other than lat/lon/elev missing!
               { 
               print "      ERROR META: $meta ($meta_value) is MISSING ($meta_positions{$meta}{MIN_VAL}, $meta_positions{$meta}{MAX_VAL}) at line $number_lines_processed in $input_file_name. Can NOT be Missing!\n"; 
               $err++; $err_meta++;
               }
            } # Missing Meta data
         } # Within limits?
      } # foreach meta 

   if ($err == 0 && $info == 1)
      {
      print "      Correct All Meta data are within max/min range for record $number_lines_processed.\n\n";
      }
   else
      { $err = 0; }       


   #-----------------------------------
   # Check Nominal DAY more closely
   #-----------------------------------
   # Feb has 29 days during Leap Years.
   #-----------------------------------
   $nominal_year = substr ($input_record, $meta_positions{UTC_nominal_year}{BEG_POS},
                           $meta_positions{UTC_nominal_year}{LENGTH});

   $nominal_month = substr ($input_record, $meta_positions{UTC_nominal_month}{BEG_POS},
                           $meta_positions{UTC_nominal_month}{LENGTH}); 

   $nominal_day = substr ($input_record, $meta_positions{UTC_nominal_day}{BEG_POS},
                           $meta_positions{UTC_nominal_day}{LENGTH}); 

   if ($debug) { print "nominal_year, nominal_month, nominal_day: $nominal_year, $nominal_month, $nominal_day\n"; }

   if (($nominal_year%4==0 && $nominal_year%100 !=0 || $nominal_year%400==0) )
      {$days_in_month[1]++;}       

   if (($nominal_day > $days_in_month[$nominal_month-1]) || ($nominal_day < 1))
      { print "      ERROR META: Bad Nominal Day ($nominal_day) at line $number_lines_processed in $input_file_name.\n"; $err_meta++;}  

   #----------------------------------------
   # Check Record Nominal Dates
   # against File Name Date Begin/End Dates.
   #----------------------------------------
   $date = $nominal_year.$nominal_month.$nominal_day;

   if ($number_lines_processed == 1)
      {
      if ($file_name_Begin_Date eq $date)
         { 
         if ($info == 1)
            { print "      Correct Match between File Name and Data Begin_Dates for Nominal Date for record $number_lines_processed.\n"; }
         } 
      else
         { print "      ERROR META: File Name Begin_Date ($file_name_Begin_Date) does NOT match first Nominal Date ($date) in found in File $input_file_name.\n"; $err_meta++;}
      }

   if ($number_lines_processed == $number_lines_in_file)
      {
      if ($file_name_End_Date eq $date)
         { 
         if ($info == 1)
            { print "      Correct Match between File Name and Data End_Dates for record $number_lines_processed.\n"; }
         }
      else
         { print "      ERROR META: File Name End_Date ($file_name_End_Date) does NOT match Last Nominal Date ($date) in found in File $input_file_name.\n"; $err_meta++;}
      }


   #-----------------------------------
   # Check Actual DAY more closely.
   #-----------------------------------
   # Feb has 29 days during Leap Years.
   #-----------------------------------
   $actual_year = substr ($input_record, $meta_positions{UTC_actual_year}{BEG_POS},
                           $meta_positions{UTC_actual_year}{LENGTH});

   $actual_month = substr ($input_record, $meta_positions{UTC_actual_month}{BEG_POS},
                           $meta_positions{UTC_actual_month}{LENGTH});

   $actual_day = substr ($input_record, $meta_positions{UTC_actual_day}{BEG_POS},
                           $meta_positions{UTC_actual_day}{LENGTH});  


   if ($debug) { print "actual_year, actual_month, actual_day: $actual_year, $actual_month, $actual_day\n"; }


   if (($actual_year%4==0 && $actual_year%100 !=0 || $actual_year%400==0) )
      {$days_in_month[1]++;}   

   if (($actual_day > $days_in_month[$actual_month-1]) || ($actual_day < 1))
      { print "      ERROR META: Bad Actual Day ($actual_day) at line $number_lines_processed in $input_file_name.\n"; $err_meta++;}

   #----------------------------------------
   # Check Record Actual Dates
   # against File Name Date Begin/End Dates.
   #----------------------------------------
   $date = $actual_year.$actual_month.$actual_day;

   if ($number_lines_processed == 1)
      { 
      if ($file_name_Begin_Date eq $date)
         { 
          if ($info == 1)
            {print "      Correct Match between File Name and Data Begin_Dates for Actual Dates.\n"; }
         }
      else
         { print "      ERROR META: File Name Begin_Date ($file_name_Begin_Date) does NOT match first Actual Date ($date) in found in File $input_file_name.\n"; $err_meta++;}
      }
 
   if ($number_lines_processed == $number_lines_in_file)
      {
      if ($file_name_End_Date eq $date) 
         { 
         if ($info == 1)
            {print "      Correct Match between File Name and Data End_Dates.\n"; }
         }
      else 
         { print "      ERROR META: File Name End_Date ($file_name_End_Date) does NOT match Last Actual Date ($date) in found in File $input_file_name.\n"; $err_meta++;}
      } 

#xxxxxxx
   #-----------------------------------------------------
   # Required to be 30 minute data with Nominal minutes
   # at either 00 or 30, only.
   #-----------------------------------------------------
   # Assume this should be true for the Actual time, too.
   # In format doc, the minute rounding of the actual time 
   # to form the nominal time is discussed. 
   #------------------------------------------------------------
   $nominal_minute = substr ($input_record, $meta_positions{UTC_nominal_minute}{BEG_POS} ,
                           $meta_positions{UTC_nominal_minute}{LENGTH});   

   $nominal_hour = substr ($input_record, $meta_positions{UTC_nominal_hour}{BEG_POS} ,
                           $meta_positions{UTC_nominal_hour}{LENGTH});


   $actual_minute = substr ($input_record, $meta_positions{UTC_actual_minute}{BEG_POS} ,
                           $meta_positions{UTC_actual_minute}{LENGTH});

   $actual_hour = substr ($input_record, $meta_positions{UTC_actual_hour}{BEG_POS} ,
                           $meta_positions{UTC_actual_hour}{LENGTH});
                                                                         

   if ($debug) { print "nominal_minute, nominal_hour, actual_minute, actual_hour: $nominal_minute, $nominal_hour, $actual_minute, $actual_hour\n"; }


   if (($nominal_minute ne "00") && ($nominal_minute ne "30"))
      { print "      ERROR META: Nominal_minute ($nominal_minute) is not 00 or 30 at line $number_lines_processed in $input_file_name.\n"; $err_meta++;}
    
   #----------------------------------------------------------------
   # The following should be true if the rules are followed exactly.
   #----------------------------------------------------------------
   if (($actual_minute ne "00") && ($actual_minute ne "30"))
      { print "      ERROR META: Actual_minute ($actual_minute) is not 00 or 30 at line $number_lines_processed in $input_file_name.\n"; $err_meta++;} 

   #------------------------------------------------------
   # These checks match the doc on the nominal time versus 
   # the actual time and what is considered valid.
   #------------------------------------------------------
   if (($actual_minute < 15) || ($actual_minute >= 45)) 
      {
      if ($nominal_minute ne "00")
         { print "      ERROR META: Actual_minute ($actual_minute) <15 or >=45, but nominal_minute ($nominal_minute) is not 00 at line $number_lines_processed in $input_file_name.\n"; $err_meta++;}
      }
   else
      {
      if ($nominal_minute ne "30") 
         { print "      ERROR META: Actual_minute ($actual_minute) >=15 or <45, but nominal_minute ($nominal_minute) is not 30 at line $number_lines_processed in $input_file_name.\n"; $err_meta++;}   
      }

   #-----------------------------------------
   # The nominal and actual date/times should
   # match exactly if rules are followed.
   #-----------------------------------------
   if (($nominal_year != $actual_year) || ($nominal_month != $actual_month) ||
       ($nominal_day != $actual_day) || ($nominal_hour != $actual_hour) ||
       ($nominal_minute != $actual_minute))
      { print "      ERROR META: Nominal Date/Time does NOT exactly match Actual Date/Time at line $number_lines_processed in $input_file_name.\n"; $err_meta++;}


   #---------------------------------------------------------
   # Pull out the record site info. Chop off trailing blanks.
   # Extract the lat/lon/elev and check against known info
   # for current stn.   
   #---------------------------------------------------------
   $record_network = substr ($input_record, $name_positions{Network}{BEG_POS},
                         $name_positions{Network}{LENGTH});  
   $record_network      =~ s/\s+$//;

   $record_Stn = substr ($input_record, $name_positions{StnID}{BEG_POS},
                         $name_positions{StnID}{LENGTH}); 
   $record_Stn      =~ s/\s+$//;

   $lat = substr ($input_record, $meta_positions{LAT}{BEG_POS},
                  $meta_positions{LAT}{LENGTH});

   $lon = substr ($input_record, $meta_positions{LON}{BEG_POS},
                  $meta_positions{LON}{LENGTH});

   $elev = substr ($input_record, $meta_positions{ELEV}{BEG_POS},
                   $meta_positions{ELEV}{LENGTH});           

                                      
   #------------------------------------------------------
   # Form and Save Uniq Network:StnID identification.
   # Can update this section to retain detailed info about
   # each site in a composite.
   #------------------------------------------------------
   $Network_StnID = $record_network.':'.$record_Stn;

   if ($debug) {print "\nUniq Network_StnID = $Network_StnID\n";}

   if (exists $stnInfo{$Network_StnID} )
      {
      #-------------------------------------
      # This stn already exists in the hash.
      #-------------------------------------
      if ($debug) { print "$Network_StnID EXISTS in the List of Uniq stns.\n";}

      #--------------------------------------------------------------
      # Verify that lat/lon/elev have not changed for this stn. Must
      # consistent over time.
      #--------------------------------------------------------------
      if ($stnInfo{ $Network_StnID }{LAT} != $lat)
         {
         print "      ERROR META: Network_StnID ($Network_StnID) LATITUDE changed from $stnInfo{ $Network_StnID }{LAT} to $lat at line $number_lines_processed in $input_file_name.\n"; 
         $stnInfo{ $Network_StnID }{LAT} = $lat; #Reset to find next change.
         $err_meta++;
         }

      if ($stnInfo{ $Network_StnID }{LON} != $lon)
         {
         print "      ERROR META: Network_StnID ($Network_StnID) LONGITUDE changed from $stnInfo{ $Network_StnID }{LON} to $lon at line $number_lines_processed in $input_file_name.\n";
         $stnInfo{ $Network_StnID }{LON} = $lon; #Reset to find next change.
         $err_meta++;
         }  

      if ($stnInfo{ $Network_StnID }{ELEV} != $elev)
         {
         print "      ERROR META: Network_StnID ($Network_StnID) ELEVATION changed from $stnInfo{ $Network_StnID }{ELEV} to $elev at line $number_lines_processed in $input_file_name.\n"; 
         $stnInfo{ $Network_StnID }{ELEV} = $elev; #Reset to find next change.
         $err_meta++;
         }  

      #-------------------------------------------
      # Keep stn's begin and end dates up-to-date.
      #-------------------------------------------
      $current_date = $nominal_year.$nominal_month.$nominal_day;

      if ($current_date < $stnInfo{ $Network_StnID }{BEGIN_DATE})
         {
         $stnInfo{ $Network_StnID }{BEGIN_DATE} = $current_date;
         } 

      if ($current_date > $stnInfo{ $Network_StnID }{END_DATE})
         {
         $stnInfo{ $Network_StnID }{END_DATE} = $current_date;
         }

      }
   else
      {
      #--------------------------------
      # This is a new stn. Add to list.
      #--------------------------------
      if ($debug) { print "$Network_StnID is NEW ID. Add to List of Uniq stns.\n";}

      $stnInfo{ $Network_StnID }{STNID} = $record_Stn;
      $stnInfo{ $Network_StnID }{LAT} = $lat;
      $stnInfo{ $Network_StnID }{LON} = $lon;  
      $stnInfo{ $Network_StnID }{ELEV} = $elev;
      $stnInfo{ $Network_StnID }{BEGIN_DATE} = $nominal_year.$nominal_month.$nominal_day;
      $stnInfo{ $Network_StnID }{END_DATE} = $nominal_year.$nominal_month.$nominal_day;
      }

   if ($debug)
      {
      print "Current date is $nominal_year.$nominal_month.$nominal_day\n";

      foreach $ID (sort keys %stnInfo)
         {
         print  "stnInfo:: $ID  ";
         foreach $info_set (sort keys %{ $stnInfo{$ID} } )
            { print "$info_set = $stnInfo{$ID}{$info_set} "; }
            print "\n";
         }
      } # debug    


   #----------------------------------------
   # Warn about leading white spaces in IDs.
   # Trim any leading spaces and compare.
   #----------------------------------------
   $name = $record_network;
   $name =~ s/^\s+//;

   if ( $record_network ne $name )
      { print "      ERROR META: Record_Network ($record_network) has LEADING SPACES at line $number_lines_processed in $input_file_name. Incorrect justification.\n"; $err_meta++;} 


   #-------------------------------------------------------------
   # Check for embedded spaces in Network and Station ID strings.
   #-------------------------------------------------------------
   @parts = split / /, $name;

   if ( $#parts != 0 )
      { 
      print "      ERROR META: Record_Network ($record_network) has EMBEDDED SPACES at line $number_lines_processed in $input_file_name. Incorrect justification.\n"; $err_meta++;
      }
   
   $name = $record_Stn; 
   $name =~ s/^\s+//; 
 
   if ( $record_Stn ne $name ) 
      { print "      ERROR META: Record_Stn ($record_Stn) has LEADING SPACES at line $number_lines_processed in $input_file_name. Incorrect justification.\n"; $err_meta++;}  

   @parts = split / /, $name; # Check for embedded spaces. 
 
   if ( $#parts != 0 )
      { 
      print "      ERROR META: Record_Stn ($record_Stn) has EMBEDDED SPACES at line $number_lines_processed in $input_file_name. Incorrect justification.\n"; $err_meta++; 
      }


   #----------------------------------
   # Note general location of station.
   #----------------------------------
   if ($info == 1)
      {
      if ($lat > 0.00)
         { print "      NOTE: These data are in the Northern Hemisphere ($lat) at line $number_lines_processed in $input_file_name.\n";}
      else
         { print "      NOTE: These data are in the Southern Hemisphere ($lat) at line $number_lines_processed in $input_file_name.\n";}

      if ($lon < 0.00)
         { print "      NOTE: These data are in the Western Hemisphere ($lon) at line $number_lines_processed in $input_file_name.\n";}
      else  
         { print "      NOTE: These data are in the Eastern Hemisphere ($lon) at line $number_lines_processed in $input_file_name.\n";}
      } 

  } #check_record_metadata{}


##################################################
#-------------------------------------------------
# parse_record_data() - subroutine that parses
#    the input CEOP record. Each Data parm and flag
#    are parsed from exact locations.
#    Actually this routine sets up the hash tables
#    to be used by the check_record_data() routine.
#
# WARNING: HARDCODED values!
#-------------------------------------------------
sub parse_record_data {

   #----------------------------------------------------------------
   # Verify that there are spaces in the minimum required positions.
   # This is every required space including the metadata spaces.
   #----------------------------------------------------------------
   @space_positions = (
        10,  16,  27,  33,  44,  60,  71,  83,  87, 
        95, 103, 105, 113, 115, 123, 125, 133, 135,
       143, 145, 153, 155, 163, 165, 173, 175, 177,
       185, 187, 192, 194, 203, 205, 213, 216, 218,
       221, 223, 231, 234, 236, 239, 241, 249, 252,
       254, 257 );

   if ($debug) { print  "\nspace_positions:: @space_positions\n"; }

   @decimalPt_positions = (
        65,  77,  92, 100, 110, 120, 130, 140, 
       150, 160, 170, 182, 200, 210, 228, 246 );

   if ($debug) { print  "\ndecimalPt_positions:: @decimalPt_positions\n"; }         


   #--------------------------------------------------
   # Set up hash containing parameter flag name versus
   # exact position in data record. Note that
   # all flags are 1 character long and must
   # be one of a specific set of acceptable chars.
   # Remember, positions in the rec start at zero,
   # not one.
   # WARNING: The Numflags are not chars, but ARE numeric!
   #--------------------------------------------------
   %flag_positions = (
        "stn_pressure"       => 104, 
        "sealvl_pressure"    => 114, 
        "calcsea_pressure"   => 124, 
        "temperature"        => 134, 
        "dew_pt"             => 144, 
        "wind_speed"         => 154, 
        "wind_direction"     => 164, 
        "precip"             => 174, 
        "gust_indicator"     => 176, 
        "gust"               => 186, 
        "presentwx"          => 193,
        "visibility"         => 204,

        "ceilingHt1_Numflag" => 214,
        "ceilingHt1_QCflag"  => 217,
        "cldamtHt1_Numflag"  => 219,
        "cldamtHt1_QCflag"   => 222,

        "ceilingHt2_Numflag" => 232,
        "ceilingHt2_QCflag"  => 235,
        "cldamtHt2_Numflag"  => 237,
        "cldamtHt2_QCflag"   => 240,

        "ceilingHt3_Numflag" => 250,
        "ceilingHt3_QCflag"  => 253,
        "cldamtHt3_Numflag"  => 255,
        "cldamtHt3_QCflag"   => 258
      );

   if ($debug)
      {
      print "\n";
      foreach $parm (sort keys %flag_positions)
         { print  "flag_positions:: $parm  $flag_positions{$parm} \n"; }
      } # debug  


   #---------------------------------------------------------------
   # Set up hash to maintain count of each flag type found by parm.
   # Also track total error count for that parm type. Note that
   # Precip has the extra "T" for trace amt of pcp flag. Also, "I"
   # flag applies only to potentially computed values. However,
   # the I flag has been included in all parms which might help
   # find errors in data or conversion.
   #---------------------------------------------------------------
   # These are the typical Quality Control (QC) flags.
   #---------------------------------------------------------------
   %flag_count = (
       "stn_pressure"      => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "sealvl_pressure"   => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "calcsea_pressure"  => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "temperature"       => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "dew_pt"            => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "wind_speed"        => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "wind_direction"    => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "precip"            => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,T=>0,UNKNOWN=>0,ERROR_CT=>0},
       "gust"              => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "presentwx"         => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "visibility"        => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "ceilingHt1_QCflag" => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "ceilingHt2_QCflag" => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "ceilingHt3_QCflag" => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "cldamtHt1_QCflag"  => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "cldamtHt2_QCflag"  => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},
       "cldamtHt3_QCflag"  => {U=>0,G=>0,M=>0,D=>0,B=>0,N=>0,X=>0,E=>0,C=>0,I=>0,UNKNOWN=>0,ERROR_CT=>0},

       "gust_indicator"   => {''=>0,G=>0,S=>0,UNKNOWN=>0,ERROR_CT=>0},

       #-------------------------------------------------------------------------------------------------
       # Numflag are the code flag and have only numeric values. Every Numflag has an associated QCflag.
       # Ceiling heights and Cloud Amounts have QC flags as well as Numeric flags for each reported value.
       #--------------------------------------------------------------------------------------------------
       "ceilingHt1_Numflag" 
          =>{c0=>0,c1=>0,c2=>0,c3=>0,c4=>0,c5=>0,c6=>0,c7=>0,c8=>0,c9=>0,c10=>0,c11=>0,c15=>0,UNKNOWN=>0,ERROR_CT=>0},
       "ceilingHt2_Numflag" 
          =>{c0=>0,c1=>0,c2=>0,c3=>0,c4=>0,c5=>0,c6=>0,c7=>0,c8=>0,c9=>0,c10=>0,c11=>0,c15=>0,UNKNOWN=>0,ERROR_CT=>0},
       "ceilingHt3_Numflag" 
          =>{c0=>0,c1=>0,c2=>0,c3=>0,c4=>0,c5=>0,c6=>0,c7=>0,c8=>0,c9=>0,c10=>0,c11=>0,c15=>0,UNKNOWN=>0,ERROR_CT=>0},

       "cldamtHt1_Numflag"  =>{c0=>0,c1=>0,c2=>0,c3=>0,c4=>0,c5=>0,c6=>0,c7=>0,c8=>0,c9=>0,c10=>0,c11=>0,c12=>0,c15=>0,
                               UNKNOWN=>0,ERROR_CT=>0},
       "cldamtHt2_Numflag"  =>{c0=>0,c1=>0,c2=>0,c3=>0,c4=>0,c5=>0,c6=>0,c7=>0,c8=>0,c9=>0,c10=>0,c11=>0,c12=>0,c15=>0,
                               UNKNOWN=>0,ERROR_CT=>0},
       "cldamtHt3_Numflag"  =>{c0=>0,c1=>0,c2=>0,c3=>0,c4=>0,c5=>0,c6=>0,c7=>0,c8=>0,c9=>0,c10=>0,c11=>0,c12=>0,c15=>0,
                               UNKNOWN=>0,ERROR_CT=>0},
     );                               

   if ($debug)
      {
      print "\n\n";
      foreach $parm (sort keys %flag_count)
         {
         print  "flag_count:: $parm  ";
         foreach $flag_type (sort keys %{ $flag_count{$parm} } )
            { print "$flag_type = $flag_count{$parm}{$flag_type} "; }
            print "\n";
         }
      } # debug  

   #---------------------------------------------------------------------
   # Set up data type hash of hashes with info about each parameter where
   #
   #    BEG_POS = Beginning char position in record.
   #    LENGTH  = Length in chars in data format.
   #    MIN_VAL = Minimum allowed value for gross limit check.
   #    MAX_VAL = Maximum allowed value for gross limit check.
   #    MIN_EXT = Minimum extreme value found for that parm in this file.
   #    MAX_EXT = Maximum extreme value found for that parm in this file.
   #    ERR_CT  = Number of data errors generated for that parm.
   #----------------------------------------------------------------------
   %parm_positions = (
      stn_pressure       =>{BEG_POS=>96,  LENGTH=>7, MIN_VAL=>300.00,  MAX_VAL=>1100.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0}, 
      sealvl_pressure    =>{BEG_POS=>106, LENGTH=>7, MIN_VAL=>300.00,  MAX_VAL=>1100.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      calcsea_pressure   =>{BEG_POS=>116, LENGTH=>7, MIN_VAL=>300.00,  MAX_VAL=>1100.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      temperature        =>{BEG_POS=>126, LENGTH=>7, MIN_VAL=>-90.00,  MAX_VAL=>60.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      dew_pt             =>{BEG_POS=>136, LENGTH=>7, MIN_VAL=>-90.00,  MAX_VAL=>35.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      wind_speed         =>{BEG_POS=>146, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>50.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      wind_direction     =>{BEG_POS=>156, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>360.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      precip             =>{BEG_POS=>166, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>250.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      gust               =>{BEG_POS=>178, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>50.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      presentwx          =>{BEG_POS=>188, LENGTH=>4, MIN_VAL=>0,       MAX_VAL=>9999,
                                           MIN_EXT=>9999,     MAX_EXT=>-999, ERR_CT=>0},
      visibility         =>{BEG_POS=>195, LENGTH=>8, MIN_VAL=>0.00,    MAX_VAL=>20000.00,
                                           MIN_EXT=>99999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      ceilingHt1         =>{BEG_POS=>206, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>50.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      ceilingHt2         =>{BEG_POS=>224, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>50.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      ceilingHt3         =>{BEG_POS=>242, LENGTH=>7, MIN_VAL=>0.00,    MAX_VAL=>50.00,
                                           MIN_EXT=>9999.99,  MAX_EXT=>-999.99, ERR_CT=>0},
      );

   if ($debug)
      { 
      print "\n\n";
      foreach $parm (sort keys %parm_positions)
         {
         print  "parm_positions:: $parm  "; 
         foreach $info_set (sort keys %{ $parm_positions{$parm} } )
            { print "$info_set = $parm_positions{$parm}{$info_set} "; }
            print "\n";
         }
      } # debug


   } # sub parse_record_data{}


##################################################
#--------------------------------------------------
# check_record_data() - subroutine that checks
#    the input CEOP record. Each Data parm and flag
#    are checked 
#
# WARNING: HARDCODED flag and missing values!
#--------------------------------------------------
sub check_record_data {
   my ($input_file_name, $input_record, $number_lines_processed) = @_; 

   #---------------------------------------------
   # Check for any Control/nonprintable Chars on
   # this line. Code copied from iscntrl.pl tool.
   # ASCII values 0-31 and > 127 are nonprintable
   # characters.
   #---------------------------------------------
   if ($input_record =~ /[\x0-\x1f\x7f-\xff]/)
      {print "   ERROR DATA: Found Control Char at line $number_lines_processed in $input_file_name.\n"; $err_data++;}                                 
   else
      {
      if ($info == 1)
         { print "      Correct Record $number_lines_processed is free of control chars.\n"; }
      }
     
   #-----------------------------------------------
   # Verify that the record is RECORD_LENGTH long. 
   # Note that the return has been removed prior to
   # begin sent to this routine. So the line should
   # be RECORD_LENGTH-1 long. $#chars gives the index
   # of the last element in that array.
   #----------------------------------------------- 
   @record_chars = split //, $input_record;
   $current_record_length = $#record_chars + 2;

   if (($current_record_length) != $RECORD_LENGTH)
      {print "   ERROR DATA: Incorrect line length of ($current_record_length) at line $number_lines_processed in $input_file_name.\n"; $err_data++;}
   else
      {
      if ($info == 1)
         { print "      Correct Record $number_lines_processed has correct record length  of $current_record_length.\n"; }
      }   

   #----------------------------------------------------------------
   # Verify that there are spaces in the minimum required positions.
   #----------------------------------------------------------------
   $err = 0;
   foreach $space (@space_positions)
      {
      if ($record_chars[$space] ne ' ')
         { 
         print "      ERROR DATA: Non-Space ($record_chars[$space]) at char $space should be SPACE at line $number_lines_processed in $input_file_name.\n"; 
         $err++; $err_data++;
         }

      if ($debug)
         { print "Checking char ($record_chars[$space]) at position $space to verify is SPACE.\n";}
      }   

   if ($err == 0 && $info == 1) 
      {
      print "      Correct SPACES in minimum number of record positions for record $number_lines_processed.\n"; 
      }
   else
      { $err = 0; }

   #----------------------------------------------------------------
   # Verify that there are decimalPts in exact required positions.
   # This should also indicate if the value justification is correct
   # or not for each data value, lat, lon, and elevation.
   #----------------------------------------------------------------
   $err = 0;
   foreach $decimalPt (@decimalPt_positions)
      {
      if ($record_chars[$decimalPt] ne '.')
         {
         print "      ERROR DATA: Non-decimalPt ($record_chars[$decimalPt]) at char $decimalPt should be DECIMAL POINT at line $number_lines_processed in $input_file_name. Incorrect field justification.\n";
         $err++; $err_data++;
         }

      if ($debug)
         { print "Checking char ($record_chars[$decimalPt]) at position $decimalPt to verify is DECIMAL POINT.\n";} }

   if ($err == 0 && $info ==1)
      {
      print "      Correct DECIMAL POINTS in exact record positions for record $number_lines_processed.\n";
      }
   else
      { $err = 0; } 


   #-------------------------------------------------------------------
   # Verify that the flags for each parm are one of the required types.
   #-------------------------------------------------------------------
   foreach $parm (keys %flag_positions)
      {
      $position = $flag_positions{$parm};
      $parm_flag = $record_chars[$position];

      if ($debug)
         {print "Checking position=$position to verify flag value of ($parm_flag) for $parm.\n"; }  

      if ($parm_flag ne 'U' && $parm_flag ne 'G' && $parm_flag ne 'M' &&
          $parm_flag ne 'D' && $parm_flag ne 'B' && $parm_flag ne 'N' &&
          $parm_flag ne 'X' && $parm_flag ne 'E' && $parm_flag ne 'C' &&
          $parm_flag ne 'I' && $parm_flag ne 'T')   # Included Trace - only OK for precip
         { 
         print "      ERROR DATA: Parameter Flag ($parm_flag) at position $position is NOT a legal value (i.e., 
U,G,D,B,X,E,C,M,N,I, (T-pcp)) at line $number_lines_processed in $input_file_name.\n";  $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;

         #--------------------------
         # Count unknown flag types.
         #--------------------------
         $flag_count{$parm}{UNKNOWN} ++;
         }   
      else
         {
         if ($parm_flag eq 'T' && $parm ne precip)
            {
            #--------------------------
            # Count unknown flag types.
            #--------------------------
            $flag_count{$parm}{UNKNOWN} ++;
            
            print "      ERROR DATA: Parameter Flag ($parm_flag) at position $position is NOT a legal value (i.e.Found Trace assigned to non-precip parm) at line $number_lines_processed in $input_file_name.\n";  $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;
            }
         else
            {
            #------------------------
            # Count Known flag types.
            #------------------------   
            $flag_count{$parm}{$parm_flag} ++;

            } # T flag but not precip
         }
      } # foreach flag_position


   if ($err == 0 && $info ==1)
      {
      print "      Correct Valid FLAG values for each parm in record $number_lines_processed.\n";
      }   
   else
      { $err = 0; } 

   #------------------------------------------------------------------
   # Verify that if the parm is missing value that the associated flag 
   # is one of the allowed types for -999.99. Remember that some parms 
   # have 7 chars and some have 8 chars.
   #
   # Verify there are no -0.00 (Negative zeroes) in the parm values.
   #
   # Verify that parm values are within specified ranges for each parm.
   #------------------------------------------------------------------
   $non_missing_values = 0;
   foreach $parm (sort keys %parm_positions)
      {
      $parm_value = substr ($input_record, $parm_positions{$parm}{BEG_POS},
                            $parm_positions{$parm}{LENGTH});

      $parm_flag = substr ($input_record, $flag_positions{$parm}, 1); 

      if ($debug) {print "$parm = $parm_value; flag = $parm_flag; ";}

      if ($debug) {print "BEG_POS=$parm_positions{$parm}{BEG_POS}; LENGTH=$parm_positions{$parm}{LENGTH}; MAX_VAL=$parm_positions{$parm}{MAX_VAL}; MIN_VAL=$parm_positions{$parm}{MIN_VAL} \n";}


      #------------------------------------------
      # Count non-missing parameter values.
      #
      # Verify non-missing value has flag of 
      # U, G, D, B, X, E, or T.
      #
      # As far as can be verified, only see
      # T with 0.00 values, not -999.99.
      #
      # Missing may have M, N, C, or I
      #
      # Save off Max and Min extremes for each
      # parameter in the file. This includes all
      # data values for all stns within the file.
      #------------------------------------------
      if ($parm_value ne "-999.99" && $parm_value ne " -999.99") 
         {
         $non_missing_values++;

         if ($parm_value > $parm_positions{$parm}{MAX_EXT})
            {$parm_positions{$parm}{MAX_EXT} = $parm_value;}

         if ($parm_value < $parm_positions{$parm}{MIN_EXT}) 
            {$parm_positions{$parm}{MIN_EXT} = $parm_value;}

#HERE
         #------------------------------------------------
         # Only certain flags valid for non-missing value.
         #------------------------------------------------
         if ($parm_flag ne 'U' && $parm_flag ne 'G' && $parm_flag ne 'D' &&
             $parm_flag ne 'B' && $parm_flag ne 'E' && $parm_flag ne 'X'  && $parm_flag ne 'T')
         {
         print "      ERROR DATA: Parameter Flag ($parm_flag) at position $flag_positions{$parm} is NOT a legal (value (i.e., U,G,D,B,E,X,T) for a NON-MISSING Value at line $number_lines_processed in $input_file_name.\n";  $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;
         }

         #---------------------------
         # Verify no Negative Zeroes.
         #---------------------------
         if ($parm_value eq "  -0.00")
            { print "      ERROR DATA: NEGATIVE ZERO found for $parm ($parm_value) at line $number_lines_processed in $input_file_name.\n";
            $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;
            }

         #-------------------------------------------------------------------
         # Verify that parm values are within specified ranges for each parm.
         # And not missing.
         #-------------------------------------------------------------------
         if ( ($parm_value > $parm_positions{$parm}{MAX_VAL}) ||
              ($parm_value < $parm_positions{$parm}{MIN_VAL})   ) 
            {
            $position = $flag_positions{$parm}; # Grab the associated flag for printout 
            $parm_flag = $record_chars[$position]; 
 
            print "      ERROR DATA: $parm ($parm_value $parm_flag) outside expected range ($parm_positions{$parm}{MIN_VAL}, $parm_positions{$parm}{MAX_VAL}) at line $number_lines_processed in $input_file_name.\n";
            $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;
            }                    

         #--------------------------------------------------------
         # Verify that the Absolute value of the wind components
         # are less than the wind speed. Take absolute value of
         # both parms just in case something else odd is going on.
         # Even though wind speed "should" never be negative.
         #--------------------------------------------------------
         if ($parm eq "U_wind_component" || $parm eq "V_wind_component")
            {
            $wind_speed = substr ($input_record, $parm_positions{wind_speed}{BEG_POS},
                                  $parm_positions{wind_speed}{LENGTH});
            if ($debug) {print "wind_speed = $wind_speed\n";}

            if (abs($parm_value) > abs($wind_speed) )
               {
               print "      ERROR DATA: $parm ($parm_value $parm_flag) Absolute Value GREATER than Wind Speed ($wind_speed) at line $number_lines_processed in $input_file_name.\n";
               $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;
               }
            } # Check wind components
         } # non-missing value
      else
         {  
         #---------------------------------------
         # Verify Missing Value with Missing Flag
         # parm_value = -999.99 or " -999.99"
         #---------------------------------------
         if ($parm_flag ne 'C' && $parm_flag ne 'M' && 
             $parm_flag ne 'N' && $parm_flag ne 'I')
            {                                        
             print "      ERROR DATA: Mismatch between MISSING $parm ($parm_value) and flag ($parm_flag) at line $number_lines_processed in $input_file_name.\n";
             $err++; $err_data++; $parm_positions{$parm}{ERR_CT}++;
            }

         } # Missing or not        

      } # foreach parm


   if ($err == 0 && $info == 1)
      {
      print "      Correct All Parms match flags, no negative zeroes, and parms within max/min range for record $number_lines_processed.\n\n";
      }
   else
      { $err = 0; } 

   #----------------------------------------------------------------
   # If all parms are missing, warn that this rec should be dropped.
   #----------------------------------------------------------------
   if ($non_missing_values == 0)
      {
      print "      WARNING All Parameters are MISSING values. DROP RECORD $number_lines_processed.\n\n";
      $total_completely_missing++;
      $total_warn++;
      }    
   } # check_record_data{} 


##################################################
# check_QCF_sfc.pl - MAIN processing.
##################################################
printf "\ncheck_QCF_sfc.pl of file $ARGV[0] began on ";print scalar localtime; printf "\n";

#----------------------------------
# Set the output information level.
#----------------------------------
$info = 1; $debug=0; $debug1=0; # default

if ($ARGV[1] == 0) {$info = 0;}
if ($ARGV[1] == 1) {$info = 1;}
if ($ARGV[1] == 2) {$info = 1; $debug = 1; $debug1=1;}


#--------------------------------------------------
# Parse, Check, and Print General File Information.
#--------------------------------------------------
check_general_file_info ($ARGV[0]);

#---------------------------------------------
# Ensure the file name is of the correct form.
#---------------------------------------------
parse_and_check_file_name ($ARGV[0]);

#------------------------------------------------
# Routines to parse data actually just set
# element positions in the record, etc. Since
# all records should be the same, this only needs
# to be once.
#------------------------------------------------
parse_record_metadata;
parse_record_data;  

#-----------------------------
# Open the file to be checked.
#-----------------------------
open (INPUT_FILE, "<$ARGV[0]") ||
  die "Can NOT open first arg $ARGV[0] input file for reading";

#-------------------------------------------------
# Verify that every line has the correct metadata,
# data and flags in the expected positions. 
#
# Process every record in the input file.
#-------------------------------------------------
while ($input_line = <INPUT_FILE>)
   {
   $number_lines_processed++;

   if ($info == 1)
      {print "\n------------Processing Line $number_lines_processed-----------\n"; }

   if ($debug) {print "\n=====Read line ($number_lines_processed): $input_line\n"; } 

   chomp($input_line); # 259 chars w/o return

   check_record_metadata( $ARGV[0], $input_line, $number_lines_processed );

   check_record_data( $ARGV[0], $input_line, $number_lines_processed );
   }

close(INPUT_FILE);

write_stationCD_file();

write_final_stats();

print "\nTotal number lines processed in file $ARGV[0]: $number_lines_processed\n";

printf "\ncheck_QCF_sfc.pl of file $ARGV[0] ended on ";print scalar localtime;
printf "\n";
