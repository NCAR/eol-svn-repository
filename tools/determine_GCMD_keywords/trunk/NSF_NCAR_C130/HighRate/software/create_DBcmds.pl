#! /usr/bin/perl -w

#--------------------------------------------------------------------------------
# create_DBcmds.pl
#
# This software generates the SQL commands to apply pre-determined GCMD keywords
# to specified netCDF datasets.
#
# This Perl script tool is the third step in auto generating GCMD:
#  1. Run determine_GCMD.pl to generate *.dataTypes and All_uniq_data_types_*_parm.txt
#  files.
#
#  2. The most knowledgable science person/data manager then adds GCMD keywords
#  to the end of the parm lines (after the xxx) in file All_uniq_data_types_*_parm.txt.
#
#  3. Run create_DBcmds.pl with All_uniq_data_types_*_parm.txt with GCMD keywords
#  to generate (by dataset) the SQL commands to apply to each dataset (*.GCMDcmd).
#
#
# For info on the determine_GCMD.pl software, please that see the header on
#    that code. Here's a short summary on determine_GCMD.pl....
#    The determine_GCMD.pl software creates the All_uniq_data_types_*_parm.txt 
#    output file. The most knowledgable science person/data manager then adds
#    GCMD keywords to the end of the parm lines (after the xxx) that are to be
#    applied for that parameter.  That updated All_uniq_data_types_*_parm.txt
#    file is an input to this create_DBcmds.pl software. determine_GCMD.pl will
#    only work on netCDF files (*.nc or *.cdf) and the first seven chars of the
#    input netCDF files needs to be the CODIAC/Dataset ID (e.g., 534-001).
#
# Execute: 
#    create_DBcmds.pl <input_data_dir> <output_dir> 
#
# Examples: 
#    create_DBcmds.pl ../input_data ../output_data
#
# Input: 
#    <input_data_dir> - directory where *.dataTypes AND All_uniq_data_types_*_parm.txt
#    files for each dataset can be found. This is typically the output directory for the 
#    determine_GCMD.pl program.
#
#    This software also requires the following input files in the same dir as this software:
#       1. all_datasets_id_archive_ident.txt - File relating dataset ID to CODIAC/ZINC internal DB ID.
#       2. all_gcmd_id_uuid.txt - File relating GCMD keywords (as set by sci staff) to GCMD number.
#
# Output: 
#    <output_data_dir> - The location where the *.GCMDcmd files will be located (e.g.,
#    105-004_RF07.20071130.155800_203700.PNI.nc.GMCDcmd, 218-001_RF02.cdf.dataTypes.GCMDcmd, 
#    etc.)
#
# Notes and Assumptions:
#
# 1. The user should search for HARDCODED, ASSUMPTIONS, BEWARE, WARNING, and ERROR in this code. 
#    The user should also search for "exit" in this code so that they know all the possible
#    places/reasons why this code will stop executing.
#
# 2. ASSUMPTION: That the *.dataTypes and All_uniq_data_types_*_parm.txt files exist
#    in the <input_data_dir>.
#
# 3. ASSUMPTION: That the <output_dir> exists.
#
#
# Created: L. Cully Feruary 2020
#
# Updates: None.
#
#--------------------------------------------------------------------------------
package create_DBcmds;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
} else {
    use lib "/work/lib/perl/Utilities";
}


my $debug  = 1; # BEWARE: Generates output to screen
my $debug2 = 0; # BEWARE: Generates output to screen

my $TotalFilesProcessed = 0;
my $OutputRec = 0;
my $parm;

printf "\ncreate_DBcmds.pl began on ";print scalar localtime;printf "\n\n";
&main();
printf "\ncreate_DBcmds.pl ended on ";print scalar localtime;printf "\n";


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
      print "Usage: create_DBcmds.pl <input_dir> <output_dir> <name_of_GCMD_ID_UUID_file>\n";
      exit(1);
      }

   my $INPUT_netCDF_DIR  = $ARGV[0];
   if ($debug) {print "INPUT_netCDF_DIR = $INPUT_netCDF_DIR \n";}

   my $OUTPUT_DIR  = $ARGV[1];
   if ($debug) {print "OUTPUT_DIR = $OUTPUT_DIR \n";}

   #---------------------------------------------------------------
   # Read in the list of dataTypes files to process (*.dataTypes).
   #---------------------------------------------------------------
#   printf "Opening INPUT_netCDF_DIR:: $INPUT_netCDF_DIR\n";
#   opendir(my $INPUT_DIR, $INPUT_netCDF_DIR) or die("Cannot open INPUT_netCDF_DIR\n");
#   my @files = grep(/\.dataTypes$/,readdir($INPUT_DIR));      # HARDCODED
#   closedir($INPUT_DIR);

   # ----------------------------------------------------------------------------
   # Open the "ALL_HERE" output file that contains all compound SQL cmds
   # for all files processed. This is the single file that can be "executed"
   # to apply all GCMD keywords for all datasets included in the input directory.
   # ----------------------------------------------------------------------------
   my $all_GCMD_cmds_file = sprintf("%s/%s.txt", $ARGV[1],"all_GCMD_commands" );
   if ($debug) {print "OPEN all_GCMD_cmds_file for writing: zzz $all_GCMD_cmds_file zzz\n";}

   open(OUTPUT_ALL_GCMD_CMDS_FILE,">", $all_GCMD_cmds_file) or die("Can't open $all_GCMD_cmds_file for writing.\n");

   #----------------------------------------------------
   # Read in info from file that relates GCMD IDs to
   # UUID's. These next files are assumed to be located
   # in same dir as this s/w.
   #---------------------------------------------------
   my $input_file = "./all_gcmd_id_uuid.txt"; # HARDCODED
   print "input_file = $input_file\n";

   my $INPUT_GCMD_RELATION_FILE;

   open($INPUT_GCMD_RELATION_FILE,"<", $input_file) or die("Can't open $input_file for reading\n");

   my @lines = <$INPUT_GCMD_RELATION_FILE>;
   my $number_lines_in_file = $#lines+1;
   my %Input_UUID_GCMDID = ();  # Clear Hash

   print "Read into Hash the info from INPUT_GCMD_RELATION_FILE. File has $number_lines_in_file lines.\n";

   foreach my $line (@lines)
      {
      chomp ($line); #remove return \n
      if ($debug2) {print "\nUUID vs GCMD ID RELATION FILE Line: zzz $line zzz\n";}

      my @record = split (/\|/, $line);
      my $key_ID = trim($record[1]);  # This is the UUID associated with GCMD keywords
      my $DB_GCMD_ID = trim($record[0]);

      if ($debug2) {print "UUID key_ID = $key_ID; DB_GCMD_ID = zzz $DB_GCMD_ID zzz\n";}

      $Input_UUID_GCMDID{$key_ID} = $DB_GCMD_ID;
      }

   if ($debug)
      {
      print "\n*****Input Hash UUID vs GCMD IDs *****\n";
      print "\nInput_UUID_GCMDID Hash Contents::\n   \n";
      foreach my $key (sort keys %Input_UUID_GCMDID) 
         {
         print "$key relates to $Input_UUID_GCMDID{$key}\n";
         }
      }


   #----------------------------------------------------
   # Read in info from file that relates datasetID to
   # DB internal ID... aka archive Ident.
   #----------------------------------------------------
   $input_file = "./all_datasets_id_archive_ident.txt";
   print "\ninput_file = $input_file\n";

   my $INPUT_DATASET_ID_RELATION_FILE;

   open($INPUT_DATASET_ID_RELATION_FILE,"<", $input_file) or die("Can't open $input_file for reading\n");

   @lines = <$INPUT_DATASET_ID_RELATION_FILE>;
   $number_lines_in_file = $#lines+1;
   my %Input_DatasetID_archID = ();  # Clear Hash

   print "Read into Hash the info from INPUT_DATASET_ID_RELATION_FILE. File has $number_lines_in_file lines.\n";

   foreach my $line (@lines)
      {
      chomp ($line); #remove return \n
      if ($debug2) {print "\nDATASET ID RELATION FILE Line: zzz $line zzz\n";}

      my @record = split (/\|/, $line);  # Split on vertial bar and add to hash.
                                         # DatasetID will be the hash key.
      my $key_ID = trim($record[1]);
      my $archID = trim($record[0]);

      if ($debug2) {print "DatasetID key_ID = $key_ID; archID = zzz $archID zzz\n";}

      $Input_DatasetID_archID{$key_ID} = $archID;
      }

   if ($debug)
      {
      print "\n*****Input Hash DatasetIDs vs Archive Idents *****\n";
      print "\nInput_DatasetID_archID Hash Contents::\n   \n";
      foreach my $key (sort keys %Input_DatasetID_archID)
         {
         print "$key relates to $Input_DatasetID_archID{$key}\n";
         }
      }

   #------------------------------------------------------
   # Read in the file created by sci/DM staff that relates
   # specific parms/variables to GCMD keywords.
   #------------------------------------------------------
   $input_file = $ARGV[2]; # Expect similar to: ./gcmd_lrt_c130.txt
   print "\ninput_file = $input_file\n";

   my $INPUT_PARM_MATCH_GCMD_FILE;

   open($INPUT_PARM_MATCH_GCMD_FILE,"<", $input_file) or die("Can't open $input_file for reading\n");

   @lines = <$INPUT_PARM_MATCH_GCMD_FILE>;
   $number_lines_in_file = $#lines+1;
   my %Input_parm_match_GCMD = ();  # Clear Hash

   print "Read into Hash the info from INPUT_PARM_MATCH_GCMD_FILE. File has $number_lines_in_file lines.\n";


   my $parm_keyword_string = ""; # This is a string of all the translated
                                 # GCMD keywords for a particular parm/variable
                                 # separated by commas. This string is used
                                 # to form the final SQL command. See sample
                                 # in comment above. This string would be
                                 # "1956,405,404,415,3034,413,479,392,1947,1942,623,1139,3069,433".
   foreach my $line (@lines)
      {
      chomp ($line); #remove return \n
      if ($debug) {print "\n----------\nPARM Match GCMD FILE Line: zzz $line zzz\n";}


      # Pick out the parm/variable name to match with GCMD keywords
      my @record = split (/xxx/, $line);           # HARDCODED as 'xxx'
      my @record_tab = split (/	/, $record[0]);    # HARDCODED as a tab
      $parm = $record_tab[0];

      if ($debug) {print "parm = zzz $parm zzz\n";}

      # Pick out the GCMD "code number" to match with those in hash table
      my @record_yyy = split (/yyy/, $record[1]);  # HARDCODED as 'yyy'
      my $number_GCMD = $#record_yyy+1;

      if ($debug2) {print "\nrecord[1]: zzz $record[1] zzz\n";}
      if ($debug) {print "\nParm: $parm has $number_GCMD GCMD keywords.\n";}

####      if ($debug && ( $number_GCMD > 1)) {print "\nArray record_yyy[]:\n zzz $record_yyy[0] zzz\n zzz $record_yyy[1] zzz\n zzz $record_yyy[2]\n zzz\n";}

      my $char = "\",";
      my $string = trim($record[1]);
      if ($debug2) {print "\nchar = zzz $char zzz  string: zzz $string zzz\n";}

      # -----------------------------------------------------------------------
      # Some parm lines may only have 1 GCMD keyword so must detech that versus
      # either empty or bunch of tabs at end of lines.
      #
      # -----------------------------------------------------------------------
      # -----------------------------------------------------------------------
      # From CBS: Here is the example query for doing things "the other way around."
      #
      # INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id) 
      # SELECT 122, g.id FROM gcmd_science_keyword g WHERE g.id IN (1956,405,404,415,3034,413,
      #      479,392,1947,1942,623,1139,3069,433);
      #
      # In this example, the 122 is the dataset ID (it corresponds to hidden test 
      # dataset 999.98) and the list of IDs in parentheses (1956,405,404,415,3034,
      # 413,479,392,1947,1942,623,1139,3069,433) are GCMD keyword IDs.
      # -----------------------------------------------------------------------
      my $has_GCMD = 0;  
      if ( $number_GCMD > 1)
         {
         $has_GCMD = 1;
         if ($debug) {print "More than one GCMD keyword. YES GCMD.\n"; }
         } # Has more than one GCMD keyword
      elsif ( (index($string, $char)) != -1 ) 
         { 
         $has_GCMD = 1; 
         if ($debug) {print "Found double quote and comma! YES GCMD.\n"; }
         } # Has at least one GCMD keyword
     

      $parm_keyword_string = ""; 

      #------------------------------------------------------------
      # Don't add last comma in SQL insert string. Causes problems.
      #------------------------------------------------------------
      my $char_sep = "";

      if ($has_GCMD)
         {
         foreach my $Gkey (@record_yyy)
           {
           if ($debug2) {print "\nGkey = zzz $Gkey zzz\n";}
     
           my @GCMD_keyword = split (/,/, $Gkey); # Split on commas & grab last element to translate

           my $GCMD_code = $GCMD_keyword[$#GCMD_keyword];
           if ($debug2) {print "\n(1)GCMD code is zzz $GCMD_code zzz\n";}

           $GCMD_code =~ tr/"//d;  # Remove all the double quotes - at begin/end
           $GCMD_code = trim($GCMD_code);  # Remove all the double quotes - at begin/end

           if ($debug) {print "\n(2)GCMD code is zzz $GCMD_code zzz\n";}

           # Translate the GCMD code into the GCMD number
           if (exists ($Input_UUID_GCMDID{$GCMD_code}) )
              {
###WAS:       $parm_keyword_string = $Input_UUID_GCMDID{$GCMD_code}.','.$parm_keyword_string;
#
              $parm_keyword_string = $Input_UUID_GCMDID{$GCMD_code}.$char_sep.$parm_keyword_string;
              $char_sep = ",";
              
              if ($debug) {print "HASH Translate:: zzz $Input_UUID_GCMDID{$GCMD_code} zzz\n";}
              if ($debug) {print "Building:: parm_keyword_string = zzz $parm_keyword_string zzz\n";}
              }
           else
              {
              if ($debug) {print "\nSkipping GCMD_code = $GCMD_code NOT FOUND in HASH\n";}
              }
           } # foreach Gkey
        } # Only for parms that have keywords defined at end of line 'tween yyy's
     else
        {
        if ($debug) {print "\n Parm $parm DOES NOT have any GCMD keywords Defined.\n";}
        $parm_keyword_string = ""; 
        }

      if ($debug) {print "\nTOTAL:: parm_keyword_string = zzz $parm_keyword_string zzz\n";}
      # Assign the string of GCMD keywords for parm into hash
      if ($parm_keyword_string ne "")
         {
         $Input_parm_match_GCMD{$parm} = $parm_keyword_string;
         }
      else
         {
         $Input_parm_match_GCMD{$parm} = "null";
         }

      if ($debug) {print "ASSIGN:: Input_parm_match_GCMD{$parm} = zzz $parm_keyword_string zzz\n";}

      } # foreach line

   if ($debug)
      {
      print "\n*****Input Hash parms vs GCMD keyword string *****\n";
      print "\nInput_parm_match_GCMD Hash Contents::\n   \n";
      foreach my $key (sort keys %Input_parm_match_GCMD)
         {
         print "$key relates to $Input_parm_match_GCMD{$key}\n";
         }
      }

   #**************************************************************
   #**************************************************************
   # All input and tables now in hashes.
   # Write out sql commands for each file which is by dataset.
   #
   # Process all the *.dataTypes files in the output directory:
   # e.g., 103-004_RF12.20070902.180000_021000.PNI.nc.dataTypes
   #**************************************************************
   #**************************************************************
   printf "\n-----------------\nReading list of dataTypes files from OUTPUT_dir: $OUTPUT_DIR\n-----------------\n";
   opendir(my $OUT_DIR, $OUTPUT_DIR) or die("Cannot open OUTPUT_DIR = $OUTPUT_DIR\n");
   if ($debug) {print "OUTPUT_DIR opened. Read *.dataTypes files.\n";}

   my @files = grep(/\.dataTypes$/,readdir($OUT_DIR));      # HARDCODED
   if ($debug) {print "There are $#files dataTypes files. dataTypes files: @files\n\n";}

###   closedir($OUT_DIR);

   #-----------------------------------------------------------------
   # Process every dataTypes file (*.dataTypes) in the output directory.
   # This is the output from the determine*.pl software.
   # Create the matching *.gcmd output file in that same output dir.
   # The dataTypes files contains a uniq set of parms found in a
   # specific dataset. The dataset ID is in the first few chars of
   # the file name. Generate the SQL commands to apply to that dataset.
   #
   # INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id)
   # SELECT 122, g.id FROM gcmd_science_keyword g WHERE g.id IN (1956,405,404,415,3034,413,
   #      479,392,1947,1942,623,1139,3069,433);
   #
   # In this example, the 122 is the dataset ID (it corresponds to hidden test
   # dataset 999.98) and the list of IDs in parentheses (1956,405,404,415,3034,
   # 413,479,392,1947,1942,623,1139,3069,433) are GCMD keyword IDs.
   #
   #-----------------------------------------------------------------
   foreach my $file (sort(@files))
      {
      #----------------------------------------------------------
      # Pick dataset ID number out of file name e.g., 103.004 for
      # file 103-004_RF12.20070902.180000_021000.PNI.nc.dataTypes
      #----------------------------------------------------------
      my @file_parts = split (/_/, $file);  # HARDCODED as underscore
      my $ID = $file_parts[0];  # 103-004   

      if ($debug) {print "Dataset ID = zzz $ID zzz\n";}
      $ID =~ s/\-/\./;
      if ($debug) {print "(2)Dataset ID = zzz $ID zzz\n";}

      my $archIdent = " ";
      $archIdent = $Input_DatasetID_archID{$ID}; # Relate datasetID to archIdent for SQL command.
      if ($debug) {print "archIdent = zzz $archIdent zzz\n";}

      #--------------------------------------------------------------------------
      # Open the input file e.g.,103-004_RF12.20070902.180000_021000.PNI.nc.dataTypes
      # that lists are uniq parms for that dataset. Read each line/parm and find the
      # associated GCMD keyword(s). Create a string of all those keywords and then
      # write out the SQL command for that parm into output file for that dataset.
      # E.g.,103-004_RF12.20070902.180000_021000.PNI.nc.gcmd will list all the SQL
      # commands that need to be applied to dataset 103.004.
      #--------------------------------------------------------------------------
      @file_parts = split (/dataTypes/, $file);  # HARDCODED

      # Open the output file that will contain all the SQL GCMD commands for that dataset.
      my $SQLfile = $ARGV[1]."/".$file_parts[0]."gcmd";
      if ($debug) {print "SQLfile = zzz $SQLfile zzz\n";}

      my $INPUT_DATASET_PARMS_FILE;

      print "Open Input dataTypes and output gcmd files. Add SQL cmds.  \n";
      open(OUTPUT_DATASET_SQL_FILE,'>', $SQLfile) or die("Can't open $SQLfile for writing\n");

      my $datatypes_file = $ARGV[1]."/".$file;

      # Open the input file that has one parm per line for that specific dataset.
      open($INPUT_DATASET_PARMS_FILE,"<", $datatypes_file) or die("Can't open $datatypes_file for reading\n");

      my @PARM_lines = <$INPUT_DATASET_PARMS_FILE>;
      print "PARM_lines: @PARM_lines  \n";


      if ($debug) {print "Process each parm line in dataTypes file.\n";}
      foreach my $line (@PARM_lines)
         {
         if ($debug2) {print "line = zzz $line zzz\n";}

         # Skip the first header line
         my $loc = index($line, "-------Parms found in File:");  # -1 = hdr line not found so process line
         next if $loc != -1;  # Skip header line equal first line in file

         if ($debug) {print "------------\n(11)Processing:: line = zzz $line zzz\n";}

##         chomp ($line); #remove return \n
         if ($debug2) {print "(22)PARM Line: zzz $line zzz\n";}

         my @record = split (/\,/, $line); # HARDCODED - 1st element in line is parm
         my $parm = trim($record[0]);  # This the parm/variable
         if ($debug) {print "(33)parm = zzz $parm zzz. Find GCMD keywords to apply!\n";}


        ### Do not write sql command if the GCMD code is null!
         my $GCMD_codes_for_parm = "unknown";
         if ($debug) {print "(44)Reset GCMD codes: zzz $GCMD_codes_for_parm zzz\n";}
         if ($debug) {print "(44-B)Input_parm_match_GCMD{$parm}: zzz $Input_parm_match_GCMD{$parm} zzz\n";}

         if ($Input_parm_match_GCMD{$parm} ne "null")
            {
            if ($debug) {print "Hash $Input_parm_match_GCMD{$parm} DOES NOT EQUAL null.\n";}

            my $GCMD_codes_for_parm = $Input_parm_match_GCMD{$parm};
            if ($debug) {print "(55)parm = zzz $parm zzz. GCMD codes: zzz $GCMD_codes_for_parm zzz\n";}

            #-------------------------------------------------------------------------------------
            # Write SQL command for that parm to *.gcmd file for that dataset.
            # The form of the sql cmd is as follows:
            #
            # INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id) 
            # SELECT 122, g.id FROM gcmd_science_keyword g WHERE g.id IN (1956,405,404,415,3034,
            # 413,479,392,1947,1942,623,1139,3069,433);
            #
            # where 122 is the archIdent/dataset ID and the list of GCMD keyword
            # IDs are in the long list of numbers at the end of the INSERT cmd.
            #
            #--------------------------------------------------------------------------------------
            my $sql_cmd = " ";
            if ($debug) {print "(66)RESET:  sql_cmd: zzz $sql_cmd zzz\n";}

###            $sql_cmd = sprintf("%s %7.3f %s %s %s", "INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id) SELECT", $archIdent, ", g.id FROM gcmd_science_keyword g WHERE g.id IN (", $GCMD_codes_for_parm, ")\n");  # HARDCODED

            $sql_cmd = "INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id) SELECT ".$archIdent. ", g.id FROM gcmd_science_keyword g WHERE g.id IN (" .$GCMD_codes_for_parm. ")\n";  # HARDCODED

            if ($debug) {print "(77)SQLcmd: zzz $sql_cmd zzz.  Print to output file. \n";}

            print OUTPUT_DATASET_SQL_FILE  $sql_cmd;

            } # GCMD code is not "null. Skip it - No Insert cmd to *.gcmd file."
         else
            {
            if ($debug) {print "GCMD code is null. Skip it - No INSERT cmd to datasetID gcmd file.\n";}
            }

         } # foreach line

      # -----------------------------------------------
      # Since many of the parms/variables have the
      # same GCMD keywords, the *.gcmd output file
      # can end up with multiple, identical insert
      # cmds.  So "sort" and "uniq" the cmds to form
      # a unique set that could be applied per dataset.
      # -----------------------------------------------
      my $syscmd = sprintf("sort  %s > %s.s", $SQLfile, $SQLfile );
      print "\nExecute SORT system cmd:: $syscmd\n";
      system ($syscmd); # sort sql insert cmd file

      $syscmd = sprintf("uniq  %s.s > %s.su", $SQLfile, $SQLfile );
      print "Execute UNIQ system cmd:: $syscmd\n";
      system ($syscmd); # uniq sql insert cmd file

      $syscmd = sprintf("/bin/rm %s.s", $SQLfile, $SQLfile );
      print "Execute REMOVE sort system cmd:: $syscmd\n"; 
      system ($syscmd); # remove the sorted but not uniq'd file

      # -------------------------------------------------------------------
      # Even sorting and uniq'ing a file appears to sometimes generate
      # duplicate UUIDs in insert cmds. For instance, parm1 has UUID
      # translated to 419 in the EMDAC DB.  Then parm2 for the same 
      # dataset has 2 UUIDs "419,3024". The code above would generate
      # 2 sql insert cmds, one for each parm. The sort/uniq above would
      # not separate the two UUIDs so we would try to insert 419 and 3024
      # twice. Supposedly this is not an issue and will not cause problems
      # when the sql are execute. Sample insert cmd:
      # INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, 
      #   gcmd_science_keyword_id) SELECT 7911, g.id FROM 
      #   gcmd_science_keyword g WHERE g.id IN (3071,3077,419,452)
      # -------------------------------------------------------------------
      my $gcmdSU_file = sprintf("%s.su", $SQLfile );
      if ($debug) {print "OPEN gcmdSU_file = zzz $gcmdSU_file zzz\n";}

      open(INPUT_GCMDSU_FILE,"<", $gcmdSU_file) or die("Can't open $gcmdSU_file for reading\n");
      
      my @ins_lines = <INPUT_GCMDSU_FILE>;
      my $ins_number_lines_in_file = $#ins_lines+1;

      my $all_gcmd = "";
      my $ins_datasetID = "null";
      my $char_sepAll = "";

      print "Read sql cmd lines from sorted/uniq'd file. File has $ins_number_lines_in_file lines.\n";
      foreach my $ins_line (@ins_lines)
         {
         my @ins1 = split(/SELECT /, $ins_line);
         my @ins2 = split(/, /, $ins1[1]);
         $ins_datasetID = trim($ins2[0]);  # These are the gcmd codes separated by commas

         if ($debug) {print "ins_datasetID = zzz $ins_datasetID zzz\n";}

         @ins1 = split(/IN \(/, $ins_line);
         my $ins_gcmd = $ins1[1]; 
         if ($debug) {print "(001 after spliti ins_line)ins_gcmd:: $ins_gcmd\n";}

         $ins_gcmd =~ s/\)//g;  # rm off trailing paren
         if ($debug) {print "(002 After s/\ )ins_gcmd:: $ins_gcmd\n";}

         $ins_gcmd = trim($ins_gcmd);  # These are the gcmd codes separated by commas

         if ($debug) {print "(003 After trim)ins_gcmd:: $ins_gcmd\n";}

###      if ($ins_gcmd ne ""){$all_gcmd = $ins_gcmd.",".$all_gcmd;}
###      if ( (index($string, $char)) != -1 )
  
         my $ins_index = (index($all_gcmd, $ins_gcmd));

         if ($debug) {print "(111)ins_gcmd = zzz $ins_gcmd zzz. all_gcmd = zzz $all_gcmd zzz\n";}
         if ($debug) {print "(222)INDEX:: $ins_index\n";}

###      if ((index($all_gcmd, $ins_gcmd)) == -1){$all_gcmd = $ins_gcmd.",".$all_gcmd;}

### WAS  if ($ins_index == -1){$all_gcmd = $ins_gcmd.",".$all_gcmd;}
         if ($ins_index == -1){$all_gcmd = $ins_gcmd.$char_sepAll.$all_gcmd;}
         $char_sepAll = ',';

####No work     $ins_gcmd =~ s/,//g;  # rm off trailing comma
         if ($debug) {print "ins_gcmd = zzz $ins_gcmd zzz. all_gcmd = zzz $all_gcmd zzz\n";}
         }

      if ($debug) {print "SINGLE INSERT CMD: ins_datasetID = zzz $ins_datasetID zzz...zzz $all_gcmd zzz\n";}

      my $ins_sql_cmd = "INSERT IGNORE INTO dataset_gcmd_science_keyword (dataset_id, gcmd_science_keyword_id) SELECT ".$ins_datasetID. ", g.id FROM gcmd_science_keyword g WHERE g.id IN (" .$all_gcmd. ")\n";  # HARDCODED

      close (INPUT_GCMDSU_FILE);
      open(INPUT_GCMDSU_FILE,">>", $gcmdSU_file) or die("Can't open $gcmdSU_file for writing SINGLE cmd to end of file.\n");

      if ($debug) {print "WRITE SINGLE INSERT CMD to end of $gcmdSU_file: $ins_sql_cmd\n";}
      print INPUT_GCMDSU_FILE  $ins_sql_cmd;

      if ($debug) {print "WRITE ALL GCMD INSERT CMD to separate file: OUTPUT_ALL_GCMD_CMDS_FILE: $ins_sql_cmd\n";}
      print OUTPUT_ALL_GCMD_CMDS_FILE  $ins_sql_cmd;

      } # foreach DataTypes file

   if ($debug) {print "----------------------------NEXT FILE--------------------------\n";} # uniq sql insert cmd file
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
