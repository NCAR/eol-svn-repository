#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The PISA_FP2_GREEN_sounding_converter.pl script is used for converting high
# resolution radiosonde data from ASCII to the modified EOL Sounding Composite (ESC) 
# format. PISA stands for the PECAN (2015) field project Sounding Array or
# "PECAN Integrated Sounding Array (PISA)". Note there there are 3 fixed PISA sites
# in the PECAN (Hays, KS) field project:  FP2 - GREEN;Greensburg, KS; 
# FP3 - ELLIS;Ellis, KS; and FP6 - HESS;Hesston, KS.  This is the converter for
# the Greensburg, KS data.   Note if you see UMBC associated with this dataset, 
# it stands for the University of Maryland, Baltimore County.  FP2 stands for
# Fixed PISA site #2. Also, sometimes HU or Howard University is associated with this site. 
# SGP stands for Southern Great Plains.
#  
# The incoming format is Vaisala FLEDT format variant (First was: Vaisala RS92 now is Vaisala RS41). 
# Since this format is very similar to the DYNAMO Male Sounding format, this code is strongly based
# on that software which was also used to convert data and put onto the GTS. The goal of
# this software is to do the same. Take incoming data from a specified incoming
# location, converted to ESC so that ASPEN could be used to check, then the converted
# data would be transmitted/sent to the GTS in near realtime. 
#
# The output format is basically ESC, except the first header line of "Data Type:" is output 
# to be "CLASS,". This is so that the output format can be read by ASPEN
# as a class formatted sounding. ASPEN is then used to convert the CLASS/ESC
# output sounding into GTS format. 
#</p> 
#
# @author Linda Cully 2015-05-11
# @version PECAN_2015 version created from the DYNAMO_2011 Male converter.
#    - Changed all references in code from Male (Maldives) to PISA FP2 GREEN
#      for the PECAN 2015 project.
#
#---------------------------------
# Note that this converter is STRONGLY based on the DYNAMO Male, Maldives data
# conversion software so comments from that conversion may also be applicable.
#
# Note that Linda Cully on 2011-10-13 did the following to the Male converter:
#  Updated to check number of lines in error file. Updated to check number of lines 
#  in error file. This helps catch embedded junk such as binary or emails in data. For cron
#  scripts is it very important to completely spell out all directory path names. 
#
# Note that the Male, Maldives raw data were in Vaisala "Digicora 3" format. The file contains
#  header info on lines 1-39. Actual data starts on line 40. The raw data file names were in
#  the form "43555_yyyymmddhhmm.tsv" where yyyy = year, mm = month, dd = day, hh=hour,
#         mm = minute. 43555 is the call sign for Male Maldives.
# (Note that the PISA data has one extra header line with the addition of the El variable.)
#---------------------------------
# This PISA code makes the following assumptions:
#
#  - That the raw data file names shall be in the form
#      WAS::  "HUBV_RS92SGP_yyyymmdd_hhmmUT.dat" where yyyy = year, mm = month, dd = day, hh=hour,
#      NOW::  "UMBC_RS41_yyyymmdd_hhmmUT.dat" where yyyy = year, mm = month, dd = day, hh=hour,
#         mm = minute.
#  - That the raw data are in the Vaisala FLEDT format variant. The file contains
#         header info on lines 1-40. Actual data starts on line 41. 
#  - That the incoming raw data are located in the 
#         /net/iftp2/pub/incoming/pecan/FP2_Greensburg  directory.
#  - That the outgoing Class soundings created by this s/w should be placed in
#         /h/eol/iss/project/pecan/data/fp2/cls   directory.
#  - That the following directories exist: ../output, ../archive.
#
#-------------------------------------------------------------- 
# This code will NOT send a sounding to the GTS directory if any of the following are true:
#
#  - If the surface lat or lon (from first record) values do NOT match HARDCODED values for Greensburg, KS.
#  - If the elevation/altitude (from first record) value does NOT match HARDCODED values for Greensburg, KS.
#      NOTE: S. Loehrer has asked that the elevation/altitude check be commented out for now. Search for
#            "ELEVATION CHECK" to find the code to uncomment it. 
#  - If the raw, input file is "empty" (i.e., no header or records).
#  - If there are NO DATA records in raw file.
#  - No valid data on any records. Possibly no header lines.
#  - If either of the lat/lon values on the first record are missing.
#  - If the code can not create a output sounding header for any reason.
#  - If all Pressure values are MISSING.
#  - If all Altitudes/Heights are MISSING in all data records.
#  - If pressure is never less than 850.00 MB. 
#  - Note: There are other errors that can come from library code that are also written to the 
#    error log. Any of these errors will also cause the sounding to NOT be sent to the GTS. 
#    In other words, if the error log file has any size other than zero, then the sounding
#    is NOT sent to the GTS. The user must review the errors first. An example of an error of this type
#    is, "Ascension Rate value -281.115 is too big for the field at time 2.00.  Setting to missing."
#
#  Note that even if the file can not be processed, a header only (if possible) Class file
#  will be generated. That file will be moved to the archived directory and based on its size
#  the user can easily tell that it's a header only file. 
#
#----------------------------------------------------------------
#
# Note the following:
#  - This code computes the ascension rate, even though this is not required. 
#  - Samples received did NOT contained a blank line at the end of the data. But
#      blank lines at end of file are ignored.
#
# BEWARE:  Search for "HARDCODED" to find project-specific items that may
#          need to updated.
#
# Cron info: This task will be run as a cron script. As of approx. 30 Sept 2011, 
#     SL will add to his merlot/local crontab to run this script frequently (every 10 mins)
#     To edit (using vi) the crontab do "crontab -e". Add in the following
#     single line to have this script run every 10 mins on the 10's (e.g., 01:10,
#     01:20, 01:40, 01:50, etc.).
#
#     */10 * * * * /net/work/Projects/pecan/upper_air/PISA_FP2_GREEN/production/
#     software/PISA_FP2_GREEN_sounding_converter.pl > /dev/null 2>&1   
#
#     To comment out a line in the crontab, place a # in front of the line.
#
#     The /dev/null 2>$1 is added to dump any output messages to the bit bucket.
#     Removing this will cause cron messaging to be active. 
#
#     Note that this executes the production version of the s/w which has the 
#     full path names for all dirs included. Not the test/processing version.
#
# ****************************************************************************
# BEWARE: Original Male, Maldives script ran under S. Loehrer's crontab and 
# it can only be changed through his login or by SIG. 
# ****************************************************************************
#
##Module------------------------------------------------------------------------
package PISA_FP2_GREEN_sounding_converter;
use strict;

use Mail::Mailer;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
}
 
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

my ($OUT);
my ($ERROUT);

my $filename;
my $errfileName;
my $outfileName;

my $errText= "";

my $rawDirName;
my $rawWorkDirName;
my $outputDirName;
my $archiveDirName;
my $gtsDirName;

my $TotalRecProc = 0;
my $dataRecProc = 0;

#-------------------------------------------------------------
# Set following value to 1 to have code verify that altitude 
# in data matches expected HARDCODED station value. Search
# for the phase "COMMENTED OUT" to find the check. Stn Elev
# is checked against HARDCODED value. If diff is > 20m, then
# an error is issued preventing class file from going to GTS.
#-------------------------------------------------------------
my $checkAltitudeDiff =0; 

#----------------------------------------------------------------
# Set either debug value to 1 for varying amounts of debug.
# BEWARE: These both must be set to zero (0) for production runs!
#-----------------------------------------------------------------
my $debug = 0;   # higher level debug output
my $debug2 = 0;  # detailed debug output

if ($debug2) {printf "\nPISA_FP2_GREEN_sounding_converter.pl began on ";print scalar localtime;printf "\n";}
&main();
if ($debug2) {printf "\nPISA_FP2_GREEN_sounding_converter.pl ended on ";print scalar localtime;printf "\n";}

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the PISA_FP2_GREEN radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = PISA_FP2_GREEN_sounding_converter->new();
    $converter->convert();
} # main

##------------------------------------------------------------------------------
# @signature PISA_FP2_GREEN_sounding_converter new()
# <p>Create a new instance of a PISA_FP2_GREEN_sounding_converter.</p>
#
# @output $self A new PISA_FP2_GREEN_sounding_converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    # HARDCODED
    $self->{"PROJECT"} = "PECAN";
    $self->{"NETWORK"} = "Fixed_PISA";
   
    #--------------------------------------------------------------
    # Input/ingest directories. Data to convert.
    #
    # Only process the most recent files in the FTP dir. 
    # This code will copy those recent files from the FTP space 
    # to the /raw_data processing area. The s/w will only process
    # the raw data files that have been copied from the FTP dir to 
    # the "work" raw_data directory.  
    #
    # WAS:  $self->{"RAW_DIR"} = "../raw_data";
    # (Use the full path names because this code will be run 
    # by a cron script.)
    #
    # rawDirName = FTP space where field data are FTP'd to.
    #
    # rawWorkDirName = Dir where this code expects files to process. 
    # Files are copied from rawDirName to rawWorkDirName.
    #---------------------------------------------------------------
    
    #-----------------------------
    #Input directories. HARDCODED
    #-----------------------------
    $rawDirName = "/net/iftp2/pub/incoming/pecan/FP2_Greensburg";                      # FINAL incoming FTP area

### $rawDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/processing/fake_FTP";  # TEST FTP area

    $rawWorkDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/production/raw_data"; # FINAL/Local work area with files to process

### $rawWorkDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/processing/raw_data"; # TEST/Local work area with files to process

    $self->{"RAW_DIR"} = $rawDirName;
    $self->{"RAW_WORK_DIR"} = $rawWorkDirName;

    #------------------------------
    # Output directories. HARDCODED
    #------------------------------
    $outputDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/production/output";  # FINAL working dir

### $outputDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/processing/output";  # TEST working dir

    $archiveDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/production/archive"; # FINAL archive copy of all raw, CLASS and log files

### $archiveDirName = "/net/work/Projects/PECAN/upper_air/FP2_GREEN/processing/archive"; # TEST archive copy of all raw, CLASS and log files

    $gtsDirName =  "/h/eol/iss/project/pecan/data/fp2/cls";                             # FINAL GTS area to place converted *.cls file

### $gtsDirName =  "/net/work/Projects/PECAN/upper_air/FP2_GREEN/production/fake_gts";  # TEST GTS area 

    $self->{"OUTPUT_DIR"} = $outputDirName;
    $self->{"ARCHIVE_DIR"} = $archiveDirName;
    $self->{"GTS_DIR"} = $gtsDirName;

    return $self;
} # new


##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});

    $self->readDataFiles();
} # convert()

##------------------------------------------------------------------------------
# @signature ClassHeader parseHeader(String file, String[] lines)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parseHeader {
    my ($self,$file,@headerlines) = @_;
    my $header = ClassHeader->new();

    my $firstDataLine = 0;

    $filename = $file;
    if ($debug2) {printf("parsing header for %s\n",$filename);}
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("CLASS");
    $header->setProject($self->{"PROJECT"});
    
    # HARDCODED
    # The Id will be the prefix of the output file
    $header->setId("GREEN");

    # Site info received from SL
    # "Release Site Type/Site ID:" header line
    $header->setSite("GREEN;Greensburg, KS");

    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
    my $index = 0;
    foreach my $line (@headerlines) 
       {
       if ($debug2) {print "parseHeader:: (index = $index); line: xxx $line xxx \n";}

       # -----------------------------------------------------------
       # Add the non-predefined header lines to the header.
       # Changed $i to $i-1 to remove extra blank line from header. 
       # for (my $i = 6; $i < 11; $i++) 
       # -----------------------------------------------------------
       if (($index > 0) && ($index < 11))
          {
          if ($line !~ /^\s*\/\s*$/) 
             {
             if ($line =~ /RS-Number/i)
                {
                chop ($line); chop ($line); # Trim control M/EOL char

                my ($label,@contents) = split(/:/,$line);
                $label = "Sonde Id/Sonde Type";
                $contents[1] = "Vaisala RS41";

                $header->setLine(($index-1), trim($label).":",trim(join("/",@contents)));
                } # RS-Number
             } #
          } # index/line 1-10

       #--------------------------------------
       # Ignore the rest of the header lines.     HARDCODED
       #--------------------------------------
       if ($index < 40) 
          { 
          if ($debug) {print "If index < 40...processed header line. index = $index\n"} 

          $index++; 
          next; 
          } # Header lines 1-40

       #--------------------------------------------------------------
       # Process the DATA lines starting at line 40 in the raw data.
       #--------------------------------------------------------------
       # Find the lat/lon for the release location in the actual data.
       # Pull out of first data line. HERERERE
       #---------------------------------------------------------------
       else # Data lines - Found FIRST data line
          {
          if ($debug2) {print "\n\n***********FOUND Data Line!************\n (firstDataLine = $firstDataLine\n";}

          if ($firstDataLine == 0) # Only extract info from first data line; Skip all other data lines 
             {
             $firstDataLine = 1; 

             my @data = split(' ',$line);

             if ($debug2) {print "data: @data\n"; print "Lon = Data(15): xxx $data[15] xxx\n"; print "Lat = Data(16): xxx $data[16] xxx\n";}
             if ($debug2) {print "Alt (Height) = Data(6): xxx $data[6] xxx\n";}

             #----------------------------------------------------------
             # If the lat/lon on first record (surface) do NOT match
             # the hardcoded, specified lat/lon for Greensburg, KS, then
             # do NOT send this sounding to the GTS. Write an error
             # to the error log for users to review. OK to complete
             # processing of sounding into Class format just don't send
             # to GTS.
             # ----------------------------------------------------------
             my $GreensburgLat =  37.61; # HARDCODED: SL says use 37.606 but raw data accurate to 2 digits
             my $GreensburgLon = -99.28; # HARDCODED: SL says use -99.276 but raw data accurate to 2 digits
             my $GreensburgAlt = 681.00; # HARDCODED: SL says use 681.00 but raw data accurate to 1 digits

# NEXT set is for testing the only sample sent from source. Used for original/only sample received.
##             my $GreensburgLat =  39.06; # HARDCODED: SL says use 37.606 but raw data accurate to 2 digits
##             my $GreensburgLon = -76.88; # HARDCODED: SL says use -99.276 but raw data accurate to 2 digits
##             my $GreensburgAlt =  52.30; # HARDCODED: SL says use 681.00 but raw data accurate to 1 digits


             my $absLatDiff = abs($data[16]-$GreensburgLat);
             my $absLonDiff = abs($data[15]-$GreensburgLon);
             my $absAltDiff = abs($data[6]-$GreensburgAlt);

             if ($debug2) {print "absLatDiff = $absLatDiff; absLonDiff= $absLonDiff; absAltDiff= $absAltDiff\n";} 

             # --------------------------------------------------
             # Check the Latitude versus expected HARCODED value.
             # --------------------------------------------------
             if ( ($absLatDiff > 0.10))
                {
                if ($debug2) {print "ERROR:: Latitude on first data record DOES NOT MATCH expected site lat! Do Not Send to GTS.\n";}
                $errText =  $errText."ERROR:: Latitude on first data record DOES NOT MATCH expected site lat! Do Not Send to GTS.\n";

                print  $ERROUT "ERROR:: Latitude on first data record DOES NOT MATCH expected site lat! Do Not Send to GTS.\n";
                } # lat DOES NOT MATCH expected HARDCODED values so error - DO NOT send to GTS
             else
                {
                if ($debug2) {print "Latitude on first data record MATCH expected site lat. Send sounding to GTS.\n";}
                }

             # --------------------------------------------------
             # Check the Longitude versus expected HARCODED value.
             # --------------------------------------------------
             if ( ($absLonDiff > 0.10))
                {
                if ($debug2) {print "ERROR:: Longitude on first data record DOES NOT MATCH expected site lon! Do Not Send to GTS.\n";}
                $errText =  $errText."ERROR:: Longitude on first data record DOES NOT MATCH expected site lon! Do Not Send to GTS.\n";

                print  $ERROUT "ERROR:: Longitude on first data record DOES NOT MATCH expected site lon! Do Not Send to GTS.\n";
                } # Longitude DOES NOT MATCH expected HARDCODED values so error - DO NOT send to GTS
             else
                {
                if ($debug2) {print "Longitude on first data record MATCH expected site lon. Send sounding to GTS.\n";}
                }

             # --------------------------------------------------------------------
             # Check the Elevation/Altitude versus expected HARCODED value.
             # HARDCODED ELEVATION CHECK: "COMMENT OUT" this next section until 
             # told to uncomment. User MUST set checkAltitudeDiff at top of code. 
             # --------------------------------------------------------------------
             # Check that the first record altitude is within 20 meters
             # of expected value. 
             # --------------------------------------------------------------------
             if ( $checkAltitudeDiff )    # HARDCODED top of code = 0
                {
                if ( ($absAltDiff > 20))
                   {
                   if ($debug2) {print "ERROR:: Elevation/Altitude on first data record DOES NOT MATCH expected site Elevation/Altitude! Do Not Send to GTS.\n";}
                   $errText =  $errText."ERROR:: Elevation/Altitude on first data record DOES NOT MATCH expected site Elevation/Altitude! Do Not Send to GTS.\n";

                   print  $ERROUT "ERROR:: Elevation/Altitude on first data record DOES NOT MATCH expected site Elevation/Altitude! Do Not Send to GTS.\n";
                   } # Elevation/Altitude DOES NOT MATCH expected HARDCODED values so error - DO NOT send to GTS
                else
                   {
                   if ($debug2) {print "Elevation/Altitude on first data record MATCH expected site Elevation/Altitude. Send sounding to GTS.\n";}
                   } # absAltDiff
                } # checkAltitudeDiff

             #-----------------------------------------------
             # Process the Lat/Lon data from the first record
             # and put in output header.
             #-----------------------------------------------
             if (($data[15] > -32768) && ($data[16] > -32768)) 
                {
                #--------------------------------------------------------------
                # Format length must be the same as the value length or
                # convertLatLong will complain (see example below)
                #
                # For Male, Maldives:
                # base lat   = 36.6100006103516    base lon = -97.4899978637695
                # Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
                #
                # For Greensburg, KS:
                # No lat/lon in header. First lat/lon in data is of the form:
                # Lat = 39.06   Lon = -76.88
                #       DDDDD         DDDDDD
                #-------------------------------------------------------------- 
                #----------
                # Longitude
                #----------
                my $lon_fmt = $data[15] < 0 ? "-" : "";
                while (length($lon_fmt) < length($data[15])) 
                   { 
                   $lon_fmt .= "D"; 
                   }

                if ($data[15] != -32768) {$header->setLongitude($data[15],$lon_fmt);}

                if ($debug2) {print "Lon = Data(15): xxx $data[15] xxx\n";}
                if ($debug2) {print "lon_fmt: xxx $lon_fmt xxx\n";}

                #----------
                # Latitude
                #----------
                my $lat_fmt = $data[16] < 0 ? "-" : "";
                while (length($lat_fmt) < length($data[16])) 
                   { 
                   $lat_fmt .= "D"; 
                   }

                if ($data[16] != -32768) {$header->setLatitude($data[16],$lat_fmt);}
 
                if ($debug2) {print "Lat = Data(16): xxx $data[16] xxx\n";}
                if ($debug2) {print "lat_fmt: xxx $lon_fmt xxx\n";}

                #----------
                # Altitude
                #----------
                if ($data[6] != -32768) {$header->setAltitude($data[6],"m");} 

                last;
                } #data[14/15] > -32768 - Not MISSING

             } # firstDataLine - only process first data line for the header info; Skip rest

          } # Data lines
       } # foreach line

    # -----------------------------------------------------------------------
    # Extract the ACTUAL RELEASE date and time information from the file name
    # NOW:: Expect file name structure: UMBC_RS41_yyyymmdd_hhmmUT.dat .      HARDCODED
    #
    # WAS:: Expect file name structure: HUBV_RS92SGP_yyyymmdd_hhmmUT.dat .  
    # WAS:::    if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
    # -----------------------------------------------------------------------
    if ($debug) { print "file name = $filename\n"; }

    my $date;
    my $time;

    if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
      {
      my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minInfo) = ($1,$2,$3,$4,$5);

      $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
      $time = join "", $hourInfo, ' ', $minInfo, ' 00';

      if ($debug) {print "date is $date\n";print "time is $time\n";}

      } # Pull date and time from filename

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    return $header;
} # parseHeader()
                           
##------------------------------------------------------------------------------
# @signature void parseRawFile(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file) = @_;

    my $altMissCt = 0;
    my $pressLess850 = 0;
    my $PNoMiss = 0;
   
    my $lonMiss = 0; my $latMiss = 0;
    my $TNoMiss = 0; my $TdNoMiss = 0; my $RHNoMiss = 0;
    my $UNoMiss = 0; my $VNoMiss = 0;
    my $WspNoMiss = 0; my $WdirNoMiss = 0;

    if ($debug2) {printf("\n********************\nEnter parseRawFile(): Processing file: %s\n",$rawDirName."/".$file); }

    open(my $FILE,$self->{"RAW_WORK_DIR"}."/".$file) or die("Can't open file: ".$file);   # Make sure working on copy
    my @lines = <$FILE>;
    my $number_lines_in_file = $#lines+1;

    my @file_info = stat $FILE;

    if ($debug2) 
       {
       print "file_info:: @file_info\n"; 
       print "number_lines_in_file:: $number_lines_in_file \n";
       print "File size = $file_info[7]\n";
       }

    close($FILE);

    #---------------------------------------------------
    # Open Error Log/Warning file. If there are any
    # errors, then the file being processed can not
    # be put on the GTS so don't move to ASPEN/GTS area.
    #----------------------------------------------------
    my @namepart = split(".dat", $file);
    $errfileName = $namepart[0].".errlog";

    if ($debug2) {print "Error Log file name is $outputDirName/$errfileName\n"; }

    open($ERROUT,">".$self->{"OUTPUT_DIR"}."/".$errfileName) or 
             die("Can't open Error Log file $errfileName for input file $file\n");
    
    #------------------------------
    # Generate the sounding header.
    #------------------------------
    my @headerlines = @lines;

    my $header = $self->parseHeader($file,@headerlines);
    
    #-----------------------------------------------------------
    # Only continue processing the file if a header was created.
    #-----------------------------------------------------------
    if (defined($header)) 
       {
       # ----------------------------------------------------
       # Create the output file name and open the output file
       # ----------------------------------------------------
       my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

       $outfileName = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
                        $header->getId(),
                        split(/,/,$header->getActualDate()),
                        $hour, $min);
 
       if ($debug2) {printf("\tHeader was created. Continue processing data recs.\nOutput file name is %s\n", $outputDirName."/".$outfileName); }

       open($OUT,">".$self->{"OUTPUT_DIR"}."/".$outfileName)
             or die("Can't open output file for $file\n");

       print($OUT $header->toString());
   
       # ----------------------------------------
       # Needed for code to derive ascension rate
       # ----------------------------------------
       my $prev_time = 9999.0;
       my $prev_alt = 99999.0;

       # ----------------------------------------------------
       # Parse the data portion of the input file
       # ----------------------------------------------------
       my $index = 0;

       foreach my $line (@lines)  
          {
          $lonMiss = 0;
          $latMiss = 0;
          $TotalRecProc++;

          if ($TotalRecProc >= 40)       # HARDCODED
             {
             $dataRecProc++;
             } # Keep track of data lines

          if ($debug2) {print "TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

          #--------------------------------------------
          # Ignore the header lines and blank lines.
          # Note that there tends to be a blank line
          # at the end of the data. Skip that line, too.
          #---------------------------------------------
          if ($debug2) {print "line: $line\n";}
          if ($debug) {if ($line =~ /^\s*$/) {print "Line is empty. line = xxx $line xxx\n";} }

          if ($index < 40 || $line =~ /^\s*$/)   # HARDCODED to skip first 40 lines Plus any blank lines
             { 
             if ($debug2) {print "Skip header line in array.\n";}
             $index++; next; 
             }
       
          #------------------------------------------------------------------------------
          # Check for blank line particularly at end of file in Greensburg, KS PISA data.
          # if missing or null or \n or line length not as expected....
          # This was something checked in the Male, Maldives data.
          #------------------------------------------------------------------------------
          my @data = split(' ',$line);
          my $record = ClassRecord->new($ERROUT,$file);  

          if ($debug2) { print "Parse Data Record:: Temp, TD, RH, Winds, etc.\n";}

          #---------------------------------------------------
          # missing values are -32768 - Assumption! HARDCODED
          #---------------------------------------------------
          $record->setTime($data[0]);

          $record->setPressure($data[7],"mb") if ($data[7] != -32768);

          if ($debug2) {print "Check Pressure less than 850.00 MB. press = $data[7]\n";}

          if (($data[7] <= 850.00) && ($data[7] != -32768) ) 
             {
             $pressLess850++;
             }
          if ($data[7] != -32768) {$PNoMiss++;}

          #---------------------------------------------------------
          # $record->setTemperature($data[2],"C") if ($data[2] != -32768);    
          # Temp and Dewpt are in Kelvin.  C = K - 273.15
          #---------------------------------------------------------
          $record->setTemperature(($data[2]-273.15),"C") if ($data[2] != -32768);    
          if ($data[2] != -32768) {$TNoMiss++;};

          $record->setDewPoint(($data[8]-273.15),"C") if ($data[8] != -32768);
          if ($data[8] != -32768) {$TdNoMiss++;};

          $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);
          if ($data[3] != -32768) {$RHNoMiss++;};

          $record->setUWindComponent($data[5],"m/s") if ($data[5] != -32768);
          if ($data[5] != -32768) {$UNoMiss++;};

          $record->setVWindComponent($data[4],"m/s") if ($data[4] != -32768);
          if ($data[4] != -32768) {$VNoMiss++;};

          $record->setWindSpeed($data[11],"m/s") if ($data[11] != -32768);
          if ($data[11] != -32768) {$WspNoMiss++;};

          $record->setWindDirection($data[10]) if ($data[10] != -32768);
          if ($data[10] != -32768) {$WdirNoMiss++;};

          #--------------------------------------------------
          # get the lat/lon data. MISSING value = -32768     
          #--------------------------------------------------
          if ($data[15] != -32768) 
             {
             my $lon_fmt = $data[15] < 0 ? "-" : "";

             while (length($lon_fmt) < length($data[15])) { $lon_fmt .= "D"; }
             $record->setLongitude($data[15],$lon_fmt);

             if ($debug2) {print "Lon = Data(15): xxx $data[15] xxx\n";}
             if ($debug2) {print "lon_fmt: xxx $lon_fmt xxx\n";}

             } # if lon not missing
          else
             {
             $lonMiss = 1;
             }

          if ($data[16] != -32768) 
             {
             my $lat_fmt = $data[16] < 0 ? "-" : "";

             while (length($lat_fmt) < length($data[16])) { $lat_fmt .= "D"; }
             $record->setLatitude($data[16],$lat_fmt);

             if ($debug2) {print "Lat = Data(16): xxx $data[16] xxx\n";}
             if ($debug2) {print "lat_fmt: xxx $lat_fmt xxx\n";}


             } # if lat not missing
          else
             {
             $latMiss = 1;
             }

          if ($debug2) {print "Latitude:: $data[16] , Longitude:: $data[15], TotalRecProc: $TotalRecProc, dataRecProc:: $dataRecProc \n";}

          #----------------------------------------------------------
          # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
          # For setVariableValue(index, value):  
          # index (1) is Ele column, index (2) is Azi column.
          #----------------------------------------------------------
          $record->setVariableValue(1, $data[13]) if ($data[13] != -32768);   # New El variable
          $record->setVariableValue(2, $data[12]) if ($data[12] != -32768);   # AZ variable

          $record->setAltitude($data[6],"m") if ($data[6] != -32768);     # AKA Height in raw data.

          if ($data[6] == -32768.00)
             {
             $altMissCt++;
             if ($debug2) { print "Missing height, altMissCt:: $altMissCt\n"; }
             }
                                             
          #-------------------------------------------------------
          # Following calc of asc rate not required for PISA GREEN
          # but S. Loehrer says OK to leave in.
          #-------------------------------------------------------
          # Calculate the ascension rate which is the difference
          # in altitudes divided by the change in time. Ascension
          # rates can be positive, zero, or negative. But the time
          # must always be increasing (the norm) and not missing.
          #
          # Only save off the next non-missing values.
          # Ascension rates over spans of missing values are OK.
          #-------------------------------------------------------
          if ($debug) 
             { 
             my $time = $record->getTime(); 
             my $alt = $record->getAltitude(); 
             print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n";
             }

          if ($prev_time != 9999  && $record->getTime()     != 9999  &&
              $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
              $prev_time != $record->getTime() ) 
             {
             $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                        ($record->getTime() - $prev_time),"m/s");

             if ($debug) { print "Calc Ascension Rate.\n"; }
             } # If input non-missing, calc asc. rate.

          #-----------------------------------------------------
          # Only save off the next non-missing values. 
          # Ascension rates over spans of missing values are OK.
          #-----------------------------------------------------
          if ($debug) 
             { 
             my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
             if ($debug) {print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n";  }
             }

          if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
             {
             $prev_time = $record->getTime();
             $prev_alt = $record->getAltitude();

             if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
             } # save next non-missing vals

          #-----------------------------------
          # Completed the ascension rate data
          #-----------------------------------
          printf($OUT $record->toString());

          #------------------------------------------------------
          # If the lat and lon are missing from the first record
          # then this is a significant error so DO NOT send this
          # sounding on to GTS. That is, create and keep an error log.
          #------------------------------------------------------
          if ($debug2) {print "Latitude MISS:: $lonMiss , Longitude MISS:: $lonMiss, TotalRecProc: $TotalRecProc, dataRecProc:: $dataRecProc \n";}

          if (($TotalRecProc == 40) && ($lonMiss) && ($latMiss)) # HARDCODED - Data begins on line 40  HERER
             {
             if ($debug2) {print "ERROR:: Latitude and Longitude of first data record are MISSING! Do NOT send sounding to GTS!\n";}
             $errText =  $errText."ERROR:: Latitude and Longitude of first data record are MISSING! Do NOT send sounding to GTS!\n";

             print  $ERROUT "ERROR:: Latitude and Longitude of first data record are MISSING! Do NOT send sounding to GTS!\n";
             } # lat/lon missing from first data rec

          } # foreach data line

       if ($debug2) {print "Processing total, data line number:: $TotalRecProc, $dataRecProc\n";} 

       } # successfully made header, process data
   else
      {
      if ($debug2) {printf("Unable to make a header\n");}
      $errText =  $errText."ERROR:: Unable to make header.\n";

      print  $ERROUT "ERROR:: Unable to make header.\n";
      } # Could not make header


   #---------------------------------------------------
   # If all altitudes are missing in data, then write
   # error to error log and this sounding should not
   # be passed to GTS.
   #---------------------------------------------------
   if (($dataRecProc >= 0) && ($dataRecProc == $altMissCt))
      {
      if ($debug2) {printf("All Altitudes/Heights are MISSING in data records.\n");}
      $errText =  $errText."ERROR:: All Altitudes/Heights are MISSING in data records. Do NOT send sounding to GTS!\n";
      print  $ERROUT "ERROR:: All Altitudes/Heights are MISSING in data records. Do NOT send sounding to GTS!\n";
      }

   if ( ($TotalRecProc == 0))
      {
      if ($debug2) {printf("ERROR:: No header lines AND No data records. EMPTY FILE! Do NOT send sounding to GTS!\n");}
      $errText =  $errText."ERROR:: No header lines AND No data records. EMPTY FILE! Do NOT send sounding to GTS!\n";
      print  $ERROUT "ERROR:: No header lines AND No data records. EMPTY FILE! Do NOT send sounding to GTS!\n";
      }

   if ($dataRecProc == 0)
      {
      if ($debug2) {printf("ERROR:: No data records. Do NOT send sounding to GTS!\n");}
      $errText =  $errText."ERROR:: No data records. Do NOT send sounding to GTS!\n";
      print  $ERROUT "ERROR:: No data records. Do NOT send sounding to GTS!\n";
      }

   #---------------------------------------------------
   # If pressure is never less than or equal to 850MB,
   # then do not put this sounding on GTS.
   #---------------------------------------------------
   if (($pressLess850 < 1) && ($PNoMiss > 0))
      {
      if ($debug2) {print "ERROR:: Pressure is never less than 850.00 MB. Do NOT send sounding to GTS!\n";}
      $errText =  $errText."ERROR:: Pressure is never less than 850.00 MB. Do NOT send sounding to GTS!\n";
      print  $ERROUT "ERROR:: Pressure is never less than 850.00 MB. Do NOT send sounding to GTS!\n";
      }

   if ($PNoMiss < 1)
      {
      if ($debug2) {print "ERROR:: All Pressure values are MISSING. Do NOT send sounding to GTS!\n";}
      $errText =  $errText."ERROR:: All Pressure values are MISSING. Do NOT send sounding to GTS!\n";
      print  $ERROUT "ERROR:: All Pressure values are MISSING. Do NOT send sounding to GTS!\n";
      }

   #---------------------------------------------------
   # If  all the data are missing, this sounding should
   # not be put on the GTS.
   #---------------------------------------------------
   my $allData = $TNoMiss + $TdNoMiss + $RHNoMiss + $UNoMiss + $VNoMiss + $WspNoMiss + $WdirNoMiss;
   if ($debug) {print "allData count:: $allData\n";}

   if ($allData == 0)
      {
      if ($debug2) {printf("ERROR:: No valid data on any records. Possibly no header lines. Do NOT send sounding to GTS!\n");}
      $errText =  $errText."ERROR:: No valid data on any records. Possibly no header lines. Do NOT send sounding to GTS!\n";
      print  $ERROUT "ERROR:: No valid data on any records. Possibly no header lines. Do NOT send sounding to GTS!\n";
      }

    close($ERROUT);
    close($OUT);

   } # parseRawFile()


##------------------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file. During production, the raw data directory will be the
# specified FTP area. This s/w is expected to ONLY process the most recent files
# copied from the FTP area into a local raw_data directory. Only the files in the 
# raw data directory will be processed. This s/w does not process data directly in the FTP dir.
#
# The only files that should be copied from the FTP directory are those files that
# have been put in the FTP directory within the last ten minutes. This script will run
# via a cron script every ten minutes on the 00, 10, 20, 30, etc. times. So any files
# in the FTP directory deposited there within the last ten minutes will be processed.
# Actually, to allow 1 minute for download of a file into the FTP directory, this s/w
# will copy/process files in the FTP area with timestamps from eleven (11) minutes ago 
# up until 1 minute prior to the time that this s/w is executed by cron.
# (i.e., T > 60 seconds and T < or = to 660 seconds). 
#
# Note that this only means that the files are copied into a local area for processing.
# The files may fail processing (e.g., no data lines, bad lat/lon, etc.) may still prevent
# the sounding files from being put into the GTS directory. 
#
# Files that are not processed plus error logs should still be moved to the local archive location
# to prevent processing files multiple time from the local raw input directory.
#
# It should be easy to check that all incoming FTP files are processed by comparing
# the list of files in the FTP area versus the local /archive directory. In the end, 
# the same exact raw data files should be in both places. The /archive dir should also
# have error logs and all Class files that could be created and were sent to the GTS. </p>
##--------------------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    my $cmd = "\n";

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    #------------------------------------------------------------------------
    # Input file names must be of the form: UMBC_RS41_2015MMDD_hhmmUT.dat       HARDCODED
    #   where UMBC_RS41 is the call sign for the Greensburg, KS, yyyy = year,
    # mm = month, dd = day, hh = hour, mm = minute, and "dat" is the
    # suffix. This is the exact form. All files with names of this
    # form will be processed.
    #------------------------------------------------------------------------
    # Assorted old code:: Remove when name structure finalized.
    #
    #    my @files = grep(/^43555_\d{12}\.tsv/,sort(readdir($RAW)));
    #    my @files = grep(/^HUBV_RS92SGP_\d{6}_\d{4}UT\.dat/,sort(readdir($RAW)));
    # WAS:  my @files = grep(/^HUBV_RS92SGP/,sort(readdir($RAW)));
    #    my @files = grep(/^UMBC_RS41/,sort(readdir($RAW)));
    #------------------------------------------------------------------------

    #------------------------------------------------------------------------
    # Grab and process ONLY the files that are between 11 and 1 minute old
    # copy those files to the working ingest directory. Any file less than 
    # one minute old might still be in the process of being downloaded so
    # don't copy or process it. Pick that one up during the next 10min slot.
    # This section of # code based on code from SL's Iquique Skewt script. 
    #------------------------------------------------------------------------
    # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
    #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
    #------------------------------------------------------------------------
    # Stat array results::
    #  0 dev device number of filesystem
    #  1 ino inode number
    #  2 mode file mode (type and permissions)
    #  3 nlink number of (hard) links to the file
    #  4 uid numeric user ID of file's owner
    #  5 gid numeric group ID of file's owner
    #  6 rdev the device identifier (special files only)
    #  7 size total size of file, in bytes
    #  8 atime last access time in seconds since the epoch
    #  9 mtime last modify time in seconds since the epoch
    # 10 ctime inode change time in seconds since the epoch (*)
    # 11 blksize preferred block size for file system I/O
    # 12 blocks actual number of blocks allocated
    #-------------------------------------------------------------------------
    my @all_files = grep(/^UMBC_RS41/,sort(readdir($RAW)));     # ID all the files (old and new) in FTP

    if ($debug2) {print "\n*******************\nEnter readDataFiles: All files in FTP area:: all_files:: @all_files \n";}
    if ($debug2) {print "--------------------\nDecide which files from FTP to copy and process.\n";}

    foreach my $file (@all_files) 
       {
       $file = $rawDirName."/".$file;   # Must form complete file name plus directory to ensure correct file

       my @file_info = stat $file;     # See comments above for output from stat command

       my $mtime = $file_info[9];      # Last modify time in seconds since the epoch
       my $tm = time;                  # Current EPOCH time
       my $timediff = $tm - $mtime;    # Difference between current time and file modify time

       if ($debug2) 
          {
          print "\n---------\nProcessing file:: $file .\n   File_info from Stat:: @file_info \n";
          print "\n   FIRST:: mtime, tm, timediff:: xxx $mtime xxx, xxx $tm xxx, xxx $timediff xxx \n";

          use POSIX 'strftime'; my $t1 = strftime('%m/%d/%Y|%H:%M', localtime($mtime));  my $t2 = strftime('%m/%d/%Y|%H:%M', localtime($tm));   print "   Translate to readable date/time: mtime, tm (Strings): xxx $t1 xxx, xxx $t2 xxx\n\n";

          print "   Process file if $timediff > 60 but <= 660 seconds, process sounding.\n";
          } # debug2

       #----------------------------------------------------
       # Only copy the recent (1-11mins old) FTP files into 
       # local raw input dir for processing.
       #----------------------------------------------------
       if (($timediff > 60) && ($timediff <= 660))  # HARDCODED - process files 1 to 11 minutes old
          {
          $cmd = sprintf ("cp %s %s/.", $file, $rawWorkDirName);
          system($cmd);

          if ($debug2) {print "   Copy $file from FTP to local raw input dir. \nCommand:: xxx $cmd xxx.\n";}

          }  # timediff check
       else
          {
          if ($debug2) {print "   DO NOT copy file from FTP. DO NOT PROCESS!\n";}
          } # timediff within limits

       }  # Find all "recent" files and copy to processing/work area to convert

    closedir($RAW);   # Close the FTP directory

    #------------------------------------------------------------------
    # Process ONLY files that have been copied to local raw directory.      HARDCODED
    #------------------------------------------------------------------
    opendir(my $RAW_WORK,$self->{"RAW_WORK_DIR"}) or die("Can't read raw directory ".$self->{"RAW_WORK_DIR"});

    my @files = grep(/^UMBC_RS41/,sort(readdir($RAW_WORK)));   # Check Local input dir for expected name format, just in case

    if ($debug2) {print "\nBefore parseRawFile():: Files copied to work space to be processed:: xxx @files xxx.\n\n";}

    foreach my $input_file (@files) 
       {
       if ($debug2) {print "\n\n------\nProcessing raw_work_dir file:: $input_file . Call parseRawFile().\n Before parseRawFile() - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

       $self->parseRawFile($input_file);   # Process each data file!

       if ($debug2) {print "After parseRawFile() - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

       #----------------------------------------------------
       # If there are any lines in the Error log then do NOT
       # copy the converted CLASS file to the GTS area.
       #
       # If the error log is empty, then copy the CLASS file
       # to the GTS area for ASPEN QC and GTS submittal.
       #
       # Either way, always move the original ingest file,
       # the *.cls file, and *.errlog file to the archive
       # directory.  Leave the ingest directory empty of
       # these files.
       #----------------------------------------------------
       open($ERROUT,"<".$self->{"OUTPUT_DIR"}."/".$errfileName) or
                die("Can't open Error Log file $errfileName\n");

       my @err_lines = <$ERROUT>;
       my $number_lines_in_ERROR_file = $#err_lines+1;

       #------------------------------------------------------------
       # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
       #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
       #------------------------------------------------------------
       my @errfile_info = stat $ERROUT;
       close ($ERROUT);

       if ($debug2)
          {
          print "errfile_info:: @errfile_info\n"; 
          print "err_lines:: $#err_lines\n";
          print "number_lines_in_ERROR_file:: $number_lines_in_ERROR_file \n";
          print "ERROR File size = $errfile_info[7]\n";
          }

       if ($#err_lines < 0)   # NO errors during processing! Copy Class file to GTS.
          {
          if ($debug2) {print "NO errors, copy class file to GTS area.\n";}

          $cmd = sprintf ("cp  %s/%s %s/.", $outputDirName, $outfileName, $gtsDirName);
          if ($debug2) {print "COPY CLASS to GTS. Issue cmd:: $cmd\n";}

          system "$cmd ";

          }
       else
          { if ($debug2) {print "THERE ARE ERRORS, don't move class file to GTS area.\n";} } # Error during processing

       #--------------------------------------------------------------
       # Move files to archive area to prep for next incoming file
       # Need to chmod on files??
       #
       # Move the copy of the raw data file, converted Class file,
       # and any error log to the local /archive location. 
       #
       # DO NOT MOVE ANY FILES FROM THE INCOMING FTP area. 
       # Users will be downloading files during the field phase from 
       # the FTP area so that dir needs a copy of all files there, too.
       # If this changes, can use the next two lines to do the move:
       #       $cmd = sprintf ("mv %s/%s %s/.", $rawDirName, $input_file, $archiveDirName);
       #       if ($debug2) {print "ARCHIVE(mv) RAW Input data. Issue cmd:: $cmd\n";}
       #---------------------------------------------------------------
       $cmd = sprintf ("mv %s/%s %s/.", $rawWorkDirName, $input_file, $archiveDirName);
       if ($debug2) {print "\nARCHIVE(mv) working/local RAW Input data. Issue cmd:: $cmd\n";}

       system "$cmd ";

       $cmd = sprintf ("mv %s/%s %s/.", $outputDirName, $outfileName, $archiveDirName);
       if ($debug2) {print "\nARCHIVE(mv) CLASS file. Issue cmd:: $cmd\n";}

       system "$cmd ";

       $cmd = sprintf ("mv %s/%s %s/.", $outputDirName, $errfileName, $archiveDirName);
       if ($debug2) {print "errfileName: $errfileName\n";}
       if ($debug2) {print "\nARCHIVE(mv) ERROR Log. Issue cmd:: $cmd\n";}

       system "$cmd ";

       #------------------------------------------------------
       # Send email saying what has been processed.
       # 
       # Add sfw\@ucar.edu and loehrer\@ucar.edu to the 
       # "addressto" for production.
       #
       # HARDCODED Email addresses below.
       #------------------------------------------------------
       my $addressfrom = sprintf "%s", "cully\@ucar.edu"; 

       my $addressto = sprintf "%s", "cully\@ucar.edu, echohawk\@ucar.edu, loehrer\@ucar.edu, sfw\@ucar.edu";   #HARDCODED FINAL email

####   my $addressto = sprintf "%s", "cully\@ucar.edu";       # TEST only

       my $errlength = length($errText);
       my $message;

       if ($errlength > 0 || $number_lines_in_ERROR_file > 0)
          { $message = "PISA FP2 Greensburg, KS: Sounding $input_file Processed - ERROR"; }
       else
          { $message = "PISA FP2 Greensburg, KS: Sounding $input_file Processed"; }

       if ($debug2) {print "email (from;to):: $addressfrom ; $addressto. \n";}

       my $mailer = Mail::Mailer->new();
       $mailer->open({
           From => $addressfrom,
           To => $addressto,
           Subject => $message,
           })
           or die "Can't open $!\n";

           if ($debug2) {print "errlength:: $errlength. \n";}

           if ($errlength > 0 || $number_lines_in_ERROR_file > 0)
              {
              print $mailer "The following PISA FP2 Greensburg, KS data file has been processed:\n $rawDirName/$input_file .\n\nThis file contained $TotalRecProc total records and $dataRecProc data records.\n\nAll files (*.dat, *.cls, *.errlog) have been COPIED to the \narchive area at  $archiveDirName .\n\nERROR MESSAGES::\n$errText \nBeware that junky input files may cause unexpected results. Review the *.errlog file for more ERROR information.\n\nThere were errors creating the Class file, so the class file WAS NOT copied \nto the GTS directory at $gtsDirName\n";
              }
           else
              {
              print $mailer "The following PISA FP2 Greensburg, KS data file has been processed:\n $rawDirName/$input_file .\n\nThis file contained $TotalRecProc total records and $dataRecProc data records.\n\nAll files (*.dat, *.cls, *.errlog) have been COPIED to the \narchive area at  $archiveDirName .\n\n$errText \nClass file created and copied to GTS directory at $gtsDirName\n";
              }

       $mailer->close();

       $TotalRecProc = 0;
       $dataRecProc = 0;

       $errText = "";

       } # process each file 


} # readDataFiles()

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
