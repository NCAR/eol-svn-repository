#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The PISA_FP6_HESS_sounding_converter.pl script is used for converting high
# resolution radiosonde data from ASCII to the modified EOL Sounding Composite (ESC)
# format. PISA stands for the PECAN (2015) field project Sounding Array or
# "PECAN Integrated Sounding Array (PISA)". Note there there are 3 fixed PISA sites
# in the PECAN (Hays, KS) field project:  FP2 - GREEN;Greensburg, KS;
# FP3 - ELLIS;Ellis, KS; and FP6 - HESS;Hesston, KS.  This is the converter for
# the Hesston, KS data. FP6 stands for Fixed PISA site #6. SGP stands for Southern
# Great Plains.
#
# The incoming format is an older Vaisala format (Vaisala RS92). The goal of the 
# software is to take incoming data from a specified incoming location and convert
# it to the modified ESC format so that ASPEN can be used to check it, and then 
# the converted data will be transmitted/sent to the GTS in near realtime.
#
# The output format is basically ESC, except the first header line of "Data Type:" is output 
# to be "CLASS,". This is so that the output format can be read by ASPEN
# as a class formatted sounding. ASPEN is then used to convert the CLASS/ESC
# output sounding into GTS format. 
#</p> 
#
# @usage PISA_FP6_HESS_sounding_converter.pl [--test_mode]
#          --test_mode Run the program in the test directories with test emails
#          The default is to run the live production version
#
# @author Linda Echo-Hawk
# @version PECAN 2015 
#          NOTE: Search on PRODUCTION CODE and HARD-CODED to find values 
#          that may require correcting for "real" data.
#          This code make the following assumptions:
#          - That the raw data filenames are in the following
#            format: sgpsonde-curC1.YYYYMMDD.HHmm.raw (e.g., 
#            sgpsonde-curC1.20150417.0514.raw)
#          - The file contains header info on lines 1-26. Actual data starts 
#            on line 28. 
#          - That the incoming raw data are located in the
#                /net/iftp2/pub/incoming/pecan/FP6_Hesston  directory.
#          - That the outgoing Class soundings created by this s/w should 
#            be placed in
#                /h/eol/iss/project/pecan/data/fp6/cls
#          - That the following directories exist:  ../outout   ../archive
#          - The radiosonde ID is obtained from the header information.
#          - No lat/lon values appear in the data. The lat/lon header values 
#            are used in the surface record (t=0). 
#          - Missing values are represented by "-32768" in the raw data.
#          - The release date and time and obtained from the header.
#          - NOTE that the code required to collect the raw data and send
#            it to the GTS after processing is based on Linda Cully's 
#            PISA_FP2_GREEN_sounding_converter.pl and her notes for that 
#            converter apply.
#          - Code checks number of lines in error file. This helps catch 
#            embedded junk such as binary or emails in data. For cron
#            scripts is it very important to completely spell out all 
#            directory path names.
#
#
# This code will NOT send a sounding to the GTS directory if any of the following are true:
#
#  - If the surface lat or lon (from first record) values do NOT match HARDCODED values for Hesston, KS.
#  - If the elevation/altitude (from first record) value does NOT match HARDCODED values for Hesston, KS.
#      NOTE: S. Loehrer has asked that the elevation/altitude check be commented out for now. Search for
#            "ELEVATION CHECK" to find the code to uncomment it. 
#  - If the raw, input file is "empty" (i.e., no header or records).
#  - If there are NO DATA records in raw file.
#  - No valid data on any records. Possibly no header lines.
#  - If either of the lat/lon values on the first record are missing. NOTE that the raw data does not contain lat/lon data so the header values are used for the surface record and the remaining value are set to missing.
#  - If the "Location" line is not found in the header.
#  - If the "RS-number" line is not found in the header.
#  - If the "Started at" line is not found in the header.
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
#  - Blank lines at end of file are ignored.
#
# BEWARE:  Search for "HARD-CODED" or "PRODUCTION" to find project-specific items that may
#          need to updated.
#
# Cron info: This task will be run as a cron script. As of approx. 30 Sept 2011, 
#     SL will add to his merlot/local crontab to run this script frequently (every 10 mins)
#     To edit (using vi) the crontab do "crontab -e". Add in the following
#     single line to have this script run every 10 mins on the 10's (e.g., 01:10,
#     01:20, 01:40, 01:50, etc.).
#
#     */10 * * * * /net/work/Projects/pecan/upper_air/PISA_FP6_HESS/production/
#     software/PISA_FP6_HESS_sounding_converter.pl > /dev/null 2>&1   
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
#
#
##Module------------------------------------------------------------------------
package PISA_FP6_HESS_sounding_converter;
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
# import module to set up command line options
use Getopt::Long;

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
# for the phrase "COMMENTED OUT" to find the check. Stn Elev
# is checked against HARDCODED value. If diff is > 20m, then
# an error is issued preventing class file from going to GTS.
#-------------------------------------------------------------
my $checkAltitudeDiff =0;

#----------------------------------------------------------------
# Set either debug value to 1 for varying amounts of debug.
# BEWARE: They all must be set to zero (0) for production runs!
#-----------------------------------------------------------------
my $debug = 0; # higher level debug output
my $debug2 = 0;  # detailed debug output
my $debug3 = 0; # even more detail


# read command line arguments 
my $result;   
# convert the altitude from feet to meters
my $test_mode;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("test_mode" => \$test_mode);

if ($test_mode)
{
	if ($debug) {printf("Run the converter in the test directories.\n");}
}

if ($debug) {printf "\nPISA_FP6_HESS_sounding_converter.pl began on ";print scalar localtime;printf "\n";}
&main();
if ($debug) {printf "\nPISA_FP6_HESS_sounding_converter.pl ended on ";print scalar localtime;printf "\n";}

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the PISA FP6 HESS radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = PISA_FP6_HESS_sounding_converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature PISA FP6 HESS_Radiosonde_Converter new()
# <p>Create a new instance of a PISA FP6 HESS_Converter.</p>
#
# @output $self A new PISA FP6 HESS_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    # HARD-CODED
    $self->{"PROJECT"} = "PECAN";
    $self->{"NETWORK"} = "Fixed_PISA";
    
    
    #---------------------------------------------------------------
    # Input/ingest directory. Data to convert.
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
	#
	# FOR HESSTON: /net/iftp2/pub/incoming/pecan/FP6_Hesston
	# WRITE to: /h/eol/iss/project/pecan/data/fp6/cls
    #---------------------------------------------------------------
	
    #------------------------------
    # Input directories. HARD-CODED
    #------------------------------
	
	if ($test_mode)
	{
		$rawDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/processing/fake_FTP"; # test area
		$rawWorkDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/processing/raw_data";
	}
	else
	{
    	$rawDirName = "/net/iftp2/pub/incoming/pecan/FP6_Hesston"; # Final incoming FTP area
		$rawWorkDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/production/raw_data";
	}
    
	$self->{"RAW_DIR"} = $rawDirName;
    $self->{"RAW_WORK_DIR"} = $rawWorkDirName;
    
    #-------------------------------
    # Output directories. HARD-CODED
    #-------------------------------

	if ($test_mode)
	{
    	$outputDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/processing/output"; # working dir
		# archive copy of all CLS and log files
    	$archiveDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/processing/archive";
		# Final GTS area to place converted *.cls file
    	$gtsDirName =  "/net/work/Projects/PECAN/upper_air/FP6_ARM/processing/fake_gts";
	}
	else
	{
    	$outputDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/production/output"; # working dir
    	# archive copy of all CLS and log files
		$archiveDirName = "/net/work/Projects/PECAN/upper_air/FP6_ARM/production/archive";
		# Final GTS area to place converted *.cls file
    	$gtsDirName =  "/h/eol/iss/project/pecan/data/fp6/cls"; 
	}
   
    $self->{"OUTPUT_DIR"} = $outputDirName;
    $self->{"ARCHIVE_DIR"} = $archiveDirName;
    $self->{"GTS_DIR"} = $gtsDirName;

    return $self;
} # new


##------------------------------------------------------------------------------
# @signature String buildLatLonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# format length must be the same as the value length or
# convertLatLong will complain (see example below)
# base lat = 36.6100006103516 base lon = -97.4899978637695
# Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub buildLatLonFormat {
    my ($self,$value) = @_;
    
    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;
}

##-------------------------------------------------------------------------
# @signature String cleanForFileName(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub cleanForFileName {
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data to the ESC format.</p>
##------------------------------------------------------------------------------
sub convert {
    my ($self) = @_;
    
    mkdir($self->{"OUTPUT_DIR"}) unless (-e $self->{"OUTPUT_DIR"});
    
    $self->readDataFiles();
}

##------------------------------------------------------------------------------
# @signature int getMonth(String abbr)
# <p>Get the number of the month from an abbreviation.</p>
#
# @input $abbr The month abbreviation.
# @output $mon The number of the month.
# @warning This function will die if the abbreviation is not known.
##------------------------------------------------------------------------------
sub getMonth {
    my $self = shift;
    my $month = shift;

    if ($month =~ /JAN/i) { return 1; }
    elsif ($month =~ /FEB/i) { return 2; }
    elsif ($month =~ /MAR/i) { return 3; }
    elsif ($month =~ /APR/i) { return 4; }
    elsif ($month =~ /MAY/i) { return 5; }
    elsif ($month =~ /JUN/i) { return 6; }
    elsif ($month =~ /JUL/i) { return 7; }
    elsif ($month =~ /AUG/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    elsif ($month =~ /OCT/i) { return 10; }
    elsif ($month =~ /NOV/i) { return 11; }
    elsif ($month =~ /DEC/i) { return 12; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature ClassHeader parseHeader(String file, String[] lines)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parseHeader {
    my ($self,$file,$empty_file,@headerlines) = @_;
    my $header = ClassHeader->new();

	$filename = $file; 
	if ($debug2) {printf("parsing header for %s\n",$filename);}
    
	# -------------------------------------------
	# Add flags to indicate header lines found
    # -------------------------------------------
	my $Location_Found = 0;
	my $RSNumber_Found = 0;
	my $Started_Found = 0;


    # Set the type of sounding "Data Type:" header line
    $header->setType("CLASS");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("HESS");
	# "Release Site Type/Site ID:" header line
    $header->setSite("HESS;Hesston, KS");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
 	my $index = 0;
	foreach my $line (@headerlines) 
	{

        # if there is a line and it is NOT a blank line
		if (($line) && ($line !~ /^\s*\/\s*$/))
		{
			if ($debug2) {print "parseHeader:: (index = $index); line: xxx $line xxx \n";}
            # if ($line =~ /^\s*$/) { next; }

	        # -----------------------------------------------------------
    	    # Add the non-predefined header lines to the header.
        	# skip over any blank lines (empty or contain white space only)
	        # -----------------------------------------------------------
   		    if ($line =~ /RS-Number/i)
   	    	{
				$RSNumber_Found = 1;
	   			chomp ($line);
   			    my ($label,@contents) = split(/:/,$line);
   				$label = "Sonde Id/Sonde Type";
				# need to remove CTRL-M
				if ($debug2) {print "SONDE ID: $contents[0]\n";}
				$contents[0] =~ s/\r//g;
				if ($debug2) {print "SONDE ID: $contents[0]\n";}
   				$contents[1] = "Vaisala RS92";
	   	        $header->setLine(5, trim($label).":",trim(join("/",@contents)));
   		    }
        	
	        if ($line =~ /Location/)
   			{
				$Location_Found = 1;
   			    chomp ($line);
   		    	my (@act_releaseLoc) = (split(' ',(split(/:/,$line))[1]));
	   		    my $lat = $act_releaseLoc[0];

   				my $lon = $act_releaseLoc[2];
				if ($debug2) {print "@act_releaseLoc\n";}
				if ($act_releaseLoc[3] =~ /W/)
				{
					$lon = "-".$lon;
				}
            	my $alt = $act_releaseLoc[4];
	            if ($debug2) {print "LAT: $lat  LON: $lon  ALT: $alt\n";}

		        $header->setLatitude($lat,$self->buildLatLonFormat($lat));
	    	    $header->setLongitude($lon,$self->buildLatLonFormat($lon));

           		$header->setAltitude($alt,"m");

	            #----------------------------------------------------------
    	      	# If the lat/lon in the raw data header do NOT match
	    	    # the hardcoded, specified lat/lon for Hesston, KS, then
    	    	# do NOT send this sounding to the GTS. Write an error
	        	# to the error log for users to review. OK to complete
    	      	# processing of sounding into Class format just don't send
        	  	# to GTS. Per Scot: FP6: 38.144N, 97.439W, 451m for Hesston
          		# ----------------------------------------------------------
				# CHANGES REQUIRED FOR PRODUCTION CODE
				# ----------------------------------------------------------
	          	my $HesstonLat =  38.14; # HARDCODED: SL says use 38.144 but raw data accurate to 2 digits
    	      	my $HesstonLon = -97.44; # HARDCODED: SL says use -97.439 but raw data accurate to 2 digits
        	  	my $HesstonAlt = 451.00; # HARDCODED: SL says use 451.00 in his email of 13 May 2015
          		# my $HesstonLat =  36.61; # HARDCODED: for test purposes to match the sample data
				# my $HesstonLon = -97.49; # HARDCODED: for test purposes to match the sample data
				# my $HesstonAlt = 315.00; # HARDCODED: for test purposes to match the sample data
        	
				my $headerLat = $header->getLatitude();
			    my $headerLon = $header->getLongitude();
				my $headerAlt = $header->getAltitude();

				if ($debug2) {print "HEADER LAT: $headerLat should match $HesstonLat \n";}
				if ($debug2) {print "HEADER LON: $headerLon should match $HesstonLon\n";}

    	      	my $absLatDiff = abs($HesstonLat-$HesstonLat);
        	  	my $absLonDiff = abs($HesstonLon-$HesstonLon);
          		my $absAltDiff = abs($HesstonAlt-$HesstonAlt);
	          	if ($debug2) {print "absLatDiff = $absLatDiff; absLonDiff= $absLonDiff; absAltDiff= $absAltDiff\n";} 

    	      	# --------------------------------------------------
        	  	# Check the Latitude versus expected HARCODED value.
          		# --------------------------------------------------
	          	if ( ($absLatDiff > 0.10))
    	        {
					if ($debug2) {print "ERROR:: Header Latitude DOES NOT MATCH expected site lat! Do Not Send to GTS.\n";}
            	 	$errText =  $errText."ERROR:: Header Latitude DOES NOT MATCH expected site lat! Do Not Send to GTS.\n";

             		print  $ERROUT "ERROR:: Header Latitude DOES NOT MATCH expected site lat! Do Not Send to GTS.\n";
	            } # lat DOES NOT MATCH expected HARDCODED values so error - DO NOT send to GTS
    	      	else
        	    {
					if ($debug2) {print "Header Latitude MATCHES expected site lat. Send sounding to GTS.\n";}
	            }

    	      	# --------------------------------------------------
        	  	# Check the Longitude versus expected HARCODED value.
          		# --------------------------------------------------
	          	if ( ($absLonDiff > 0.10))
    	        {
					if ($debug2) {print "ERROR:: Header Longitude DOES NOT MATCH expected site lon! Do Not Send to GTS.\n";}
            	 	$errText =  $errText."ERROR:: Header Longitude DOES NOT MATCH expected site lon! Do Not Send to GTS.\n";

             		print  $ERROUT "ERROR:: Header Longitude DOES NOT MATCH expected site lon! Do Not Send to GTS.\n";
	            } # Longitude DOES NOT MATCH expected HARDCODED values so error - DO NOT send to GTS
    	      	else
        	    {
					if ($debug2) {print "Header Longitude MATCHES expected site lon. Send sounding to GTS.\n";}
	            }


    	      	# --------------------------------------------------------------------
        	  	# Check the Elevation/Altitude versus expected HARCODED value.
          		# HARDCODED ELEVATION CHECK
	          	# --------------------------------------------------------------------
    	      	# Check that the first record altitude is within 20 meters
        	  	# of expected value. 
          		# --------------------------------------------------------------------
	          	if ( ($absAltDiff > 20))
				{
        	     	if ($debug2) {print "ERROR:: Header Elevation/Altitude DOES NOT MATCH expected site Elevation/Altitude! Do Not Send to GTS.\n";}
            	 	$errText =  $errText."ERROR:: Header Elevation/Altitude DOES NOT MATCH expected site Elevation/Altitude! Do Not Send to GTS.\n";

             		print  $ERROUT "ERROR:: Header Elevation/Altitude DOES NOT MATCH expected site Elevation/Altitude! Do Not Send to GTS.\n";
	            } # Elevation/Altitude DOES NOT MATCH expected HARDCODED values so error - DO NOT send to GTS
    	      	else
        	    {	
            	 	if ($debug2) {print "Header Elevation/Altitude MATCHES expected site Elevation/Altitude. Send sounding to GTS.\n";}
	            }

   			}
   			# "Started at       1 August 2008 11:30 UTC" 
	  		if ($line =~ /Started at/)
   			{
				$Started_Found = 1;
   				chomp ($line);
	   		    my (@releaseTime) =  (split(' ',trim($line)));
   				my ($hour, $min) = (split(':',trim($releaseTime[5])));
   				if ($debug2) {print "HOURS: $hour   MIN: $min\n";}
   				my $time = sprintf("%02d:%02d:00", $hour, $min);
	            my $date = sprintf("20%02d, %02d, %02d", $releaseTime[4],
   			           $self->getMonth($releaseTime[3]),$releaseTime[2]);
   			    if ($debug2) {print "DATE: $date   TIME: $time\n";}
        	    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    			$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
		    }
    	
   			$index++;
		}
	} # foreach


   	# -----------------------------------------------------
  	# Check that the Header Lines were present in raw data.
	# -----------------------------------------------------
	# Location Line in Raw Data Header, 
	# e.g., Location : 36.61 N  97.49 W    315 m
	# -----------------------------------------------------
   	if (!$Location_Found)
    {
		if ($debug2) {print "ERROR:: Location not found in raw data, header lat/lon/alt are missing! Do Not Send to GTS.\n";}
   	 	$errText =  $errText."ERROR:: Location not found in raw data, header lat/lon/alt are missing! Do Not Send to GTS.\n";
   		print  $ERROUT "ERROR:: Location not found in raw data, header lat/lon/alt are missing! Do Not Send to GTS.\n";
    }
   	else
    {
		if ($debug2) {print "Location (header lat/lon/alt values) found in raw data. Send sounding to GTS.\n";}
    }

	# -----------------------------------------------------
    # Radiosonde Line in Raw Data Header, 
	# e.g., RS-number: J1933114
	# -----------------------------------------------------
   	if (!$RSNumber_Found)
    {
		if ($debug2) {print "ERROR:: Radiosonde Line not found in raw data header! Do Not Send to GTS.\n";}
   	 	$errText =  $errText."ERROR:: Radiosonde Line not found in raw data header! Do Not Send to GTS.\n";
   		print  $ERROUT "ERROR:: Radiosonde Line not found in raw data header! Do Not Send to GTS.\n";
    }
   	else
    {
		if ($debug2) {print "Radiosonde Line found in raw data. Send sounding to GTS.\n";}
    }

	# -----------------------------------------------------
   	# Started Line in Raw Data Header, 
	# e.g., Started at: 17 APR 15 05:14 UTC
	# -----------------------------------------------------
	if (!$Started_Found)
    {
		if ($debug2) {print "ERROR:: Started Line (Date/Time) not found in raw data header! Do Not Send to GTS.\n";}
   	 	$errText =  $errText."ERROR:: Started Line (Date/Time) not found in raw data header! Do Not Send to GTS.\n";
   		print  $ERROUT "ERROR:: Started Line (Date/Time) not found in raw data header! Do Not Send to GTS.\n";
    }
   	else
    {
		if ($debug2) {print "Started Line (Date/Time) found in raw data. Send sounding to GTS.\n";}
    }



    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: sgpsonde-curC1.20150417.0514.rawa
	#
	# NOTE: June 3 actual data have: sgpsonde-curS01.20150602.0300.raw
	#
	# CHANGES MAY BE REQUIRED FOR PRODUCTION CODE
	#
    # ----------------------------------------------------------
    if ($debug2) {print "file name = $file\n";}

	if ($file =~ /^sgpsonde.*\.(\d{4})(\d{2})(\d{2}).(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
        my $date = join ", ", $year, $month, $day;
		my $time = join ":", $hour,$min,'00';
        if ($debug2) {print "FROM FILE: DATE:  $date   TIME:  $time\n";}

		# if the raw data is empty (zero file size)
		# date and time info for the output file will
		# need to be set here, based on the input file
		# name, since there is no header info
		if (($empty_file) || (!$Started_Found))
		{
			$header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	    	$header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
		}
	}

    return $header;
}
                           
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
    my $WspNoMiss = 0; my $WdirNoMiss = 0;

	my $HdrSurfaceAltMismatch = 0;


    if ($debug) {printf("\n********************\nProcessing file: %s\n\n",$rawDirName."/".$file); }
    if ($debug2) {printf("\n********************\nEnter parseRawFile(): Processing file: %s\n",$rawDirName."/".$file); }
    
    open(my $FILE,$self->{"RAW_WORK_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;

    my $number_lines_in_file = $#lines+1;

    #------------------------------------------------------------
    # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
    #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
    #------------------------------------------------------------
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
    #------------------------------------------------------------
    my @file_info = stat $FILE;

    if ($debug2) 
    {
		print "file_info:: @file_info\n"; 
       	print "number_lines_in_file:: $number_lines_in_file \n";
       	print "File size = $file_info[7]\n";
    }

	my $empty_file = 0;

	if ($file_info[7] == 0)
	{
		$empty_file = 1;
		if ($debug) {print "\n\n\tEMPTY:: Setting File Value to Empty for $file\n\n";}
	}


    close($FILE);
    
    #---------------------------------------------------
    # Open Error Log/Warning file. If there are any
    # errors, then the file being processed can not
    # be put on the GTS so don't move to ASPEN/GTS area.
    #----------------------------------------------------
    my @namepart = split(".raw", $file);
    $errfileName = $namepart[0].".errlog";

    if ($debug2) {print "Error Log file name is $outputDirName/$errfileName\n"; }

    open($ERROUT,">".$self->{"OUTPUT_DIR"}."/".$errfileName) or 
             die("Can't open Error Log file $errfileName for input file $file\n");

    if ($empty_file)
	{
		if ($debug2) {printf("ZERO SIZE RAW DATA FILE -- Unable to make a good header for empty file. Do NOT send sounding to GTS!\n");}

	   	$errText =  $errText."ERROR:: ZERO SIZE RAW DATA FILE -- Unable to make good header for empty file. Do NOT send sounding to GTS!\n";

   		print  $ERROUT "ERROR:: ZERO SIZE RAW DATA FILE -- Unable to make good header for empty file. Do NOT send sounding to GTS!\n";
	}
    
    #------------------------------
    # Generate the sounding header.
    #------------------------------
	my @headerlines = @lines[0..22];
    # the parseHeader function needs to know if this is an empty
	# file so that it can set the release time based on the raw
	# date file name. This is needed in order to create an output 
	# file name
    my $header = $self->parseHeader($file,$empty_file,@headerlines);

    #-----------------------------------------------------------
    # Only continue processing the file if a header was created.
    #-----------------------------------------------------------
    if (defined($header))
	{

	    # ----------------------------------------------------
    	# Create the output file name and open the output file
	    # ----------------------------------------------------
    	# my $outfile;
		my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

		$outfileName = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
  	 						   $header->getId(),
	   						   split(/,/,$header->getActualDate()),
	   						   $hour, $min);

        if ($debug2) {printf("\tHeader was created. Continue processing data recs.\nOutput file name is %s\n", $outputDirName."/".$outfileName); }
	    # if ($debug2) {printf("\tOutput file name:  %s\n", $outfileName);}


		open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfileName)
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
		my $surfaceRecord = 1;

		foreach my $line (@lines) 
		{
        	$TotalRecProc++;

			# Ignore the header lines.
			# HARD-CODED to skip the header lines
			# --------------------------------------------
			# CHANGES MAY BE REQUIRED FOR PRODUCTION CODE
			# --------------------------------------------
	    	if ($index < 27) { $index++; next; }
	
        	# if it is a blank line
        	if ($line =~ /^\s*$/) { next; }

			# if it is a data record, create a record
			elsif ($line =~ /^\s*\d+\s+\d+\s+/) 
			{
                $dataRecProc++;
          		if ($debug3) {print "TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}
	    		my @data = split(' ',$line);

			    my $record = ClassRecord->new($ERROUT,$file);

    	    	# min $data[0]		# temp [5]
				# sec $data[1]		# RH [6]
				# ascent rate [2]	# dewpt [7]
				# height [3]		# wind dir [8]
				# pressure [4]		# wind spd [9]


          		if ($debug3) { print "Parse Data Record:: Temp, TD, RH, Winds, etc.\n";}

	            #----------------------------------------------------
      		    # missing values are -32768 - Assumption! HARD-CODED
          		#----------------------------------------------------

	    		$record->setTime($data[0],$data[1]);
		    	
				$record->setPressure($data[4],"mb") if ($data[4] != -32768);
                if (($data[4] <= 850.00) && ($data[4] != -32768) ) {$pressLess850++;}
				if ($data[4] != -32768) {$PNoMiss++;}

			    $record->setTemperature($data[5],"C") if ($data[5] != -32768);
				if ($data[5] != -32768) {$TNoMiss++;};
				
				$record->setDewPoint($data[7],"C") if ($data[7] != -32768);
				if ($data[7] != -32768) {$TdNoMiss++;};

			    $record->setRelativeHumidity($data[6]) if ($data[6] != -32768);
				if ($data[6] != -32768) {$RHNoMiss++;};

		    	$record->setWindSpeed($data[9],"m/s") if ($data[9] != -32768);
                if ($data[9] != -32768) {$WspNoMiss++;};

		    	$record->setWindDirection($data[8]) if ($data[8] != -32768);
				if ($data[8] != -32768) {$WdirNoMiss++;};

		        $record->setAscensionRate($data[2],"m/s") if ($data[2] != -32768);
    	
				if ($surfaceRecord)
				{
					$record->setLatitude($header->getLatitude(), 
				                     $self->buildLatLonFormat($header->getLatitude()));
					$record->setLongitude($header->getLongitude(), 
			    	                 $self->buildLatLonFormat($header->getLongitude()));
					# check that surface altitude matches header altitude
					my $hdr_alt = $header->getAltitude();
					if ($data[3] != $hdr_alt)
					{
						
						$HdrSurfaceAltMismatch = 1;
						if ($debug) {print "Header Altitude $hdr_alt does not match Surface Altitude $data[3]\n";}
					}
					$surfaceRecord = 0;
				}
			
				$record->setAltitude($data[3],"m") if ($data[3] != -32768);
				if ($data[3] =~ /-32768/)
				{
					$altMissCt++;
					if ($debug2) { print "Missing height, altMissCt:: $altMissCt\n"; }
				}
			    
       			if ($debug3) {print "Processing total, data line number:: $TotalRecProc, $dataRecProc\n";} 
				printf($OUT $record->toString());
	    	}

    		# if it is not a data line
			else 
			{
				last;
			}

	    } #foreach

	} #if $header
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

	#---------------------------------------------------
    # If surface altitude does not match header
	# altitude, then write error to error log and
    # this sounding should not be passed to GTS.
    #---------------------------------------------------
    if ($HdrSurfaceAltMismatch)
    {
		if ($debug2) {printf("Header altitude does not match surface record altitude.\n");}
        $errText =  $errText."ERROR:: Header altitude does not match surface record altitude. Do NOT send sounding to GTS!\n";
      	print  $ERROUT "ERROR:: Header altitude does not match surface record altitude. Do NOT send sounding to GTS!\n";
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
    my $allData = $TNoMiss + $TdNoMiss + $RHNoMiss + $WspNoMiss + $WdirNoMiss;
    if ($debug2) {print "allData count:: $allData\n";}

    if ($allData == 0)
    {
        if ($debug2) {printf("ERROR:: No valid data on any records. Possibly no header lines. Do NOT send sounding to GTS!\n");}
        $errText =  $errText."ERROR:: No valid data on any records. Possibly no header lines. Do NOT send sounding to GTS!\n";
        print  $ERROUT "ERROR:: No valid data on any records. Possibly no header lines. Do NOT send sounding to GTS!\n";
    }

    close($ERROUT);
    # close($OUT);

} # parseRawFile()


##------------------------------------------------------------------------------
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
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    my $cmd = "\n";
    
	opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

	# -------------------------------------------------------------------------
	# Input file names must be of the form: sgpsonde-curC1.YYYYMMDD.HHmm.raw,
	#  e.g., sgpsonde-curC1.20150417.0514.raw, where YYYY = year, MM = month,
	# DD = day, HH = hour and mm = minute, and "raw" is the suffix. This is 
	# the exact form. All files with names of this form will be processed.
	# -------------------------------------------------------------------------
		
    #---------------------------------------------------------------------
    # Grab and process ONLY the files that are between 11 and 1 minute old
    # copy those files to the working ingest directory. Any file less than 
    # one minute old might still be in the process of being downloaded so
    # don't copy or process it. Pick that one up during the next 10min slot.
    # This section of # code based on code from SL's Iquique Skewt script. 
    #
    #------------------------------------------------------------
    # ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
    #  $atime,$mtime,$ctime,$blksize,$blocks) = stat($filename);
    #------------------------------------------------------------
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
    #---------------------------------------------------------------------
	# HARD-CODED FILE NAME
	# CHANGES MAY BE REQUIRED FOR PRODUCTION CODE
	#
    my @all_files = grep(/^sgp.*\.raw$/,sort(readdir($RAW)));
    
    if ($debug2) {print "\n*******************\nEnter readDataFiles: All files in FTP area:: all_files:: @all_files \n";}
    if ($debug2) {print "--------------------\nDecide which files from FTP to copy and process.\n";}
    
    foreach my $file (@all_files) 
    {

        $file = $rawDirName."/".$file;   # Must form complete file name plus directory to ensure correct file

        my @file_info = stat $file;

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

        #------------------------------------------------------------------------
        # Only copy the recent (1-11 mins old) FTP files into 
		# local raw input dir for processing.
        #------------------------------------------------------------------------
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

	
	closedir($RAW);

    #------------------------------------------------------------------
    # Process ONLY files that have been copied to local raw directory.
    #------------------------------------------------------------------
    opendir(my $RAW_WORK,$self->{"RAW_WORK_DIR"}) or die("Can't read raw directory ".$self->{"RAW_WORK_DIR"});

    # -------------------------------------------
	# CHANGES MAY BE REQUIRED FOR PRODUCTION CODE
	# -------------------------------------------
    my @files = grep(/^sgp.*\.raw$/,sort(readdir($RAW_WORK)));   # Check Local input dir for expected name format, just in case

    if ($debug2) {print "\nBefore parseRawFile():: Files copied to work space to be processed:: xxx @files xxx.\n\n";}

    foreach my $input_file (@files) 
   {
       if ($debug2) {print "Processing raw_work_dir file:: $input_file . Call parseRawFile().\n Before parseRawFile() - TotalRecProc: $TotalRecProc, dataRecProc: $dataRecProc\n";}

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
	    { 
			if ($debug2) {print "THERE ARE ERRORS, don't move class file to GTS area.\n";}  # Error during processing
		}
    	
    	#----------------------------------------------------------
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
		#
    	#----------------------------------------------------------
        $cmd = sprintf ("mv %s/%s %s/.", $rawWorkDirName, $input_file, $archiveDirName);
   		if ($debug2) {print "ARCHIVE(mv) working/local RAW Input data. Issue cmd:: $cmd\n";}

       	system "$cmd ";

        $cmd = sprintf ("mv %s/%s %s/.", $outputDirName, $outfileName, $archiveDirName);
   	    if ($debug2) {print "ARCHIVE(mv) CLASS file. Issue cmd:: $cmd\n";}

       	system "$cmd ";

        $cmd = sprintf ("mv  %s/%s %s/.", $outputDirName, $errfileName, $archiveDirName);
   	    if ($debug2) {print "errfileName: $errfileName\n";}
       	if ($debug2) {print "ARCHIVE(mv) ERROR Log. Issue cmd:: $cmd\n";}
      
        system "$cmd ";

   	    #------------------------------------------------------
       	# Send email saying what has been processed.
        # 
   	    # Add sfw\@ucar.edu and loehrer\@ucar.edu to the 
       	# "addressto" for production.
        #------------------------------------------------------
		my $addressfrom = sprintf "%s", "echohawk\@ucar.edu"; #HARDCODED EMAIL
		my $addressto;

	    if ($test_mode)
		{
			$addressto = sprintf "%s", "echohawk\@ucar.edu"; # TEST ONLY - HARDCODED EMAIL
		}
		else
		{

			# $addressto = sprintf "%s", "echohawk\@ucar.edu"; # TEST ONLY - HARDCODED EMAIL
			$addressto = sprintf "%s", "cully\@ucar.edu, echohawk\@ucar.edu, loehrer\@ucar.edu, sfw\@ucar.edu"; #HARDCODED PRODUCTION CODE EMAIL
		}

        my $errlength = length($errText);
   	    my $message;

       	if ($errlength > 0 || $number_lines_in_ERROR_file > 0)
        { 
			$message = "PISA FP6 Hesston, KS: Sounding $input_file Processed - ERROR"; 
		}
	    else
   	    { 
			$message = "PISA FP6 Hesston, KS: Sounding $input_file Processed"; 
		}

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
       	    print $mailer "The following PISA FP6 Hesston, KS data file has been processed:\n $rawDirName/$input_file .\n\nThis file contained $TotalRecProc total records and $dataRecProc data records.\n\nAll files (*.dat, *.cls, *.errlog) have been COPIED to the \narchive area at  $archiveDirName .\n\nERROR MESSAGES::\n$errText \nBeware that junky input files may cause unexpected results. Review the *.errlog file for more ERROR information.\n\nThere were errors creating the Class file, so the class file WAS NOT copied \nto the GTS directory at $gtsDirName\n";
       	}
        else
   	    {
       	    print $mailer "The following PISA FP6 Hesston, KS data file has been processed:\n $rawDirName/$input_file .\n\nThis file contained $TotalRecProc total records and $dataRecProc data records.\n\nAll files (*.dat, *.cls, *.errlog) have been COPIED to the \narchive area at  $archiveDirName .\n\n$errText \nClass file created and copied to GTS directory at $gtsDirName\n";
       	}

        $mailer->close();

   	    $TotalRecProc = 0;
       	$dataRecProc = 0;

        $errText = "";

    } # process each file 

} # readDataFiles

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
}
