#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The Ron_Brown_SND_Converter.pl script is used for converting the sounding data
# from the R/V Ron Brown.
#
# @usage RonBrownConverter.pl [--skip] [--limit] [--max=10]
#        --skip   Skip the pre-processing steps; default is false (don't skip)
#                 These steps strip out the blank lines in a file
#        --limit  Limit the number of records processed to --max and put
#                 output files in output directory
#        --max    If not specified, the default limit is 9999 (1-sec data) records
#        NOTE:    If the limit option is not used, all records will be processed
#                 and placed in the verbose_output directory; however, times 
#                 greater than 9999.9 will be changed to "missing" value.
#
# @author Linda Echo-Hawk 2011-01-11
# @version VOCALS_2008 corrected data received for processing Nov. 2010
#          - Added @record_list array: All data records are read in and then
#            processed in reverse order to remove the descending data that 
#            was included in the corrected Ron Brown data (i.e., ascension
#            rate was negative).
#          - Because there was not a standard missing value for Temperature,
#            incorrect dewpoints were being calculated when they should have
#            been set to missing.  Scot L. had me set the dewpoint to "missing"
#            if the RH value was missing.  A check of the RH value was added
#            to the code before setting dewpoint. 
#
# @author Linda Echo-Hawk 2009-10-07
# @version VOCALS_2008 Command line options were added to allow the user to 
#          skip pre-processing steps, and to limit the number of data records
#          processed.  Clarification (2009-12-09): The problem is with the data 
#          records with TIME values > 9999.9.  This data field is limited
#          to 6 characters.
# BEWARE:  The SCUDS skew-t generator cannot handles files with >9999
#          records (per Scot Loehrer to Linda Cully, 2008-11-12). 
#          - This data is not always 1-second data.  Some seconds are skipped, 
#          so just limiting the number of records is not a good solution;  
#          I need to check the time instead.
#          - Added code to handle missing wind values of "0.0" by comparing
#          to last valid actual wind value. (2009-12-09 echohawk)   
#
# @author L. Cully 
#  October 2008
#  Completed upgrade to s/w started by J. Clawson where he had taken the R/V
#  Ulloa sounding conversion s/w from the NAME project and began converting to
#  handle newer form of R/V Ron Brown sounding data. J. Clawson began his work
#  using a sample sounding from the STRATUS07 Ron Brown project.  L. Cully fixed
#  s/w to now output stationCD.out file and associated station summary file.
#  Added fns to create these station output files. Added some comments and debug.
#  Updated to use proper libs based on OS. Updated call to create new ClassRecord
#  which now used 3 input parameters instead of 2 used by older version of libs.
#  Updated s/w to handle VOCALS type Ron Brown soundings that might contain 
#  undetermined number of/location of blank lines. Strip blank lines. Also,
#  now grab "Height" instead of "GPS_Alt" for the altitude in hdr and data recs.
#  Added code to calc Ascension Rate.  Calc the Ascension rate with times (even
#  large times before resetting large times to missing. Changed output format to pad 
#  lat/lon with zeroes. Per SLoehrer's request also changed ClassRecord.pm module to
#  no longer do Dewpt range checks and output -99.9. Done for this conversion,
#  but will affect other sonde processing.   Updated code to now search for and
#  use the first non-missing lat/lon pair from data lines as the release Location.
#
# WARNING: Not yet confident what MISSING values in raw data will be for 
#  each parm. This needs to be confirmed and code updated accordingly. WARNING!!!
#  (Search for the word "Verify".)
#
# WARNING: From SL on 21 Oct 2008: The only potential item is the Nominal time
#  within the header.  But it's not clear what their release sched
#  will be for the cruise and if it will change at some point.  It looks
#  like they're starting with the 6/day sched that Iquique is also using.
#  So we'll just keep the Nominal as the actual for now.
#
##Module-------------------------------------------------------------------------
package Ron_Brown_SND_Converter;
use strict;

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}

use DpgDate qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use SimpleStationMap;
use Station;
# import module to set up command line options
use Getopt::Long;


my ($WARN);


printf "\nRon_Brown_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug_header = 0;
my $debug_ascent = 0;

# read command line arguments 
my $result;   
# skip pre-processing steps for raw data files
my $skip;
# limit number of data records processed; default is process all records
my $limit;
# if ($limit), specify number of records to process; default is 9999
my $maxRecords = 9999;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("skip" => \$skip, "limit" => \$limit, "max:i" => \$maxRecords);

if ($skip)
{
 	printf("Skip pre-processing steps.\n");
}
else
{
   	printf("Perform pre-processing.\n");
}
if ($limit)
{
 	printf("Processing with limit option set.\n\n");
}
else
{
	printf("Process all records - no limit set.\n\n");
}                   


&main();

if ($limit)
{
 	printf("\nProcessing limited to records with times less than 10000.0 seconds.  Remaining records were cut off.  Converted output in output directory.\n\n");
}
else
{
	printf("\nProcessed all records - no limit set.  Converted output in verbose_output directory.\n\n");
}      
printf "\nRon_Brown_Converter.pl ended on ";print scalar localtime;printf "\n";

#-------------------------------------------------
# A collection of functions that contain constants
#-------------------------------------------------
sub getNetworkName { return "RonBrown";  }                        
sub getVerboseOutputDirectory { return "../verbose_output"; }
sub getSkewtOutputDirectory { return "../output"; }
sub getProjectName { return "VOCALS_2008"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Ron_Brown_SND_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getVerboseOutputDirectory()) unless (-e getVerboseOutputDirectory());
	if ($limit)
	{
		mkdir(getSkewtOutputDirectory()) unless (-e getSkewtOutputDirectory());
	}
    mkdir("../final") unless (-e "../final");

    $self->readRawDataFiles();
    $self->printStationFiles();
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
# @signature Ron_Brown_SND_Converter new()
# <p>Create a new Ron_Brown_SND_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = SimpleStationMap->new();

    $self->{"FINAL_DIR"} = "../final";
    $self->{"NETWORK"} = getNetworkName();
    $self->{"PROJECT"} = getProjectName(); 

    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));

    return $self;
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);

    open($STN, ">".$self->getStationFile()) || die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.txt$/);
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readRawFile(String file_name)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file_name The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my $file_name = shift;
    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);

    my $number_recs_processed = 0; # not sure this is useful

    # Strip blank lines from the raw data file
    # use the --skip cmd line option if this step has been performed already
	if (!$skip)
	{
		my $cmd_perl = "perl -pe 's/^\\s\+\$//'";
		my $cmd;

        #----------------------------------------------------------------------
        # Preprocess each file by stripping all blank lines. 
        # Doing this because the input file formats particularly for VOCALS 2008
        # project seem to be varying and contain mixed in blank lines. Stripping
        # blank lines will add some consistency. (LEC)
        # E.g., command to strip blank lines: perl -pe 's/^\s+$//' infile > outfile
        #-----------------------------------------------------------------------
        $cmd = sprintf "%s %s > %s.noBlanks", $cmd_perl, $file, $file;

        print "\nIssue the following command: $cmd\n";
        system $cmd;

        # Save the original input file in *.orig
        print "Executing: /bin/mv $file $file.orig \n";
        system "/bin/mv -f $file $file.orig";

        print "Executing: /bin/mv $file.noBlanks $file \n";
        system "/bin/mv -f $file.noBlanks $file";
    }


    printf("\nProcessing file: %s\n",$file_name);
    open(my $FILE,$file) or die("Cannot open file: $file\n");

    my $header = ClassHeader->new();
    $header->setProject($self->getProjectName());
    $header->setType("Ron Brown Soundings");
    $header->setReleaseDirection("Ascending");

    # Locate the Sounding Serial Number. Assume in 1st line.
    <$FILE> =~ /RS Serial .:\s*(.+)/;       
    $header->setLine(5,"Radiosonde Serial Number:", trim($1));

    # For Ron Brown appears to be WTEC. Assume in 2nd line.
	<$FILE> =~ /Station Name:\s*(.+)\s*/;   
    $header->setId(trim($1));
    $header->setSite(sprintf("R/V Ron Brown/%s", trim($1) ));

    # Locate the Launch Time. Assume in 3rd line. 
	<$FILE> =~ /Launch Time:\s+(\d\d)(\d\d)Z/;   
    my $time = sprintf("%02d:%02d", $1, $2);

    # Locate the Launch Date. Assume in 4th line.
	<$FILE> =~ /Launch Date:\s*(\d{1,2})\s+(.+)\s+(\d{2})/;     
    my $date = sprintf("20%02d, %02d, %02d", $3, $self->getMonth($2), $1);

    # Set the Release date/time 
	$header->setNominalRelease($date, "YYYY, MM, DD", $time, "HH:MM", 0); 
	$header->setActualRelease($date, "YYYY, MM, DD", $time, "HH:MM", 0);  

    #-----------------------------------------------------------------------
    # In original s/w needed to read 6 blank or unused header lines.
    # Now that we strip the blank lines, only need to skip 3 header lines::
    # "EDT LEVEL OUTPUT with GPS Lat, Long, Alt" 
    # "Time     Height  Pressure      Temp    Dew P.    RH    W Spd    W Dir
    #             Lat            Long      GPS Alt"
    # "sec       mtrs       hPa        .C       .C    Pct      m/s     Az .
    #       Decimal .       Decimal .         mtrs"
    #-----------------------------------------------------------------------
    <$FILE>;<$FILE>;<$FILE>; # Skip the remaining three unused lines of the header.

    $number_recs_processed = 7;

    #-----------------------------------------------------------------
    # All soundings come from the Research Vessel Ron Brown. Since the
    # Ron Brown is a mobile station, the lat/lons will be missing
    # in the stationCD.out file and the type will be 'm'.
    #-----------------------------------------------------------------
    my $station = $self->{"stations"}->getStation(getNetworkName(),$self->{"NETWORK"});
    if (!defined($station)) {
        $station = $self->build_default_station(getNetworkName(),$self->{"NETWORK"});
        $self->{"stations"}->addStation($station);
    }
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    #-------------------------------------------------
    # Open the output file in the ../output directory.
    #-------------------------------------------------
    my $filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$header->getId(),
			   split(/, /,$header->getActualDate()),
			   split(/:/,$header->getActualTime()));
    
	my $OUT;
	if (!$limit)
	{
		    open($OUT,sprintf(">%s/%s",getVerboseOutputDirectory(),$filename)) or 
			die("Cannot open output file\n");
	}
	else
	{
		    open($OUT,sprintf(">%s/%s",getSkewtOutputDirectory(),$filename)) or 
			die("Cannot open skewt output file\n");
	}
    
	# ---------------------------------------------
    # Get the remaining (data) lines from the file
    # ---------------------------------------------
    my @lines = <$FILE>; 
    if ($debug_header) { print "first input file data line::\n $lines[0]"; }

    
	# --------------------------------------------
    # Create an array to hold all of the data records.
	# This is required so additional processing can take
    # place to remove descending data records at the
	# end of the data files
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------
    
	my $first_line = 1;
	my $ReleaseLocFound = 0;

    my $prev_record;
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;


    # The last non-zero, non-missing wind speed value.
	# If file starts with zero winds, set to zero until
	# a valid non-zero wind is found. Initializing with
	# value "2" (value > 1) guarantees this.
	my $lastWindspd = 2;

    #------------------------------------------------------
    # Process each of the data lines.
    # Note that $WARN is the ../output/warning.log file.
    #
    # WARNING: Missing values vary by parameter.
    # Pressure = -9999.9
    # Wind speed and direction = 0.0
    # Lat and Lon = -999.9900000
    # RH = -32768
	# Temp = No standard value for missing
	# Dewpoint = No standard value for missing
    # Altitude = -9999.9
    #  Assuming missing time is -999.9 
    # The raw data missing values need to be correctly
    # translated into the proper output missing values.
    #------------------------------------------------------
    foreach my $line (@lines) 
	{
	    my $rec = ClassRecord->new($WARN, $filename, $prev_record);
        my @data = split(' ', trim($line));
        if (($limit) && ($data[0] >= 10000.0))
		{
			printf("Limited to records with times <10000.0s for %s.\n", $filename);
			last;
		}

        $number_recs_processed++;
        
        # Verify Missing Value in raw data!!!
        $rec->setTime($data[0])             unless ($data[0] == -999.99); #  no missing found
        $rec->setPressure($data[2], "hPa")  unless ($data[2] == -9999.9); #  9999.0 Default Missing 
        $rec->setTemperature($data[3], "C") unless ($data[3] == -999.99); #  999.0 M
        $rec->setRelativeHumidity($data[5]) unless ($data[5] == -32768);  #  999.0
		if ($data[5] != -32768)
		{
			# per Scot, if the RH is missing, set dewpoint to missing
			$rec->setDewPoint($data[4], "C")    unless ($data[4] == -999.99); # no known missing value
		}
          

        #---------------------------------------------------
        # Set Wind parms and location
        #---------------------------------------------------
        # Because "0.0" was used for the Missing value but
        # could also be a valid value, read in and save the
        # last non-zero, non-missing wind speed value
        #---------------------------------------------------
		my $windSpd = $data[6];
		my $windDir = $data[7];
        if ($windSpd == 0.0)
		{
			if ($windDir == 0.0)
	        {
	            if ($lastWindspd >= 1.0)
	            {
					$windSpd = -999.9;
					$windDir = -999.9;
	            }
	        }
        }
        else
        {
	        $lastWindspd = $windSpd;
        }
    	
        $rec->setWindSpeed($windSpd, "m/s") unless ($windSpd == -999.9); # Missing value set above
        $rec->setWindDirection($windDir)    unless ($windDir == -999.9); # Missing value set above

        $rec->setLatitude($data[8], $self->buildLatLonFormat($data[8]))  
				unless ($data[8] == -999.9900000); # Verified
    	$rec->setLongitude($data[9], $self->buildLatLonFormat($data[9])) 
				unless ($data[9] == -999.9900000); # Verified
    	$rec->setAltitude($data[1], "m") unless ($data[1] == -9999.9); # Use Height, not GPS_Alt.

        # -----------------------------
        # Get header info from the data
        # either on the first line or
	    # within the first 60 seconds
	    # -----------------------------
    	if ($first_line) 
		{
            if ($debug_header) { print "First Data Line. lat, lon:: $data[8], $data[9] \n"; }

	        $header->setLatitude($data[8], $self->buildLatLonFormat($data[8]))  
					unless ($data[8] == -999.9900000); # Verified
	        $header->setLongitude($data[9], $self->buildLatLonFormat($data[9])) 
					unless ($data[9] == -999.9900000); # Verified

	        $header->setAltitude($data[1], "m") 
					unless ($data[1] == -100000.0); # Use Height, not GPS_Alt.

	        if ( $data[8] > -999.00 ||  $data[9] > -999.00 )  # Lat/Lon Not Missing
            {
				if ($debug_header) 
				{
					my $currtime=$rec->getTime(); 
					print "Found release loc on FIRST line at time=$currtime.\n";
				}

                $ReleaseLocFound = 1;
                print($OUT $header->toString());
            }

            $prev_time = $rec->getTime();
            $prev_alt = $rec->getAltitude();

	        $first_line = 0;
   	    } # end if($first_line)
        else
        {
            # for all lines after the first line


            #---------------------------------------------------------------
		    # Header info not found on line 1 of the data, keep looking.
            # Keep trying to determine the Release location up to 1 min past
            # the release time, if not found before. 
            # Sometimes the first (so many) lat/lons are missing.
            #---------------------------------------------------------------
            if (!$ReleaseLocFound && ($rec->getTime() <= 60.0) )
            {
				if ($debug_header) 
				{
					print "Did NOT find  release loc on first line. Keep Searching\n";
					print "lat, lon:: $data[8], $data[9] \n";
				}

                $header->setLatitude($data[8], $self->buildLatLonFormat($data[8]))  
						unless ($data[8] == -999.9900000); # Verified
                $header->setLongitude($data[9], $self->buildLatLonFormat($data[9])) 
						unless ($data[9] == -999.9900000); # Verified

	            if ( $data[8] > -999.00 ||  $data[9] > -999.00 )  # lat/lon Not Missing
                {
                    if ($debug_header) 
					{
						my $curtime=$rec->getTime(); 
						print "Found release loc on line at time=$curtime.\n"; 
					}

                    $ReleaseLocFound = 1;
                    print($OUT $header->toString());
                }

            } # Searching for Release Loc in first 60 secs.

            if (!$ReleaseLocFound && ($rec->getTime() > 60.0) )
            {
				$ReleaseLocFound = 1;   # Give up! Can't find release loc within time limit.
                print($OUT $header->toString());  # Print the header to output.
            }

            #-------------------------------------------------------
			# The ascent rate is not included in the raw data 
			# for VOCALS 2008 and must be calculated.
            #-------------------------------------------------------
            # Calculate the ascension rate which is the difference
            # in altitudes divided by the change in time. Ascension
            # rates can be positive, zero, or negative. But the time
            # must always be increasing (the norm) and not missing.
		    # BEWARE: Continuous negative ascension rates indicate
			# the sonde is falling.
            #
            # Only save off the next non-missing values.
            # Ascension rates over spans of missing values are OK.
            #-------------------------------------------------------
            if ($debug_ascent) 
			{ 
				my $time = $rec->getTime(); 
				my $alt = $rec->getAltitude(); 
                print "Calculate Ascent Rate.\n";
				print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: ";
				print "$prev_time, $time, $prev_alt, $alt\n"; 
			}

            if ($prev_time != 9999  && $rec->getTime()     != 9999  &&
                $prev_alt  != 99999 && $rec->getAltitude() != 99999 &&
                $prev_time != $rec->getTime() ) 
            {
				$rec->setAscensionRate( ($rec->getAltitude() - $prev_alt) /
                                        ($rec->getTime() - $prev_time),"m/s");

                if ($debug_ascent) { print "Calculated Ascension Rate.\n"; }
            }

            #-----------------------------------------------------
            # Save the next non-missing values. 
            # Ascension rates over spans of missing values are OK.
            #-----------------------------------------------------
            if ($debug_ascent) 
			{ 
				my $rectime = $rec->getTime(); 
				my $recalt = $rec->getAltitude();
                print "Current Record: Time: $rectime, Alt: $recalt\n"; 
			}

            # reset previous time and altitude with latest, if not missing
            if ($rec->getTime() != 9999 && $rec->getAltitude() != 99999)
            {
				$prev_time = $rec->getTime();
                $prev_alt = $rec->getAltitude();

                if ($debug_ascent) 
				{ 
					print "Saved current rec with valid Time and Alt as previous.\n"; 
				}
            }

        }# end Calc Ascension Rate
     
        # --------------------------------------------
		# add each record to the record_list array
        # for further processing to remove descending 
        # data before calling print toString
        push(@record_list, $rec);
        # --------------------------------------------

    } # End Foreach my lines. - process all data lines.
    
    if ($debug_header) 
	{ 
		print "readRawFile:: Total number_recs_processed = $number_recs_processed .\n\n"; 
	}

	# --------------------------------------------------
	# Remove the last records in the file that are 
    # descending (ascent rate is negative)
	# --------------------------------------------------
	foreach my $record (reverse(@record_list))
	{
		if (($record->getAscensionRate() < 0.0) ||
			($record->getAscensionRate() == 999.0))
		{
			undef($record);
		} 
		else 
		{
			last;
		}
	}
    
	#-------------------------------------------------------------
    # Convert ClassRecord to String. This call also checks values.
    # See ClassRecord.pm/check_values() for DewPt < -99.9 check.
    # Note that if Td < -99.9, warning message is written to 
    # ../output/warning.log file. Also, every parm is compared to
    # to it's pre-known missing value and the allowed output format
    # size/length. The values is reset to it's missing value if
    # too big for output format. Even Time is checked. A warning
    # message of "xxx value yyy is too big for the field at time zzz.
    # Setting to missing." is issued for each parm.
    #-------------------------------------------------------------
    # Print the records to the file.
	#-------------------------------------------------------------
	foreach my $record(@record_list) 
	{
		print ($OUT $record->toString()) if (defined($record));
	}	
           
    close($OUT);
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the Ron Brown Ship using the specified
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);

    $station->setStationName($network);
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(999);
    $station->setMobilityFlag("m");
    return $station;
}


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
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name {
    my ($self,$text) = @_;

    # Convert spaces to underscores.
    $text =~ s/\s+/_/g;

    # Remove all hyphens
    $text =~ s/\-//g;

    return $text;
}

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing whitespace around a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed line.
##------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}






