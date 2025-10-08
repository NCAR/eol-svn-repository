#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The Haenam_Radiosonde_Converter.pl script is used for converting 
# the sounding data from the South Korean Haenam site.
#
# @usage Haenam_Radiosonde_Converter.pl [--skip]
#        --skip   Skip the pre-processing steps to strip out blank lines.
#                 This step only needs to be done once.  The default is false.
#        --limit  Limit the number of records processed to --max and put
#                 output files in output directory
#        --max    If not specified, the default limit is 500 (2-sec data) records
#        NOTE:    If the limit option is not used, all records will be processed
#                 and placed in the verbose_output directory; however, times 
#                 greater than 9999.9 will be changed to "missing" value.   
#
# @author Linda Echo-Hawk 2010_04_21
# @version T-PARC 2008 Adapted from the RonBrownSoundingConverter.pl
#          - Raw data wind speed is converted from knots to m/s.
#          - Ascent rate is derived in the code.  Raw data ascent rate with
#            units of meters/minute should not be used per Scot L.
#          - Command line options were added to allow the user to 
#            skip the pre-processing steps to strip out blank lines.  This
#            only needs to be done once.  Using the --skip option speeds 
#            up the converter.
#          - Command line options to limit the number of records processed 
#            to those with times < 10000 were added.  If the "limit" option
#            is not used, all records will be processed and placed in the
#            verbose output directory; however, times greater than 9999.9 
#            will be changed to "missing" value.
# BEWARE:  The SCUDS skew-t generator cannot handles files with times >9999
#          (per Scot Loehrer to Linda Cully, 2008-11-12).
#
##Module-------------------------------------------------------------------------
package Haenam_Radiosonde_Converter;
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
use DpgConversions;
use ClassHeader;
use ClassRecord;
use ElevatedStationMap;
use Station;
# import module to set up command line options
use Getopt::Long;


my ($WARN);


printf "\nHaenam_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_ascent_rate = 0;
my $debug_release = 0;

# read command line arguments 
my $result;   
# skip pre-processing steps for raw data files
my $skip;
# limit number of data records processed; default is process all records
my $limit;
# if ($limit), specify number of records to process; default is 500
my $maxRecords = 500;  
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
printf "\nHaenam_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

#-------------------------------------------------
# A collection of functions that contain constants
#-------------------------------------------------
sub getNetworkName { return "Haenam";  }                        
sub getVerboseOutputDirectory { return "../verbose_output"; }
sub getSkewtOutputDirectory { return "../output"; }
sub getProjectName { return "T-PARC"; }
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
    my $converter = Haenam_Radiosonde_Converter->new();
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
# @signature Haenam_Radiosonde_Converter new()
# <p>Create a new Haenam_Radiosonde_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

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


    # use the --skip cmd line option if this step has been performed already
	if (!$skip)
	{
		my $cmd_perl = "perl -pe 's/^\\s\+\$//'";
		my $cmd;

        #----------------------------------------------------------------------
        # Preprocess each file by stripping all blank lines. 
        # E.g., command to strip blank lines: perl -pe 's/^\s+$//' infile > outfile
        #-----------------------------------------------------------------------
        $cmd = sprintf "%s %s > %s.noBlanks", $cmd_perl, $file, $file;

        print "\nIssue the following command: $cmd\n";
        system $cmd;

        print "Executing: /bin/mv $file $file.orig \n"; # Save the original input file in *.orig
        system "/bin/mv -f $file $file.orig";

        print "Executing: /bin/mv $file.noBlanks $file \n";
        system "/bin/mv -f $file.noBlanks $file";
    }

    printf("Processing file: %s\n",$file_name);
    open(my $FILE,$file) or die("Cannot open file: $file\n");

    my $header = ClassHeader->new();
    $header->setProject($self->getProjectName());
    $header->setType("Haenam Radiosonde");
    $header->setReleaseDirection("Ascending");

    $header->setId("HAENAM");
    $header->setSite("Haenam, Korea/WMO 47261");
    
	# --------------------------------------------------------------
	# for Haenam there is only one line of header info
	# 
	# Date:05.09.2008 Start time:05:40:04 Number of probe:D2943176
    # File name shows nominal time
    # UPP_RAW_47261_200810041800_ADD.txt
	# Wind speed in knots will require conversion
	# ---------------------------------------------------------------
    my @fileInfo = (split(/_/,$file_name));
	$fileInfo[3] =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/;
	
    my $nomDate = sprintf("%04d, %02d, %02d", $1, $2, $3);
    my $nomTime = sprintf("%02d:%02d", $4, $5);
	
	<$FILE> =~ /Date:(\d{2}).(\d{2}).(\d{4})\sStart\stime:(\d{2}).(\d{2}).(\d{2})\sNumber of probe:(D\d+)/;
    my $date = sprintf("%04d, %02d, %02d", $3, $2, $1);
    my $time = sprintf("%02d:%02d:%02d", $4, $5, $6);
	my $probe = $7;

	my $sondeType = "Vaisala RS92-SGP with GPS wind-finding";
    $header->setLine(5,"Sonde Id/Sonde Type:", join ('/', $probe, $sondeType));
    
    $header->setNominalRelease($nomDate, "YYYY, MM, DD", $nomTime, "HH:MM", 0); 
    $header->setActualRelease($date, "YYYY, MM, DD", $time, "HH:MM:SS", 0);  

    #-----------------------------------------------------------------------
    <$FILE>; # skip the column headers


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

    
	my @lines = <$FILE>;   # Grab all the remaing lines
    my $first_line = 1;

    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    #------------------------------------------------------
    # Process each of the remaining data lines.
    # Note that $WARN is the ../output/warning.log file.
    #------------------------------------------------------
    foreach my $line (@lines) {
	    my $rec = ClassRecord->new($WARN, $filename);
        my @data = split(' ', trim($line));
        
		if (($limit) && ($prev_time == 9998.0))
		{
			printf("Limited processing to records with times less than 10000.0 seconds for %s %s.  Remaining records were cut off.\n",
			getNetworkName(), $filename);
			last;
		}
 
		my($min,$sec) = split (":", $data[0]);
  		# print "TIME: $min $sec\n";
        $rec->setTime($min,$sec);

        $rec->setPressure($data[1], "hPa")  unless ($data[1] == -9999); 
        $rec->setTemperature($data[2], "C") unless ($data[2] == -9999);
        $rec->setRelativeHumidity($data[3]) unless ($data[3] == -9999); 

		$rec->setDewPoint($data[8], "C") unless ($data[8] == -9999); 
		
		if ($data[4] != -9999)
		{
			my $windSpd = convertVelocity($data[4],"knot", "m/s");
    	    $rec->setWindSpeed($windSpd, "m/s");
		}

        $rec->setWindDirection($data[5])    unless ($data[5] == -9999);

	    my $lat_fmt = $data[10] < 0 ? "-" : "";
	    while (length($lat_fmt) < length($data[10])) { $lat_fmt .= "D"; }

     	my $lon_fmt = $data[11] < 0 ? "-" : "";
    	while (length($lon_fmt) < length($data[11])) { $lon_fmt .= "D"; }

        $rec->setLatitude($data[10], $lat_fmt)  unless ($data[10] == -9999);
    	$rec->setLongitude($data[11], $lon_fmt) unless ($data[11] == -9999); 
    	$rec->setAltitude($data[7], "m") unless ($data[7] == -9999); 
        
        # -----------------------------
        # Get header info from the data
	    # -----------------------------
    	if ($first_line) {
            if ($debug) { print "First Data Line. lat, lon:: $data[10], $data[11] \n"; }

	        $header->setLatitude($data[10], $lat_fmt)  unless ($data[10] == -9999);
	        $header->setLongitude($data[11], $lon_fmt) unless ($data[11] == -9999);
	        $header->setAltitude($data[7], "m") unless ($data[7] == -9999);
	        
            print($OUT $header->toString());

            #-----------------------------------------------------------------
            # Set the station information
            #-----------------------------------------------------------------
            my $station = $self->{"stations"}->getStation(getNetworkName(),$self->{"NETWORK"}, 
	                                  $header->getLatitude(),$header->getLongitude(),
							          $header->getAltitude());
            if (!defined($station)) {
                $station = $self->build_default_station(getNetworkName(),$self->{"NETWORK"});

	            $station->setLatitude($header->getLatitude(),$self->buildLatlongFormat($header->getLatitude()));
	            $station->setLongitude($header->getLongitude(),$self->buildLatlongFormat($header->getLongitude()));
	            $station->setElevation($header->getAltitude(),"m");
		
                $self->{"stations"}->addStation($station);
            }
            $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");     
            #-----------------------------------------------------------------

            $prev_time = $rec->getTime();
            $prev_alt = $rec->getAltitude();

	        $first_line = 0;
   	    }   # end if($first_line)
        else
        {
            #-------------------------------------------------------
            # Calculate the ascension rate which is the difference
            # in altitudes divided by the change in time. Ascension
            # rates can be positive, zero, or negative. But the time
            # must always be increasing (the norm) and not missing.
            #
            # Only save the next non-missing values.
            # Ascension rates over spans of missing values are OK.
            #-------------------------------------------------------
            if ($prev_time != 9999  && $rec->getTime()     != 9999  &&
                $prev_alt  != 99999 && $rec->getAltitude() != 99999 &&
                $prev_time != $rec->getTime() ) 
		    {
            $rec->setAscensionRate( ($rec->getAltitude() - $prev_alt) /
                                    ($rec->getTime() - $prev_time),"m/s");
		    }

            if ($rec->getTime() != 9999 && $rec->getAltitude() != 99999)
            {
                $prev_time = $rec->getTime();
                $prev_alt = $rec->getAltitude();

                if ($debug_ascent_rate) { print "Current record has valid Time and Alt. Save as previous.\n"; }
            }
        }# Calc Ascension Rate if possible
     
        print($OUT $rec->toString()); 

    } # Foreach my lines. - process all data lines.

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

    $station->setStationName("Haenam, Korea");
	$station->setLatLongAccuracy(3);
    $station->setStateCode("99");
    $station->setReportingFrequency("6 hourly");
    $station->setNetworkIdNumber("47261");
	# platform 87, Rawinsonde, Other
    $station->setPlatformIdNumber(87);
    return $station;
}

 ##------------------------------------------------------------------------------
# @signature String buildLatlongFormat(String value)
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
sub buildLatlongFormat {
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






