#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The TAMU_Mobile_Sounding_Converter.pl script is used for converting high 
# resolution radiosonde data to the EOL Sounding Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 28 Jan 2014
# @version MPEX -- Created based on SUNY Radiosonde converter for VORTEX2. 
#
#          - Expects filename similar to: TAMU_201305211204Z_3050.LOG
#          - Raw data files are DOS format so run dos2unix
#          - Converter expects the actual data to begin
#            line 1 of the raw data file.  
#          - Header altitude info is obtained from the data.  
#          - Surface lat/lon info is obtained from header info.  
#          - Release time is obtained from the file name.
#          - Ground surface equipment number is obtained from filename
#            and included in the output file name as part of the header Id.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
##Module------------------------------------------------------------------------
package TAMU_Mobile_Sounding_Converter;
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

use DpgConversions;
use SimpleStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;

my ($WARN);

printf "\nTAMU_Mobile_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nTAMU_Mobile_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the TAMU_Mobile_Sounding_Converter data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = TAMU_Mobile_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature TAMU_Mobile_Sounding_Converter new()
# <p>Create a new instance of a SUNY_Radiosonde_Converter.</p>
#
# @output $self A new TAMU_Mobile_Sounding_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "MPEX";
    # HARD-CODED
    $self->{"NETWORK"} = "TAMU_Mobile";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";

    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->cleanForFileName($self->{"NETWORK"}),
				      $self->cleanForFileName($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}

##------------------------------------------------------------------------------
# @signature Station buildDefaultStation(String station_id, String network)
# <p>Create a default station for the SUNY Oswego network using the 
# specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);
    # $station->setStationName($network);
	# info in 48-char field in stationCD.out file
    $station->setStationName("TAMU Mobile");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 591 Radiosonde, iMet-1
    $station->setPlatformIdNumber(591);
	$station->setMobilityFlag("m");

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
    mkdir($self->{"FINAL_DIR"}) unless (-e $self->{"FINAL_DIR"});
    
    $self->readDataFiles();
    $self->printStationFiles();
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
    my ($self,$file,$sounding,@headerlines) = @_;
    my $header = ClassHeader->new();

    $filename = $file;

    # HARD-CODED
    # Set the type of sounding
    $header->setType("TAMU Mobile");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
    
    # --------------------------------------------
	# Wait to set the id until we have the
	# ground station code
	# --------------------------------------------
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
	# $header->setId("TAMU_Mobile");
	
    # -------------------------------------------------
	# This info goes in the "Release Site Type/Site ID:" header line
    # -------------------------------------------------
	$header->setSite("TAMU Mobile");

    # -------------------------------------------------
    # Get the header lat/lon data (from the surface record)
    # -------------------------------------------------
    my @headerData = split(' ',$headerlines[0]);    
	
	my $lat = $headerData[11];
	my $lon = $headerData[12];
	my $alt = $headerData[4];

    $header->setLatitude($lat, $self->buildLatlongFormat($lat));
	$header->setLongitude($lon, $self->buildLatlongFormat($lon)); 
    $header->setAltitude($alt,"m"); 


    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: TAMU_201305211204Z_3050.LOG
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;
	my $ground_station;
	my $ground_station_id;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})Z_(\d{4})/)
	{
		my ($year, $month, $day, $hour, $min, $gss) = ($1,$2,$3,$4,$5,$6);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$min,'00';
        # print "Ground Station: $gss\n";
	    $ground_station = join("-", "iMET", $gss);

        $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

		
	    # The Id will be the prefix of the output file
        # and appears in the stationCD.out file
		$ground_station_id = sprintf("TAMU_Mobile_%s", $gss);
        # $header->setId("TAMU_Mobile");
        $header->setId($ground_station_id);

	}
	else
	{
        print "Filename does not match\n";
	}
	my $sonde_type = "iMet-1 with GPS windfinding";	
	print "ground station = $ground_station\n";
	my $surface_source = "Temperature/Humidity/Wind speed - Kestral 4000; Wind direction - compass/streamer; Pressure - Climatronics 092";

	$header->setLine(5, "Sonde Type:", ($sonde_type));
	$header->setLine(6, "Ground Station Software:", ($ground_station));
	$header->setLine(7, "Surface Data Source:", $surface_source);

    return $header;
}
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file,$sounding) = @_;
    
    printf("\nProcessing file: %s\n",$file);

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
    # Generate the sounding header.
	my @headerlines = $lines[0];
	# ------------------------------------------------
	# Because the data files do not have headers
	# header values will come from the surface 
	# data record
	# ------------------------------------------------
    my $header = $self->parseHeader($file,$sounding,@headerlines);
    
    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"});
	if (!defined($station)) {
	    $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    # ----------------------------------------------------
    # Create the output file name and open the output file
    # ----------------------------------------------------
    my $outfile;
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime()); 

   	$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
					   	   $header->getId(),
					   	   split(/,/,$header->getActualDate()),
					   	   $hour, $min);
 
    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
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
	my $recTime = 0;
	foreach my $line (@lines) 
	{
        # There are no header lines, so begin on line 1
		chomp($line);
		my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

        # need to confirm missing values
	    $record->setTime($recTime);
	    $record->setTemperature($data[0],"C") if ($data[0] !~ /-99.0/);
	    $record->setRelativeHumidity($data[2]) if ($data[2] !~ /-99.0/);
	    $record->setPressure($data[3],"mb") if ($data[3] !~ /9999.0/);
		
		if ($data[7] !~ /-99.0/)
		{
			# The wind direction is in radians, so convertAngle is used
			# to convert the values to degrees.
			# use convertAngle (initial angle, initial units, target units)
			my $convertedWindDir = convertAngle($data[7],"rad","deg");
			# set the wind dir if the original value was not "missing"
			$record->setWindDirection($convertedWindDir);
		}

		if ($data[8] !~ /-99.0/)
		{
			my $convertedWindSpeed = convertVelocity($data[8],"knot", "m/s");
			# set the wind speed if the original value was not "missing"
			$record->setWindSpeed($convertedWindSpeed,"m/s");
		}

        if ($data[11] !~ /999.000/)
		{
		    $record->setLatitude($data[11], $self->buildLatlongFormat($data[11]));
		}
        if ($data[12] !~ /999.000/)
		{
			$record->setLongitude($data[12], $self->buildLatlongFormat($data[12]));
		}

		# NOTE: Check missing value of altitude
		$record->setAltitude($data[4],"m") if ($data[4] !~ /-99.0/);


        # increment the time, frequency is 1 second. 
		# Time in files is to nearest minute.
		$recTime++;


        #-------------------------------------------------------
        # this code from Ron Brown converter:
        # Calculate the ascension rate which is the difference
        # in altitudes divided by the change in time. Ascension
        # rates can be positive, zero, or negative. But the time
        # must always be increasing (the norm) and not missing.
        #
        # Only save off the next non-missing values.
        # Ascension rates over spans of missing values are OK.
        #-------------------------------------------------------
        if ($debug) { my $time = $record->getTime(); my $alt = $record->getAltitude(); 
            print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; }

        if ($prev_time != 9999  && $record->getTime()     != 9999  &&
            $prev_alt  != 99999 && $record->getAltitude() != 99999 &&
            $prev_time != $record->getTime() ) 
        {
            $record->setAscensionRate( ($record->getAltitude() - $prev_alt) /
                                     ($record->getTime() - $prev_time),"m/s");

            if ($debug) { print "Calc Ascension Rate.\n"; }
        }

        #-----------------------------------------------------
        # Only save off the next non-missing values. 
        # Ascension rates over spans of missing values are OK.
        #-----------------------------------------------------
        if ($debug) { my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
              print "Try SAVE Line: rec Time, rec Alt:: $rectime, $recalt\n"; }

        if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        {
            $prev_time = $record->getTime();
            $prev_alt = $record->getAltitude();

            if ($debug) { print "Current rec has valid Time and Alt. Save as previous.\n"; }
        }
        #-------------------------------------------------------
		# Completed the ascension rate data
        #-------------------------------------------------------


	    printf($OUT $record->toString());
    }
	}
	else
	{
		printf("Unable to make a header\n");
	}
}

##------------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub printStationFiles {
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
}

##------------------------------------------------------------------------------
# @signature void readDataFiles()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub readDataFiles {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    # my @files = grep(/^D\d{8}_\d{6}.+\.eol/,sort(readdir($RAW)));
    my @files = grep(/^TAMU.+\.LOG/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	# printf("Ready to read the files\n");
	# ------------------------------------------------
	# For SUNY Oswego data (10 total soundings)
	# ------------------------------------------------
	# Because header altitude values are hard-coded
	# into the @surfaceElevation array, the sounding
	# number is required to index into array
	# ------------------------------------------------
	my $sounding = 0;
    foreach my $file (@files) {
	$self->parseRawFile($file,$sounding);
	$sounding++;
    }
    
    close($WARN);
}

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
