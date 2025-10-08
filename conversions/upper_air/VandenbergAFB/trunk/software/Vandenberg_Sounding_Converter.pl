#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Vandenberg_Sounding_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format.</p> 
#
# @usage Vandenberg_Sounding_Converter.pl
#
# @author Linda Echo-Hawk 2013-08-14
# @version DC3 Vandenberg 
#    This code was created by modifying the Redstone Arsenal converter used for DC3.
#    - The code expects files with names like *_YYYY_MM_DD_HH_mm_ss.txt (where YYYY
#      is the year, MM is month, DD is day, HH is hour, mm is minute, ss is second),
#      for example: RTAMPS_72393_WS0000L01651O1D_BIG9_2013_06_14_11_17_06.txt.
#    - The converter stops when it reaches "END_OF_RECORD_1" in the raw data file.
#    - Search for "HARD-CODED" to find project-specific items that
#      may require changing.
##Module------------------------------------------------------------------------
package Vandenberg_Sounding_Converter;
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
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

# import module to set up command line options
use Getopt::Long;

my ($WARN);


printf "\nVandenberg_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nVandenberg_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Vandenberg AFB radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Vandenberg_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Vandenberg_Sounding_Converter new()
# <p>Create a new instance of a Vandenberg_Sounding_Converter.</p>
#
# @output $self A new Vandenberg_Sounding_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "MPEX";
    # HARD-CODED
    $self->{"NETWORK"} = "VandenbergAFB";
    
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
# <p>Create a default station for the Vandenberg network using the 
# specified station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub buildDefaultStation {
    my ($self,$station_id,$network) = @_;
    my $station = Station->new($station_id,$network);
    $station->setStationName($network);
    $station->setLatLongAccuracy(3);
    # HARD-CODED
	$station->setCountry("US");
    # $station->setStateCode("48");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 415, Radiosonde, Vaisala RS92-SGPD
	# Sippican, 590, 
    $station->setPlatformIdNumber(590);
    # $station->setMobilityFlag("m"); 
    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatlonFormat(String value)
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
sub buildLatlonFormat {
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
    my ($self,$file,@headerlines) = @_;
    my $header = ClassHeader->new();

    $filename = $file;
	# printf("parsing header for %s\n",$filename);
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("Vandenberg AFB");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("VandenbergAFB");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Vandenberg AFB/72393");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	
    # lat/lon data provided by Scot L.
	my $lat = 34.737;
	my $lon = -120.584;
	
	$header->setLongitude($lon,$self->buildLatlonFormat($lon));
	$header->setLatitude($lat,$self->buildLatlonFormat($lat));
	
    # ------------------------------------------------------------------
    # Extract the date and time information from the file name
    # BEWARE: Expect filename similar to: *_2013_07_17_11_42_49.txt
    # ------------------------------------------------------------------
    # print "file name = $filename\n"; 

    my $date;
	my $time;

	if ($filename =~ /(\d{4})_(\d{2})_(\d{2})_(\d{2})_(\d{2})_(\d{2})\.txt/)
	{
		my ($yearInfo,$monthInfo,$dayInfo,$hourInfo,$minInfo,$secInfo) = ($1,$2,$3,$4,$5,$6);
                                                     
	    $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
	    # print "date: $date   ";
	    $time = join ":", $hourInfo, $minInfo, $secInfo;
        # print "time: $time\n";
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
      
	
	my $sonde_type = "Sippican Mark II";
	$header->setLine(5, "Sonde Type:", $sonde_type);
	
	return $header;
}
                           
##------------------------------------------------------------------------------
# @signature void parseRawFiles(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parseRawFile {
    my ($self,$file) = @_;
    
    printf("\nProcessing file: %s\n",$file);

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
    # Generate the sounding header.
	# We actually don't end up using the @headerlines, but may need this
	# if Scot gets more information from the source, so I'll leave in place.
	my @headerlines = @lines[0..1];
    my $header = $self->parseHeader($file,@headerlines);
    
    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
						      $header->getLatitude(),$header->getLongitude(),
						      $header->getAltitude());
	if (!defined($station)) {
	    $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
	    $station->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
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
 
    printf("\n\tOutput file name:  %s\n", $outfile);

	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

    # not yet because we need the altitude
	# print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $surface = 1;
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 4) { $index++; next; }
		# --------------------------------
        # Stop when you get to 
		# "END_OF_RECORD_01"
		# --------------------------------
		if ($line =~ /END/)
		{
			last;
		}
	    
		chomp($line);
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

	    $record->setTime($data[0]);
	    $record->setPressure($data[9],"mb"); 
	    $record->setTemperature($data[7],"C");  
		$record->setDewPoint($data[8],"C") if ($data[8] !~ /-99/);
	    $record->setRelativeHumidity($data[10]) if ($data[10] !~ /-99/);
		
		# wind speed in knots
		if ($data[6] !~ /-99.9/)
		{
			my $windSpd = convertVelocity($data[6], "knot", "m/s");
			$record->setWindSpeed($windSpd,"m/s");
		}
		
        # dir is in degrees (no conversion required)
	    $record->setWindDirection($data[4]) if ($data[4] !~ /-99/);
		
		# height is in feet
		my $geopotHeight = convertLength($data[2], "ft", "m");
		$record->setAltitude($geopotHeight,"m");
		
		if ($surface)
		{
			# get the lat/lon data 
			$record->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
			$record->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));

			$header->setAltitude($geopotHeight,"m");
            #  print out header records
	        print($OUT $header->toString());
			$surface = 0;
		}
	                                          
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
        if ($debug) 
		{ 
			my $time = $record->getTime(); 
			my $alt = $record->getAltitude(); 
            # print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; 

			print "\nNEXT Line:: prev_time:  $prev_time,  rec Time:  $time, ";
			print "prev_alt:  $prev_alt,  rec Alt:  $alt\n";
		}

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
	# HARD-CODED FILE NAME
    my @files = grep(/(\d{4})_(\d{2})_(\d{2})_(\d{2})_(\d{2})_(\d{2})\.txt$/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	# printf("Ready to read the files\n");
    foreach my $file (@files) {
	$self->parseRawFile($file);
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

