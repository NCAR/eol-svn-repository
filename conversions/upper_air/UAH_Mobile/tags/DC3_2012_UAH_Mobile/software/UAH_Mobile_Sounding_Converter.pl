#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The UAH_Mobile_Sounding_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk 2013-02-26
# @version DC3 UAH Mobile
#    - The converter expects raw data files with a ".LOG" extension.
#    - The raw data files were originally names YYYYMMDD_HHmmUTC.LOG. The
#      sonde information was in a separate file.  Since there were not a
#      lot of raw data files, I renamed the files so that the sonde ID was
#      appended on to the end of the file name:  20120518_1801UTC_P20166.LOG
#    - The date, time and sonde ID are taken from the file name.
#    - The lat, lon, and alt information are obtained from the surface 
#      data record (the first line of the raw data). 
#    - The windspeed was set to missing for values greater than 5 chars 
#      (including decimal pt) so Scot asked that we set the wind dir to
#      missing also in these cases.
#    - The wind direction is in radians, so convertAngle is used to convert
#      the values to degrees.
#    - Code was added to remove the descending sondes (asc rate < 0 or 999.0)
#    - Search for "HARD-CODED" to find project-specific items that
#      may require changing.
#
#
##Module------------------------------------------------------------------------
package UAH_Mobile_Sounding_Converter;
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
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

my ($WARN);


printf "\nUAH_Mobile_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nUAH_Mobile_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the UAH Mobile (AL) radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = UAH_Mobile_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature UAH_Mobile_Sounding_Converter new()
# <p>Create a new instance of a UAH_Mobile_Sounding_Converter.</p>
#
# @output $self A new UAH_Mobile_Sounding_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "DC3";
    # HARD-CODED
    $self->{"NETWORK"} = "UAH_Mobile";
    
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
# <p>Create a default station for the UAH Mobile network using the 
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
    $station->setPlatformIdNumber(415);
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
    $header->setType("UAH Mobile");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("UAH_Mobile");
	# "Release Site Type/Site ID:" header line
    $header->setSite("UAH Mobile");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	
    # my ($alt, $lat, $lon) = split(" ",$headerlines[0]); 
    my (@header_info) = split(" ",$headerlines[0]); 
	my $lon = trim($header_info[12]);
	my $lat = trim($header_info[11]);
	my $alt = trim($header_info[4]);
	
	$header->setLongitude($lon,$self->buildLatlonFormat($lon));
	$header->setLatitude($lat,$self->buildLatlonFormat($lat));
    $header->setAltitude($alt,"m"); 
	
    # ------------------------------------------------------------------
    # Extract the date, time and sonde information from the file name
    # BEWARE: Expects filename like: 20120611_1836UTC_P20099.LOG
    # ------------------------------------------------------------------
    # print "file name = $filename\n"; 

    my $date;
	my $time;
	my $hour;
	my $min;
	my $snding;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})UTC_(P?\d+)\.LOG/)
	{
		my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minuteInfo, $sonde) = ($1,$2,$3,$4,$5,$6);

		$hour = $hourInfo;
		$min = $minuteInfo;
		# $snding = join "", $monthInfo, $dayInfo;
		$snding = $sonde;
		print "sounding $snding\n";
	    $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
	    print "date: $date   ";
	    $time = join "", $hour, ' ', $min, ' 00';
        print "time: $time\n";
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

	
	my $sonde_type = "iMET-1-AB";
	my $ground_station = "iMET-3150";
    # one file has a bogus sonde ID
	if ($snding =~ /^002$/)
	{
		$snding = "No information";
	}
	$header->setLine(5, "Sonde Id/Sonde Type:", join('/', $snding,$sonde_type));
	$header->setLine(6, "Ground Station Equipment:", ($ground_station));
	
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
    
    # Generate the sounding header. Need only the first data line.
	my @headerlines = $lines[0];
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
 
    printf("\tOutput file name:  %s\n", $outfile);

	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
	# --------------------------------------------
	# Create an array to hold all of the data records.
	# This is required so additional processing can take
	# place to remove descending data records at the
	# end of the data files
	# --------------------------------------------
	my @record_list = ();
	# --------------------------------------------

    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 9999.0;
    my $prev_alt = 99999.0;
    my $recordTime = 0;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $surface = 1;
	foreach my $line (@lines) {
	    
		chomp($line);
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

	    $record->setTime($recordTime);
	    $record->setPressure($data[3],"mb") if ($data[3] !~ /-999/);
	    $record->setTemperature($data[0],"C") if ($data[0] !~ /-999/);    
	    $record->setRelativeHumidity($data[2]) if ($data[2] !~ /-999/);

		my $windSpeed = convertVelocity($data[8],"knot", "m/s");
		$record->setWindSpeed($windSpeed,"m/s"); 
		
		# The windspeed will be converted to "missing" if it is greater
		# than 5 chars including the decimal point (perl format F5.1).
		# If the windspeed is "missing" set the wind dir to missing also,
		# i.e., DON'T SET the wind direction.
		if ($windSpeed < 1000.0)
		{
			# use convertAngle (initial angle, initial units, target units)
			my $windDir = convertAngle($data[7],"rad","deg");
			$record->setWindDirection($windDir) unless $data[8] =~ /-999/;
		}
		else
		{
			# print "\n\tWind speed high value:   $windSpeed\n";
		}
		$record->setAltitude($data[4],"m");
		
		# get the lat/lon data 
		$record->setLongitude($data[12],$self->buildLatlonFormat($data[12])) if ($data[12] !~ /-999/);
		$record->setLatitude($data[11],$self->buildLatlonFormat($data[11])) if ($data[11] !~ /-999/);;

        # -----------------------------------------------------------
        # --- NOTE: FOR UAH, DO NOT SET SINCE VALUES ARE ALL ZERO ---
        # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
		# For setVariableValue(index, value):
		# index (1) is Ele column, index (2) is Azi column.
        # -----------------------------------------------------------
		# if ($data[5] !~ /-999/)
		# {
		# 	my $elevationAngle = convertAngle($data[5],"rad","deg");
		# 	$record->setVariableValue(1, $elevationAngle);
		# }
		# if ($data[6] !~ /-999/)
		# {
		# 	my $azimuthAngle = convertAngle($data[6],"rad","deg");
		# 	$record->setVariableValue(2, $azimuthAngle);
		# }
        # -----------------------------------------------------------
		
		$recordTime++;
	                                          
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

	    # printf($OUT $record->toString());
		push(@record_list, $record);
    } # end foreach my $line
	
	
    # --------------------------------------------------
	# Remove the last records in the file that are 
    # descending (ascent rate is negative)
	# --------------------------------------------------
	foreach my $last_record (reverse(@record_list))
	{
	    if (($last_record->getAscensionRate() < 0.0) ||
		    ($last_record->getAscensionRate() == 999.0))
	    {
		    undef($last_record);
	    } 
	    else 
	    {
		    last;
	    }
	}
    #-------------------------------------------------------------
    # Print the records to the file.
	#-------------------------------------------------------------
	foreach my $rec(@record_list) 
	{
	    print ($OUT $rec->toString()) if (defined($rec));
	}	

	} # end if (defined($header)) ???
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
    # my @files = grep(/^\d{8}_12UTC\.txt/,sort(readdir($RAW)));
    my @files = grep(/LOG$/,sort(readdir($RAW)));
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
