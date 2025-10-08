#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The SUNY_Radiosonde_Converter.pl script is used for converting high 
# resolution radiosonde data to the EOL Sounding Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk 2010-02-18
# @version VORTEX2  Created based on T-PARC_2008 Minami Daito Jima. 
#          - Raw data files are Excel format converted to csv format
#            with .csv extensions (also run dos2unix)
#          - Converter expects the actual data to begin
#            line 4 of the raw data file.  
#          - Header altitude info is obtained from the data.  
#          - Surface lat/lon info is obtained from header info.  
#          - Release time is obtained from the file name.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
##Module------------------------------------------------------------------------
package SUNY_Radiosonde_Converter;
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
 
use SimpleStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;
use DpgCalculations;

my ($WARN);

printf "\nSUNY_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 1;

&main();
printf "\nSUNY_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the SUNY Oswego radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = SUNY_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature SUNY_Radiosonde_Converter new()
# <p>Create a new instance of a SUNY_Radiosonde_Converter.</p>
#
# @output $self A new SUNY_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "VORTEX2";
    # HARD-CODED
    $self->{"NETWORK"} = "SUNY_Oswego";
    
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
    $station->setStationName("Mobile SUNY Oswego (CO, NE, OK, TX)");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 61, Radiosonde, SUNY-A
    $station->setPlatformIdNumber(61);
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
    $header->setType("SUNY Oswego Radiosonde");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("SUNY_Oswego");

    # -------------------------------------------------
    # Get the header lat/lon data
    # -------------------------------------------------
	my ($headerData,$conditions) = split(/\",,,/,$headerlines[0]);
	my @locationData = split(' ',$headerData);
    
	
	# my @locationData = split(' ',$headerlines[0]);
	my $lat = $locationData[2];
	$lat = trim($lat);
	$lat =~ s/,//g;
	my $lon = (split(/"/,$locationData[3]))[0];
	$lon = trim($lon);
	# print "LAT: $lat LON: $lon\n";
    $header->setLatitude($lat, $self->buildLatlongFormat($lat));
	$header->setLongitude($lon, $self->buildLatlongFormat($lon)); 

	my $releaseConditions = (split(/:/,$conditions))[1];
	chomp($releaseConditions);
	$releaseConditions = trim($releaseConditions);
	$releaseConditions =~ s/,,,//g;
	# print "REL COND: $releaseConditions\n";
	$header->setLine(5,"Release Conditions:",$releaseConditions);

    # -------------------------------------------------
	# This info goes in the "Release Site Type/Site ID:" header line
    # -------------------------------------------------
	my $siteInfo = (split(/:/,$headerlines[1]))[1];
    my $site = (split(/"/,$siteInfo))[0];
	# print "SITE: $site\n";
	$site = trim($site);
    $header->setSite($site);
    
	# -------------------------------------------------
    # Get header altitude from surface data record
    # -------------------------------------------------
    # my @surfaceRecord = split(/,/,$headerlines[3]);
	# my $height =  $surfaceRecord[1];

	# ------------------------------------------------
	# For SUNY Oswego data (10 total soundings)
	# ------------------------------------------------
	# Because header altitude values are hard-coded
	# into the @surfaceElevation array, the sounding
	# number is required to index into array
	# ------------------------------------------------
    # surface elevation data provided by Scot L.
	# ------------------------------------------------
	my @surfaceElevation = (844,314,345,381,338,336,656,1648,1235,363);
	my $height = $surfaceElevation[$sounding];
	# print "HEIGHT: $height\n";
    $header->setAltitude($height,"m"); 

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: 20090607_0048Z_2s.csv 
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})Z/)
	{
		my ($year, $month, $day, $hour, $min) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$min,'00';

        $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
        $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
	}

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
	my @headerlines = @lines;
	# ------------------------------------------------
	# For SUNY Oswego data (10 total soundings)
	# ------------------------------------------------
	# Because header altitude values are hard-coded
	# into the @surfaceElevation array, the sounding
	# number is required to index into array
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

    # ---------------------------------------------
    # Needed for code to derive geopotential height
    # ---------------------------------------------
	my $previous_record;
	my $geopotential_height;

    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $surfaceRecord = 1;
	my $index = 0;
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 3) { $index++; next; }
	    
	    my @data = split(/,/,$line);
	    my $record = ClassRecord->new($WARN,$file);

        # missing values are ///// 
	    $record->setTime($data[0]);
	    $record->setPressure($data[2],"mb") if ($data[2] !~ /\/+/);
	    $record->setTemperature($data[3],"C") if ($data[3] !~ /\/+/);    
	    $record->setRelativeHumidity($data[4]) if ($data[4] !~ /\/+/);
	    $record->setWindSpeed($data[6],"m/s") if ($data[6] !~ /\/+/);
	    $record->setWindDirection($data[5]) if ($data[5] !~ /\/+/);
        
        # Dewpt, UWind and VWind get calculated when they
		# aren't set because toString() calls the following:
		# getDewPoint() calls calculateDewPoint(temp & RH), 
        # getUWindComponent() calls calculateUVfromWind(speed & dir), 
		# getVWindComponent() calls calculateUVfromWind(speed & dir), 

		# ----------------------------------------------------
	    # get the lat/lon data for use in surface record only  
        # ----------------------------------------------------
		if (!$surfaceRecord)
		{
            #-------------------------------------------------------------
            # BEWARE:  For VORTEX2 (2009) SLoehrer says there are issues 
            # with the raw data altitudes, so compute the geopotential 
			# height/altitude and insert for all other than surface record.
            # call calculateAltitude(last_press,last_temp,last_dewpt,last_alt,
		    #                        this_press,this_temp,this_dewpt,this_alt,1)
			# Note that the last three parms in calculateAltitude
            # are the pressure, temp, and dewpt (undefined for this dataset)
            # for the current record. To check the altitude calculations, see
            # the web interface tool at 
            #
            # http://dmg.eol.ucar.edu/cgi-bin/conversions/calculations/altitude
            #------------------------------------------------

            if ($debug) 
            { 
                my $prev_press = $previous_record->getPressure(); 
                my $prev_temp = $previous_record->getTemperature(); 
                my $prev_alt = $previous_record->getAltitude();

                print "\nCalc Geopotential Height from previous press = $prev_press, temp = $prev_temp, alt = $prev_alt,\n";
				print "and current press = $data[2] and temp = $data[3]\n"; 
            }

            if ($previous_record->getPressure() < 9990.0)
            {
                if ($debug){ print "prev_press < 9990.0 - NOT missing so calculate the geopotential height.\n"; }
 
                $geopotential_height = calculateAltitude($previous_record->getPressure(),
                                                         $previous_record->getTemperature(), 
														 undef, $previous_record->getAltitude(), 
														 $data[2], $data[3], undef, 1);
                if (defined($geopotential_height))
				{
	                $record->setAltitude($geopotential_height,"m");
				}
				else
				{
					print "WARNING: Was not able to calculate geopotential height\n";
					$geopotential_height = 99999.0;
				}
            }
            else
            {
                if ($debug_geopotential_height){print "WARNING: prev_press > 9990.0 - MISSING! Set geopot alt to missing.\n"; }
                $geopotential_height = 99999.0;
            }
		}
		else
		{
		    # if surface record, use header lat/lon
	        $record->setLatitude($header->getLatitude(),
			            $self->buildLatlongFormat($header->getLatitude()));
	        $record->setLongitude($header->getLongitude(),
			            $self->buildLatlongFormat($header->getLongitude()));

			# for surface and header altitude, use value provided by Scot
            $record->setAltitude($header->getAltitude(),"m");

			$surfaceRecord = 0;

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



        #---------------------------------------------
        # Only save off current rec as previous rec
        # if not completely missing. This affects
        # the calculations of the geopotential height,
        # as the previous height must be non-missing. 
        #---------------------------------------------
        if ($debug) 
		{
			my $press = $record->getPressure(); 
			my $temp = $record->getTemperature();
            my $alt = $record->getAltitude();
            print "Current Rec: press = $press, temp = $temp, alt = $alt\n";
        }
        if ( ($record->getPressure() < 9999.0)  && ($record->getTemperature() < 999.0)
             && ($record->getAltitude() < 99999.0) )
		{
			if ($debug) { print "Move to next record! previous_record = record \n\n"; }
            $previous_record = $record;
        } 
		else 
		{
			if ($debug) 
			{
				print "Do NOT assign current record to previous_record! ";
				print "Current record has missing values.\n\n";
		    }
        }
	




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
    my @files = grep(/^2009.+\.csv/,sort(readdir($RAW)));
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
