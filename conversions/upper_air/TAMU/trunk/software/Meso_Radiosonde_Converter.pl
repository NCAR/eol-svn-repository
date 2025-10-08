#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Meso_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from the SHARPpy ascii format to the 
# EOL Sounding Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 17 July 2019
# @version Meso 2018-19 TAMU Radiosonde Data
#          - The first record is the surface record.
#          - Header altitude is taken from the surface 
#            record HGHT (geopotential height) column 
#          - Lat/Lon values were provided for each
#            of the four locations by Scot L.
#          - Assume the data are at 5-sec. intervals.
#          - Need to derive RH and wind components.
#          - Wind speed is converted from knots to m/s.
#          - A "readSurfaceValuesFile" function exists to 
#            read in surface values from a separate file, 
#            but is not called since the values were provided 
#            by Scot and no separate file was used. 
#          - Ascension rate is calculated by the converter.
#          - The converter expects file names similar to:
#            upperair.TAMU_sonde.201810311200.College_Station_TX_SHARPpy.txt
#            The "readDataFiles" function uses a case-insensitive 
#            match to find all the files.
#
# @author Linda Echo-Hawk 14 June 2019
# @version Meso 2018-19 NOAA/ATDD Radiosonde Data
#          - The first record is the surface record.
#          - Header altitude is taken from the surface 
#            record HGHT (geopotential height) column 
#          - Lat/Lon values were provided for each
#            of the four locations by Scot L.
#          - Assume the data are at 1-sec. intervals.
#          - Need to derive RH and wind components.
#          - A "readSurfaceValuesFile" function exists to 
#            read in surface values from a separate file, 
#            but is not called since the values were provided 
#            by Scot and no separate file was used. 
#          - Ascension rate is calculated by the converter.
#          - The converter expects file names similar to:
#            upperair.ATDD_sonde.201811300000.Montgomery_AL_SHARPpy.txt
#            upperair.ATDD_sonde.201902060000.Sikeston_MO_SHARPpy.txt
#            upperair.ATDD_sonde.201903090000.SE_Montgomery_AL_SHARPpy.txt
#            upperair.ATDD_sonde.201904130000.Auburn_AL_SHARPpy.txt
#            The "readDataFiles" function uses a case-insensitive 
#            match to find all the files.
#
# @author Linda Echo-Hawk May-June 2019
# @version Meso 2018-19 ULM Radiosonde Data
#          - The first record is the surface record.
#          - Header altitude is taken from the surface 
#            record HGHT (geopotential height) column 
#          - Lat/Lon values were provided for each
#            of the two locations (Monroe and 
#            Breaux Bridge LA) by Scot L.
#          - Assume the data are at 5-sec. intervals.
#          - Need to derive RH and wind components.
#          - A "readSurfaceValuesFile" function exists to 
#            read in surface values from a separate file, 
#            but is not called since the values were provided 
#            by Scot and no separate file was used. 
#          - Ascension rate is calculated by the converter.
#          - The file names differ in that some have SHARPPY 
#            all in upper case, while others use a lower case 
#            "py" (e.g., SHARPpy). The "readDataFiles" function 
#            uses a case-insensitive match to find all the files.
#
#
# @author Linda Echo-Hawk 19 October 2018
# @version Meso 2018-19 based on the VORTEX-SE 2016 ULM Mobile
#            Sounding Converter.
#          - NOTE that this converter was developed to test
#            our ability to convert SHARPpy data to the EOL format.
#          - Header info on lat/lon/alt as well as sonde info
#            would need to be provided by the source. These
#            values are hard-coded into this test converter.
#          - Need to derive RH and wind components.
#          - Height parameter is MSL so we need to derive 
#            the geopotential height. Scot is confirming
#            with source that first record is at surface.
#          - Scot is confirming with source the standard
#            time interval. For now, use 5 seconds.
#          - I left code in from the ULM conversion that
#            read in surface values from a separate file.
#            This code is currently commented out, but
#            since this info is not provided in the raw
#            data, we may have to use this approach for
#            future SHARPpy data conversions.
#
#
##Module------------------------------------------------------------------------
package Meso_Radiosonde_Converter;
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
use DpgConversions;
my ($WARN);

printf "\nMeso_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nMeso_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the ULM radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Meso_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Meso_Radiosonde_Converter new()
# <p>Create a new instance of a Meso_Radiosonde_Converter.</p>
#
# @output $self A new Meso_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = SimpleStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "VORTEX-SE Meso18-19";
    # HARD-CODED
    $self->{"NETWORK"} = "TAMU";
    
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
# <p>Create a default station for the ULM network using the 
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
    $station->setStationName("Meso");
    # HARD-CODED
    $station->setStateCode("99");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber("99");
    # platform, 591	Radiosonde, iMet-1
    $station->setPlatformIdNumber(591);
	$station->setMobilityFlag("m");

    return $station;
}

##------------------------------------------------------------------------------
# @signature String buildLatLonFormat(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# format length must be the same as the value length or
# convertLatLong will complain (see example below)
# base lat =   36.6100006103516 base lon =    -97.4899978637695
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

    # my $filename = $file;

    # HARD-CODED
    # Set the type of sounding
    $header->setType("TAMU Sounding Data");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("TAMU");
	# $header->setSite("NOAA ATDD Mobile");


	my $sfc_lat;
	my $sfc_lon;

	if ($file =~ /College_Station/i)
	{
		$sfc_lon = -96.335;
		$sfc_lat = 30.619;
		$header->setSite("College Station TX");

	}
	else
	{
		print "WARNING: Unrecognized location\n";

	}

    $header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 

	
	my @headerData = split(",",$headerlines[0]);
	my $sfc_elev = $headerData[1];
	$header->setAltitude($sfc_elev,"m");

    # -------------------------------------------------
   	# Other header info provided by Scot
	# -------------------------------------------------
    my $sondeType = "iMet-4";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));
	my $groundStationSoftware = "iMet-OS-II";
	$header->setLine(6,"Radiosonde System Software:", ($groundStationSoftware));

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to:
	# upperair.TAMU_sonde.201810311200.College_Station_TX_SHARPpy.txt
    # ----------------------------------------------------------
    my $date;
	my $time;

	if ($file =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $minute) = ($1,$2,$3,$4,$5);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$minute,'00';

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
    my ($self,$file) = @_;
    
    printf("\nProcessing file: %s\n",$file);

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);

    my @lines = <$FILE>;
    close($FILE);

	my @headerlines = $lines[6];
	print @headerlines;

                        
	# Generate the sounding header.
	my $header = $self->parseHeader($file, @headerlines);
    
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
	my @location = split (" ", $header->getSite());
	my $citystate = join "_", @location;
	print "citystate $citystate location @location\n";

   	$outfile = sprintf("%s_%s_%04d%02d%02d%02d%02d.cls", 
					   	   $header->getId(),
						   $citystate,
					   	   split(/,/,$header->getActualDate()),
					   	   $hour, $min);

    printf("\tOutput file name is %s\n", $outfile);


	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	
    # ----------------------------------------
    # Needed for code to derive ascension rate
    # ----------------------------------------
    my $prev_time = 0.0;
    my $prev_alt = $header->getAltitude();


    # ----------------------------------------------------
    # Parse the data portion of the input file
    # ----------------------------------------------------
	my $index = 0;
	my $fake_surface_time = 0;
	my $raw_data_time;
	my $fake_surface_data = 1;
	my $RH_warnings = 0;
    
    # Now grab the data from each line
	foreach my $line (@lines) 
	{
		my $record = ClassRecord->new($WARN,$file);
		
		if ($index >= 6)
		{
			if ($fake_surface_data)
			{
				# The raw data surface record does not
				# contain the time, lat or lon values,
				# so the header lat/lon values are used.
			    $record->setTime($fake_surface_time);
	        	$record->setLatitude($header->getLatitude(),
			    	    $self->buildLatLonFormat($header->getLatitude()));
		        $record->setLongitude($header->getLongitude(),
				        $self->buildLatLonFormat($header->getLongitude()));
        	    $record->setAltitude($header->getAltitude(),"m");

				$fake_surface_data = 0;
			}
			
            #--------------------------------------------
			# RH and wind components must be calculated;
			# convert wind speed from knots to m/s
			#--------------------------------------------
			my $temp;
			my $geopotential_height;
			my $dewpoint;
			my $pressure;
			my $wind_spd;
			my $wind_dir;
			
			chomp($line);
		    my @data = split(',',$line);

			if ($data[0] =~ /%END%/)
			{
				last;
			}
	    	

			$pressure = trim($data[0]);
			$record->setPressure($pressure,"mb") if ($pressure !~ /^999$/);

			$geopotential_height = trim($data[1]);
			$record->setAltitude($geopotential_height,"m");

			$temp = trim($data[2]);
			$record->setTemperature($temp,"C") if ($temp !~ /^999$/);

		   	$dewpoint = trim($data[3]);
			$record->setDewPoint($dewpoint,"C") if ($dewpoint !~ /^999$/);
			
            my $calculated_RH = calculateRelativeHumidity($temp, $dewpoint);
			if ($calculated_RH)
			{
				$record->setRelativeHumidity($calculated_RH);
				$RH_warnings = 0;
			}
			else
			{
				$RH_warnings = 1;
			}

			$wind_spd = trim($data[5]); # wind spd (m/s)
			if (($wind_spd =~ /0\.0/) && ($index == 6))
			{
				 $record->setWindSpeed($wind_spd,"m/s");
			}
			else
			{
				my $convertedWindSpeed = convertVelocity($wind_spd,"knot", "m/s");
				if ($convertedWindSpeed)
				{
					$record->setWindSpeed($convertedWindSpeed,"m/s");
				}
			}
			
			$wind_dir = trim($data[4]); # wind dir (deg)
			$record->setWindDirection($wind_dir) if ($wind_dir !~ /999/);
		
        	#-------------------------------------
			# The first data line is index = 6.
			# Set initial time to zero, then 
			# use ` second increments, so increment
			# $raw_data_time by one for lines
			# with $index > 6
			#-------------------------------------
			if ($index == 6)
			{                                                 
				$raw_data_time = 0;
		    	$record->setTime($raw_data_time);
				$raw_data_time += 5;  # 5-sec data
				# $raw_data_time += 1;         
			}
			elsif ($index > 6)
			{
				$record->setTime($raw_data_time);
				$raw_data_time += 5;
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
	        if ($index >= 3)
			{
			
			if ($debug) 
			{
				my $time = $record->getTime(); my $alt = $record->getAltitude(); 
            	# print "\nNEXT Line: prev_time, rec Time, prev_alt, rec Alt:: $prev_time, $time, $prev_alt, $alt\n"; 
				print "Gather ascension rate data for index $index:\n";
				print "\tPrevious Time $prev_time  altitude = $prev_alt\n";
				print "\tCurrent Time $time  altitude = $alt\n";
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
        	# Only save the next non-missing values. 
        	# Ascension rates over spans of missing values are OK.
        	#-----------------------------------------------------
	        if ($debug) 
			{ 
				my $rectime = $record->getTime(); my $recalt = $record->getAltitude();
            	print "Current record: Time $rectime  Altitude = $recalt "; 
			}

    	    if ($record->getTime() != 9999 && $record->getAltitude() != 99999)
        	{
            	$prev_time = $record->getTime();
	            $prev_alt = $record->getAltitude();
    	
        	    if ($debug) 
				{ 
					print " has valid Time and Alt.\n"; 
				}
    	    }
    		} # if ($index >= 3) calculate ascension rate
        	#-------------------------------------------------------
			# Completed the ascension rate data
    	    #-------------------------------------------------------

		    
		printf($OUT $record->toString());
		} # end if ($index >= 4)
			
	    $index++;
	} # end foreach $line

	if ($RH_warnings)
	{
		print "WARNING: RH could not be calculated\n";
		$RH_warnings = 0;
	}

	} # end if (defined($header))
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
    # my @files = grep(/^2016.+txt$/,sort(readdir($RAW)));
    my @files = grep(/SHARPpy.txt$/i,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	# printf("Ready to read the files\n");
    foreach my $file (@files) {
	$self->parseRawFile($file);
    }
    
    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readSurfaceValuesFile(file_name)
# <p>Read the contents of the file into an array.</p>
#
# @input $file_name The name of the raw data file to be read.
# @output array of surface values (lat/lon/elev)
##------------------------------------------------------------------------------
sub readSurfaceValuesFile {
    my $self = shift;

    open(my $FILE, sprintf("ULM_sfc_alt.txt")) or die("Can't read file into array\n");
    my @surface_data = <$FILE>;
    close ($FILE);

    return @surface_data;
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
