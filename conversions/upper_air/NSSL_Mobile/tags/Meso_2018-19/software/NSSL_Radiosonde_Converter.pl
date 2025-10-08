#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The NSSL_Radiosonde_Converter.pl script is used for converting 
# high resolution radiosonde data from a csv ascii format to the 
# EOL Sounding Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 17 July 2019
# @version Meso 2018-19 NSSL Radiosonde Data
#          - These data are in a comma-delimited ASCII format.
#          - NOTE: Code is in place to read the city and state
#            from the file name, then set the site (setSite)
#            using this information without the underscore.
#            BEWARE that if new sites are added this code 
#            will need to be modified.
#          - The converter expects file names similar to:
#            upperair.NSSL_sonde_FortSmith_AR_MW41_output_20190430_192045.csv
#            upperair.NSSL_sonde_Greenville_MS_MW41_output_20190414_105659.csv
#            upperair.NSSL_sonde_Canton_MS_MW41_output_20190413_230812.csv
#            upperair.NSSL_sonde_Marlow_OK_MW41_output_20190430_192045.csv
#            The "readDataFiles" function uses a case-insensitive 
#            match to find all the files.
#
#          - Assume each record is at a one second interval.
#          - The wind components will need to be calculated. 
#          - The temp and dewpt will need to be converted to C.
#          
#          - Ascension rate is calculated by the converter.
#          - The headers should be as follows:
#            
#        Data Type: NSSL Sounding Data/Ascending
#        Project ID: VORTEX-SE Meso18-19
#        Release Site Type/Site ID: Use the station name from the file name
#        Release Location: Use the lat/lon and height from the first data record
#        UTC Release Time: Grab from the file name
#        Radiosonde Type: Vaisala RS41
#        Radiosonde Serial Number: Use the value from the Serial Number parameter
#
#          - A "readSurfaceValuesFile" function exists to 
#            read in surface values from a separate file, 
#            but is not called since the values were taken
#            from the first data record
#
#
##Module------------------------------------------------------------------------
package Meso_Radiosonde_Converter;
# NSSL_Radiosonde_Converter.pl
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

printf "\nNSSL Meso_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;

&main();
printf "\nNSSL Meso_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

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
    $self->{"NETWORK"} = "NSSL";
    
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
    # platform, 1172	Vaisala RS41
    $station->setPlatformIdNumber(1172);
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
    $header->setType("NSSL Sounding Data");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("NSSL");
	# $header->setSite("NOAA ATDD Mobile");


	if ($file =~ /FortSmith/i)
	{
		$header->setSite("Fort Smith AR");

	}
	elsif ($file =~ /Greenville/i)
	{
		$header->setSite("Greenville MS");

	}
	elsif ($file =~ /Marlow/i)
	{
		$header->setSite("Marlow OK");

	}
	elsif ($file =~ /Canton/i)
	{
		$header->setSite("Canton MS");

	}
	else
	{
		print "\tWARNING: Unrecognized location\n";

	}


	# -----------------------------------------------
	# Header lat/lon/elev values come from 
	# the surface record
	# -----------------------------------------------
	my @surfaceData = split(",",$headerlines[2]);
	my $sfc_elev = $surfaceData[30];
	$header->setAltitude($sfc_elev,"m");

	my $sfc_lat = $surfaceData[9];
    $header->setLatitude($sfc_lat, $self->buildLatLonFormat($sfc_lat));
	
	my $sfc_lon = $surfaceData[10];
	$header->setLongitude($sfc_lon, $self->buildLatLonFormat($sfc_lon)); 

    # -------------------------------------------------
   	# Other header info provided by Scot
	# -------------------------------------------------
    my $sondeType = "Vaisala RS41";
	$header->setLine(5,"Radiosonde Type:", ($sondeType));
	my @headerData = split(",",$headerlines[0]);
	my $sondeId = $headerData[2];
	$header->setLine(6,"Radiosonde Serial Number:", ($sondeId));

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
	# Expects filename similar to:
	# upperair.NSSL_sonde_FortSmith_AR_MW41_output_20181104_230008.csv
	# upperair.NSSL_sonde_Greenville_MS_MW41_output_20190412_230007.csv
    # ----------------------------------------------------------
    my $date;
	my $time;

	if ($file =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2})/)
	{
		my ($year, $month, $day, $hour, $minute, $second) = ($1,$2,$3,$4,$5,$6);
	    $date = join ", ", $year, $month, $day;
	    $time = join ":", $hour,$minute,$second;

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

	my @headerlines = @lines[1..3];
	# print "\tHEADER LINES: \n";
	# print @headerlines;

                        
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
	print "\tcitystate $citystate location @location\n";

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
		
		if ($index >= 3)
		{
			if ($fake_surface_data)
			{
			    $record->setTime($fake_surface_time);
				$fake_surface_data = 0;
			}
			
            #--------------------------------------------
			# Wind components must be calculated;
			# convert temp and dewpoint from K to C
			#--------------------------------------------

			# Filtered Temperature (K)  $data[2]
			# Filtered Humidity         $data[3]
			# Filtered Dewpoint (K)     $data[4]
			# Filtered Pressure (mb)    $data[5]
			# Filtered Wind Dir         $data[6]
			# Filtered Wind Spd (m/s)   $data[7]
			# Filtered Latitude         $data[9]
			# Filtered Longitude        $data[10]
			# FiltGeoPotHeight (m)      $data[31]
			
			chomp($line);
		    my @data = split(',',$line);


            if ($data[5])
			{
				$record->setPressure($data[5],"mb") if ($data[5] !~ /-32768/);
			}


			if ($data[2])
			{
				my $converted_temp = convertTemperature($data[2], "K", "C") if ($data[2] !~ /-32768/);
				$record->setTemperature($converted_temp,"C");
			}

			if ($data[4])
			{
				my $converted_dewpt = convertTemperature($data[4], "K", "C") if ($data[4] !~ /-32768/);
				$record->setDewPoint($converted_dewpt,"C");
			}
			
			if ($data[3])
			{
                $record->setRelativeHumidity($data[3]) if ($data[3] !~ /-32768/);

			}

			if ($data[7])
			{
				$record->setWindSpeed($data[7],"m/s") if ($data[7] !~ /-32768/);
			}

			if ($data[6])
			{
				$record->setWindDirection($data[6]) if ($data[6] !~ /-32768/);
			}

            if ($data[9])
			{
				$record->setLatitude($data[9], $self->buildLatLonFormat($data[9]));
			}
			if ($data[10])
			{
				$record->setLongitude($data[10], $self->buildLatLonFormat($data[10]));
			}
            if ($data[30])
			{
				$record->setAltitude($data[30],"m") if ($data[30] !~ /-32768/);
			}
		
        	#-------------------------------------
			# The first data line is index = 3.
			# Set initial time to zero, then 
			# use 1 second increments, so increment
			# $raw_data_time by one for lines
			# with $index > 3
			#-------------------------------------
			if ($index == 3)
			{                                                 
				$raw_data_time = 0;
		    	$record->setTime($raw_data_time);
				$raw_data_time += 1;         
			}
			elsif ($index > 3)
			{
				$record->setTime($raw_data_time);
				$raw_data_time += 1;
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
    my @files = grep(/.csv$/i,sort(readdir($RAW)));
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
