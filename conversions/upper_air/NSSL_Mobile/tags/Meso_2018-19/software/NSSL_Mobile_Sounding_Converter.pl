#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The NSSL_Mobile_Sounding_Converter.pl script is used for converting high 
# resolution radiosonde data to the EOL Sounding Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk 28 Jan 2014
# @version MPEX -- Created for the NSSL Mobile radiosonde data.
#
#          - BEWARE: The raw data was run through a preprocessor 
#            before conversion. Use the RemoveDescending.pl script 
#            to pre-process data. The purpose of the script is to 
#            remove the descending sounding data at the end of the 
#            files.  The raw data file was read into an array which 
#            was reversed. The NSSL raw data had a column for comments. 
#            Starting at the end of the file, each line was checked 
#            and if it contained the comment "descent" it was removed 
#            from the array. Once good data was found, the loop ends 
#            and the remaining records are written out to a new raw 
#            data file.
#          - Raw data files are ascii format with no headers, 
#            and the converter expects data to begin on line 1
#            of the raw data file.
#          - Header lat/lon/altitude info is obtained from the 
#            first line of the data (surface data).  
#          - Release time is obtained from the file name.
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
#          - The data records do not have a "time" column.  The
#            record time variable is initialized to zero and 
#            incremented after the record is complete. Scot has
#            indicated that this is 1-second data.
#          - Code was added to set all values to missing when 
#            pressure is 100.0, PTH is -99.0 and lat/lon is 
#            999.000, as instructed by Scot.
#          - Code was added to prepend a negative sign onto the
#            longitude data.  Scot indicated that the longitude
#            data should be negative (W).
#          - Code was added to set the ascension rate flag (QdZ 
#            code) at the surface to missing.
#
#
#
#
#
##Module------------------------------------------------------------------------
package NSSL_Mobile_Sounding_Converter;
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

printf "\nNSSL_Mobile_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
my $debug_geopotential_height = 1;

&main();
printf "\nNSSL_Mobile_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the NSSL_Mobile_Sounding_Converter data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = NSSL_Mobile_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature NSSL_Mobile_Sounding_Converter new()
# <p>Create a new instance of a SUNY_Radiosonde_Converter.</p>
#
# @output $self A new NSSL_Mobile_Sounding_Converter object.
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
    $self->{"NETWORK"} = "NSSL_Mobile";
    
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
    $station->setStationName("NSSL Mobile");
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
    $header->setType("NSSL Mobile");
    $header->setReleaseDirection("Ascending");
    $header->setProject($self->{"PROJECT"});
	# The Id will be the prefix of the output file
    # and appears in the stationCD.out file
    $header->setId("NSSL_Mobile");
	
    # -------------------------------------------------
	# This info goes in the "Release Site Type/Site ID:" header line
    # -------------------------------------------------
	$header->setSite("NSSL Mobile");

    # -------------------------------------------------
    # Get the header lat/lon data
    # -------------------------------------------------
    my @headerData = split(' ',$headerlines[0]);    
	my $releaseLat;
	my $releaseLon;
	my $alt;

    # Four files were missing surface release info
	if ($filename =~ /NSSL_20130523_2341.txt/)
	{
		$releaseLat = 32.44;
		$releaseLon = -100.40;
		$alt = $headerData[12];
	}
	elsif (($filename =~ /NSSL_20130611_1815.txt/) ||
	       ($filename =~ /NSSL_20130611_1900.txt/))
	{
		$releaseLat = 41.814;
		$releaseLon = -99.380;
		$alt = $headerData[12];
	}
	elsif ($filename =~ /NSSL_20130612_0053.txt/)
	{
		$releaseLat = 41.859;
		$releaseLon = -101.047;
		$alt = 1270.3;
	}
	else
	{
		$releaseLat = $headerData[3];
		$releaseLon = $headerData[4];
		$releaseLon = "-".$releaseLon;

		$alt = $headerData[12];
	}

	print "HEADER LAT: $releaseLat LON: $releaseLon\n";

    $header->setLatitude($releaseLat, $self->buildLatlongFormat($releaseLat));
	$header->setLongitude($releaseLon, $self->buildLatlongFormat($releaseLon)); 
    $header->setAltitude($alt,"m"); 

	my $sonde_type = "iMet-1 with GPS windfinding";
	my $ground_station = "iMET-3050 or iMet-3150";
	my $surface_source = "Independent instruments mounted atop Dodge Minivan (approx 4 m AGL)";

	$header->setLine(5, "Sonde Type:", join('/', $sonde_type));
	$header->setLine(6, "Ground Station Software:", ($ground_station));
	$header->setLine(7, "Surface Data Source:", $surface_source);


    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: NSSL_20130528_2100.txt
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 
    my $date;
	my $time;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})/)
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

    # ---------------------------------------------
    # Needed for code to derive geopotential height
    # ---------------------------------------------
	my $previous_record;
	my $geopotential_height;

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

        # missing values vary -- see readme
	    $record->setTime($recTime);

        # -------------------------------------------------------
        # if pressure is 100.0 and all other PTH parameters 
		# are -99.0 and lat/lon is 999.000, set all parameters 
		# for that record to our missing values and set the 
		# accompanying flags to missing.  By NOT setting the
		# values, the toString function sets these to missing.
        # -------------------------------------------------------
        if (($data[6] =~ /100.0/) &&
			($data[8] =~ /-99.0/) &&
			($data[10] =~ /-99.0/) &&
			($data[3] =~ /999.000/) &&
			($data[4] =~ /999.000/))
		{
			# print this record as "all missing"
			# printf($OUT $record->toString());
			push(@record_list, $record);
			$recTime++;
		}
		else
		{
	        $record->setPressure($data[6],"mb") if ($data[6] !~ /9999.0/);
        	$record->setPressureFlag($data[7]); 
		    $record->setTemperature($data[8],"C") if ($data[8] !~ /-99.0/);
			$record->setTemperatureFlag($data[9]);
		    $record->setRelativeHumidity($data[10]) if ($data[10] !~ /-99.0/);
			$record->setRelativeHumidityFlag($data[11]);

        	$record->setUWindComponent($data[14],"m/s") if ($data[14] !~ /-99.0/);
			$record->setUWindComponentFlag($data[16]);
			$record->setVWindComponent($data[15],"m/s") if ($data[15] !~ /-99.0/);
			$record->setVWindComponentFlag($data[16]);

	        if ($data[3] !~ /999.000/)
			{
			    $record->setLatitude($data[3], $self->buildLatlongFormat($data[3]));
			}
			# all longitude data should be negative (W)
        	if ($data[4] !~ /999.000/)
			{
				my $lon = "-".$data[4];
				# print "LON: $lon\n";
				$record->setLongitude($lon, $self->buildLatlongFormat($lon));
			}

			# NOTE: Check missing value of altitude
			$record->setAltitude($data[12],"m") if ($data[12] !~ /-99.0/);
		
	        # set the ascension rate flag (QdZ code) at the surface to missing
			if ($recTime == 0)
			{
				$record->setAscensionRateFlag("9.0");
			}
			else
			{
				$record->setAscensionRateFlag($data[13]);
			}


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

		    # printf($OUT $record->toString());
			push(@record_list, $record);
		} # end else (pressure is not 100.0)

	} # end foreach my $line (@lines)

    
    # --------------------------------------------------
	# Remove the last records in the file that are 
    # descending (ascent rate is negative)
	# --------------------------------------------------
	foreach my $last_record (reverse(@record_list))
	{
	    if (($last_record->getPressure() =~ /9999/) &&
			($last_record->getTemperature() =~ /999/) &&
			($last_record->getRelativeHumidity() =~ /999/) &&
			($last_record->getLatitude() =~ /999/) &&
			($last_record->getLongitude() =~ /999/))
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
    # my @files = grep(/^D\d{8}_\d{6}.+\.eol/,sort(readdir($RAW)));
    my @files = grep(/^NSSL.+\.txt/,sort(readdir($RAW)));
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
