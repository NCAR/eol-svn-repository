#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Purdue_Mobile_Sounding_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format.</p> 
#
# @author Linda Echo-Hawk 22 Oct 2013 thru 4 Mar 2014
# @version MPEX Purdue Mobile Soundings
#    - BEWARE: This data had a lot of problems, so this converter has
#      several hard-coded fixes that may not be appropriate for other
#      sounding datasets.
#    - Based on the UAH Mobile converter for DC3
#    - The date, time and sonde ID are taken from the file name.
#    - The lat, lon, and alt information are obtained from the surface 
#      data record (the first line of the raw data).
#    - A separate script, AddMissingColums.pl, was run on 
#      research.Purdue_sonde.20130604011707.skewT.txt as a pre-processing
#      step to insert missing RH columns at the end of the file.
#    - For research.Purdue_sonde.20130524001306.skewT.txt three lines 
#      were manually changed to remove a digit and spaces before the data.
#    - Inserted code that made sure that missing wind values would also
#      have missing wind direction.
#    - Added code to check for missing pressure and stopped processing at
#      that point.  (Several files had missing pressure records for several
#      lines at the end of the file.)
#    - For research.Purdue_sonde.20130518230503.skewT.txt, Scot L. had 
#      indicated that a section of code had bad wind values and asked that
#      all wind values in that section be set to missing.  The beginning and
#      ending line of that section had to be identified by hard-coding in
#      the time and wind speed.  
#    - Several files had sections were "extra" columns were created by the
#      insertion of tabs in the raw data.  This caused the lat/lon data to
#      appear in an unexpected column, so code was added to identify these
#      lines and look in the correct location for lat/lon info.
#    - Added a fix for: The wind directions are not being converted properly.
#      They are being treated as degrees, but they are in radians. (from Scot L.)
#    - Hard-coded fixes for files that Scot specified should have records cut
#      off after certain times.
#    - Hard-coded fixes for various files where temperatures should be set
#      to missing at particular times as indicated by Scot.
#
#
##Module------------------------------------------------------------------------
package Purdue_Mobile_Sounding_Converter;
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


printf "\nPurdue_Mobile_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nPurdue_Mobile_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Purdue Mobile (AL) radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Purdue_Mobile_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Purdue_Mobile_Sounding_Converter new()
# <p>Create a new instance of a Purdue_Mobile_Sounding_Converter.</p>
#
# @output $self A new Purdue_Mobile_Sounding_Converter object.
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
    $self->{"NETWORK"} = "Purdue_Mobile";
    
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
# <p>Create a default station for the Purdue Mobile network using the 
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
    # platform, 591, Radiosonde, iMet-1
    $station->setPlatformIdNumber(591);
    $station->setMobilityFlag("m"); 
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
    $header->setType("Purdue Mobile");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("Purdue_Mobile");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Purdue Mobile");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
    my (@header_info) = split(" ",$headerlines[0]); 
	my $lon = trim($header_info[13]);
	my $lat = trim($header_info[12]);
	my $alt = trim($header_info[5]);
	
	$header->setLongitude($lon,$self->buildLatlonFormat($lon));
	$header->setLatitude($lat,$self->buildLatlonFormat($lat));
    $header->setAltitude($alt,"m"); 
	
    # ------------------------------------------------------------------
    # Extract the date and time information from the file name
    # BEWARE: Expects filename like: 
	# research.Purdue_sonde.20130517000401.skewT.txt
    # ------------------------------------------------------------------
    # print "file name = $filename\n"; 

    my $date;
	my $time;
	my $hour;
	my $min;
	my $snding;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})\.skewT\.txt$/)
	{
		my ($yearInfo, $monthInfo, $dayInfo, $hourInfo, $minuteInfo, $second) = ($1,$2,$3,$4,$5,$6);

		$hour = $hourInfo;
		$min = $minuteInfo;
	    $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
	    $time = join "", $hour, ' ', $min, ' ', $second;
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

	
	my $sonde_type = "iMet-1 with GPS windfinding";
	my $ground_station = "iMET-3050";
	my $surface_source = "Radiosonde";

	$header->setLine(5, "Sonde Type:", join('/', $sonde_type));
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
    my ($self,$file) = @_;
    
    printf("\nProcessing file: %s\n",$file);

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);


    # set pressure to missing for 201305240013 data
	# research.Purdue_sonde.20130524001306.skewT.txt
    my $bad_pressure;
	if ($file =~ /research.Purdue_sonde.20130524001306.skewT.txt/)
	{
		print "\tFound $file with bad pressure\n";
		$bad_pressure = 1;
	}
    # set cut-off times for the files indicated by Scot.
	my $adjust_file;
	if ($file =~ /(\d{14})/)
	{
		$adjust_file = ($1);
	}

    my %last_good_time = (
		'20130518230503' => '1527',
   		'20130519000005' => '1613',
		'20130519004106' => '1157',
		'20130519205303' => '2039',
		'20130519214404' => '1389',
		'20130520001106' => '1595',
		'20130520005007' => '1230',
		'20130520192203' => '1204',
		'20130520195804' => '1259',
		'20130520204505' => '1310',
		'20130520211806' => '3261',
		'20130523192802' => '719',
		'20130527234204' => '1582',
		'20130528005505' => '1419',
		'20130528235705' => '1545',
		'20130529200904' => '2850',
		'20130530203404' => '1980',
		'20130530235709' => '1148',
		'20130601010005' => '1035',
		'20130601015506' => '2128',
		'20130601023604' => '1146',
		'20130603220703' => '1956',
		'20130603230604' => '1758',
		'20130604011707' => '2289',
		'20130608205603' => '1314',
		'20130608223306' => '1365',
		'20130609005007' => '892',
		'20130611231606' => '3474',
	);

    # print "File adjustment for File $adjust_file: $last_good_time{$adjust_file}\n";

    
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
	   					   $hour, $min, $sec);
 
    printf("\tOutput file name:  %s\n", $outfile);

	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".$outfile)
	    or die("Can't open output file for $file\n");

	print($OUT $header->toString());
	

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
	my $writeOnce = 0;
	my $goodWindSpeed = 1;

	foreach my $line (@lines) {
	    
		chomp($line);
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

	    $record->setTime($recordTime);
        # don't process records with missing pressure 
		if ($data[4] =~ /-4316020/)
		{
			print "\tPressure records missing at t=$recordTime, end loop\n";
			last;
		}
		else
		{
	    	# Scot has requested that some files be cut off 
			# after a certain point in time.  See the hard-coded
			# hash for files and "last times".
			if ($file =~ /$adjust_file/)
			{
				# print "FILE: $file found \n";
				# last;
				if (exists ($last_good_time{$adjust_file}))
				{
					if (($last_good_time{$adjust_file}) < $recordTime)
					{
						print "\tFILE $file found\n\tLast Good Time $last_good_time{$adjust_file} at record = $recordTime\n";

					    print "\tDrop Records after $last_good_time{$adjust_file}\n";
						last;
						
					}
				}
			}

            # set pressure to missing for 201305240013 data
			# research.Purdue_sonde.20130524001306.skewT.txt
			if (!$bad_pressure)
			{
				$record->setPressure($data[4],"mb") if ($data[4] !~ /-4316020/);
			}

            # --------------------------------------------------------------
            # Set the temperature unless this is one of the files with
			# special corrections made by Scot
	        # $record->setTemperature($data[1],"C") if ($data[1] !~ /-999/);    
            # --------------------------------------------------------------
			$record->setTemperature($data[1],"C") unless 
				((($file =~ /research.Purdue_sonde.20130528013306.skewT.txt/) &&
			            ($recordTime == 1184)) ||
				  (($file =~ /research.Purdue_sonde.20130603235906.skewT.txt/) &&
				        ($recordTime >= 1471)) ||
				  (($file =~ /research.Purdue_sonde.20130608205603.skewT.txt/) &&
				        ($recordTime == 1304)) ||
				   ($data[1] =~ /-999/));
			
	        $record->setRelativeHumidity($data[3]) if ($data[3] !~ /-4316020/);
		
            # ******************************************
			# Wind Section has hard-coded solutions to
			# various problems.
			# NOTE: Wind Speed is in knots, 
			# Wind Direction is in radians
			# Col 9: Wind direction (radians)(i.e., $data[8])
			# Col 10: Wind speed (kts)(i.e., $data[9])
            # ******************************************
			my $windDir = $data[8];
			my $windSpeed = $data[9];

			# For one file (research.Purdue_sonde.20130518230503.skewT.txt), 
			# the entire section must be set to missing. The begin and end
			# values are not automatically picked up by the perl library 
			# code so are set here
			if (($data[0] =~ /23:27:12/) && ($windSpeed =~ /316.53/))
			{
				$goodWindSpeed = 0;
				print "Bad WindSpeed Found == $windSpeed\n";
			}
            if ($goodWindSpeed)
			{
				# Convert Winds from knots to m/s if the value is not "missing"
				# my $convertedWindSpeed = convertVelocity($data[8],"knot", "m/s");
				my $convertedWindSpeed = convertVelocity($windSpeed,"knot", "m/s");
                # set the wind speed if the original value was not "missing"
				$record->setWindSpeed($convertedWindSpeed,"m/s") if ($windSpeed !~ /-999/);
			}

			# If the windspeed is not in the "missing" section for the file 
			# research.Purdue_sonde.20130518230503.skewT.txt, then convert
			# the wind direction from radians to degrees and set the wind 
			# direction values
			# 
			# The windspeed will be converted to "missing" if it is greater
			# than 5 chars including the decimal point (perl format F5.1).
			# If the windspeed is "missing" set the wind dir to missing also,
			# i.e., DON'T SET the wind direction.
			if (($goodWindSpeed) && ($windSpeed < 1000.0))
			{
                # The wind direction is in radians, so convertAngle is used 
				# to convert the values to degrees.
                # use convertAngle (initial angle, initial units, target units)
				# my $windDir = convertAngle($data[8],"rad","deg");
				my $convertedWindDir = convertAngle($windDir,"rad","deg");
                # set the wind dir if the original value was not "missing"
				$record->setWindDirection($convertedWindDir) if ($windDir !~ /-999/);
			}
		    # reset $goodWindSpeed to true
			# once we are through with this section of bad data
			# (for research.Purdue_sonde.20130518230503.skewT.txt)
			if (($data[0] =~ /23:29:21/) && ($windSpeed =~ /422.95/))
			{
				$goodWindSpeed = 1;
				print "Set wind speed back to good\n";
			}
            # ******************************************
			# End Wind Section 
            # ******************************************


		    $record->setAltitude($data[5],"m");

		    # Some files have records with extra columns
			# Use the columns that end with a letter (hex?)
			# to determine the correct location for lat/lon data
		    # if ($data[13] =~ /(F|B|E|A)$/)
		    if ($data[13] =~ /\D$/)
		    {
			    $record->setLongitude($data[16],$self->buildLatlonFormat($data[16])) if ($data[16] !~ /-999/);
			    $record->setLatitude($data[14],$self->buildLatlonFormat($data[14])) if ($data[14] !~ /-999/);

		    }
			# elsif ($data[14] =~ /(F|B|E|A)$/)a
			elsif ($data[14] =~ /\D$/)
			{
				if (!$writeOnce)
				{
					print "\tFound 0F in data[14]\n";
					$writeOnce = 1;
				}
			    $record->setLongitude($data[17],$self->buildLatlonFormat($data[17])) if ($data[16] !~ /-999/);
			    $record->setLatitude($data[15],$self->buildLatlonFormat($data[15])) if ($data[14] !~ /-999/);

			}
		    else
		    {
			    $record->setLongitude($data[13],$self->buildLatlonFormat($data[13])) if ($data[13] !~ /-999/);
			    $record->setLatitude($data[12],$self->buildLatlonFormat($data[12])) if ($data[12] !~ /-999/);
		    }
		
		    $recordTime++;
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

	    printf($OUT $record->toString());
    } # end foreach my $line
	

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
	# HARD-CODED FILE NAME
    # my @files = grep(/^\d{8}_12UTC\.txt/,sort(readdir($RAW)));
    my @files = grep(/txt$/,sort(readdir($RAW)));
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
