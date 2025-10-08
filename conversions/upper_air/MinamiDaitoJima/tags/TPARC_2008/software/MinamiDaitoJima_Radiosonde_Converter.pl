#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The MinamiDaitoJima_Radiosonde_Converter.pl script is used for converting 
# Minami Daito Jima Weather Station (JMA - Japanese Meteorological Society) 
# high resolution radiosonde data to the EOL Sounding Composite (ESC) format.</p> 
#
#
# @author Linda Echo-Hawk 2009-12-2
# @version T-PARC_2008  Created for T-PARC_2008 Minami Daito Jima data based on 
#          the GAUS_Converter.pl.
#          - Converter expects the actual data to begin
#            line 41 of the raw data file.  
#          - Header lat/lon/alt info is obtained from the data.  
#          - Release time is obtained from the file name.
#          - Some files have the same date and time with a "_0" 
#            or "_2" appended before the .tsv. NOTE:  On 6/17/2010
#            Scot instructed that the *_2.tsv files be removed from the
#            raw data.  No code change is required, and this is left in
#            place in case we decide to process these later.
#            [OLD NOTE: The temporary fix for this is to assume this is 
#            a sounding number and to substitute this in for the minutes 
#            portion (always "00") of the output file name, so files are 
#            not overwritten.]
#          - Code was added from the Ron Brown Converter to derive
#            the ascension rate.
#
# @usage   MinamiDaitoJima_Radiosonde_Converter.pl >& results.txt
##Module------------------------------------------------------------------------
package MinamiDaitoJima_Radiosonde_Converter;
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
 
use ElevatedStationMap;
use Station;
use ClassConstants qw(:DEFAULT);
use ClassHeader;
use ClassRecord;

my ($WARN);

printf "\nMinamiDaitoJima_Radiosonde_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nMinamiDaitoJima_Radiosonde_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;
my $sounding = "";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Minami Daito Jima radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = MinamiDaitoJima_Radiosonde_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature MinamiDaitoJima_Radiosonde_Converter new()
# <p>Create a new instance of a MinamiDaitoJima_Radiosonde_Converter.</p>
#
# @output $self A new MinamiDaitoJima_Radiosonde_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "T-PARC";
    # HARD-CODED
    $self->{"NETWORK"} = "Minami_Daito_Jima";
    
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
# <p>Create a default station for the Minami Daito Jima network using the 
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
    # $station->setStateCode("99");
    $station->setCountry("Japan");
    $station->setReportingFrequency("12 hourly");
    $station->setNetworkIdNumber("47945");
    # platform, 87, Rawinsonde, Other 
	# believe these are Vaisala Autosonde RS92-AGP based on web doc
	# "Improvements in the Upper-Air Observation Systems in Japan" by
    # by Masahito Ishihara, et al.  NOTE: Scot confirmed this June 18, 2010
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
    # my ($self,$file,@lines) = @_; 
    my $header = ClassHeader->new();

    $filename = $file;
	# printf("parsing header for %s\n",$filename);
    $header->setReleaseDirection("Ascending");

    # Set the type of sounding
    $header->setType("Minami Daito Jima Radiosonde (JMA)");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("MINAMI_DAITO_JIMA");
	# Per Scot, the site ID is 47945.  This info goes in the 
	# "Release Site Type/Site ID:" header line
    $header->setSite("Minami Daito Jima, Okinawa, Japan/47945");

    # Read through the file for additional header info
 	my $index = 0;
	foreach my $line (@headerlines) 
	{
        # Add the non-predefined header lines to the header.
        # Changed $i to $i-1 to remove extra blank line from header. 
        # for (my $i = 6; $i < 11; $i++) 
		if (($index > 0) && ($index < 11))
	    {
		    if ($line !~ /^\s*\/\s*$/) 
		    {
			    if ($line =~ /RS-Number/)
			    {
					chomp($line);
				    my ($label,@contents) = split(/:/,$line);
				    # each sonde ID ends with "!00" (e.g., D2033580!00) so 
                    # I am removing it (@contents has only one element)
					$label = "Sonde Id/Sonde Type";
					$contents[0] =~ s/!00//g;
			        # need to add "Vaisala RS92-AGP" after sonde ID (@contents)
			  	    $contents[1] = "Vaisala RS92-AGP";
			        $header->setLine(5, trim($label).":",trim(join("/",@contents))); 
			        # $header->setLine(($index-1), trim($label).":",trim(join(":",@contents)));
		        }
	        }
	    }   

	    # Ignore the header lines.
	    if ($index < 40) { $index++; next; }
        # Find the lat/lon for the release location in the actual data.
        # NOTE:  It looks as though these values are the same for every 
        # file and could have been hard-coded.  But this is reusable.	
		else
		{
			my @data = split(' ',$line);
			if (($data[15] > -32768) & ($data[16] > -32768))
			{
                # format length must be the same as the value length or
                # convertLatLong will complain (see example below)
                # base lat = 36.6100006103516 base lon = -97.4899978637695
                # Lat format = DDDDDDDDDDDDDDDD  Lon format = -DDDDDDDDDDDDDDDD  
 		    	my $lon_fmt = $data[15] < 0 ? "-" : "";
				while (length($lon_fmt) < length($data[15])) { $lon_fmt .= "D"; }
				$header->setLongitude($data[15],$lon_fmt);

 				my $lat_fmt = $data[16] < 0 ? "-" : "";
				while (length($lat_fmt) < length($data[16])) { $lat_fmt .= "D"; }
				$header->setLatitude($data[16],$lat_fmt);
 
            	$header->setAltitude($data[6],"m"); 
				last;
			}
		}
	}


    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # BEWARE: Expects filename to be similar to: fledt080900.tsv 
	# or fledt080900_2.tsv
    # NOTE:  Removed the "*_2.tsv" raw data files as Scot
    # instructed on June 17, 2010
    # ----------------------------------------------------------
    # print "file name = $filename\n"; 

    my $date;
	my $time;

	if ($filename =~ /(\d{2})(\d{2})(\d{2})/)
	{
		my ($monthInfo, $dayInfo, $timeInfo) = ($1,$2,$3);
	    # printf("MonthInfo = %s\n", $monthInfo);
	    # printf("DayInfo = %s\n", $dayInfo);
	    # printf("TimeInfo = %s\n", $timeInfo);
		# printf("Filename = %s\n", $filename);
		if ($filename =~ /(_\d)/)
		{
			$sounding = $1;
			$sounding =~ s/_/0/g;
		}
		else
		{
			$sounding = "";
		}

		if ($sounding)
		{
			printf("Sounding number = %s\n", $sounding);
		}
		$monthInfo = join "", $monthInfo, ' ';
	    $date = join ",", '2008 ', $monthInfo, $dayInfo;
	    # print "date is $date\n";
	    $time = join "", $timeInfo,':00:00';
        # print "time is $time\n";
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

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
	my @headerlines = @lines;
    my $header = $self->parseHeader($file,@headerlines);
    
    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
						      $header->getLatitude(),$header->getLongitude(),
						      $header->getAltitude());
	if (!defined($station)) {
	    $station = $self->buildDefaultStation($header->getId(),$self->{"NETWORK"});
	    $station->setLatitude($header->getLatitude(),$self->buildLatlongFormat($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->buildLatlongFormat($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    # ----------------------------------------------------
    # Create the output file name and open the output file
    # ----------------------------------------------------
    my $outfile;
	my ($hour, $min, $sec) = split (/:/, $header->getActualTime());

	# ----------------------------------------------------------
	# If there is a sounding number, two files have the same
	# date and time with a "_0" or "_2" appended before the .tsv.
	# I replaced the minutes portion of the time (always "00") with
	# the sounding number so the files don't overwrite each other.
    #
	# ----------------------------------------------------------
	if ($sounding)
	{
		printf("SOUNDING ADDED TO FILENAME: %s\n", $sounding);
		$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls",
 							   $header->getId(),
							   split(/,/,$header->getActualDate()),
							   $hour, $sounding);
	}
	else
	{
		$outfile = sprintf("%s_%04d%02d%02d%02d%02d.cls", 
  							   $header->getId(),
							   split(/,/,$header->getActualDate()),
							   $hour, $min);
	}
 
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
	my $index = 0;
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 40) { $index++; next; }
	    
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

        # docs for T-REX_2006 leeds state that missing values are -32768 
        # which matches this data
	    $record->setTime($data[0]);
	    $record->setPressure($data[7],"mb") if ($data[7] != -32768);
	    # $record->setTemperature($data[2],"C") if ($data[2] != -32768);    
        # Temp and Dewpt are in Kelvin.  C = K - 273.15
	    $record->setTemperature(($data[2]-273.15),"C") if ($data[2] != -32768);    
		$record->setDewPoint(($data[8]-273.15),"C") if ($data[8] != -32768);
	    $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);
	    $record->setUWindComponent($data[5],"m/s") if ($data[5] != -32768);
	    $record->setVWindComponent($data[4],"m/s") if ($data[4] != -32768);
	    $record->setWindSpeed($data[11],"m/s") if ($data[11] != -32768);
	    $record->setWindDirection($data[10]) if ($data[10] != -32768);

	    # get the lat/lon data 
	    if ($data[15] != -32768) {
		my $lon_fmt = $data[15] < 0 ? "-" : "";
		while (length($lon_fmt) < length($data[15])) { $lon_fmt .= "D"; }
		$record->setLongitude($data[15],$lon_fmt);
	    }
	    if ($data[16] != -32768) {
		my $lat_fmt = $data[16] < 0 ? "-" : "";
		while (length($lat_fmt) < length($data[16])) { $lat_fmt .= "D"; }
		$record->setLatitude($data[16],$lat_fmt);
	    }
        # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
		# For setVariableValue(index, value):  
		# index (1) is Ele column, index (2) is Azi column.
		$record->setVariableValue(1, $data[12]) if ($data[12] != -32768);
		$record->setVariableValue(2, $data[13]) if ($data[13] != -32768);
	    $record->setAltitude($data[6],"m") if ($data[6] != -32768);
	    
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
    my @files = grep(/^fledt.+\.tsv/,sort(readdir($RAW)));
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
