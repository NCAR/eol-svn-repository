#! /usr/bin/perl -w
##Module------------------------------------------------------------------------
# <p>The Haast_Radisonde_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format. The incoming format is Vaisala (Digicora 3).</p> 
#
# @author Linda Echo-Hawk
# @version DEEPWAVE 2014 for Haast soundings
#          - The converter expects filenames in the format:
#            01-07-2014-release_0600Z-FLEDT.tsv
#          - see comments for Hobart BoM -- these all apply
#
# @author Linda Echo-Hawk
# @version DEEPWAVE 2014 for Hobart BoM
#          - The converter expects filenames in the following
#            format: 94975_YYYYMMDDHHmmss.tsv (e.g., 94975_20140721111731.tsv)
#          - The file contains header info on lines 1-39. Actual data starts 
#            on line 41. 
#          - The radiosonde ID is obtained from the header information.
#          - The lat/lon/alt header values are obtained from the surface
#            data record (t=0).
#          - Missing values are represented by "-32768.00" in the raw data.
#          - The release date and time and obtained from the file name.
#          - Temperature and dewpoint are in Kelvin and must be converted to 
#            Celsius by subtracting 273.15 or using the Perl Library function 
#            convertTemperature.
#
#
# @author Linda Echo-Hawk
# @version DYNAMO 2011 for Sipora Indonesia
#    This code was created by modifying the R/V Sagar Kanya converter.
#          - Header lat/lon/alt info is obtained from the data.  
#          - Release time is obtained from the file name.
#          - Search for "HARD-CODED" to find project-specific items that
#            may require changing.
# This code makes the following assumptions:
#  - That the raw data file names shall be in the form
#        "yymmddhhEDT.tsv" where yy = year, mm = month, dd = day, hh=hour. 
#  - That the raw data is in the Vaisala "Digicora 3" format. The file contains
#         header info on lines 1-39. Actual data starts on line 40. 
#
#
##Module------------------------------------------------------------------------
package Haast_Converter;
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

printf "\nHaast_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nHaast_Converter.pl ended on ";print scalar localtime;printf "\n";

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Haast radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Haast_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Haast_Converter new()
# <p>Create a new instance of a Haast_Converter.</p>
#
# @output $self A new Haast_Converter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = ElevatedStationMap->new();

    # HARD-CODED
    $self->{"PROJECT"} = "DEEPWAVE";
    # HARD-CODED
    $self->{"NETWORK"} = "Haast";
    
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
# <p>Create a default station for the West Texas Mesonetnetwork using the 
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
	$station->setCountry("99");
    # $station->setStateCode("48");
    $station->setReportingFrequency("3 hourly");
    $station->setNetworkIdNumber("99");
    # platform, Radiosonde, Vaisala RS92
    $station->setPlatformIdNumber(944);
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

	# printf("parsing header for %s\n",$file);

    # Set the type of sounding "Data Type:" header line
    $header->setType("NIWA Radiosonde");
    $header->setReleaseDirection("Ascending");

    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("NIWA_Haast");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Hannahs Clearing, South Westland, New Zealand/Haast");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
 	my $index = 0;
	foreach my $line (@headerlines) 
	{
        # skip over any blank lines (empty or contain white space only)
		if ($line !~ /^\s*$/) 
		{
		    if ($line =~ /RS-Number/i)
		    {
				chomp ($line);
			    my ($label,@contents) = split(/:/,$line);
				$label = "Sonde Id/Sonde Type";
				$contents[1] = "Vaisala RS92";
		        $header->setLine(5, trim($label).":",trim(join("/",@contents)));
		    }
	    }

	    # Ignore the header lines.
	    # if ($index < 40) { $index++; next; }
        # Find the lat/lon for the release location in the actual data.
		# else
		if ($index == 40)
		{
			my @data = split(' ',$line);
			if (($data[15] !~ /-32768/) & ($data[16] !~ /-32768/))
			{
				$header->setLongitude($data[15],$self->buildLatlonFormat($data[15]));
				$header->setLatitude($data[16],$self->buildLatlonFormat($data[16]));
            	$header->setAltitude($data[6],"m"); 
				last;
			}
		}

   		$index++;
	}

    $header->setLine(6,"Ground Station Equipment: ", "Vaisala DigiCORA MW41");

    # ----------------------------------------------------------
    # Extract the date and time information from the file name
    # Expects filename similar to: DD-MM-YYYY-release-HHMMZ-FLEDT.tsv
	# e.g., 01-07-2014-release_0600Z-FLEDT.tsv
    # ----------------------------------------------------------
    # print "file name = $file\n"; 

	if ($file =~ /(\d{2})-(\d{2})-(\d{4})-release_(\d{2})(\d{2})/)
	{
		my ($day, $month, $year, $hour, $min) = ($1,$2,$3,$4,$5);
        my $date = join ", ", $year, $month, $day;
		my $time = join ":", $hour,$min, "00";
        # print "DATE:  $date   TIME:  $time\n";

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
    
    # Generate the sounding header.
	my @headerlines = @lines[0..43];
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

	    $record->setTime($data[0]);
	    $record->setPressure($data[7],"mb") if ($data[7] != -32768);
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
		$record->setLongitude($data[15],$self->buildLatlonFormat($data[15]));
	    }
	    if ($data[16] != -32768) {
		$record->setLatitude($data[16],$self->buildLatlonFormat($data[16]));
	    }
        # Insert Ele (Elevation Angle) and Azi (Azimuth Angle) data
		# For setVariableValue(index, value):  
		# index (1) is Ele column, index (2) is Azi column.
		$record->setVariableValue(1, $data[13]) if ($data[13] != -32768);
		$record->setVariableValue(2, $data[12]) if ($data[12] != -32768);
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
	# HARD-CODED FILE NAME
    # my @files = grep(/^(d{2})-(d{2})-(\d{4})-release_(\d{4})Z-FLEDT\.tsv/,sort(readdir($RAW)));
    my @files = grep(/\.tsv$/,sort(readdir($RAW)));
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
