#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The Redstone_Sounding_Converter.pl script is used for converting high
# resolution radiosonde data from ASCII formatted data) to the EOL Sounding 
# Composite (ESC) format.</p> 
#
# @usage Redstone_Sounding_Converter.pl [--skip] 
#        --skip   Skip the pre-processing steps; default is false (don't skip)
#                 These steps strip out the blank lines in a file
#
# @author Linda Echo-Hawk 2013-02-19
# @version DC3 Redstone 
#    This code was created by modifying the DYNAMO Male Maldives converter.
#    - Added some code to strip out blank lines in the raw data prior to
#      creating a headerlines array.
#    - Added a command line option to skip the preprocessing (stripping blank
#      lines) to speed up processing.
#    - The code expects files with names like YYYYMMDD_12UTC.txt (where YYYY
#      is the year, MM is month, and DD is day).
#    - There is a separate file for sounding IDs (/Redstone/sonde_ids.txt).
#      The sonde ID's were hard-coded into a hash in the converter.
#    - The "missing" value is 999 for RH and 99.9 for dewpoint.
#    - The converter stops when it reaches "MANDATORY LEVELS" or 
#      "SIGNIFICANT LEVELS" line in the raw data file.
#    - Search for "HARD-CODED" to find project-specific items that
#      may require changing.
##Module------------------------------------------------------------------------
package Redstone_Sounding_Converter;
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

# import module to set up command line options
use Getopt::Long;

my ($WARN);

# read command line arguments 
my $result;   
# skip pre-processing steps for raw data files
my $skip;
# "limit:i" i is optional, "limit=i" i is required
$result = GetOptions("skip" => \$skip);

if ($skip)
{
 	printf("Skip pre-processing steps.\n");
}
else
{
   	printf("Perform pre-processing.\n");
}


printf "\nRedstone_Sounding_Converter.pl began on ";print scalar localtime;printf "\n";
my $debug = 0;
&main();
printf "\nRedstone_Sounding_Converter.pl ended on ";print scalar localtime;printf "\n";

my $filename;

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Redstone (AL) radiosonde data by converting it from 
# the native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Redstone_Sounding_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature Redstone_Sounding_Converter new()
# <p>Create a new instance of a Redstone_Sounding_Converter.</p>
#
# @output $self A new Redstone_Sounding_Converter object.
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
    $self->{"NETWORK"} = "Redstone";
    
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
# <p>Create a default station for the Redstone network using the 
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
    $header->setType("Redstone");
    $header->setProject($self->{"PROJECT"});
    
    # HARD-CODED
	# The Id will be the prefix of the output file
    $header->setId("Redstone");
	# "Release Site Type/Site ID:" header line
    $header->setSite("Redstone Arsenal/74001");


    # ------------------------------------------------
    # Read through the file for additional header info
    # ------------------------------------------------
	
    my ($alt, $lat, $lon) = split(" ",$headerlines[0]); 
	
	$header->setLongitude($lon,$self->buildLatlonFormat($lon));
	$header->setLatitude($lat,$self->buildLatlonFormat($lat));
    $header->setAltitude($alt,"m"); 
	
	my @release = split(" ",$headerlines[1]);
	my $rel_hours = $release[7];
	my $rel_minutes = $release[8];
	my $rel_time = join "", $rel_hours, ' ', $rel_minutes, ' 00';
	print "Release time: $rel_time\n";

    # ------------------------------------------------------------------
    # Extract the date and time information from the file name
    # BEWARE: Expects filename to be similar to: 20120515_12UTC.txt
    # ------------------------------------------------------------------
    # print "file name = $filename\n"; 

    my $date;
	my $time;
	my $hour;
	my $min;
	my $snding;

	if ($filename =~ /(\d{4})(\d{2})(\d{2})_(\d{2})UTC/)
	{
		my ($yearInfo, $monthInfo, $dayInfo, $hourInfo) = ($1,$2,$3,$4);

		$hour = $hourInfo;
		$snding = join "", $monthInfo, $dayInfo;
		print "sounding $snding\n";
	    $date = join ", ", $yearInfo, $monthInfo, $dayInfo;
	    print "date: $date   ";
	    $time = join "", $hourInfo, ' ', '00', ' 00';
        print "time: $time\n";
	}

    $header->setActualRelease($date,"YYYY, MM, DD",$rel_time,"HH:MM:SS",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);

    # ------------------------------------------------------------------
	# Insert the correct sonde ID by date
	# IDs from sonde_ids.txt file
    # ------------------------------------------------------------------
	
    # HARD-CODED
    my %sonde_id = (
	    '0515' => 'H1314508',
		'0518' => 'H1314503',
		'0521' => 'H1314581',
		'0529' => 'H1314580',
		'0531' => 'H1314538',
		'0604' => 'H1314545',
		'0605' => 'H1314507',
		'0611' => 'H1314548',
		'0614' => 'H1314506',
		'0615' => 'H1314546',
	);
	
	my $sonde_type = "Vaisala RS92-SGPD";
	print $sonde_id{$snding};
	$header->setLine(5, "Sonde Id/Sonde Type:", join('/', $sonde_id{$snding},$sonde_type));
	
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
    
    # Strip blank lines from the raw data file
    # use the --skip cmd line option if this step has been performed already
	if (!$skip)
	{
		my $cmd_perl = "perl -pe 's/^\\s\+\$//'";
		my $cmd;

        #----------------------------------------------------------------------
        # Preprocess each file by stripping all blank lines. 
        # E.g., command to strip blank lines: perl -pe 's/^\s+$//' infile > outfile
        #-----------------------------------------------------------------------
        $cmd = sprintf "%s ../raw_data/%s > ../raw_data/%s.noBlanks", $cmd_perl, $file, $file;

        print "\nIssue the following command: $cmd\n";
        system $cmd;

        # Save the original input file in *.orig
        print "Executing: /bin/mv $file $file.orig \n";
        system "/bin/mv -f ../raw_data/$file ../raw_data/$file.orig";

        print "Executing: /bin/mv $file.noBlanks $file \n";
        system "/bin/mv -f ../raw_data/$file.noBlanks ../raw_data/$file";
    }

    printf("\nProcessing file: %s\n",$file);

    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
    # Generate the sounding header.
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
	foreach my $line (@lines) {
	    # Ignore the header lines.
	    if ($index < 3) { $index++; next; }
		# --------------------------------
        # Stop when you get to "MANDATORY  
		# LEVELS" or "SIGNIFICANT LEVELS"
		# --------------------------------
		if ($line =~ /LEVELS/)
		{
			last;
		}
	    
	    my @data = split(' ',$line);
	    my $record = ClassRecord->new($WARN,$file);

	    $record->setTime($recordTime);
	    $record->setPressure($data[1],"mb"); 
	    $record->setTemperature($data[2],"C");  
		$record->setDewPoint($data[4],"C") if ($data[4] !~ /99.9/);
	    $record->setRelativeHumidity($data[3]) if ($data[3] !~ /999/);
	    $record->setWindSpeed($data[7],"m/s");
	    $record->setWindDirection($data[6]);
		$record->setAltitude($data[0],"m");
		
		if ($surface)
		{
			# get the lat/lon data 
			$record->setLongitude($header->getLongitude(),$self->buildLatlonFormat($header->getLongitude()));
			$record->setLatitude($header->getLatitude(),$self->buildLatlonFormat($header->getLatitude()));
			$surface = 0;
		}
		$recordTime += 2;
	                                          
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
    my @files = grep(/^\d{8}_12UTC\.txt/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
	printf("Ready to read the files\n");
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
