#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The SanJose_SND_Converter.pl script is used for converting the sounding data
# from the San Jose site in Costa Rica from the raw Vaisala variant to the 
# CLASS format.</p>
#
# @author Joel Clawson
# @version NAME_1.0 This was originally created for the NAME project.
##Module-------------------------------------------------------------------------
package SanJose_SND_Converter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version3";
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::ElevatedStationMap;
use Station::Station;

my ($WARN);
*STDERR = *STDOUT;

&main();

# A collection of functions that contain constants
sub getNetworkName { return "SANJOSE"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "NAME"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getStationList { return "../docs/station.list"; }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = SanJose_SND_Converter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir("../final") unless (-e "../final");

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");
    $self->{"warn"} = $WARN;

    $self->printGeneralWarnings();

    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void correctHeader(ClassHeader header)
# <p>Make corrections to the header based on the project.</p>
#
# @param header The header to be corrected.
##------------------------------------------------------------------------------
sub correctHeader {
    my ($self,$header) = @_;

    if (getProjectName() =~ /NAME/) {
	if (($header->getNominalDate() =~ /2004, 07, 03/ && $header->getNominalTime() =~ /18:00:00/) ||
	    ($header->getNominalDate() =~ /2004, 07, 04/ && $header->getNominalTime() =~ /00:00:00/) ||
	    ($header->getNominalDate() =~ /2004, 07, 05/ && $header->getNominalTime() =~ /12:00:00/)) {

	    printf({$self->{"warn"}} "Changing actual date/time from %s %s to %s %s for NAME.\n",
		   $header->getActualDate(),$header->getActualTime(),
		   $header->getNominalDate(),$header->getNominalTime());

	    $header->setActualRelease($header->getNominalDate(),"YYYY, MM, DD",
				      $header->getNominalTime(),"HH:MM:SS",0);
	}
    }
}

##------------------------------------------------------------------------------
# @signature int getMonth(String abbr)
# <p>Get the number of the month from an abbreviation.</p>
#
# @input $abbr The month abbreviation.
# @output $mon The number of the month.
# @warning This function will die if the abbreviation is not known.
##------------------------------------------------------------------------------
sub getMonth {
    my $self = shift;
    my $month = shift;

    if ($month =~ /JUN/i) { return 6; }
    elsif ($month =~ /JUL/i) { return 7; }
    elsif ($month =~ /AUG|agosto/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature SanJose_SND_Converter new()
# <p>Create a new SanJose_SND_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = Station::ElevatedStationMap->new();

    return $self;
}

##---------------------------------------------------------------------------
# @signature void printGeneralWarnings()
# <p>Print general warning messages about the conversion.</p>
##---------------------------------------------------------------------------
sub printGeneralWarnings {
    my ($self) = @_;

    if (getProjectName() =~ /NAME/) {
	printf({$self->{"warn"}} "The actual date/time for files MROC_200407031800.cls, MROC_200407040000.cls, and MROC_200407051200.cls are being set to their nominal date/time.\n");
    }
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);

    open($STN, ">".$self->getStationFile()) || die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->getRawDirectory()) or die("Cannot open raw directory.\n");
    my @files = readdir($RAW);
    closedir($RAW);

    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.txt$/i);
#	$self->readRawFile($file) if ($file =~ /^18060418.*.txt$/i);
    }
}

##------------------------------------------------------------------------------
# @signature void readData(FileHandle FILE)
# <p>Read in the raw data from the file pointer and convert it to CLASS format
# by placing it in the ClassSounding object that will hold it.
#
# @input $FILE The file handle containing the data.
##------------------------------------------------------------------------------
sub readData {
    my $self = shift;
    my ($FILE,$OUT,$filename,$header) = @_;

    # Read blank lines before data.
    my $line = <$FILE>; while ($line =~ /^\s*$/) { $line = <$FILE>; }

    # Read in the data
    my $start = 1;
    my $previous_record;
    my $line_length = 0;
    while ($line) {
	chomp($line);

#	printf("%s\n",$line);

	# Ignore blank lines.
	if ($line =~ /^\s*$/) { $line = <$FILE>; next; }

	$line_length = length($line) 
	    if ($line_length == 0 && length($line) > 60 && length($line) < 63);
	
	# Handle the case where a second set of data is found in the file.
	if ($line =~ /^\s+Time\s+AscRate/) {
	    printf($WARN "%s: Found another set of data.  Using the second set.\n",
		   $filename);
	    <$FILE>; # Remove unit line
	    $line = <$FILE>;
	    
	    close($OUT);
	    open($OUT,sprintf(">%s/%s",getOutputDirectory(),$filename)) or 
		die("Cannot reopen $filename\n");
	    $start = 1;
	    $previous_record = undef();
	    next;
	}

	my @data = split(' ',$line);
    

	# Ignore incomplete lines
	if (length($line) != $line_length || scalar(@data) != 10) {

#	    printf("HERE!!!\n");

	    if ($line =~ /^(\s*[a-zA-Z0-9]{4,6})+\s*$/) {
#		printf("%s\n",$line);
		last;
	    }


	    printf($WARN "%s: Bad data line at %s %s.\n",$filename,$data[0],$data[1]);
	    $line = <$FILE>;
	    next;
	}

	if (getProjectName() =~ /NAME/) {
	    # Screwed up time
	    if (defined($previous_record) && $previous_record->getTime() == 1514 && 
		$header->getNominalDate() =~ /2004\/06\/30/ &&
		$header->getNominalTime() =~ /12:00/) {
		$data[1] = 16;
		printf($WARN "%s: Changing the second instance of 2004/06/30 12:00 1514 to 1516.\n",$filename);
	    }
	    
	    # Repeated Times, Keep first instance
	    if (defined($previous_record) &&
		$previous_record->getTime() >= (60 * $data[0] + $data[1]) &&
		$header->getNominalDate() =~ /2004\/06\/23/ &&
		$header->getNominalTime() =~ /12:00/) {

		printf($WARN "%s: Ignoring duplicate instance of time %d.\n",
		       $filename,60 * $data[0] + $data[1]);

		$line = <$FILE>;
		next;
	    }

	    if (defined($previous_record) &&
		$previous_record->getTime() == 60 * $data[0] + $data[1] &&
		$header->getNominalDate() =~ /2004\/07\/24/ &&
		$header->getNominalTime() =~ /18:00/) {

		printf($WARN "%s: Ignoring duplicate instance of time %d.\n",
		       $filename,$previous_record->getTime());

		$line = <$FILE>;
		next;
	    }
	}

	my $record = Sounding::ClassRecord->new($WARN,$filename,$previous_record);
	$record->setTime($data[0],$data[1]);
	$record->setAscensionRate($data[2],"m/s") if ($data[2] !~ /\/+/ && !$start);
	$record->setAltitude($data[3],"m") if ($data[3] !~ /\/+/);
	$record->setPressure($data[4],"hPa") if ($data[4] !~ /\/+/);
	$record->setTemperature($data[5],"C") if ($data[5] !~ /\/+/);
	$record->setRelativeHumidity($data[6]) if ($data[6] !~ /\/+/);
	$record->setDewPoint($data[7],"C") if ($data[7] !~ /\/+/);
	$record->setWindDirection($data[8]) if ($data[8] !~ /\/+/);
	$record->setWindSpeed($data[9],"m/s") if ($data[9] !~ /\/+/);


	# Set the first line of the latitude and longitude to the station data.
	if ($start) {
	    my $lat = $header->getLatitude();
	    my $lon = $header->getLongitude();

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    
	    $record->setLatitude($lat,$lat_fmt);
	    $record->setLongitude($lon,$lon_fmt);

	    $start = 0;

	    $self->correctHeader($header);
	    print($OUT $header->toString());
	}

	print($OUT $record->toString());
	$previous_record = $record;
	$line = <$FILE>;
    }
}

##------------------------------------------------------------------------------
# @signature ClassSounding readHeader(FileHandle FILE, String file)
# <p>Read in the header information from the file handle.</p>
#
# @input $FILE The FileHandle containing the data to be read.
# @input $file The name of the file being read.
# @output $cls The ClassSounding holding the header data.
##------------------------------------------------------------------------------
sub readHeader {
    my $self = shift;
    my ($FILE,$file) = @_;

    # Define the station for the StationList.
    my $station = Station::Station->new();
    $station->setNetworkName($self->getNetworkName());
    $station->setReportingFrequency("6 hourly");
    $station->setCountry("CS");
    $station->setStateCode("XX");
    $station->setNetworkIdNumber(15);
    $station->setPlatformIdNumber(310);

    my $header = Sounding::ClassHeader->new($station);

    my $line = <$FILE>;
    # Loop until the last line of the header is reached.
    while ($line !~ /^\s*min\s+s\s+m\/s\s+m/) {
	chomp($line);
	$line = trim($line);
	
	# These are to be put into the class header.
	if ($line =~ /^sounding\s*program.+using\s+(.+)\s*$/i) {
	    $header->setLine("Wind Finding Methodology:",$1) if (defined($1));
	} elsif ($line =~ /^location\s*[:\.]?\s*([\d\.:]+)\s*([NS])\s+([\d\.:]+)\s*([EW])\s+([\d\.]+)\s*(\S+)/i) {


	    # Get the values from the matching.
	    my ($lat,$lat_unit,$lon,$lon_unit,$elev,$elev_unit) =
		($1,uc($2),$3,uc($4),$5,lc($6));
	    $lat =~ s/:/./g;
	    $lon =~ s/:/./g;
	    $elev_unit = "m" if ($elev_unit eq "mts");
	    
	    my ($lat_fmt,$lat_inc,$lat_mult);
	    if ($lat_unit =~ /N/i) {
		$lat_fmt = "";
		$lat_mult = 1;
	    } else {
		$lat_fmt = "-";
		$lat_mult = -1;
	    }
	    $lat *= $lat_mult;
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    $station->setLatitude($lat,$lat_fmt);



	    my ($lon_fmt,$lon_inc,$lon_mult);
	    if ($lon_unit =~ /E/i) {
		$lon_fmt = "";
		$lon_mult = 1;
	    } else {
		$lon_fmt = "-";
		$lon_mult = -1;
	    }
	    $lon *= $lon_mult;
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    $station->setLongitude($lon,$lon_fmt);
	    
	    $station->setElevation($elev,$elev_unit);
	} elsif ($line =~ /^station\s*:?\s*(.+)/i) {
	    $station->setStationId("MROC");
	    $station->setStationName(trim($1));
	} elsif ($line =~ /started\s+at\s*[:\.]?\s*(\d+)\s*([\D\S]+)\s*(\d+)?\s+(\d+)\D(\d+)/i) {
	    my $year = defined($3) ? $3 : getProjectName() eq "NAME" ? 4 : 99;
	    my $date = sprintf("%04d%02d%02d",2000+$year,$self->getMonth($2),$1);
	    my $time = sprintf("%02d%02d",$4,$5);
	    $header->setActualRelease($date,"YYYYMMDD",$time,"HHMM",0);
	} elsif ($line =~ /start\s+up(\s+date)?\s*:?\s*(\d+)\s+([\D\S]+)\s*(\d+)?\s+(\d+)\D(\d+)/i) {
	    my $year = defined($4) ? $4 : getProjectName() eq "NAME" ? 4 : 99;
	    my $date = sprintf("%04d%02d%02d",2000+$year,$self->getMonth($3),$2);
	    my $time = sprintf("%02d%02d",$5,$6);
	    $header->setActualRelease($date,"YYYYMMDD",$time,"HHMM",0);	    
	} elsif ($line =~ /^\s*(\d+)\s+de\s+(\w+)\s+del\s+(\d+)/i) {
	    my $date = sprintf("%04d%02d%02d",$3,$self->getMonth($2),$1);
	    $header->setActualRelease($date,"YYYYMMDD",$header->getActualTime(),
				      "HH:MM:SS",0);
	} elsif ($line =~ /^\s*sondeo\s+.*(\d+)z/i) {
	    my $time = sprintf("%02d00",$1);
	    $header->setActualRelease($header->getActualDate(),"YYYY, MM, DD",$time,"HHMM",0);
	} elsif ($line =~ /^(rs.+)?numb?er\s*(at)?\s*:?\s*(.?\d+)/i) {
	    $header->setLine("Radiosonde Serial Number:",$3);
	} elsif ($line =~ /radiosond[ea]\s+model\s*:?\s*([\w\-]+)/i || 
		 $line =~ /(rs\s*\d+.*)/i) {
	    $header->setLine("Radiosonde Model:",$1);
	} elsif ($line =~ /soundin?g\s*:?\s*(\d+)/i) {
	    $header->setLine("Ascension No:",$1);
	} elsif ($line =~ /^\s*$/ || $line =~ /Time AscRate/ ||
		 $line =~ /system\s*test/i || $line =~ /ground\s*check/i ||
		 $line =~ /(pressure|temperature|humidity)/i ||
		 $line =~ /(\d+\s+)+/ || $line =~ /signal\s+strength/i ||
		 $line =~ /continued/i || $line =~ /\.txt/i || $line =~ /\d+z/i) {
	} elsif ($line =~ /datos perdidos por cortes de corriente/i) {
	    $header->setLine("System Notes:","Data missing from power failure.");
	} elsif ($line =~ /^\s*(([a-zA-Z0-9]{4,6})+\s*)+\s*$/) {
	    printf($WARN "%s: No header terminator (unit line) was found.  No output will be created.\n",$file);
	    return undef();
	} elsif ($line =~ /(\d+)\/(\d+)\/(\d+)/) {
	    $header->setActualRelease(sprintf("%04d/%02d/%02d",$3,$2,$1),"YYYY/MM/DD",
				      sprintf("%02d:00",substr($file,6,2)),"HH:MM",0);
	} else {
	    printf("Header line: %s not recognized\n",$line);
	    die();
	}
    

	$line = <$FILE>;

	return undef() if (!$line);
    }

    $station->setStationId("MROC") if ($station->getStationId() =~ /Station/);

    if ($station->getLatitude() =~ /-9+\.9+/) {
	$station->setLatitude("10.00","DDDDD");
	$station->setLongitude("-84.21","-DDDDD");
	$station->setElevation(921,"m");
    }

    # Get the station if it has already been created.
    if ($self->{"stations"}->hasStation($station->getStationId(),
					$station->getNetworkName(),
					$station->getLatitude(),
					$station->getLongitude(),
					$station->getElevation())) {
	$station = $self->{"stations"}->getStation($station->getStationId(),
						   $station->getNetworkName(),
						   $station->getLatitude(),
						   $station->getLongitude(),
						   $station->getElevation());
    } else {
	$station->setStationName(sprintf("%s San Jose, Costa Rica",
					 $station->getStationName()));
	$self->{"stations"}->addStation($station);
    }

    # Define the new CLASS formatted sounding.
    $header->setType("San Jose Sounding");
    $header->setProject($self->getProjectName());


    my $nominal = sprintf("20%02d/%02d/%02d",substr($file,4,2),substr($file,2,2),
			  substr($file,0,2));
    $header->setNominalRelease($nominal,"YYYY/MM/DD",
			       sprintf("%02d00",substr($file,6,2)),"HHMM",0);
    $station->insertDate($nominal,"YYYY/MM/DD");

    return $header;
}

##------------------------------------------------------------------------------
# @signature void readRawFile(String file_name)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file_name The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my $file_name = shift;
    my $file = sprintf("%s/%s",$self->getRawDirectory(),$file_name);

    printf("Processing file: %s\n",$file_name);

    open(my $FILE,$file) or die("Cannot open file: $file\n");

    my $header = $self->readHeader($FILE,$file_name);

    if (defined($header)) {

	my $filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$header->getId(),
			       split(/, /,$header->getNominalDate()),
			       split(/:/,$header->getNominalTime()));


	open(my $OUT, sprintf(">%s/%s",getOutputDirectory(),$filename)) or
	    die("Cannot open $filename\n");
	
	$self->readData($FILE,$OUT,$filename,$header);
	
	close($OUT);
#	die();
    } else {
	printf($WARN "No header data for file: %s.  Not creating output file.\n",$file);
    }
	

    close($FILE);
}

##------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the leading and trailing whitespace around a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed line.
##------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
