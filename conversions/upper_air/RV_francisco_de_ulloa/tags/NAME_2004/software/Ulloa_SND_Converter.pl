#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The Ulloa_SND_Converter.pl script is used for converting the sounding data
# from the Francisco Ulloa Ship from the raw Vaisala variant to the CLASS format.</p>
#
# @author Joel Clawson
# @version NAME_1.0 This was originally created for the NAME project.
##Module-------------------------------------------------------------------------
package Ulloa_SND_Converter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version3";
use DpgDate qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::SimpleStationMap;
use Station::Station;

my ($WARN);

&main();

# A collection of functions that contain constants
sub getNetworkName { return "Ulloa"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "NAME"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = Ulloa_SND_Converter->new();
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

    $self->readRawDataFiles();
    $self->printStationFiles();
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
    elsif ($month =~ /AUG/i) { return 8; }
    elsif ($month =~ /SEP/i) { return 9; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature Ulloa_SND_Converter new()
# <p>Create a new Ulloa_SND_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = Station::SimpleStationMap->new();

    return $self;
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

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    foreach my $file (sort(@files)) {
	$self->readRawFile($file) if ($file =~ /\.CAP$/);
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readData(FileHandle IN, FileHandle OUT, String infile, ClassHeader header)
# <p>Read in the raw data from the file pointer and convert it to CLASS format
# by placing it in the ClassSounding object that will hold it.
#
# @input $IN The file handle containing the data.
# @input $infile The name of the input file
# @input $header The ClassHeader containing the header data.
##------------------------------------------------------------------------------
sub readData {
    my $self = shift;
    my ($IN,$OUT,$infile,$header) = @_;

    # Read blank lines before data.
    my $line = <$IN>; while ($line =~ /^\s*$/) { $line = <$IN>; }

    my $previous_record;
    my $descension_count = 0;
    
    # Read in the data
    my $start = 1;
    while ($line && $line !~ /^\s*$/ && $line !~ /[a-z]/i) {
	chomp($line);

	my @data = split(' ',$line);
	
	# Ignore incomplete lines
	if (scalar(@data) < 11) {
	    $line = <$IN>;
	    next;
	}
	
	my $cls = Sounding::ClassRecord->new($WARN,$infile,$previous_record);

	my $time = 60 * $data[0] + $data[1];

	# Put the data in the ClassSounding
	$cls->setTime($data[0],$data[1]);

	# First Ascension Rate should be missing, so only save if not start.
	$cls->setAscensionRate($data[2],"m/s") if ($data[2] !~ /\/+/ && !$start);
	$cls->setAltitude($data[3],"m") if ($data[3] !~ /\/+/);
	$cls->setPressure($data[4],"hPa") if ($data[4] !~ /\/+/);
	$cls->setTemperature($data[5],"C") if ($data[5] !~ /\/+/);
	$cls->setRelativeHumidity($data[6]) if ($data[6] !~ /\/+/);
	$cls->setDewPoint($data[7],"C") if ($data[7] !~ /\/+/);
	$cls->setWindDirection($data[8]) if ($data[8] !~ /\/+/);
	$cls->setWindSpeed($data[9],"m/s") if ($data[9] !~ /\/+/);

	# Set the first line of the latitude and longitude to the station data.
	if ($start) {
	    my $lat = $header->getLatitude();
	    my $lon = $header->getLongitude();

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    
	    $cls->setLatitude($lat,$lat_fmt);
	    $cls->setLongitude($lon,$lon_fmt);

	    $start = 0;
	}

	$previous_record = $cls;
	
	if (!($data[4] =~ /^\/+$/ && $data[5] =~ /^\/+$/ &&
	      $data[6] =~ /^\/+$/ && $data[7] =~ /^\/+$/ &&
	      $data[3] !~ /^\/+$/ && $data[3] < 0)) {
	    $descension_count = $cls->getAscensionRate() < 0 ? $descension_count + 1 : 0
		if ($cls->getAltitude() >= 0);
	    if ($descension_count >= 5) { last; }
	}

	print($OUT $cls->toString());

	$line = <$IN>;
    }
}

##------------------------------------------------------------------------------
# @signature ClassHeader readHeader(FileHandle FILE, String file)
# <p>Read in the header information from the file handle.</p>
#
# @input $FILE The FileHandle containing the data to be read.
# @input $file The name of the file being read.
# @output $cls The ClassHeader holding the header data.
##------------------------------------------------------------------------------
sub readHeader {
    my $self = shift;
    my ($FILE,$file) = @_;
    my ($date,$time,$serial,$wind_prog);

    # Define the station for the StationList.
    my $station = Station::Station->new();
    $station->setNetworkName($self->getNetworkName());
    $station->setReportingFrequency("6 hourly");
    $station->setStateCode("XX");
    $station->setCountry("MX");
    $station->setNetworkIdNumber(15);
    $station->setPlatformIdNumber(314);
    $station->setMobilityFlag("m");

    # Define the new CLASS formatted sounding.
    my $cls = Sounding::ClassHeader->new($station);
    $cls->setType("CICESE Sounding");
    $cls->setProject($self->getProjectName());
    $cls->setVariableParameter(1,"Ele","deg");
    $cls->setVariableParameter(2,"Azi","deg");

    my $line = <$FILE>;
    # Loop until the last line of the header is reached.
    while ($line !~ /^\s*min\s+s\s+m\/s\s+m/) {
	chomp($line);
	$line = trim($line);
	
	# These are to be put into the class header.
	if ($line =~ /^sounding\s+program.+using\s+(.+)\s*$/i) {
	    $wind_prog = $1 if (defined($1));
	} elsif ($line =~ /^location\s*:\s*([\d\.]+)\s*([NS])\s+([\d\.]+)\s*([EW])\s+([\d\.]+)\s*(\S+)/i) {

	    # Get the values from the matching.
	    my ($lat,$lat_unit,$lon,$lon_unit,$elev,$elev_unit) =
		($1,uc($2),$3,uc($4),$5,lc($6));
	    
	    my ($lat_fmt);
	    if ($lat_unit =~ /N/i) {
		$lat_fmt = "";
	    } else {
		$lat_fmt = "-";
		$lat *= -1;
	    }
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    $cls->setLatitude($lat,$lat_fmt);

	    my ($lon_fmt);
	    if ($lon_unit =~ /E/i) {
		$lon_fmt = "";
	    } else {
		$lon_fmt = "-";
		$lon *= -1;
	    }
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    $cls->setLongitude($lon,$lon_fmt);
	    
	    $cls->setAltitude($elev,$elev_unit);
	} elsif ($line =~ /^ship\s*:\s*(\S+)\s*(\S+)/i) {
	    $station->setStationId($2);
	    $station->setStationName(sprintf("%s Ship Francisco de Ulloa",$1));
	} elsif ($line =~ /started\s+at\s*:\s*(\d+)\s*([\D\S]+)\s*(\d+)\s+(\d+)\D(\d+)/i) {
	    $date = sprintf("%04d%02d%02d",2000+$3,$self->getMonth($2),$1);
	    $time = sprintf("%02d%02d",$4,$5);
	} elsif ($line =~ /^rs.+number\s*:\s*(\d+)/i) { $serial = $1; }


	$line = <$FILE>;

	return undef() if (!$line);
    }

    # Get the station if it has already been created.
    if ($self->{"stations"}->hasStation($station->getStationId(),
					$station->getNetworkName())) {
	$station = $self->{"stations"}->getStation($station->getStationId(),
						   $station->getNetworkName());
    } else {
	$self->{"stations"}->addStation($station);
    }

    $cls->setActualRelease($date,"YYYYMMDD",$time,"HHMM",0)
	if (defined($date) && defined($time));
    $cls->setLine("Radiosonde Serial Number:",$serial) if (defined($serial));
    $cls->setLine("Wind Finding Methodology:",$wind_prog) if (defined($wind_prog));

    my @nominal_time = split(/:/,$cls->getActualTime());
    my ($nom_date,$nom_time) = adjustDateTime($cls->getActualDate(),"YYYY, MM, DD",
					      $cls->getActualTime(),"HH:MM:SS",
					      0,3 - ($nominal_time[0] % 3),
					      -1 * $nominal_time[1],
					      -1 * $nominal_time[2]);

    $cls->setNominalRelease($nom_date,"YYYY, MM, DD",$nom_time,"HH:MM:SS",0);

    $station->insertDate($nom_date,"YYYY, MM, DD");

    return $cls;
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
			       split(/, /,$header->getActualDate()),
			       split(/:/,$header->getActualTime()));

	open(my $OUT,sprintf(">%s/%s",getOutputDirectory(),$filename)) or 
	    die("Cannot open output file\n");

	print($OUT $header->toString());
	$self->readData($FILE,$OUT,$file_name,$header);
	close($OUT);
    }
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
