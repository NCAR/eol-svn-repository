#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The PHX_SND_Converter.pl script is used for converting the sounding data
# from the Phoenix site from the raw Vaisala variant to the CLASS format.</p>
#
# @author Joel Clawson
# @version NAME_1.0 This was originally created for the NAME project.
##Module-------------------------------------------------------------------------
package PHX_SND_Converter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version2";
use ClassSounding;
use ElevatedStationMap;
use Station;

my ($WARN);

&main();

# A collection of functions that contain constants
sub getNetworkName { return "PHOENIX"; }
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
    my $converter = PHX_SND_Converter->new();
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
# @signature PHX_SND_Converter new()
# <p>Create a new PHX_SND_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = ElevatedStationMap->new();

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
        print($STN $station->toQCF_String()) if ($station->getBeginDate !~ /^9+$/);
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
	$self->readRawFile($file) if ($file =~ /\.txt$/);
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readData(FileHandle FILE, ClassSounding cls)
# <p>Read in the raw data from the file pointer and convert it to CLASS format
# by placing it in the ClassSounding object that will hold it.
#
# @input $FILE The file handle containing the data.
# @input $cls The ClassSounding that holds the data.
##------------------------------------------------------------------------------
sub readData {
    my $self = shift;
    my ($FILE,$cls) = @_;

    # Read blank lines before data.
    my $line = <$FILE>; while ($line =~ /^\s*$/) { $line = <$FILE>; }

    # Read in the data
    my $start = 1;
    while ($line && $line !~ /^\s*$/) {
	chomp($line);

	my @data = split(' ',$line);
	
	# Ignore incomplete lines
	return undef() if (scalar(@data) < 13);
	
	my $time = 60 * $data[0] + $data[1];

	# Put the data in the ClassSounding
	# First Ascension Rate should be missing, so only save if not start.
	$cls->setAscensionRate($time,$data[2],"m/s") if ($data[2] !~ /\/+/ && !$start);
	$cls->setAltitude($time,$data[3],"m") if ($data[3] !~ /\/+/);
	$cls->setPressure($time,$data[4],"hPa") if ($data[4] !~ /\/+/);
	$cls->setTemperature($time,$data[5],"C") if ($data[5] !~ /\/+/);
	$cls->setRelativeHumidity($time,$data[6]) if ($data[6] !~ /\/+/);
	$cls->setDewPoint($time,$data[7],"C") if ($data[7] !~ /\/+/);
	$cls->setWindDirection($time,$data[8]) if ($data[8] !~ /\/+/);
	$cls->setWindSpeed($time,$data[9],"m/s") if ($data[9] !~ /\/+/);

	# Set the first line of the latitude and longitude to the station data.
	if ($start) {
	    my $lat = $cls->getReleaseLatitude();
	    my $lon = $cls->getReleaseLongitude();

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	    
	    $cls->setLatitude($time,$lat,$lat_fmt);
	    $cls->setLongitude($time,$lon,$lon_fmt);

	    $start = 0;
	}

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
    my ($date,$time,$serial,$wind_prog);

    # Define the station for the StationList.
    my $station = Station->new();
    $station->setNetworkName($self->getNetworkName());
    $station->setReportingFrequency("6 hourly");
    $station->setStateCode("AZ");
    $station->setNetworkIdNumber(4);
    $station->setPlatformIdNumber(294);

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
	    
	    my ($lat_fmt,$lat_inc,$lat_mult);
	    if ($lat_unit =~ /N/i) {
		$lat_fmt = "";
		$lat_inc = 0;
		$lat_mult = 1;
	    } else {
		$lat_fmt = "-";
		$lat_inc = 1;
		$lat_mult = -1;
	    }
	    while (length($lat_fmt) < length($lat)+$lat_inc) { $lat_fmt .= "D"; }
	    $station->setLatitude($lat*$lat_mult,$lat_fmt);

	    my ($lon_fmt,$lon_inc,$lon_mult);
	    if ($lon_unit =~ /E/i) {
		$lon_fmt = "";
		$lon_inc = 0;
		$lon_mult = 1;
	    } else {
		$lon_fmt = "-";
		$lon_inc = 1;
		$lon_mult = -1;
	    }
	    while (length($lon_fmt) < length($lon)+$lon_inc) { $lon_fmt .= "D"; }
	    $station->setLongitude($lon*$lon_mult,$lon_fmt);
	    
	    $station->setElevation($elev,$elev_unit);
	} elsif ($line =~ /^station\s*:\s*(\d+)\s+(\S+)/i) {
	    $station->setStationId($1);
	    $station->setStationName($2);
	} elsif ($line =~ /started\s+at\s*:\s*(\d+)\s*([\D\S]+)\s*(\d+)\s+(\d+)\D(\d+)/i) {
	    $date = sprintf("%04d%02d%02d",2000+$3,$self->getMonth($2),$1);
	    $time = sprintf("%02d%02d",$4,$5);
	} elsif ($line =~ /^rs.+number\s*:\s*(\d+)/i) { $serial = $1; }


	$line = <$FILE>;

	return undef() if (!$line);
    }

    # Correction for NAME for the files without header information.
    if (getProjectName() =~ /NAME/) {
	if ($file =~ /04080106\.txt/ || $file =~ /04081806\.txt/) {
	    $station->setStationId(74626);
	    $station->setLatitude("33.45","DDDDD");
	    $station->setLongitude("-111.95","-DDDDDD");
	    $station->setElevation(379,"m");
	}
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
	$self->{"stations"}->addStation($station);
    }

    # Define the new CLASS formatted sounding.
    my $cls = ClassSounding->new($WARN,$station);
    $cls->setType("Phoenix Sounding");
    $cls->setProjectId($self->getProjectName());
    $cls->setId("PSR");
    $cls->setVariableParameter(1," Ele "," deg ");
    $cls->setVariableParameter(2," Azi "," deg ");

    $cls->setActualReleaseTime($date,"YYYYMMDD",$time,"HHMM",0)
	if (defined($date) && defined($time));
    $cls->setHeaderLine(6,"Radiosonde Serial Number",$serial) if (defined($serial));
    $cls->setHeaderLine(7,"Wind Finding Methodology",$wind_prog) if (defined($wind_prog));

    my $nominal = sprintf("20%02d/%02d/%02d",substr($file,0,2),substr($file,2,2),
			  substr($file,4,2));
    $cls->setNominalReleaseTime($nominal,"YYYY/MM/DD",
				sprintf("%02d00",substr($file,6,2)),"HHMM",0);
    $station->insertDate($nominal);

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

    my $cls = $self->readHeader($FILE,$file_name);
    $self->readData($FILE,$cls) if (defined($cls));

    close($FILE);

    if (defined($cls)) {
	$cls->saveToFile($self->getOutputDirectory());
    } else {
	printf($WARN "No data for file: %s.  Not creating output file.\n",$file_name);
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
