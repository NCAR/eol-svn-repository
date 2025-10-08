#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The Yuma_SND_Converter.pl script is used for converting the sounding data
# from the Yuma Proving Grounds sites from the raw Vaisala variant to the CLASS 
# format.</p>
#
# @author Joel Clawson
# @version NAME_1.0 This was originally created for the NAME project.
##Module-------------------------------------------------------------------------
package Yuma_SND_Converter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version3";
use DpgDate qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::ElevatedStationMap;
use Station::Station;
$| = 1;

my ($WARN);

&main();

# A collection of functions that contain constants
sub getNetworkName { return "YUMA"; }
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
    my $converter = Yuma_SND_Converter->new();
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

##---------------------------------------------------------------------------
# @signature ClassRecord create_record(String line, ClassHeader header, String filename, ClassRecord previous_record)
# <p>Create a new ClassRecord from the data line.</p>
#
# @input $line The raw data line.
# @input $header The ClassHeader for the file.
# @input $filename The name of the raw data file.
# @input $previous_record The record that occured in the raw file directory
# before this file.
# @output $record The new record from the parsed data line.
##---------------------------------------------------------------------------
sub create_record {
    my ($self,undef(),$header,$filename,$previous_record) = @_;
    my @data = split(' ',$_[1]);
    my $record = Sounding::ClassRecord->new($WARN,$filename,$previous_record);

    $record->setTime($data[0],$data[1]) unless ($data[0] =~ /^\/+$/ || $data[1] =~ /^\/+$/);
    $record->setAltitude($data[2],"m") unless ($data[2] =~ /^\/+$/);
    $record->setPressure($data[3],"hPa") unless ($data[3] =~ /^\/+$/);
    $record->setTemperature($data[4],"C") unless ($data[4] =~ /^\/+$/);
    $record->setRelativeHumidity($data[5]) unless ($data[5] =~ /^\/+$/);
    $record->setDewPoint($data[6],"C") unless ($data[6] =~ /^\/+$/);
    $record->setWindDirection($data[10]) unless ($data[10] =~ /^\/+$/);
    $record->setWindSpeed($data[11],"knot") unless($data[11] =~ /^\/+$/);

    # Set the latitutde and longitude for the first record.
    if (!defined($previous_record)) {
	my $lat = $header->getLatitude();
	my $lon = $header->getLongitude();

	my $lat_fmt = $lat < 0 ? "-" : "";
	while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }

	my $lon_fmt = $lon < 0 ? "-" : "";
	while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

	$record->setLatitude($lat,$lat_fmt);
	$record->setLongitude($lon,$lon_fmt);
    }

    return $record;
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
    elsif ($month =~ /OCT/i) { return 10; }
    else { die("Unknown month: $month\n"); }
}

##------------------------------------------------------------------------------
# @signature Yuma_SND_Converter new()
# <p>Create a new Yuma_SND_Converter instance.</p>
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

##---------------------------------------------------------------------------
# @signature ClassHeader read_header(FileHandle FILE, String filename)
# <p>Create the header portion of the class formatted file.</p>
#
# @input $FILE The file handle containing the raw data.
# @input $filename The name of the raw data file.
# @output $header The ClassHeader for the file.
##---------------------------------------------------------------------------
sub read_header {
    my ($self,$FILE,$filename) = @_;
    my $station = Station::Station->new();
    my $header = Sounding::ClassHeader->new($station);
    $header->setType("Yuma Soundings");
    $header->setProject($self->getProjectName());
    $station->setNetworkName($self->getProjectName());
    $station->setReportingFrequency("no set schedule");
    $station->setStateCode("AZ");
    $station->setNetworkIdNumber(15);
    $station->setPlatformIdNumber(90);
    $station->setLatLongAccuracy(2);

    # Set default values for the stations.
    if ($filename =~ /3555/) {
	$station->setStationId("3555");
	$station->setLatitude("32.87","DDDDD");
	$station->setLongitude("-114.33","-DDDDDD");
	$station->setElevation(131,"m");
    } elsif ($filename =~ /Tower\s*31/i) {
	$station->setStationId("T31");
	$station->setLatitude("32.86","DDDDD");
	$station->setLongitude("-114.03","-DDDDDD");
	$station->setElevation(231,"m");
    } elsif ($filename =~ /Tower\s*M/i) {
	$station->setStationId("TWR_M");
	$station->setLatitude("32.92","DDDDD");
	$station->setLongitude("-113.80","-DDDDDD");
	$station->setElevation(145,"m");
    }

    # Parse out the header lines.
    my $line = <$FILE>;
    while ($line !~ /^\s*min\s+s\s+m/) {

	if ($line =~ /^\s*station\s*:\s*(\S+)\s+(\S+)\s*$/i) {
	    $station->setStationName(sprintf("%s Yuma Proving Grounds",$1));
	    my $id = trim($2);$id =~ s/\s+/_/g;
	    $station->setStationId($id);
	} elsif ($line =~ /^\s*location\s*:\s*([\d\.]+)\s*([NS])\s*([\d\.]+)\s*([EW])\s*([\d\.]+)\s*([a-z]+)\s*$/i) {
	    my $lat = $2 eq "N" ? $1 : -1 * $1;
	    my $lon = $4 eq "E" ? $3 : -1 * $3;

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

	    $station->setLatitude($lat,$lat_fmt);
	    $station->setLongitude($lon,$lon_fmt);
	    $station->setElevation($5,lc($6));
	} elsif ($line =~ /^\s*started at\s*:\s*(\d+)\s*([\D\S]+)\s*(\d+)\s+(\d+):(\d+)/i) {
	    $header->setActualRelease(sprintf("%02d%02d20%02d",$1,$self->getMonth($2),$3),"DDMMYYYY",
				      sprintf("%02d:%02d",$4,$5),"HH:MM",0);
	} elsif ($line =~ /^\s*sounding program rev .+ using ([\S]+)\s*$/i) {
	    $header->setLine("Wind Finding Methodology:",$1);
	} elsif ($line =~ /^\s*rs\-number\s*:\s*(\d+)\s*$/i) {
	    $header->setLine("Radiosonde Serial Number:",$1);
	} elsif ($line =~ /^\s*$/) {
	} elsif ($line =~ /^\s*loran\-c chain \d/i) {
	} elsif ($line =~ /^\s*phase fitting/i) {
	} elsif ($line =~ /^\s*ground check/i) {
	} elsif ($line =~ /^\s*pressure\s*:/i) {
	} elsif ($line =~ /^\s*temperature\s*:/i) {
	} elsif ($line =~ /^\s*humidity\s*:/i) {
	} elsif ($line =~ /^\s*time hgt\/msl pressure/i) {
	} elsif ($line =~ /start up date/i) {
	} elsif ($line =~ /^\s*system test/i) {
	} elsif ($line =~ /^\s*w12\-signal strength/i) {
	} elsif ($line =~ /^\s*w10\-receiver/i) {
	} elsif ($line =~ /^\s*continued/i) {
	} elsif ($line =~ /^\s*unit\s*:/i) {
	} elsif ($line =~ /^\s*\d+\s+\d+\s+/) {
	    printf($WARN "%s:  Data was found without header information.\n",$filename);
	    return undef();
	} else { die("Unknown header line:  $line\n"); }

	$line = <$FILE>;
    }

    # Get the station if it already was created.
    if ($self->{"stations"}->hasStation($station->getStationId(),$station->getNetworkName(),
					$station->getLatitude(),$station->getLongitude(),$station->getElevation())) {
	my $desc = $station->getStationName();
	$station = $self->{"stations"}->getStation($station->getStationId(),$station->getNetworkName(),
						   $station->getLatitude(),$station->getLongitude(),
						   $station->getElevation());
	$station->setStationName($desc) if ($station->getStationName() eq "Description");
    } else {
	$self->{"stations"}->addStation($station);
    }
    
    # Set nominal times.
    my $hour = substr($header->getActualTime(),0,2);
    my ($date,$time) = adjustDateTime($header->getActualDate(),"YYYY, MM, DD",
				      $header->getActualTime(),"HH:MM:SS",
				      0,$hour % 3 == 0 ? 0 : 3 - ($hour % 3),
				      -1 * substr($header->getActualTime(),3,2),0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM:SS",0);
    $station->insertDate($date,"YYYY, MM, DD");
    
    return $header;
}

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,getRawDirectory()) or die("Cannot read raw data directory.\n");
    my @files = ();
    foreach my $file (sort(readdir($RAW))) { 
	push(@files,sprintf("%s/%s",getRawDirectory(),$file));
    }
    closedir($RAW);
    
    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    while (scalar(@files) > 0) {
	my $file = shift(@files);

	if (-d $file && $file !~ /\.+$/) {
	    opendir(my $RAW,$file) or die("Cannot read directory $file\n");
	    foreach my $new (sort(readdir($RAW))) {
		push(@files,sprintf("%s/%s",$file,$new));
	    }
	    closedir($RAW);
	} elsif ($file =~ /\.CAP$/) {
	    $self->readRawFile($file);
	}
    }

    close($WARN);
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

    my $header = $self->read_header($FILE,$file_name);

    return if (!defined($header));

    my $outfile = sprintf("%s/%s_%04d%02d%02d%02d%02d%02d.cls",$self->getOutputDirectory(),
			  $header->getId(),split(", ",$header->getActualDate()),
			  split(":",$header->getActualTime()));

    open(my $OUT,sprintf(">%s",$outfile)) or die("Cannot open $outfile\n");
    print($OUT $header->toString());
    
    my $previous_record;
    foreach my $line (<$FILE>) {
	chomp($line);
	$line =~ s/\cZ//g;

	if ($line =~ /^\s*$/) { next; }
	elsif ($line =~ /^\s*\d+\s+\d+\s+/) {
	    my $record = $self->create_record($line,$header,$file_name,$previous_record) ;
	    print($OUT $record->toString()) if defined($record);
	    $previous_record = $record;
	} else {
	    last;
	}
    }

    close($OUT);
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


