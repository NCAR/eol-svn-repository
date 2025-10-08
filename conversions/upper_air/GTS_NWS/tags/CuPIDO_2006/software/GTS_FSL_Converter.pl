#! /usr/bin/perl -w

##Module---------------------------------------------------------------------

##Module---------------------------------------------------------------------
package GTS_FSL;
use strict;
use lib "/net/work/software/conversion_modules/Version6";
use DpgDate qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::ElevatedStationMap;
use Station::Station;

my ($WARN);
&main();

sub getNetworkName { return "GTS"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "CuPIDO"; }
sub getRawDataDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_sounding_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##---------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script to convert the data.</p>
##---------------------------------------------------------------------------
sub main {
    my $converter = GTS_FSL->new();
    $converter->convert();
}

##---------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data into the CLASS format.</p>
##---------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir("../final") unless (-e "../final");

    open($WARN,">".getWarningFile()) or die("Cannot open warning file.\n");

    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##---------------------------------------------------------------------------
# @signature EOL_Dropsonde_Converter new()
# <p>Create a new converter object.</p>
#
# @output $self The new converter.
##---------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    $self->{"stations"} = Station::ElevatedStationMap->new();

    return $self;
}

sub parseMonth {
    my ($self,$month) = @_;

    if ($month =~ /jul/i) { return 7; }
    elsif ($month =~ /aug/i) { return 8; }
    else { die("Unknown month: $month\n"); }
}

sub printSounding {
    my ($self,$filename,$header,$records) = @_;

    my $station = $self->{"stations"}->getStation($header->getId(),getNetworkName(),$header->getLatitude(),$header->getLongitude(),$header->getAltitude());

    if (!defined($station)) {
	$station = Station::Station->new($header->getId(),getNetworkName());

	$station->setStationName(sprintf("%s (GTS)",$header->getId(),$header->getSite()));
#	$station->setStateCode(substr($station->getStationName(),length($station->getStationName())-2));
	
	my $lat = $header->getLatitude();
	my $lat_fmt = $lat < 0 ? "-" : "";
	while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	$station->setLatitude($lat,$lat_fmt);

	my $lon = $header->getLongitude();
	my $lon_fmt = $lon < 0 ? "-" : "";
	while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
	$station->setLongitude($lon,$lon_fmt);

	$station->setElevation($header->getAltitude(),"m");

	$station->setNetworkIdNumber(99);
	$station->setPlatformIdNumber(202);
	$station->setReportingFrequency("12 hourly");
	$station->setLatLongAccuracy(2);

	$self->{"stations"}->addStation($station);
    }
    
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    open(my $OUT,sprintf(">%s/%s",getOutputDirectory(),$filename)) or die("Can't write to $filename\n");

    print($OUT $header->toString());
    
    foreach my $record (@{$records}) {
	print($OUT $record->toString());
    }

    close($OUT);
}

##---------------------------------------------------------------------------
# @signature void printStationFiles()
# <p>Generate the stationCD.out file and the station summary log for the stations
# in the conversion.</p>
##---------------------------------------------------------------------------
sub printStationFiles {
    my $self = shift;
    my ($STN, $SUMMARY);

    open($STN, ">".$self->getStationFile()) || 
	die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->getSummaryFile()) || 
	die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

##---------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Read all of the raw data files and convert them.</p>
##---------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,getRawDataDirectory()) or die("Can't open raw data directory\n");
    my @files = grep(/\.txt$/,readdir($RAW));
    closedir($RAW);

    
    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",getRawDataDirectory(),$file)) or die("Can't open file: $file\n");
	
	printf("Processing: %s ...\n",$file);
	
	$self->readRawFile($FILE);
	
	close($FILE);
    }
}

##---------------------------------------------------------------------------
# @signature void readRawFile(FileHandle FILE)
# <p>Read the data in the file handle and print it to an output file.</p>
#
# @input $FILE The file handle holding the raw data.
##---------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my ($FILE) = @_;

    my ($header,$records,$windUnits,$filename);

    foreach my $line (<$FILE>) {
	my @data = split(' ',$line);
    
	if ($data[0] == 254) {
	    $self->printSounding($filename,$header,$records) if defined($header);

	    # Redefine the data holders for the new sounding.
	    $header = Sounding::ClassHeader->new($WARN);
	    $records = undef();
	    $windUnits = undef();
	    $filename = undef();

	    $header->setType("GTS Sounding");
	    $header->setProject($self->getProjectName());
	    $header->setNominalRelease(sprintf("%04d%02d%02d",$data[4],$self->parseMonth($data[3]),
					       $data[2]),"YYYYMMDD",sprintf("%02d",$data[1]),"HH",0);
	} elsif ($data[0] == 1) {
	  if ($data[3] =~ /([\d\.]+[NS])([\d\.]+[EW])/i) {
	    my $lat = $1;
	    my $lon = $2;
	    $line =~ s/$lat$lon/$lat $lon/;
	    @data = split(' ', $line);
	  }

	    my $lat = $data[3] =~ /N$/ ? substr($data[3],0,length($data[3])-1) : -1 * substr($data[3],0,length($data[3])-1);
	    my $lon = $data[4] =~ /E$/ ? substr($data[4],0,length($data[4])-1) : -1 * substr($data[4],0,length($data[4])-1);

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

	    $header->setSite($data[2]);
	    $header->setLatitude($lat,$lat_fmt);
	    $header->setLongitude($lon,$lon_fmt);
	    $header->setAltitude($data[5],"m");

	    if ($header->getNominalTime() =~ /^00:00/ && $data[6] > 2000 && $data[6] != 99999) {
		my ($date) = adjustDateTime($header->getNominalDate(),"YYYY, MM, DD","00:00","HH:MM",-1,0,0,0);
		$header->setActualRelease($date,"YYYY, MM, DD",sprintf("%04d",$data[6]),"HHMM",0);
	    }

	} elsif ($data[0] == 2) {
	} elsif ($data[0] == 3) {
	    $header->setId($data[1]);
	    if (defined($header->getSite())) {
	      $header->setSite(sprintf("%s %s", $header->getSite(), $header->getId()));
	    }
	    
	    if ($data[2] == 10) { $header->setLine("Sonde Type:","VIZ \"A\""); }
	    elsif ($data[2] == 11) { $header->setLine("Sonde Type:","VIZ \"B\""); }
	    elsif ($data[2] == 12) { $header->setLine("Sonde Type:","Space Data Corp."); }
	    elsif ($data[2] == 51) { $header->setLine("Sonde Type:","VIZ-B2 (USA)"); }
	    elsif ($data[2] == 52) { $header->setLine("Sonde Type:","Vaisala RS80-57H"); }
	    elsif ($data[2] == 99999) { }
	    else { die("Unknown sonde type: ".$data[2]."\n".$line."\n"); }

	    if ($data[3] eq "ms") { $windUnits = "ms"; }
	    elsif ($data[3] eq "kt") { $windUnits = "knot"; }
	    else { die("Unknown wind units: ".$data[3]."\n"); }
	} elsif (4 <= $data[0] && $data[0] <= 9) {
	    if (!defined($filename)) {
		$filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$header->getId(),
				    split(", ",$header->getActualDate()),
				    split(":",$header->getActualTime()));
	    }

	    my $record = Sounding::ClassRecord->new($WARN,$filename);

	    if ($data[0] == 9) {
		$record->setTime(0);

		my $lat = $header->getLatitude();
		my $lat_fmt = $lat < 0 ? "-" : "";
		while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
		$record->setLatitude($lat,$lat_fmt);

		my $lon = $header->getLongitude();
		my $lon_fmt = $lon < 0 ? "-" : "";
		while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }
		$record->setLongitude($lon,$lon_fmt);
	    }

	    $record->setPressure(trim($data[1])/10,"mbar") unless (trim($data[1]) == 99999);
	    $record->setAltitude(trim($data[2]),"m") unless (trim($data[2]) == 99999);
	    $record->setTemperature(trim($data[3])/10,"C") unless (trim($data[3]) == 99999);
	    $record->setDewPoint(trim($data[4])/10,"C") unless (trim($data[4]) == 99999);
	    $record->setWindDirection(trim($data[5])) unless (trim($data[5]) == 99999);
	    if ($windUnits eq "ms") {
		$record->setWindSpeed(trim($data[6])/10,"m/s") unless (trim($data[6]) == 99999);
	    } elsif ($windUnits eq "knot") {
		$record->setWindSpeed(trim($data[6]),"knot") unless (trim($data[6]) == 99999);
	    }

	    push(@{$records},$record);
	} else {
	    die("Unknown value ".$data[0]." in first column of data line!\n");
	}
    }

    $self->printSounding($filename,$header,$records);
}

##---------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove surrounding white space of a String.</p>
# 
# @input $line The String to trim.
# @output $line The trimmed line.
##---------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
