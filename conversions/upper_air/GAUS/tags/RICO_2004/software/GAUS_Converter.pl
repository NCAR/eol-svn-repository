#! /usr/bin/perl -w

package GAUS_Converter;
use strict;
use lib "/work/software/RICO/library/conversion_modules/Version4";
use DpgCalculations qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use Sounding::ClassConstants qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::ElevatedStationMap;
use Station::Station;
use Time::Local;
$| = 1;

my ($WARN);

&main();

# A collection of functions that contain constants
sub getFinalDirectory { return "../final"; }
sub getNetworkName { return "GAUS"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "RICO"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("%s/%s_%s_stationCD.out",getFinalDirectory(),
				    getNetworkName(),getProjectName()); }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = GAUS_Converter->new();
    $converter->convert();
}

sub calc_time {
    my ($self,$date,$time) = @_;

    my $gmt = timegm(substr($time,4),substr($time,2,2),substr($time,0,2),
		     substr($date,4,2),substr($date,2,2) - 1,"20".substr($date,0,2)-1900);
    
    if (!defined($self->{"starttime"})) {
	$self->{"starttime"} = $gmt;
    }

    return $gmt - $self->{"starttime"};
}

##------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert all of the raw data and create the output files for the conversion.</p>
##------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-e getOutputDirectory());
    mkdir(getFinalDirectory()) unless (-e getFinalDirectory());

    $self->readRawDataFiles();
}

sub create_header {
    my ($self,@lines) = @_;

    my $header = Sounding::ClassHeader->new($WARN);
    $header->setId("EOL_GAUS");
    $header->setSite("EOL_GAUS");

    @lines = grep(/GAUS\-T01 COM/,@lines);

    foreach my $line (@lines) {
	if ($line =~ /Data Type\/Data Channel:\s+(.+)$/i) {
	    $header->setType(trim($1));
	} elsif ($line =~ /Project Name\/Mission ID:\s+(.+)$/i) {
	    $header->setProject(trim($1));
	} elsif ($line =~ /Launch Time \([^\)]+\):\s*(\d{4}\/\d{2}\/\d{2}), (\d{2}:\d{2}:\d{2})/) {
	    $header->setActualRelease($1,"YYYY/MM/DD",$2,"HH:MM:SS",0,0);
	    my $offset = 0;
	    if ((substr($2,0,2) % 3) == 1) {
		$offset = 2;
	    } elsif ((substr($2,0,2) % 3) == 2) {
		$offset = 1;
	    }
	    $header->setNominalRelease($1,"YYYY/MM/DD",substr($2,0,2),"HH",$offset,0);
	} elsif ($line =~ /Sonde ID[^:]+:\s+(.+)$/) {
	    $header->setLine("Sonde Type/ID/Sensor ID/Tx Freq:",trim($1));
	} elsif ($line =~ /Pre\-launch Obs Data System\/Time:\s+(.+)$/) {
	    $header->setLine("Pre-launch Obs Data System/time:",trim($1));
	} elsif ($line =~ /Pre\-launch Obs \(p,t,d,h\):\s+(.+)$/) {
	    $header->setLine("Pre-launch Obs (p,t,d,h):",trim($1));
	} elsif ($line =~ /Pre\-launch Obs \(wd,ws\):\s+(.+)$/) {
	    $header->setLine("Pre-launch Obs (wd,ws):",trim($1));
	} elsif ($line =~ /Pre\-launch Obs \(lon,lat,alt\):\s+([\-\d\.]+) deg,\s+([\-\d\.]+) deg,\s+([\-\d\.]+) m/) {
	    $header->setLongitude(sprintf("%08.3f",$1),$1 < 0 ? "-DDDDDDD" : "DDDDDDD") if ($1 != 0);
	    $header->setLatitude(sprintf("%07.3f",$2),$2 < 0 ? "-DDDDDDD" : "DDDDDDD") if ($2 != 0);
	} elsif ($line =~ /Operator Name\/Comments:\s(.+)$/) {
	    $header->setLine("System Operator:",trim($1));
	} elsif ($line =~ /Comments:\s+(.+)$/) {
	    $header->setLine("Comments:",trim($1));
	}
    }

    if (getProjectName() =~ /RICO/) {
	if ($header->getLatitude() == 99 && (($header->getActualDate() eq "2005, 01, 06" &&
					      $header->getActualTime() eq "23:03:52") ||
					     ($header->getActualDate() eq "2005, 01, 11" &&
					      $header->getActualTime() eq "23:08:35"))) {
	    $header->setLatitude("17.551","DDDDDD");
	    $header->setLongitude("-61.737","-DDDDDD");
	}


	if ($header->getLatitude() eq "17.551" && $header->getLongitude() eq "-61.737") {
	    $header->setAltitude(13,"m");
	} elsif ($header->getLatitude() eq "17.591" && $header->getLongitude() eq "-61.819") {
	    $header->setAltitude(5,"m");
	} elsif (($header->getLatitude() eq "17.608" || $header->getLatitude() eq "17.607") && 
		 $header->getLongitude() eq "-61.824") {
	    $header->setAltitude(8,"m");
	} elsif ($header->getLatitude() eq "17.608" && $header->getLongitude() eq "-61.827") {
	    $header->setAltitude(8,"m");
	} elsif ($header->getLatitude() eq "17.666" && $header->getLongitude() == -61.79) {
	    $header->setAltitude(40,"m");
	} else {
	    die(sprintf("Unknown alt for stn at %s %s\n",$header->getLatitude(),$header->getLongitude()));
	}
    }

    return $header;
}

sub create_record {
    my ($self,$file,$dataline,$header,$previous,$previous_alt) = @_;
    my $record = Sounding::ClassRecord->new($WARN,$file,$previous);
    my @data = split(' ',$dataline);

    if (!defined($previous)) {
	$data[11] = $header->getLongitude();
	$data[12] = $header->getLatitude();
	$record->setAltitude($header->getAltitude(),"m");
	$record->setTime(-1);
    } else {
	$record->setTime($self->calc_time($data[3],$data[4]));
    }
	
    

    $record->setPressure($data[5],"mb") unless ($data[5] == 9999);
    $record->setTemperature($data[6],"C") unless ($data[6] == 99);
    $record->setRelativeHumidity($data[7]) unless ($data[7] == 999);
    $record->setWindDirection($data[8]) unless ($data[8] == 999);
    $record->setWindSpeed($data[9],"m/s") unless ($data[9] == 999);

    $record->setAltitude(calculateAltitude($previous_alt->getPressure() == 9999 ? undef() : $previous_alt->getPressure(),
					   $previous_alt->getTemperature() == 999 ? undef() : $previous_alt->getTemperature(),
					   $previous_alt->getDewPoint() == 999 ? undef() : $previous_alt->getDewPoint(),
					   $previous_alt->getAltitude() == 99999 ? undef() : $previous_alt->getAltitude(),
					   $record->getPressure() == 9999 ? undef() : $record->getPressure(),
					   $record->getTemperature() == 999 ? undef() : $record->getTemperature(),
					   $record->getDewPoint() == 999 ? undef() : $record->getDewPoint(),
					   1,$WARN),"m") if (defined($previous_alt));
    
    if ($data[11] != 999) {
	my $lon_fmt = $data[11] < 0 ? "-" : "";
	while (length($lon_fmt) < length($data[11])) { $lon_fmt .= "D"; }
	$record->setLongitude($data[11],$lon_fmt);
    }
    if ($data[12] != 99) {
	my $lat_fmt = $data[12] < 0 ? "-" : "";
	while (length($lat_fmt) < length($data[12])) { $lat_fmt .= "D"; }
	$record->setLatitude($data[12],$lat_fmt);
    }
    

    if ($data[1] =~ /^S(\d)(\d)$/) {
	$record->setPressureFlag($QUESTIONABLE_FLAG) if ($1 && $record->getPressureFlag() != $MISSING_FLAG);
	$record->setTemperatureFlag($QUESTIONABLE_FLAG) if ($1 && $record->getTemperatureFlag() != $MISSING_FLAG);
	$record->setRelativeHumidityFlag($QUESTIONABLE_FLAG) if ($1 && $record->getRelativeHumidityFlag() != $MISSING_FLAG);
	$record->setUWindComponentFlag($QUESTIONABLE_FLAG) if ($2 && $record->getUWindComponentFlag() != $MISSING_FLAG);
	$record->setVWindComponentFlag($QUESTIONABLE_FLAG) if ($2 && $record->getVWindComponentFlag() != $MISSING_FLAG);	
    }


#    printf("%s",$record->toString());

    return $record;
}

##------------------------------------------------------------------------------
# @signature GAUS_Converter new()
# <p>Create a new GAUS_Converter instance.</p>
#
# @output $converter The new converter.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    printf("\n\nWARNING\nDoes not create station list!!!\n\n");

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

##------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Determine all of the raw data files that need to be processed and then
# process them.</p>
##------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,getRawDirectory()) or die("Cannot read raw data directory.\n");
    my @files = grep(/^D\d{8}_\d{6}_P\.1a\.wf\.new\.RadCor$/,sort(readdir($RAW)));
    closedir($RAW);
    
    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    foreach my $file (@files) {
	$self->readRawFile(sprintf("%s/%s",getRawDirectory(),$file));
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readRawFile(String file)
# <p>Convert the specified file into the CLASS format.</p>
#
# @input $file The name of the raw data file to be converted.
##------------------------------------------------------------------------------
sub readRawFile {
    my ($self,$file) = @_;

    printf("Processing file: %s\n",$file);

    open(my $FILE,$file) or die("Cannot open file: $file\n");
    my @lines = <$FILE>;
    close($FILE);

    my $header = $self->create_header(@lines);

    return if (!defined($header));

    my $outfile = sprintf("%s/%s_%04d%02d%02d%02d%02d%02d.cls",$self->getOutputDirectory(),
			  $header->getId(),split(", ",$header->getActualDate()),
			  split(":",$header->getActualTime()));
    my @record_list = ();


    my @start = grep(/^GAUS\-[A-Z]\d{2}\s+A11/,@lines);
    my $previous_record = $self->create_record($outfile,$start[0],$header);
    push(@record_list,$previous_record);

    my $previous_alt_record = $previous_record->getAltitude() == 99999 ? undef() : $previous_record;

    delete($self->{"starttime"});
    foreach my $line (grep(/^GAUS\-[A-Z]\d{2}\s+S\d{2}/,@lines)) {
	my $record = $self->create_record($outfile,$line,$header,$previous_record,$previous_alt_record);
	push(@record_list,$record);
	$previous_record = $record;
	$previous_alt_record = $previous_record if ($previous_record->getAltitude() != 99999);
    }

    foreach my $record (reverse(@record_list)) {
	if ($record->toString() =~ /9\.0  9\.0  9\.0  9\.0  9\.0  9\.0$/) {
	    undef($record);
	} else {
	    last;
	}
    }


    open(my $OUT,sprintf(">%s",$outfile)) or die("Cannot open $outfile\n");
    print($OUT $header->toString());

    foreach my $record (@record_list) {
	printf($OUT $record->toString()) if (defined($record));
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


