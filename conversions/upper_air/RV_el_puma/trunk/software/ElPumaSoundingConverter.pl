#! /usr/bin/perl -w

package ElPumaSoundingConverter;
use strict;
use lib "/work/software/NAME/library/conversion_modules/Version4";
use DpgDate qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::SimpleStationMap;
use Station::Station;

&main();

sub get_final_directory { return "../final"; }
sub get_location_file { return "../docs/bitacora.txt"; }
sub get_network_name { return "EL_PUMA"; }
sub get_output_directory { return "../output"; }
sub get_project_name { return "NAME"; }
sub get_raw_directory { return "../raw_data/csv"; }
sub get_release_file { return "../docs/name_release_times.txt"; }
sub get_station_file { return sprintf("%s/%s_%s_stationCD.out",get_final_directory(),
				      get_network_name(),get_project_name()); }
sub get_summary_file { return sprintf("%s/station_summary.log",get_output_directory()); }
sub get_warning_file { return sprintf("%s/warning.log",get_output_directory()); }

sub main {
    my $converter = ElPumaSoundingConverter->new();

    mkdir(get_output_directory()) unless(-e get_output_directory());
    mkdir(get_final_directory()) unless(-e get_final_directory());

    $converter->convert();
}

sub convert {
    my ($self) = @_;

    open(my $WARN,">".get_warning_file()) or die("Cannot open the warning file.\n");
    $self->{"warn"} = $WARN;

    $self->read_ship_location_data();
    $self->read_release_data();
    $self->read_raw_files();
    $self->printStationFiles();

    close($WARN);
}

sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    $self->{"stations"} = Station::SimpleStationMap->new();

    return $self;
}

sub parse_file {
    my ($self,$file) = @_;
    $file =~ /adas[\-_](\d+)/;

    my $nom_date = sprintf("20%s",$1);
    my $rel_date = $self->{"release"}->{"date"}->{$1};
    my $rel_time = $self->{"release"}->{"time"}->{$1};

    my $station = $self->{"stations"}->getStation("XCUM",get_network_name());
    my $header = Sounding::ClassHeader->new($self->{"warn"},$station);

    $header->setType("R/V El Puma Tethersonde Data");
    $header->setProject(get_project_name());

    my $date;
    if (defined($rel_date) && defined($rel_time)) {
	$date = sprintf("%s%s",$rel_date,$rel_time);
	$date =~ s/[\-:]//g;
    }
    $date = sprintf("%s00",$nom_date) if (!defined($date));
	
    if (get_project_name =~ /NAME/) {
	if ($nom_date == 2004081112) {
	    $date = 200408111154;
	}
    }

    my $lat = $self->{"location"}->{sprintf("%s00",$date)}->{"latitude"};
    my $lon = $self->{"location"}->{sprintf("%s00",$date)}->{"longitude"};

    if (defined($lat) && defined($lon)) {
	$header->setLatitude($lat,sprintf("%sDD MMMMMM",(split(' ',$lat))[0] < 0 ? "-" : ""));
	$header->setLongitude($lon,sprintf("%sDDD MMMMMM",(split(' ',$lon))[0] < 0 ? "-" : ""));
    }

    $header->setActualRelease($rel_date,"YYYY-MM-DD",$rel_time,"HH:MM",7) if (defined($rel_date) && defined($rel_time));
    $header->setNominalRelease(substr($nom_date,0,8),"YYYYMMDD",substr($nom_date,8,2),"HH",7);
    $station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

    open(my $OUT,sprintf(">%s/%s_%s%s.cls",get_output_directory(),"XCUM",
			 formatDate($header->getActualDate(),"YYYY, MM, DD","YYYYMMDD"),
			 formatTime($header->getActualTime(),"HH:MM:SS","HHMM"))) 
	or die("Cannot create output file\n");
    print($OUT $header->toString());

    $file = sprintf("%s/%s",get_raw_directory(),$file);
    
    open(my $FILE,$file) or die("Cannot open $file\n");
    my $prev_record;
    foreach my $line (<$FILE>) {
	chomp($line);

	# Correct negative signs to go with the correct value!
	$line =~ s/\s+\-\s*,\s*/,\-/g;

	my @data = split(/,/,$line);

	if (@data == 7 && $data[0] =~ /^\d+$/) {
	    $data[0] = sprintf("%06d",$data[0]);
	    my $record = Sounding::ClassRecord->new($self->{"warn"},$file,$prev_record);
	    $record->setTime(3600*substr($data[0],0,2) + 
			     60*substr($data[0],2,2) + substr($data[0],4,2));
	    $record->setTemperature($data[1],"C")  unless ($data[1] =~ /\\/ || $data[1] == -99999.9);
	    $record->setPressure($data[3],"mb")    unless ($data[3] =~ /\\/ || $data[3] == -99999.9);
	    $record->setRelativeHumidity($data[4]) unless ($data[4] =~ /\\/ || $data[4] == -99999.9);
	    $record->setWindSpeed($data[5],"m/s")  unless ($data[5] =~ /\\/ || $data[5] == -99999.9);
	    $record->setWindDirection($data[6])    unless ($data[6] =~ /\\/ || $data[6] == -99999.9);

	    if (!defined($prev_record) && defined($lat) && defined($lon)) {
		$record->setLatitude($lat,sprintf("%sDD MMMMMM",(split(' ',$lat))[0] < 0 ? "-" : ""));
		$record->setLongitude($lon,sprintf("%sDDD MMMMMM",(split(' ',$lon))[0] < 0 ? "-" : ""));
	    }
	    
	    print($OUT $record->toString());
	    $prev_record = $record;
	}
    }
    close($FILE);

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

    open($STN, ">".$self->get_station_file()) || 
	die("Cannot create the ".$self->getStationFile()." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);

    open($SUMMARY, ">".$self->get_summary_file()) || 
	die("Cannot create the ".$self->getSummaryFile()." file.\n");
    print($SUMMARY $self->{"stations"}->getStationSummary());
    close($SUMMARY);
}

sub read_raw_files {
    my ($self) = @_;

    opendir(my $RAW,get_raw_directory()) or die("Cannot open raw directory.\n");
    my @files = grep(/^adas.+\.CSV$/i,readdir($RAW));
    closedir($RAW);

    foreach my $file (sort(@files)) {
	printf("Processing File: %s\n",$file);
	$self->parse_file($file);
    }
}

sub read_release_data {
    my ($self) = @_;

    open(my $FILE,get_release_file()) or die("Can't open release file\n");

    foreach my $line (<$FILE>) {
	my @data = split(' ',$line);

	if (@data > 3) {
	    $self->{"release"}->{"date"}->{$data[0]} = $data[1];
	    $self->{"release"}->{"time"}->{$data[0]} = $data[2];
	}
    }

    close($FILE);
}

sub read_ship_location_data {
    my ($self) = @_;

    my $station = Station::Station->new("XCUM",get_network_name());
    $station->setStationName("R/V El Puma: Cruise ECAC-5");

    $station->setStateCode("XX");
    $station->setCountry("MX");
    $station->setReportingFrequency("no set schedule");
    $station->setMobilityFlag("m");
    $station->setLatLongAccuracy(0);
    $station->setNetworkIdNumber(15);
    $station->setPlatformIdNumber(313);

    $self->{"stations"}->addStation($station);

    open(my $FILE,get_location_file()) or die("Cannot open ship location file\n");
    my @locs = grep(/^\d+\-\D+\-\d+\s+/,<$FILE>);
    close($FILE);

    foreach my $line (@locs) {
	chomp($line);
	my @data = split(' ',$line);

	$data[0] =~ s/Aug/08/i;

	($data[0],$data[1]) = adjustDateTime($data[0],"DD-MM-YYYY",$data[1],"HH:MM:SS",
					     0,0,0,60 - (split(/:/,$data[1]))[2]);

	my $date = sprintf("%s%s",formatDate($data[0],"DD-MM-YYYY","YYYYMMDD"),
			   formatTime($data[1],"HH:MM:SS","HHMMSS"));

	my $lat = sprintf("%s%02d %06.3f",$data[4] eq "N" ? "" : "-",
			  $data[2],$data[3]);
	my $lon = sprintf("%s%03d %06.3f",$data[7] eq "E" ? "" : "-",
			  $data[5],$data[6]);

	$self->{"location"}->{$date}->{"latitude"} = $lat;
	$self->{"location"}->{$date}->{"longitude"} = $lon;
    }
}


