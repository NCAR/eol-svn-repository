#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The SMN_SND_Converter.pl is a script that converts sounding data from the
# Mexican National Weather Service (SMN) into the CLASS format.</p>
#
# @author Joel Clawson
# @version NAME This was originally created for the NAME project.
##Module------------------------------------------------------------------------
package SMN_SND_Converter;
use strict;
use lib "/work/software/conversion_modules/Version3";
use Sounding::ClassHeader;
use Sounding::ClassRecord;
use Station::SimpleStationMap;
use Station::Station;

my $WARN;
*STDERR = *STDOUT;

&main();

# Define constants to be used.
sub getFinalDirectory { return "../final"; }
sub getNetworkName { return "SMN"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "NAME"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getStationList { return "../docs/station.list"; }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##-------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script.</p>
##-------------------------------------------------------------------------------
sub main {
    my $converter = SMN_SND_Converter->new();
    $converter->convert();
}

##-------------------------------------------------------------------------------
# @signature void convert()
# <p>Convert the raw data into the CLASS format.</p>
##-------------------------------------------------------------------------------
sub convert {
    my $self = shift;

    mkdir(getOutputDirectory()) unless (-d getOutputDirectory());
    mkdir(getFinalDirectory()) unless (-d getFinalDirectory());

    open($WARN,">".getWarningFile()) or die("Cannot open the warning file.\n");

    $self->loadStations();
    $self->readRawDataFiles();
    $self->printStationFiles();

    close($WARN);
}

##-------------------------------------------------------------------------------
# @signature void load_header(ClassSounding cls, Station station, String id, String file)
# <p>Load the header information for the CLASS file from the information in the file.</p>
#
# @input $cls The ClassSounding object that will contain the data.
# @input $station The station where the sounding was released.
# @input $id The id of the station.
# @input $file The name of the file (without the extension) that contains the data.
##-------------------------------------------------------------------------------
sub load_header {
    my $self = shift;
    my $header = shift;
    my $station = shift;
    my $id = shift;
    my $file = sprintf("%s.APA",shift);
   
    my $file_name = sprintf("%s/%s/%s",$self->getRawDirectory(),$id,$file);

    open(my $FILE,$file_name) or die("Cannot open file: $file_name\n");
    while (<$FILE>) {
	if ($_ =~ /^station/i) {
	    # Defines station information
	    my @data = split(' ',(split(/:/,$_))[1]);
	    my ($lat_fmt,$lon_fmt) = ("","");

	    # Convert N or S to appropriate degrees.
	    if ($data[1] =~ /^S$/i) {
		$data[0] *= -1;
		$lat_fmt = "-";
	    }

	    # Convert E or W to appropriate degrees
	    if ($data[3] =~ /^W$/i) {
		$data[2] *= -1;
		$lon_fmt = "-";
	    }

	    while (length($lat_fmt) < length($data[0])) { $lat_fmt .= "D"; }
	    while (length($lon_fmt) < length($data[2])) { $lon_fmt .= "D"; }

	    $station->setLatitude($data[0],$lat_fmt);
	    $station->setLongitude($data[2],$lon_fmt);
	    $station->setElevation($data[4],$data[5]);
	} elsif ($_ =~ /^rs-number/i) {
	    # Radio sonde serial number
	    $header->setLine("Radiosonde Serial Number:",(split(' ',$_))[1]);
	} elsif ($_ =~ /^gc-corrections/i) {
	    # Grond Control Correction Data
	    $header->setLine("Ground Control-Corrections:",
			     trim((split(/:/,$_))[1]));
	}
    }

    close($FILE);
}

##-------------------------------------------------------------------------------
# @signature void loadStations()
# <p>Read in the station information from the station list.</p>
##-------------------------------------------------------------------------------
sub loadStations {
    my $self = shift;

    open(my $STNS, $self->getStationList()) or die("Cannot read station list file.\n");

    while(<$STNS>) {
	my @data = split(",");

	$self->{"map"}->{$data[0]} = $data[2];
#	$self->{"name"}->{$data[0]} = $data[2];

	# Duplicate id for Mexico City.  Only want one in station map.
#	next if ($data[0] eq "AI");

	my $station = Station::Station->new($data[2],$self->getNetworkName());
	$station->setStationName(sprintf("%s %s",$data[1],$data[3]));

	my $lat_fmt = $data[4] < 0 ? "-" : "";
	while (length($lat_fmt) < length($data[4])) { $lat_fmt .= "D"; }
	my $lon_fmt = $data[5] < 0 ? "-" : "";
	while (length($lon_fmt) < length($data[5])) { $lon_fmt .= "D"; }

	$station->setLatitude($data[4],$lat_fmt);
	$station->setLongitude($data[5],$lon_fmt);

	$station->setCountry("MX");
	$station->setStateCode("XX");
	$station->setReportingFrequency("12 hourly");
	$station->setNetworkIdNumber(15);
	$station->setPlatformIdNumber(312);

	$self->{"stations"}->addStation($station);
    }

    close($STNS);
}

##-------------------------------------------------------------------------------
# @signature SMN_SND_Converter new()
# <p>Create a new converter that will handle the conversion process.</p>
##-------------------------------------------------------------------------------
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

##-------------------------------------------------------------------------------
# @signature void readRawDataFiles()
# <p>Read in the files that contain the raw data to be converted and convert each
# file individually.</p>
##-------------------------------------------------------------------------------
sub readRawDataFiles {
    my $self = shift;

    opendir(my $RAW,$self->getRawDirectory()) or die("Cannot open raw directory.\n");
    my @sub_dirs = grep(/^[^\.]+$/,readdir($RAW));
    closedir($RAW);

    # Loop through station directories.
    foreach my $dir (sort(@sub_dirs)) {
	opendir(my $RAW,sprintf("%s/%s",$self->getRawDirectory(),$dir)) or
	    die(sprintf("Cannot open directory: %s/%s",$self->getRawDirectory(),$dir));
	my @files = readdir($RAW);
	closedir($RAW);

	# Loop through files in the station directory.
	foreach my $file (sort(@files)) {
	    $self->readRawFile($dir,$file) if ($file =~ /\.AED$/);
#	    $self->readRawFile($dir,$file) if ($dir =~ /^AI/ && $file =~ /\.AED$/);
#	    $self->readRawFile($dir,$file) if ($dir =~ /^AP/ && $file =~ /04071512\.AED$/);
	}
    }
}

##-------------------------------------------------------------------------------
# @signature void readRawFile(String stn_id, String file_name)
# <p>Parse the data in the specified file and convert it to the CLASS format.</p>
#
# @input $stn_id The station id for the data in the file.
# @input $file_name The name of the file containing the raw data.
##-------------------------------------------------------------------------------
sub readRawFile {
    my $self = shift;
    my $stn_id = shift;
    my $file_name = shift;

    my $file = sprintf("%s/%s/%s",$self->getRawDirectory(),$stn_id,$file_name);

    printf("Processing file: %s/%s\n",$stn_id,$file_name);

    my $station = $self->{"stations"}->getStation($self->{"map"}->{$stn_id},
						  $self->getNetworkName());

    # Only process stations that are known
    return if (!defined($station));


    # Define the header part of a class file.
    my $header = Sounding::ClassHeader->new($station);
    $header->setType("Mexican National Weather Service");
    $header->setProject($self->getProjectName());
    $header->setActualRelease(sprintf("20%s",substr($file_name,0,6)),"YYYYMMDD",
			      sprintf("%s00",substr($file_name,6,2)),"HHMM",0);
    $header->setNominalRelease(sprintf("20%s",substr($file_name,0,6)),"YYYYMMDD",
			       sprintf("%s00",substr($file_name,6,2)),"HHMM",0);

    # Load header data from a separate header file if it exists.
    if (getProjectName() =~ /NAME/) {
	# Don't process the header for NAME.
    } else {
	$self->load_header($header,$station,$stn_id,substr($file_name,0,8)) 
	    if (-e sprintf("%s/%s/%s.APA",$self->getRawDirectory(),
			   $stn_id,substr($file_name,0,8)));
    }



    open(my $FILE,$file) or die("Cannot open file: $file\n");

    # Define the output file.
    my $file_time = sprintf("%s%s",$header->getActualDate(),$header->getActualTime());
    $file_time =~ s/[,\s:]//g;
    my $filename = sprintf("%s/%s_%s.cls",getOutputDirectory(),$self->{"map"}->{$stn_id},
			   substr($file_time,0,10));
    open(my $OUT,sprintf(">%s",$filename)) or die("Can't open output file.\n");
    

    # Loop through the data in the file.
    my $count = 0;
    my @previous_records;

    while (<$FILE>) {
	my $line = $_;
	if (length($line) != 70 || substr($line,50) =~ /\d/ || substr($line,48,1) !~ /(\d|\/)/) {
	    if (length($line) > 1) {
		printf($WARN "%s: Malformed line found.  Stopped reading data.\n",
		       $filename);
		undef(@previous_records);
	    }

	    last;
	}

	my @data = split(' ',substr($line,0,50));

	if ($data[0] =~ /^\d+$/) {

	    my $cls = Sounding::ClassRecord->new($WARN,$filename,scalar(@previous_records) > 0 ? $previous_records[-1] : undef());

	    $data[0] = trim(substr($line, 0,4));
	    $data[1] = trim(substr($line, 5,3));
	    $data[2] = trim(substr($line, 8,5));
	    $data[3] = trim(substr($line,13,7));
	    $data[4] = trim(substr($line,20,6));
	    $data[5] = trim(substr($line,26,4));
	    $data[6] = trim(substr($line,30,7));
	    $data[7] = trim(substr($line,37,5));
	    $data[8] = trim(substr($line,42,7));

	    $cls->setTime($data[0],$data[1]);
	    $cls->setAltitude($data[2],"m") if ($data[2] !~ /\/+/);
	    $cls->setPressure($data[3],"mb") if ($data[3] !~ /\/+/);
	    $cls->setTemperature($data[4],"C") if ($data[4] !~ /\/+/);
	    $cls->setRelativeHumidity($data[5]) if ($data[5] !~ /\/+/);
	    $cls->setDewPoint($data[6],"C") if ($data[6] !~ /\/+/);
	    $cls->setWindDirection($data[7]) if ($data[7] !~ /\/+/);
	    $cls->setWindSpeed($data[8],"m/s") if ($data[8] !~ /\/+/);

	    # Set the data in the first line and in the header to the same values.
	    if ($count == 0) {
		$station->setElevation($data[2],"m");
		my $lat = $station->getLatitude();
		my $lon = $station->getLongitude();

		my $lat_fmt = $lat < 0 ? "-" : "";
		while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }
		my $lon_fmt = $lon < 0 ? "-" : "";
		while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

		$cls->setLatitude($lat,$lat_fmt);
		$cls->setLongitude($lon,$lon_fmt);

		# There is data, so put the date into the station.
		$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");

		print($OUT $header->toString());
	    }

	    $count++;

	    push(@previous_records,$cls);

	    if (scalar(@previous_records) > 4) {
		print($OUT (shift(@previous_records))->toString());
	    }

	}
    }

    if (@previous_records) {
	while (scalar(@previous_records) > 0) {
	    print($OUT (shift(@previous_records))->toString());
	}
    }
    
    close($FILE);
    close($OUT);
    
    if (-z $filename) {
	printf($WARN "%s: File has zero size and is being removed.\n",$filename);
	unlink($filename);
    }
}

##-------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove the surrounding whitespace of a String.
#
# @input $line The line to be trimmed.
# @output $line The trimmed line.
##-------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    return $line;
}
