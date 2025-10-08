#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------
# <p>The StMaarten_SND_Converter.pl script is used for converting the sounding data
# from the StMaarten site from the raw Vaisala variant to the CLASS format.</p>
#
# @author Joel Clawson
# @version RAINEX 1.0 This was originally created for the RAINEX project.  It was
# adapted from the Belize converter.
##Module-------------------------------------------------------------------------
package StMaarten_SND_Converter;
use strict;
use lib "/work/software/RAINEX/library/conversion_modules/Version5";
use DpgCalculations qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use Station::ElevatedStationMap;
use Station::Station;
use Sounding::ClassHeader;
use Sounding::ClassRecord;
$| = 1; # Turn on AutoFlush of output streams.

my ($WARN);

&main();

# A collection of functions that contain constants
sub getNetworkName { return "St_Maarten"; }
sub getOutputDirectory { return "../output"; }
sub getProjectName { return "RAINEX"; }
sub getRawDirectory { return "../raw_data"; }
sub getStationFile { return sprintf("../final/%s_%s_sounding_stationCD.out",
				    getNetworkName(),getProjectName()); }
sub getStationList { return "../docs/station.list"; }
sub getSummaryFile { return "../output/station_summary.log"; }
sub getWarningFile { return "../output/warning.log"; }

##------------------------------------------------------------------------------
# @signature void main()
# <p>Execute the conversion of the data.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = StMaarten_SND_Converter->new();
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
# @signature StMaarten_SND_Converter new()
# <p>Create a new StMaarten_SND_Converter instance.</p>
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

    my @header_files = sort(grep(/^H\d+$/i,@files));
    my @highres_files = sort(grep(/^R\d+$/i,@files));

    open($WARN,">".$self->getWarningFile()) or die("Can't open warning file.\n");

    my $h_file = shift(@header_files);
    my $r_file = shift(@highres_files);

    while (scalar(@header_files) > 0 || scalar(@highres_files) > 0) {
	$h_file =~ /H(\d+)/;
	my $h_num = $1;
	$r_file =~ /R(\d+)/;
	my $r_num = $1;

	while ($r_num != $h_num) {
	    if ($r_num < $h_num) {
		if (scalar(@highres_files) == 0) {
		    printf($WARN "Header file without a high resolution data file for ascension %d\n",$h_num);
		    last;
		} else {
		    printf($WARN "High resolution data file without a header file for ascension %d\n",$r_num);

#		    printf("%d\n",$r_num);
		    if (getProjectName() =~ /NAME/ && $r_num == 169) {
			printf("Processing Ascension:  %d\n",$r_num);
			my $header = $self->read_header($r_num);
			$self->read_data($r_num,$header);
		    }
		    
		    $r_file = shift(@highres_files);
		    $r_file =~ /R(\d+)/;
		    $r_num = $1;
		}
	    } else {
		if (scalar(@header_files) == 0) {
		    printf($WARN "High resolution data file without a header file for ascension %d\n",$r_num);

		    last;
		} else {
		    printf($WARN "Header file without a high resolution data file for ascension %d\n",$h_num);
		    
		    $h_file = shift(@header_files);
		    $h_file =~ /H(\d+)/;
		    $h_num = $1;
		}
	    }
	}

	if ($r_num == $h_num) {
	    
	    printf("Processing Ascension:  %d\n",$r_num);

	    my $header = $self->read_header($h_num);
	    $self->read_data($r_num,$header);

#	    last;
	}

	$h_file = shift(@header_files) if (scalar(@header_files) > 0);
	$r_file = shift(@highres_files) if (scalar(@highres_files) > 0);
    }

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature void readData(FileHandle FILE)
# <p>Read in the raw data from the file pointer and convert it to CLASS format
# by placing it in the ClassSounding object that will hold it.
#
# @input $FILE The file handle containing the data.
##------------------------------------------------------------------------------
sub read_data {
    my $self = shift;
    my ($asc_no,$header) = @_;

    my $filename = sprintf("%s_%04d%02d%02d%02d%02d.cls",$header->getId(),
			   split(/, /,$header->getActualDate()),
			   split(/:/,$header->getActualTime()));

    open(my $RAW,sprintf("%s/R%03d",getRawDirectory(),$asc_no)) or
	die(sprintf("Cannot open R%03d\n",$asc_no));

    open(my $OUT, sprintf(">%s/%s",getOutputDirectory(),$filename)) or
	die("Cannot open $filename\n");
    print($OUT $header->toString());

    my $start = 1;
    my $previous_record;
    my %prev_alt_data;

    foreach my $line (<$RAW>) {
	chomp($line);
	next if ($line =~ /^\s*$/);

#	if (length($line) != 38) {
#	    printf($WARN "%s: Line beginning with %s does not have the correct length.  Not creating record entry.\n",$filename,substr($line,0,9));
#	    next;
#	}

	my $record = Sounding::ClassRecord->new($WARN,$filename,$previous_record);
	$record->setTime(trim(substr($line,4,3)),trim(substr($line,7,2)));
	$record->setPressure(trim(substr($line,9,6))/100,"mb") 
	    if (substr($line,9,6) !~ /^9+$/);
	$record->setTemperature(trim(substr($line,20,4))/10,"C") 
	    if (substr($line,20,4) !~ /^9+$/);
	$record->setRelativeHumidity(trim(substr($line,24,4))/10) 
	    if (substr($line,24,4) !~ /^9+$/);
	$record->setWindDirection(trim(substr($line,31,3)))
	    if (substr($line,31,3) !~ /^9+$/);
	$record->setWindSpeed(trim(substr($line,34,4))/10,"m/s")
	    if (substr($line,34,4) !~ /^9+$/);


	$record->setAltitude(trim(substr($line,15,5)),"m") 
	    if (substr($line,15,5) !~ /^9+$/);
	
	if ($start) {

	    my $lat = $header->getLatitude();
	    my $lon = $header->getLongitude();

	    my $lat_fmt = $lat < 0 ? "-" : "";
	    while (length($lat) > length($lat_fmt)) { $lat_fmt .= "D"; }
	    my $lon_fmt = $lon < 0 ? "-" : "";
	    while (length($lon) > length($lon_fmt)) { $lon_fmt .= "D"; }

	    $record->setLatitude($lat,$lat_fmt);
	    $record->setLongitude($lon,$lon_fmt);
	    
	    $start = 0;
	}# else {
	    
	    # Set the altitude from the calculation.
	  #  my $alt = calculateAltitude($prev_alt_data{"pressure"},
	#				$prev_alt_data{"temperature"},
	#				$prev_alt_data{"dew_point"},
	#				$prev_alt_data{"altitude"},
	#				$record->getPressure() == 9999 ? undef() :
	#				$record->getPressure(),
	#				$record->getTemperature() == 999 ? undef() :
	#				$record->getTemperature(),
	#				$record->getDewPoint() == 999 ? undef() :
	#				$record->getDewPoint(),
	#				1, $WARN);
	#    $record->setAltitude($alt,"m");
	#}

	# Update the previous altitude data hash
	#$prev_alt_data{"pressure"} = $record->getPressure()
	#    if ($record->getPressure() != 9999);
	#$prev_alt_data{"temperature"} = $record->getTemperature()
	#    if ($record->getTemperature() != 999);
	#$prev_alt_data{"dew_point"} = $record->getDewPoint()
	#    if ($record->getDewPoint() != 999);
	#$prev_alt_data{"altitude"} = $record->getAltitude()
	#    if ($record->getAltitude() != 99999);

	print($OUT $record->toString());
	$previous_record = $record;
    }
    

    close($OUT);
    close($RAW);
}

##------------------------------------------------------------------------------
# @signature ClassSounding readHeader(FileHandle FILE, String file)
# <p>Read in the header information from the file handle.</p>
#
# @input $FILE The FileHandle containing the data to be read.
# @input $file The name of the file being read.
# @output $cls The ClassSounding holding the header data.
##------------------------------------------------------------------------------
sub read_header {
    my $self = shift;
    my ($asc_no) = @_;

    # Define the station for the StationList.
    my $station = Station::Station->new();
    $station->setNetworkName($self->getNetworkName());
    $station->setReportingFrequency("12 hourly");
    $station->setCountry("XX");
    #$station->setStateCode("XX");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(346);

    my $header = Sounding::ClassHeader->new($WARN,$station);
    $header->setType("St. Maarten Sounding");
    $header->setProject($self->getProjectName());
    $header->setLine("Ascension No:",$asc_no);

    open(my $RAW,sprintf("%s/H%d",getRawDirectory(),$asc_no)) or
	die(sprintf("Cannot open raw file: H%d\n",$asc_no));
    my @lines = <$RAW>;
    close($RAW);
    
    if (scalar(@lines) > 1) {
	die(sprintf("Too many lines in header file: H%d\n",$asc_no));
    }
    
    $station->setStationId("TNCM");
    $station->setStationName(sprintf("%s St. Maarten",trim(substr($lines[0],1,8))));
	
    my $lat = trim(substr($lines[0],9,4));
    my $lat_fmt = "DDMM";
    if (substr($lines[0],13,1) eq "S") { $lat = "-".$lat; $lat_fmt = "-".$lat_fmt; }
    $station->setLatitude($lat,$lat_fmt);
    
    my $lon = trim(substr($lines[0],14,5));
    my $lon_fmt = "DDDMM";
    if (substr($lines[0],19,1) eq "W") { $lon = "-".$lon; $lon_fmt = "-".$lon_fmt; }
    $station->setLongitude($lon,$lon_fmt);
    
    $station->setElevation(trim(substr($lines[0],20,4)),"m");
    
    $header->setActualRelease(trim(substr($lines[0],24,8)),"YYYYMMDD",
			      trim(substr($lines[0],34,4)),"HHMM",0);
    
    $header->setLine(substr($lines[0],55,1) ? "Baroswitch Number:" : "Sonde Serial Number:",
		     trim(substr($lines[0],56,20)));
    

    my $act_hour = substr($header->getActualTime(),0,2);
    my ($nominal_date,$nominal_time) = adjustDateTime($header->getActualDate(),
						      "YYYY, MM, DD",
						      sprintf("%02d:00",$act_hour),
						      "HH:MM",
						      0,$act_hour % 3 == 0 ? 
						      0 : 3 - ($act_hour % 3),0,0);
    $header->setNominalRelease($nominal_date,"YYYY, MM, DD",$nominal_time,"HH:MM",0);
    

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
    
    $station->insertDate($nominal_date,"YYYY, MM, DD");

    return $header;
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
