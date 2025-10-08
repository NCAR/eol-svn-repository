#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The LeedsConverter script is used for converting Univiersity of Leeds 
# radiosonde data into the EOL Sounding Composite (ESC) format.</p>
#
# @author Joel Clawson
# @version TREX_2006 This was originally created for the T-REX project.
##Module------------------------------------------------------------------------
package LeedsConverter;
use strict;
if (-e "/net/work/") {
  use lib "/net/work/software/TREX/library/conversion_modules/Version6";
} else {
  use lib "/work/software/TREX/library/conversion_modules/Version6";
}

use Station::ElevatedStationMap;
use Station::Station;
use Sounding::ClassConstants qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;

my ($WARN);

&main();

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the Leeds radiosonde data by converting it from the native ASCII
# format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = LeedsConverter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature LeedsConverter new()
# <p>Create a new instance of a LeedsConverter.</p>
#
# @output $self A new LeedsConverter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"stations"} = Station::ElevatedStationMap->new();

    $self->{"PROJECT"} = "T-REX";
    $self->{"NETWORK"} = "Leeds";
    
    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";
    
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",$self->{"FINAL_DIR"},
				      $self->clean_for_file_name($self->{"NETWORK"}),
				      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";
    
    return $self;
}

##------------------------------------------------------------------------------
# @signature Station build_default_station(String station_id, String network)
# <p>Create a default station for the ISS network using the specified 
# station_id and network.</p>
#
# @input $station_id The identifier of the station to be created.
# @input $network The network the station belongs to.
# @return The new station object with the default values for the network.
##------------------------------------------------------------------------------
sub build_default_station {
    my ($self,$station_id,$network) = @_;
    my $station = Station::Station->new($station_id,$network);
    $station->setStationName("University of Leeds at Independence Airport");
    $station->setLatLongAccuracy(2);
    $station->setStateCode("CA");
    $station->setReportingFrequency("no set schedule");
    $station->setNetworkIdNumber(99);
    $station->setPlatformIdNumber(359);

    return $station;
}

##------------------------------------------------------------------------------
# @signature String build_latlong_format(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub build_latlong_format {
    my ($self,$value) = @_;
    
    my $fmt = $value < 0 ? "-" : "";
    while (length($fmt) < length($value)) { $fmt .= "D"; }
    return $fmt;
}

##-------------------------------------------------------------------------
# @signature String clean_for_file_name(String text)
# <p>Remove/translate characters in a String so it can be used in a file name.</p>
#
# @input $text The String to be cleaned.
# @output $text The cleaned up String.
##-------------------------------------------------------------------------
sub clean_for_file_name {
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
    
    $self->read_data_files();
    $self->print_station_files();
}

##------------------------------------------------------------------------------
# @signature String get_sounding_id(String date, String time)
# <p>Get the sounding id at the specified date and time.</p>
#
# @input $date The date of the sounding in "YYYY, MM, DD" format.
# @input $time The time of the sounding in "HH:MM:SS" format.
# @output $id The id of the sounding of <code>undef</code> if the sounding id
# is not in the sounding id file.
##------------------------------------------------------------------------------
sub get_sounding_id {
    my ($self,$date,$time) = @_;

    if (!defined($self->{"sounding_ids"})) {
	open(my $FILE,"../docs/sonde_ids.txt") or die("Can't read sounding id file.\n");
	my @lines = <$FILE>;
	close($FILE);

	my $filedate = undef();
	while (@lines) {
	    my $line = shift(@lines);
	    if ($line =~ /(\d{2})\/(\d{2})\/(\d{2})/) {
		$filedate = sprintf("%04d, %02d, %02d",2000+$3,$2,$1);
	    } elsif ($line =~ /^\s*(.+)\((\d{4})\)\s*$/) {
		my $filetime = sprintf("%02d:%02d:00",substr($2,0,2),substr($2,2,2));
		$self->{"sounding_ids"}->{$filedate}->{$filetime} = $1;
	    }
	}
    }

#    printf("Sounding: %s %s (%s)\n",$date,$time,$self->{"sounding_ids"}->{$date}->{$time});

    return $self->{"sounding_ids"}->{$date}->{$time};
}

##------------------------------------------------------------------------------
# @signature ClassHeader parse_header(String file, String[] lines)
# <p>Parse the header lines from the specified file into the ESC header format.</p>
#
# @input $file The name of the file being parsed.
# @input $lines[] The header lines to be parsed.
# @output $header The header data in ESC format.
##------------------------------------------------------------------------------
sub parse_header {
    my ($self,$file,@lines) = @_;
    my $header = Sounding::ClassHeader->new();

    my (undef(),$lat,$lon,$day,$month,$year,$hour,$minute) = split(/,/,$lines[0]);
    $lat = $lat/100;
    $lon = $lon/100;
    
    $header->setReleaseDirection("Ascending");
    $header->setType("University of Leeds Sounding");
    $header->setProject($self->{"PROJECT"});    
    $header->setId("K2O7");
    $header->setSite("Independence Airport, CA");
    
    $header->setLatitude($lat,$self->build_latlong_format($lat));
    $header->setLongitude($lon,$self->build_latlong_format($lon));
    
    my $date = sprintf("%04d, %02d, %02d",$year,$month,$day);
    my $time = sprintf("%02d:%02d",$hour,$minute);
    $header->setActualRelease($date,"YYYY, MM, DD",$time,"HH:MM",0);
    $header->setNominalRelease($date,"YYYY, MM, DD",$time,"HH:MM",0);
    
    my $sonde_id = $self->get_sounding_id($header->getActualDate(),$header->getActualTime());
    if (defined($sonde_id)) {
	$header->setLine("Sonde ID:",$sonde_id);
    }

    if ($self->{"PROJECT"} =~ /T\-REX/i) {
        $header->setAltitude(1162,"m");
    }
    
    return $header;
}

##------------------------------------------------------------------------------
# @signature void parse_raw_files(String file)
# <p>Read the data from the specified file and convert it to the ESC format.</p>
#
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_raw_file {
    my ($self,$file) = @_;
    
    printf("Processing file: %s\n",$file);
    
    open(my $FILE,$self->{"RAW_DIR"}."/".$file) or die("Can't open file: ".$file);
    my @lines = <$FILE>;
    close($FILE);
    
    # Generate the sounding header.
    my $header = $self->parse_header($file,@lines[0..3]);
    
    # Only continue processing the file if a header was created.
    if (defined($header)) {

	# Determine the station the sounding was released from.
	my $station = $self->{"stations"}->getStation($header->getId(),$self->{"NETWORK"},
						      $header->getLatitude(),$header->getLongitude(),
						      $header->getAltitude());
	if (!defined($station)) {
	    $station = $self->build_default_station($header->getId(),$self->{"NETWORK"});
	    $station->setLatitude($header->getLatitude(),$self->build_latlong_format($header->getLatitude()));
	    $station->setLongitude($header->getLongitude(),$self->build_latlong_format($header->getLongitude()));
	    $station->setElevation($header->getAltitude(),"m");
	    $self->{"stations"}->addStation($station);
	}
	$station->insertDate($header->getNominalDate(),"YYYY, MM, DD");
	
	open(my $OUT,">".$self->{"OUTPUT_DIR"}."/".sprintf("%s_%04d%02d%02d%02d%02d%02d.cls",
							   $header->getId(),
							   split(/,/,$header->getActualDate()),
							   split(/:/,$header->getActualTime())))
	    or die("Can't open output file for $file\n");
	
	print($OUT $header->toString());
	
	
	my $prev_record;
	my $index = 0;
        my @record_list = ();
	foreach my $line (@lines) {
	    $index++;
	    # Ignore the header lines.
	    if ($index < 5) { next; }
	    
	    my @data = split(' ',$line);
	    my $record = Sounding::ClassRecord->new($WARN,$file,$prev_record);
	    $record->setTime($data[0]);
	    $record->setPressure($data[1]/10,"hPa") if ($data[1] != -32768);
	    $record->setTemperature($data[2]/10,"C") if ($data[2] != -32768);
	    $record->setRelativeHumidity($data[3]) if ($data[3] != -32768);
	    $record->setAltitude($data[4],"m") if ($data[4] != -32768);
	    $record->setWindDirection($data[6]) if ($data[6] != -32768);
	    $record->setWindSpeed($data[7]/10,"m/s") if ($data[7] != -32768);
	    
            push(@record_list,$record);

	    $prev_record = $record;

	    # Ignore the last record because it is often (always?) a bad record.
	    if ($self->{"PROJECT"} =~ /T\-REX/) {
		if ($index == @lines - 1) { last; }
	    }
	}

        # Remove the last records in the file that are descending after the balloon burst.
        foreach my $record (reverse(@record_list)) {
            if ($record->getAscensionRate() < 0) {
                undef($record);
            } else {
                last;
            }
        }

        # Print the records to the file.
        foreach my $record (@record_list) {
            print($OUT $record->toString()) if (defined($record));
        }
    }
}

##------------------------------------------------------------------------------
# @signature void print_station_files()
# <p>Generate the output files containing station information.</p>
##------------------------------------------------------------------------------
sub print_station_files {
    my ($self) = @_;

    open(my $STN, ">".$self->{"STATION_FILE"}) || 
	die("Cannot create the ".$self->{"STATION_FILE"}." file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
        print($STN $station->toString()) if ($station->getBeginDate !~ /^9+$/);
    }
    close($STN);
}

##------------------------------------------------------------------------------
# @signature void read_data_files()
# <p>Read in the files from the raw data directory and convert each into an
# ESC formatted file.</p>
##------------------------------------------------------------------------------
sub read_data_files {
    my ($self) = @_;
    
    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});
    my @files = grep(/\.vsf$/,sort(readdir($RAW)));
    closedir($RAW);
    
    
    open($WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});
    
    foreach my $file (@files) {
	$self->parse_raw_file($file);
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
