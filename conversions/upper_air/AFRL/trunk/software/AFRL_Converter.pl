#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The AFRLConverter script converts radiosonde and thermosonde data into the
# ESC format.  It merges txt PTU files, .win wind files, and .txt rise rate files
# into single records on altitude.</p>
#
# @author Joel Clawson
# @version TREX_2006 This was originally created for the T-REX project.
##Module------------------------------------------------------------------------
package AFRLConverter;
use strict;
if (-e "/net/work") {
    use lib "/net/work/software/TREX/library/conversion_modules/Version6";
} else {
    use lib "/work/software/TREX/library/conversion_modules/Version6";
}
use Station::SimpleStationMap;
use Station::Station;
use Sounding::ClassConstants qw(:DEFAULT);
use Sounding::ClassHeader;
use Sounding::ClassRecord;

&main();

##------------------------------------------------------------------------------
# @signature void main()
# <p>Process the AFRL radiosonde/thermosonde data by converting it from the 
# native ASCII format into the ESC format.</p>
##------------------------------------------------------------------------------
sub main {
    my $converter = AFRLConverter->new();
    $converter->convert();
}

##------------------------------------------------------------------------------
# @signature AFRLConverter new()
# <p>Create a new instance of a AFRLConverter.</p>
#
# @output $self A new AFRLConverter object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"stations"} = Station::SimpleStationMap->new();

    $self->{"PROJECT"} = "T-REX";
    $self->{"NETWORK"} = "AFRL";

    $self->{"FINAL_DIR"} = "../final";
    $self->{"OUTPUT_DIR"} = "../output";
    $self->{"RAW_DIR"} = "../raw_data";

    $self->{"HEADER_INFO_FILE"} = "../docs/header_info.txt";
    $self->{"STATION_FILE"} = sprintf("%s/%s_%s_sounding_stationCD.out",
                                      $self->{"FINAL_DIR"},
                                      $self->clean_for_file_name($self->{"NETWORK"}),
                                      $self->clean_for_file_name($self->{"PROJECT"}));
    $self->{"WARN_LOG"} = $self->{"OUTPUT_DIR"}."/warning.log";

    return $self;
}

##------------------------------------------------------------------------------
# @signature String build_latlong_format(String value)
# <p>Generate the decimal format for the specified value.</p>
#
# @input $value The value of the lat/lon being formatted.
# @output $fmt The format that corresponds the the value.
##------------------------------------------------------------------------------
sub build_latlon_format {
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

    # Convert spaces to underscores
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

    open(my $WARN,">".$self->{"WARN_LOG"}) or die("Can't create ".$self->{"WARN_LOG"});

    $self->load_header_info();
    $self->read_data_files($WARN);
    $self->generate_output_files();
    $self->print_station_files();

    close($WARN);
}

##------------------------------------------------------------------------------
# @signature ClassRecord find_record(FILE* WARN, String sounding, float alt, String file)
# <p>Find a record for the specified sounding and altitude.</p>
#
# @input $WARN The FileHandle where warnings are to be stored.
# @input $sounding The sounding that the record belongs to.
# @input $alt The altitude of the record.
# @input $file The file that the record is being generated from.
# @output $record The record in the hash.
##------------------------------------------------------------------------------
sub find_record {
    my ($self,$WARN,$sounding,$alt,$file) = @_;

    my $record = $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%10.1f",$alt*10000)};
    if (!defined($record)) {
        $record = Sounding::ClassRecord->new($WARN,$file);
        $record->setAltitude($alt,"km");

        $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%10.1f",$alt*10000)} = $record;
    }

    return $record;
}

##------------------------------------------------------------------------------
# @signature void generate_output_file()
# <p>Create the class files from the information stored in the data hash.</p>
##------------------------------------------------------------------------------
sub generate_output_files {
    my ($self) = @_;

    foreach my $key (keys(%{ $self->{"soundings"}})) {
        my $header = $self->{"soundings"}->{$key}->{"header"};

        open(my $OUT,sprintf(">%s/%s_%04d%02d%02d%02d%02d%02d.cls",$self->{"OUTPUT_DIR"},
                             $header->getId(),split(/,/,$header->getActualDate()),
                             split(/:/,$header->getActualTime()))) or die("Can't open output file!\n");

        print($OUT $header->toString());

        foreach my $alt (sort {$a <=> $b} (keys(%{ $self->{"soundings"}->{$key}->{"records"}}))) {
            print($OUT $self->{"soundings"}->{$key}->{"records"}->{$alt}->toString());
        }

        $self->get_station()->insertDate($header->getNominalDate(),"YYYY, MM, DD");

        close($OUT);
    }
}

##------------------------------------------------------------------------------
# @signature String get_station()
# <p>Get the station where all of the soundings were released from.</p>
##------------------------------------------------------------------------------
sub get_station {
    my ($self) = @_;

    my $station = $self->{"stations"}->getStation("AFRL",$self->{"NETWORK"});
    if (!defined($station)) {
        $station = Station::Station->new("AFRL",$self->{"NETWORK"});
        $station->setStationName("AFRL at Ash Mountain Helibase");
        $station->setLatitude(36.4872,"DDDDDDD");
        $station->setLongitude(-118.84048,"-DDDDDDDDD");
        $station->setElevation(503,"m");
        $station->setLatLongAccuracy(3);
        $station->setStateCode("CA");
        $station->setReportingFrequency("no set schedule");
        $station->setNetworkIdNumber(99);
        $station->setPlatformIdNumber(90);
 
        $self->{"stations"}->addStation($station);
    }

    return $station;
}

##------------------------------------------------------------------------------
# @signature void load_header_info()
# <p>Create the headers for the class files from the information stored in
# the header information file.</p>
##------------------------------------------------------------------------------
sub load_header_info {
    my ($self) = @_;

    open(my $INFO,$self->{"HEADER_INFO_FILE"}) or die("Can't open file ".$self->{"HEADER_INFO_FILE"});
    foreach my $line (<$INFO>) {
        chomp($line);
        my @data = split(/;/,$line);

        my $header = Sounding::ClassHeader->new($self->{"WARN"},$self->get_station());
        $header->setReleaseDirection("Ascending");
        $header->setType("AFRL ".$data[1]);
        $header->setProject($self->{"PROJECT"});
        $header->setSite($data[0]);
        $header->setLine("Ground Station:",$data[4]);

        $header->setActualRelease($data[2],"YYYY/MM/DD",$data[3],"HH:MM",0);
        $header->setNominalRelease($data[2],"YYYY/MM/DD",$data[3],"HH:MM",0);

        $self->{"soundings"}->{$data[0]}->{"header"} = $header;
    }
    close($INFO);
}

##------------------------------------------------------------------------------
# @signature void parse_rr_file(FileHandle WARN, String file)
# <p>Parse the rise rate file and add the ascension rate to the sounding records
# at the corresponding altitude.  This will only add the ascension rate to the
# record if the record already exists.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_rr_file {
    my ($self,$WARN,$file) = @_;

    printf("Processing file: %s\n",$file);

    $file =~ /(t\-rex\d{3})/i;
    my $sounding = uc($1);

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
    foreach my $line (<$FILE>) {
        chomp($line);
        my @data = split(' ',$line);

        my $record = $self->{"soundings"}->{$sounding}->{"records"}->{sprintf("%10.1f",$data[0]*10000)};
        if (defined($record)) {
            $record->setAscensionRate($data[1],"m/s");
        }
    }
    close($FILE);
}

##------------------------------------------------------------------------------
# @signature void parse_pth_file(FileHandle WARN, String file)
# <p>Parse the non-wind values from the file and add them to the record at the
# record's altitude.</p>
#
# @input $WARN The FileHandle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_pth_file {
    my ($self,$WARN,$file) = @_;

    printf("Processing file: %s\n",$file);

    $file =~ /(t\-rex\d{3})/i;
    my $sounding = uc($1);

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
    foreach my $line (<$FILE>) {
        chomp($line);

        # Skip any blank lines.
        next if ($line =~ /^\s*$/);

        my @data = split(' ',$line);

        my $record = $self->find_record($WARN,$sounding,$data[0],$file);
        $record->setPressure($data[1],"hPa") unless($data[1] =~ /\/+/);
        $record->setTemperature($data[2],"C") unless($data[2] =~ /\/+/);
        $record->setRelativeHumidity($data[3]) unless($data[3] =~ /\/+/);
    }
    close($FILE);
}

##------------------------------------------------------------------------------
# @signature void parse_win_file(FileHandle WARN, String file)
# <p>Parse the wind values for the records in the file by altitude.</p>
#
# @input $WARN The file handle where warnings are to be stored.
# @input $file The name of the file to be parsed.
##------------------------------------------------------------------------------
sub parse_win_file {
    my ($self,$WARN,$file) = @_;

    printf("Processing file: %s\n",$file);

    $file =~ /(t\-rex\d{3})/i;
    my $sounding = uc($1);

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read $file\n");
    foreach my $line (<$FILE>) {
        chomp($line);
        my @data = split(' ',$line);

        my $record = $self->find_record($WARN,$sounding,$data[0],$file);
        $record->setWindSpeed($data[1],"m/s") unless ($data[1] =~ /\/+/);
        $record->setWindDirection($data[2]) unless ($data[2] =~ /\/+/);
    }
    close($FILE);
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
    my ($self,$WARN) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't read raw directory ".$self->{"RAW_DIR"});

    my @files = grep(/\.win$/i,sort(readdir($RAW)));
    foreach my $winfile (@files) {
        $self->parse_win_file($WARN,$winfile);
    }
    rewinddir($RAW);

    @files = grep(/\d{3}\.txt$/i,sort(readdir($RAW)));
    foreach my $pthfile (@files) {
        $self->parse_pth_file($WARN,$pthfile);
    }
    rewinddir($RAW);

    @files = grep(/_RR_data\.txt$/i,sort(readdir($RAW)));
    foreach my $rrfile (@files) {
        $self->parse_rr_file($WARN,$rrfile);
    }

    closedir($RAW);
}
