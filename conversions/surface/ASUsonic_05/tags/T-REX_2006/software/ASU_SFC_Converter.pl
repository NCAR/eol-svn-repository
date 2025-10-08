#! /usr/bin/perl -w

package ASU_SFC_Converter;

##Module-------------------------------------------------------------------
# <p>The ASU_SFC_Converter is a script for converting the ASU Flux Tower 
# Sonics data from its source ASCII format to the QCF format.  This
# conversion only generates the 5 minutes resolution QCF format file.  It
# does not generate a source frequency or special record files.</p>
#
# @author Joel Clawson
# @version TREX_2006 This conversion was originally developed for T-REX.
##Module-------------------------------------------------------------------
use strict;
if (-e "/work") {
    use lib "/work/software/TREX/library/conversion_modules/Version6";
} elsif (-e "/net/work") {
    use lib "/net/work/software/TREX/library/conversion_modules/Version6";
} else {
    die("Cannot find conversion module library directory.\n");
}
use DpgCalculations;
use DpgDate;
use RecordMap;
use Station::SimpleStationMap;
use Station::Station;
use Surface::QCFConstants;
use Surface::QCFSurfaceRecord;

$! = 1;

&main();

##-------------------------------------------------------------------------
# @signature void main()
# <p>Execute the script to run the conversion.</p>
##-------------------------------------------------------------------------
sub main {
    my $converter = ASU_SFC_Converter->new();
    $converter->convert();
    $converter->clean_empty_files();
}

##-------------------------------------------------------------------------
# @signature ASU_SFC_Converter new()
# <p>Create a new instance of the ASU_SFC_Converter class.  This defines
# the class and the constants used by the conversion.</p>
##-------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = {};
    bless($self,$class);

    $self->{"stations"} = Station::SimpleStationMap->new();
    $self->{"records"} = RecordMap->new();
    $self->{"record_times"} = {};

    $self->{"NETWORK"} = "ASUsonic";
    $self->{"PROJECT"} = "T-REX";

    $self->{"START_TOI"} = "2006/03/01";
    $self->{"END_TOI"} = "2006/04/30";

    $self->{"OUT_DIR"} = "../output";
    $self->{"FINAL_DIR"} = "../final";
    $self->{"RAW_DIR"} = "../raw_data";

    $self->{"DUPES_FILE"} = sprintf("%s/dupes.log",$self->{"OUT_DIR"});
    $self->{"SEQUENCE_FILE"} = sprintf("%s/sequence.log",$self->{"OUT_DIR"});
    $self->{"STN_SUM_FILE"} = sprintf("%s/station_summary.log",$self->{"OUT_DIR"});
    $self->{"warning_file"} = sprintf("%s/warning.log",$self->{"OUT_DIR"});

    return $self;
}

##-------------------------------------------------------------------------
# @signature void add_to_record_map(Record record, String time)
# <p>Add the specified record into the record map, replacing a previous
# record if this one is a better match to the nominal time.</p>
#
# @input $record The record to be added into the RecordMap.
# @input $time The actual time of the record in HH:MM:SSSS format.
##-------------------------------------------------------------------------
sub add_to_record_map {
    my ($self,$record,$time) = @_;

    if ($time =~ /[49]:\d{2}\.\d$/ || $time =~ /[50]:00\.0$/) {

        my $date = $record->getNominalDate();
        my $time = $record->getNominalTime();

        if ($time =~ /[49]$/) {
            ($date,$time) = adjustDateTime($date,"YYYY/MM/DD",$time,"HH:MM",0,0,1,0);
        }

        if (!defined($self->{"datamap"}->{$date}->{$time})) {
            $self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"temp"}->{"sum"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"usum"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"vsum"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"press"}->{"sum"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"rh"}->{"count"} = 0;
            $self->{"datamap"}->{$date}->{$time}->{"rh"}->{"sum"} = 0;
        }

        # Add valid temperatures to the data map.
        if (-30 <= $record->getTemperature() && $record->getTemperature() <= 40) {
            $self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"}++;
            $self->{"datamap"}->{$date}->{$time}->{"temp"}->{"sum"} += 
                    $record->getTemperature();
        }

        # Add valid winds to the data map.
        if (0 <= $record->getWindSpeed() && $record->getWindSpeed() <= 50 &&
            0 <= $record->getWindDirection() && $record->getWindDirection <= 360) {
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"}++;
            my ($uwind,$vwind) = calculateUVfromWind($record->getWindSpeed(),$record->getWindDirection());
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"usum"} += $uwind;
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"vsum"} += $vwind;
        }

        # Add valid pressures to the data map.
        if (840 <= $record->getPressure() && $record->getPressure() <= 900) {
            $self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"}++;
            $self->{"datamap"}->{$date}->{$time}->{"press"}->{"sum"} += $record->getPressure();
        }

        # Add valid relative humidity values to the data map.
        if (0 <= $record->getRelativeHumidity() && $record->getRelativeHumidity() <= 104) {
            $self->{"datamap"}->{$date}->{$time}->{"rh"}->{"count"}++;
            $self->{"datamap"}->{$date}->{$time}->{"rh"}->{"sum"} += $record->getRelativeHumidity();
        }
    }
}

##-------------------------------------------------------------------------
# @signature void build_record_map()
# <p>Generate the 5 minute records by averaging the data that was summed
# and counted previously.</p>
##-------------------------------------------------------------------------
sub build_record_map {
    my ($self) = @_;

    my $WARN = $self->{"WARN"};
    my $station = $self->load_station();

    foreach my $date (keys(%{ $self->{"datamap"}})) {
        foreach my $time (keys(%{ $self->{"datamap"}->{$date}})) {
            my $record = Surface::QCFSurfaceRecord->new($WARN,$station);
            $record->setReadingTime($date,"YYYY/MM/DD",$time,"HH:MM",0);


            # Only process records that are in the time of interest for the project.
            if ($self->in_time_of_interest($record)) {

                # Average the temps and add it to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"} >= 200) {
                    $record->setTemperature($self->{"datamap"}->{$date}->{$time}->{"temp"}->{"sum"}/$self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"},"C");
                }

                # Average the winds and add them to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"} >= 200) {
                    my $uwind = $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"usum"}/$self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"};
                    my $vwind = $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"vsum"}/$self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"};
                    my ($windspd,$winddir) = calculateWindFromUV($uwind,$vwind);
                    $record->setWindSpeed($windspd,"m/s");
                    $record->setWindDirection($winddir);
                }

                # Average the pressures and add it to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"} >= 200) {
                    $record->setPressure($self->{"datamap"}->{$date}->{$time}->{"press"}->{"sum"}/$self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"},"mbar");
                }
 
                # Average the relative humidity and add it to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"rh"}->{"count"} >= 200) {
                    $record->setRelativeHumidity($self->{"datamap"}->{$date}->{$time}->{"rh"}->{"sum"}/$self->{"datamap"}->{$date}->{$time}->{"rh"}->{"count"});
                }

                $self->update_for_TREX($record) if ($self->{"PROJECT"} =~ /T\-?REX/);

                # Now add the record to the record map.
                $self->{"records"}->addRecord($record);
            }
        }
    }
}

##-------------------------------------------------------------------------
# @signature void check_for_duplicates()
# <p>Search the records generated by the conversion to see if duplicate
# records were found and print them to a duplicate log file.</p>
##-------------------------------------------------------------------------
sub check_for_duplicates {
    my ($self) = @_;
    
    open(my $DUPES,sprintf(">%s",$self->{"DUPES_FILE"})) or die("Can't create dupes file.\n");
    foreach my $dupe ($self->{"records"}->getDuplicateRecords()) {
        my $rec = $self->{"records"}->getRecord($dupe->getStationId(),$dupe->getNetworkId(),$dupe->getNominalDate(),$dupe->getNominalTime());

        if ($rec->toQCF_String(0) eq $dupe->toQCF_String(0)) {
            printf($DUPES "Exact duplicate found at %s in %s at %s %s.\n",$rec->getStationId(),$rec->getNetworkId(),$rec->getNominalDate(),$rec->getNominalTime());
        } else {
            printf($DUPES "Duplicate records found: %s %s at %s %s.  Keeping the first record.\n\t%s\t%s",$rec->getStationId(),$rec->getNetworkId(),$rec->getNominalDate(),$rec->getNominalTime(),$rec->toQCF_String(0),$dupe->toQCF_String(0));
        }
    }
    close($DUPES);
}

##-------------------------------------------------------------------------
# @signature void check_for_sequence_problems()
# <p>Check the records generated by the conversion to see if any of the
# expected records are missing.  The messages are placed into a sequence
# log file.</p>
##-------------------------------------------------------------------------
sub check_for_sequence_problems {
    my ($self) = @_;
    
    open (my $SEQ,sprintf(">%s",$self->{"SEQUENCE_FILE"})) or die("Can't create the sequence file.\n");
    print($SEQ $self->{"records"}->check5minuteSequence($self->{"START_TOI"},$self->{"END_TOI"}));
    close($SEQ);
}

##-------------------------------------------------------------------------
# @signature void clean_empty_files()
# <p>Remove all zero length log files and display a message to the user
# that the particular log was not generated.</p>
##-------------------------------------------------------------------------
sub clean_empty_files {
    my ($self) = @_;

    # Remove an empty warning file.
    if (-z $self->{"warning_file"}) {
	printf("There were not any warnings generated for the conversion.\n");
	unlink($self->{"warning_file"});
    }

    # Remove an empty duplicate record file.
    if (-z $self->{"DUPES_FILE"}) {
	printf("There were not any duplicate records found during the conversion.\n");
	unlink($self->{"DUPES_FILE"});
    }

    # Remove an empty sequence file.
    if (-z $self->{"SEQUENCE_FILE"}) {
	printf("There were not any missing records found during the conversion.\n");
	unlink($self->{"SEQUENCE_FILE"});
    }
}

##-------------------------------------------------------------------------
# @signature void convert()
# <p>Perform the conversion of the raw data to the QCF format.  This will
# generate the necessary directories, QCF data files, station lists, and
# other log files.</p>
##-------------------------------------------------------------------------
sub convert {
    my ($self) = @_;

    mkdir($self->{"OUT_DIR"}) unless(-e $self->{"OUT_DIR"});
    mkdir($self->{"FINAL_DIR"}) unless(-e $self->{"FINAL_DIR"});

    open($self->{"WARN"},">".$self->{"warning_file"}) or die("Can't create warning file.\n");

    $self->read_raw_files();
    $self->generate_output_files();
    $self->generate_station_files();

    close($self->{"WARN"});
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

##-------------------------------------------------------------------------
# @signature void generate_output_files()
# <p>Generate the output files including the data file, duplicate record
# file, sequence problem file, and warning file.</p>
##-------------------------------------------------------------------------
sub generate_output_files {
    my ($self) = @_;

    printf("Generating output files...\n");

    $self->build_record_map();

    # Check for problems with the records in the record map.
    $self->check_for_duplicates();
    $self->check_for_sequence_problems();

    # Generate the data file.
    my $outfile = sprintf("%s/%s.0qc",$self->{"OUT_DIR"},lc($self->clean_for_file_name($self->{"NETWORK"})));
    open(my $OUT,">$outfile") or die("Can't create output file: $outfile\n");
    foreach my $record ($self->{"records"}->getAllRecords()) {
        print($OUT $record->toQCF_String());
	$self->load_station()->insertDate($record->getNominalDate(),"YYYY/MM/DD");
    }
    close($outfile);
}

##-------------------------------------------------------------------------
# @signature void generate_station_files()
# <p>Create the station list and the station summary files.</p>
##-------------------------------------------------------------------------
sub generate_station_files {
    my ($self) = @_;

    my $station_file = sprintf("%s/%s_%s_surface_stationCD.out",$self->{"FINAL_DIR"},
			       $self->clean_for_file_name($self->{"NETWORK"}),
			       $self->clean_for_file_name($self->{"PROJECT"}));
    open(my $STN,">$station_file") or die("Can't create station file: $station_file\n");
    foreach my $station ($self->{"stations"}->getAllStations()) {
	print($STN $station->toString()) unless ($station->getBeginDate() =~ /^9+$/);
    }
    close($STN);

    open(my $SUM,">".$self->{"STN_SUM_FILE"}) or die("Can't create the station summary file.\n");
    print($SUM $self->{"stations"}->getStationSummary());
    close($SUM);
}

##-------------------------------------------------------------------------
# @signature int in_time_of_interest(Record record)
# <p>Determine if the current record is in the time of interest for the 
# current project.</p>
# 
# @input $record The record to be tested to be in the time of interest.
# @return $result <code>1</code> if the record is in the time of interest,
# <code>0</code> otherwise.
##-------------------------------------------------------------------------
sub in_time_of_interest {
    my ($self,$record) = @_;

    return compareDates($self->{"START_TOI"},"YYYY/MM/DD",$record->getNominalDate(),"YYYY/MM/DD") >= 0 && compareDates($record->getNominalDate(),"YYYY/MM/DD",$self->{"END_TOI"},"YYYY/MM/DD") >= 0;
}

##-------------------------------------------------------------------------
# @signature Station load_station()
# <p>Load the single station where the data was collected.  This function
# will create the station if it has not existed previously or will access
# the station that has already been created.</p>
#
# @return $station The Station where the data was collected.
##-------------------------------------------------------------------------
sub load_station {
    my ($self) = @_;

    my $stationId = "ASUsonic";

    my $station = $self->{"stations"}->getStation($stationId,$self->{"NETWORK"});
    if (!defined($station)) {
        $station = Station::Station->new($stationId,$self->{"NETWORK"});
	$station->setStationName("ASU Flux Tower Sonics");

	# These values were pulled from the raw data README.
        $station->setLatitude("36.79827","DDDDDDDD");
        $station->setLongitude("-118.17578","-DDDDDDDDD");
        $station->setElevation(1172,"m");

        $station->setLatLongAccuracy(4);
        $station->setStateCode("CA");
        $station->setReportingFrequency("5 minute");
        $station->setPlatformIdNumber(358);
        $station->setNetworkIdNumber(99);

        $self->{"stations"}->addStation($station);
    }
    return $station;
}

##-------------------------------------------------------------------------
# @signature String date, String format parse_date_time(String datetime)
# <p>Parse the date time into a seperate date and time values in a consistant
# format.  The date is to be in the YYYY-MM-DD format and the time is to be
# in the HH:MM:SS.S format.</p>
#
# @input $datetime The date/time value to be parsed.
# @return $date The date formatted into YYYY-MM-DD format.
# @return $time The time formatted into HH:MM:SS.S format.
##-------------------------------------------------------------------------
sub parse_date_time {
    my ($self,$datetime) = @_;

    # Remove quotes (typically surrounding the datetime value)
    $datetime =~ s/\"//g;
    # Force whole seconds to have the the decimal fraction included.
    if ($datetime =~ /\d{2}:\d{2}:\d{2}$/) { $datetime .= ".0"; }
    return split(' ',$datetime);
}

##-------------------------------------------------------------------------
# @signature void parse_raw_file(String file)
# <p>Process the file by parsing the raw data and converting it into the
# QCF format.</p>
#
# @input $file The full path to the file to be parsed.
##-------------------------------------------------------------------------
sub parse_raw_file {
    my ($self,$file) = @_;

    printf("Processing file: %s...\n",$file);

    open(my $FILE,$file) or die("Cannot open file $file to be read.\n");

    my $station = $self->load_station();
    my $OUT = $self->{"5MIN_FILE"};
    my $WARN = $self->{"WARN"};

    my $lineCount = 0;
    while (my $line = <$FILE>) {
        $lineCount++;
	# Ignore the header lines.
        next if ($lineCount < 5);

	# Prepare the data to be put into the QCF Record.
        chomp($line);
        my @data = split(/,/,$line);
        my ($date,$time) = $self->parse_date_time($data[0]);

	if ($time =~ /\d{2}:\d{2}:\d{2}\.\d{2}/) {
	    printf($WARN "Invalid time frequency %s %s.  Ignoring record.\n",$date,$time);
	    next;
	}

	# Put the data values into the QCF Record.
        my $record = Surface::QCFSurfaceRecord->new($WARN,$station);
        $record->setReadingTime($date,"YYYY-MM-DD",$time,"HH:MM:SSSS",0);
        $record->setWindSpeed($data[5],"m/s") unless($data[5] =~ /NAN$/i);
        $record->setWindDirection($data[6]) unless($data[6] =~ /NAN$/i);
        $record->setRelativeHumidity($data[11]) unless ($data[11] =~ /NAN/i);
        $record->setPressure($data[18],"bar") unless ($data[18] =~ /NAN/i);
        $record->setTemperature($data[22],"C") unless ($data[22] =~ /NAN/i);

        my $nomdate = $record->getNominalDate();
        $nomdate =~ s/\///g;
        if (20060326 <= $nomdate && $nomdate <= 20060408) {
           $record->setWindDirection(($record->getWindDirection() + 11) % 360);
        }


	#$self->update_for_TREX($record) if ($self->{"PROJECT"} =~ /T\-?REX/);

	# Print the record to the appropriate output files.
        $self->add_to_record_map($record,$time);
    }

    close($FILE);
}

##-------------------------------------------------------------------------
# @signature void read_raw_files()
# <p>Read in the list of raw data files in the raw data directory and 
# parse them into the QCF format.</p>
##-------------------------------------------------------------------------
sub read_raw_files {
    my ($self) = @_;
    
    opendir(my $RAWDIR,$self->{"RAW_DIR"}) or die("Unable to read raw directory: ".
						  $self->{"RAW_DIR"}."\n");
    my @files = sort(grep(/\.dat$/,readdir($RAWDIR)));
    closedir($RAWDIR);

    my $count = 0;    
    foreach my $file (@files) {
        $count++;
	$self->parse_raw_file(sprintf("%s/%s",$self->{"RAW_DIR"},$file));
        #last if ($count > 1);
    }
}

##-------------------------------------------------------------------------
# @signature void update_for_TREX(QCFSurfaceRecord record)
# <p>Update the specified record for changes that only apply during the
# T-REX project.</p>
#
# @input $record The record to be updated.
##-------------------------------------------------------------------------
sub update_for_TREX {
    my ($self,$record) = @_;

    # Flag wind directions over 360 as BAD.
    if ($record->getWindDirection() > 360) {
	$record->setWindDirectionFlag($BAD_FLAG) if ($record->getWindDirection() != $MISSING);
    }

    # Bad sensors during T-REX, so set them to missing.
    $record->setPressure($MISSING);
    $record->setDewPoint($MISSING);

    #$record->setPressureFlag($BAD_FLAG) if ($record->getPressure() != $MISSING);
    #$record->setDewPointFlag($BAD_FLAG) if ($record->getDewPoint() != $MISSING);
}
