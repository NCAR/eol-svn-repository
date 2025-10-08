#! /usr/bin/perl -w

package Leeds_AWS_SFC_Converter;

##Module-------------------------------------------------------------------
# <p>The Leeds_AWS_SFC_Converter is a script for converting the Desert Research
# Institute (Leeds_AWS) surface data from its source netCDF format to the QCF format.
#  This conversion only generates the 5 minutes resolution QCF format file.  It
# does not generate a source frequency or special record files.</p>
#
# @author Joel Clawson
# @version TREX_2006 This conversion was originally developed for T-REX.  It
#    uses code written by Janine Goldstein for the reading on NetCDF data used
#    in the MADIS surface conversion.
##Module-------------------------------------------------------------------
use strict;
use lib "/work/software/TREX/library/conversion_modules/Version6";
use lib "/net/work/software/TREX/library/conversion_modules/Version6";
use DpgCalculations;
use DpgDate;
use NetCDF;
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
    my $converter = Leeds_AWS_SFC_Converter->new();
    $converter->convert();
    $converter->clean_empty_files();
}

##-------------------------------------------------------------------------
# @signature Leeds_AWS_SFC_Converter new()
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

    $self->{"NETWORK"} = "LeedsAWS05";
    $self->{"PROJECT"} = "T-REX";

    $self->{"START_TOI"} = "2006/03/01";
    $self->{"END_TOI"} = "2006/04/30";

    $self->{"OUT_DIR"} = "../output";
    $self->{"FINAL_DIR"} = "../final";
    $self->{"RAW_DIR"} = "../raw_data";
    $self->{"STATION_LIST"} = "../docs/station.list";

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

    if (($time =~ /[49]:\d{2}$/ && $time !~ /[49]:0+$/)|| $time =~ /[50]:00$/) {

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
#        if (-30 <= $record->getTemperature() && $record->getTemperature() <= 40) {
	if ($record->getTemperature() != $MISSING) {
            $self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"}++;
            $self->{"datamap"}->{$date}->{$time}->{"temp"}->{"sum"} += $record->getTemperature();
        }

        # Add valid winds to the data map.
#        if (0 <= $record->getWindSpeed() && $record->getWindSpeed() <= 50 &&
#            0 <= $record->getWindDirection() && $record->getWindDirection <= 360) {
        if ($record->getWindDirection() != $MISSING && 
	    0 <= $record->getWindDirection() && $record->getWindDirection <= 360) {
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"}++;
            my ($uwind,$vwind) = calculateUVfromWind($record->getWindSpeed(),$record->getWindDirection());
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"usum"} += $uwind;
            $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"vsum"} += $vwind;
        }

        # Add valid pressures to the data map.
#        if (840 <= $record->getPressure() && $record->getPressure() <= 900) {
	if ($record->getPressure() != $MISSING) {
            $self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"}++;
            $self->{"datamap"}->{$date}->{$time}->{"press"}->{"sum"} += $record->getPressure();
        }

        # Add valid relative humidity values to the data map.
        if ($record->getRelativeHumidity() != $MISSING && 
	    0 <= $record->getRelativeHumidity() && $record->getRelativeHumidity() <= 104) {
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
    my ($self,$station) = @_;

    my $WARN = $self->{"WARN"};

    foreach my $date (keys(%{ $self->{"datamap"}})) {
        foreach my $time (keys(%{ $self->{"datamap"}->{$date}})) {
            my $record = Surface::QCFSurfaceRecord->new($WARN,$station);
            $record->setReadingTime($date,"YYYY/MM/DD",$time,"HH:MM",0);


            # Only process records that are in the time of interest for the project.
            if ($self->in_time_of_interest($record)) {

                # Average the temps and add it to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"} >= 8) {
                    $record->setTemperature($self->{"datamap"}->{$date}->{$time}->{"temp"}->{"sum"}/$self->{"datamap"}->{$date}->{$time}->{"temp"}->{"count"},"C");
                }

                # Average the winds and add them to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"} >= 8) {
                    my $uwind = $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"usum"}/$self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"};
                    my $vwind = $self->{"datamap"}->{$date}->{$time}->{"wind"}->{"vsum"}/$self->{"datamap"}->{$date}->{$time}->{"wind"}->{"count"};
                    my ($windspd,$winddir) = calculateWindFromUV($uwind,$vwind);
                    $record->setWindSpeed($windspd,"m/s");
                    $record->setWindDirection($winddir);
                }

                # Average the pressures and add it to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"} >= 8) {
                    $record->setPressure($self->{"datamap"}->{$date}->{$time}->{"press"}->{"sum"}/$self->{"datamap"}->{$date}->{$time}->{"press"}->{"count"},"mbar");
                }

                # Average the relative humidity and add it to the record.
                if ($self->{"datamap"}->{$date}->{$time}->{"rh"}->{"count"} >= 8) {
                    $record->setRelativeHumidity($self->{"datamap"}->{$date}->{$time}->{"rh"}->{"sum"}/$self->{"datamap"}->{$date}->{$time}->{"rh"}->{"count"});
                }

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

    $self->load_stations();
    $self->read_raw_files();
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
    my ($self,$OUT,$station) = @_;

    printf("Generating output files...\n");

    $self->build_record_map($station);

    # Check for problems with the records in the record map.
    $self->check_for_duplicates();
    $self->check_for_sequence_problems();

    # Generate the data file.
    foreach my $record ($self->{"records"}->getAllRecords()) {
        if ($self->in_time_of_interest($record)) {
            my $out = $record->toQCF_String();
            $out =~ s/ \-0\.00 /  0\.00 /g;
            print($OUT $out);
	    $self->{"stations"}->getStation($record->getStationId(),$self->{"NETWORK"})
	        ->insertDate($record->getNominalDate(),"YYYY/MM/DD") if ($out ne "");
        }
    }

    $self->{"records"}->clear();
    delete($self->{"datamap"});
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

sub getFields {
    return ("time","speed","dir","press","hum","t2");
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

sub is_missing {
    my ($value,$missing) = @_;

    my @miss = split("or",$missing);
    foreach my $miss_value (@miss) {
	return 1 if ($miss_value == $value);
    }
    return 0;
}

##-------------------------------------------------------------------------
# @signature void load_stations()
# <p>Load the stations where the data was collected from the station list.
##-------------------------------------------------------------------------
sub load_stations {
    my ($self) = @_;

    open(my $STN,$self->{"STATION_LIST"}) or die("Can't open station list file.\n");

    foreach my $line (<$STN>) {
	    chomp($line);
	    my @data = split(/;/,$line);

        my $station = Station::Station->new($data[0],$self->{"NETWORK"});
	    $station->setStationName("Leeds AWS Station ".$data[1]);

	    # These values were pulled from the raw data README.
        $station->setLatitude($data[2],"DDDDDDDDDDDD");
        $station->setLongitude($data[3],"-DDDDDDDDDDDDD");
        $station->setElevation($data[4],"m");

        $station->setLatLongAccuracy(4);
        $station->setStateCode("CA");
        $station->setReportingFrequency("3 second");
        $station->setPlatformIdNumber(370);
        $station->setNetworkIdNumber(99);

        $self->{"stations"}->addStation($station);
    }
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
    my ($self,$file,$OUT) = @_;

    printf("Processing file: %s...\n",$file);

    my $ncid = NetCDF::open($file,0);
    die("Unable to read netcdf file $file\n") if ($ncid == -1);

    #-----------------------------------------------------------------------
    # Find out the name of the record dimension ($recDimName) and how many
    # variables ($nvars) are in the input file. The other values read in are
    # not essential to this code, so identify them in case future mods need
    # them, then ignore them.
    #-----------------------------------------------------------------------
    my ($recDimName,$nvars,$dimsize) = &getFileStats($ncid,$ARGV);
    
    #-----------------------------------------------------------------------
    # If the number of records in the file is zero, then there is no data in
    # this file.  Warn the user and get the next file.
    #-----------------------------------------------------------------------
    if ($dimsize == 0) {
	print "WARNING: File $ARGV contains no records\n";
	return($dimsize,"0");
    }

    #-----------------------------------------------------------------------
    # Read in all the information about the variables in this NetCDF file,
    # i.e. variable name, type, dimensions, attributes.
    #-----------------------------------------------------------------------
    my %var = &getVariableDescriptions($ncid,$nvars,$ARGV);
    my $var = \%var;

    #---------------------------------------------------------------
    # Now read in the data for the variables we are interested in,
    # i.e. the variable we want to put out in the QCF record.
    # Data are in %{$variable}{values} where $variable is each parameter
    # name in &getFields.
    #---------------------------------------------------------------
    &getData($ncid,$recDimName, $var);
    
    #-----------------------------------------------------------------------
    # Let's make sure that all variable arrays are the same length.
    # Currently, all values for a single variable are in an array. In order
    # to successfully transform the matrix to get all values for a single
    # time into a record, we need to make sure our matrix is square and not
    # ragged.
    #
    # When we are done, we will have the correct lenght of the arrays, i.e.
    # the number of records in this file, stored in $arrayLen.
    #-----------------------------------------------------------------------
    my @varList = &getFields;
    my $arrayLen = scalar (@{$var{$varList[0]}{values}});
    if ($arrayLen != $dimsize) {
	print "ERROR: The number of records as listed in the record ";
	print "dimension declaration at the top of the NetCDF file (=";
	print "$dimsize) does not match the actual number of records for";
	print " variable $varList[0] (=$arrayLen)\n";
    }
    my $variable;
    foreach $variable (@varList) {
	if ( scalar(@{$var{$variable}{values}}) != $arrayLen) {
	    print "ERROR: Variable $variable has an incorrect number of ";
	    print "values: ".scalar(@{$var{$variable}{values}})." Should ";
	    print "be $arrayLen\n";
	    exit(1);
	}
    }
    
    #-------------------------------------
    # All done.  Close the input file
    #-------------------------------------
    
    if (NetCDF::close($ncid) == -1) {
	die "Can't close $ARGV:$!\n";
    }


    my $year = 2006;
    $file =~ /([^\/]+)\.nc/;
    my $station = $self->{"stations"}->getStation($1,$self->{"NETWORK"});
    for (my $i = 0; $i < $arrayLen; $i++) {
#    for (my $i = 0; $i < 10000; $i++) {

	$var->{"time"}{"values"}[$i] =~ /(\d+)(\.\d+)?/;
	my ($day,$time) = ($1,$2);
	if (!defined($time)) { $time = 0; }

        my $hour = int($time * 24);
	my $min = int(($time * 24 - $hour) * 60);
	my $sec = sprintf("%.0f",((($time * 24 - $hour) * 60) - $min) * 60);
	
	$time = sprintf("%02d:%02d:%02d",$hour,$min,$sec);
	my $fmt = "HH:MM:SS";

	my $record = Surface::QCFSurfaceRecord->new($self->{"WARN"},$station);
	$record->setReadingTime(sprintf("%04d/%03d",$year,$day),"YYYY/JJJ",$time,$fmt,0);

	foreach my $field (&getFields) {
	    my $value = $var->{$field}{"values"}[$i];

	    if (defined($var->{$field}{"missing_value"}) && !is_missing($value,$var->{$field}{"missing_value"})) {
		$record->setTemperature($value,$var->{$field}{"units"}) if ($field eq "t2");
		$record->setRelativeHumidity($value) if ($var->{$field}{"units"} eq "%" && $field eq "hum");
		$record->setPressure($value,$var->{$field}{"units"}) if ($field eq "press");
		$record->setWindSpeed($value,$var->{$field}{"units"}) if ($field eq "speed");
		$record->setWindDirection($value) if ($field eq "dir" && $var->{$field}{"units"} =~ /deg/i);
	    }
	}

	if ($self->in_time_of_interest($record)) {
	    #print($OUT $record->toQCF_String()) if ($record->getStationId() eq "Notch");
	    $self->add_to_record_map($record,$time);
	}
    }

    return $station;
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
    my @files = sort(grep(/\.nc$/,readdir($RAWDIR)));
    closedir($RAWDIR);

    my $outfile = sprintf("%s/%s.0qc",$self->{"OUT_DIR"},lc($self->clean_for_file_name($self->{"NETWORK"})));
    open(my $OUT,">$outfile") or die("Can't create output file: $outfile\n");

    my $out_all = sprintf("%s/%s.0qc.all",$self->{"OUT_DIR"},lc($self->clean_for_file_name($self->{"NETWORK"})));
    open(my $ALL,">$out_all") or die("Can't create output file: $out_all\n");

    foreach my $file (@files) {
	my $station = $self->parse_raw_file(sprintf("%s/%s",$self->{"RAW_DIR"},$file),$ALL);
	$self->generate_output_files($OUT,$station);

#	last;
    }

    close($OUT);
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

}






##------------------------------------------------------------------------------
# @signature void getFileStats($ARGV)
# <p>Find out the name of the record dimension and how many variables ($nvars)
# are in the input file. The other values read in are not essential to this
# code, so identify them in case future mods need them, then ignore them.
#
# @input  $ncid - the NetCDF ID of the input file from the previous call to
#                 nc_open
# @input  $ARGV - the name of the input file.  Used for error reporting only.
#
# @output $recDimName - the name of the record dimension.
# @output $nvars - the number of variables in the input file.
# @output $dimsize - the number of records in the file
##------------------------------------------------------------------------------
sub getFileStats {
    my $ncid = shift;
    my $input_file = shift;
    
    my $ndims;          # The number of dimensions defined for this NetCDF
                        # input file.
    my $nvars;          # The number of variables defined for this file.
    my $natts;          # The number of global attributes defined for this file.
    my $recdim;         # A pointer to which dimension is the record dimension.
                        # The record dimension is the dimension that contains
                        # an integer giving the number of records in the file.
                        # It is defined dynamically when the file is
                        # written and can grow or shrink as necessary.
    my $recDimName;     # The name of the record dimension.
    my $dimsize;        # The number saved in the record dimension = the number
                        # of records in the file.

    # NetCDF::inquire()
    # Inquire of a NetCDF file how many dimensions, variable, and global
    # attributes it has, and which dimension is it's record dimension.
    #
    # @input - the NetCDF id of the file
    #
    # @output - the number of dimensions, variables, and global attributes
    #           in the file, and the location of the file's record dimension.

    if (NetCDF::inquire($ncid,$ndims,$nvars,$natts,$recdim) == -1) {
        die "Can't inquire of $input_file:$!\n";
    }

    # NetCDF::diminq
    # Inquire of a NetCDF dimension, it's name and size, given a pointer to it.
    #
    # @input - the NetCDF id of the file
    # @input - the location of the file's record dimension
    #
    # @output - the name of the file's record dimension
    # @output - the size of the file's record dimension

    $recdim = 0;

    if (NetCDF::diminq($ncid,$recdim,$recDimName,$dimsize) == -1) {
        die "Can't inquire record dimension of $input_file:$!\n";
    }

#    if (&DEBUGFileStats) {
#        print "The id assigned to $input_file is $ncid\n";
#        print "The total number of dimensions in $input_file is $ndims\n";
#        print "The total number of variables in $input_file is $nvars\n";
#        print "The total number of attributes in $input_file is $natts\n";
#        print "The name of the record dimension is $recDimName\n";
#        print "The size of the record dimension is $dimsize\n";
#    }

    return($recDimName,$nvars,$dimsize);
}
##--------------------------------------------------------------------
# @signature void getVariableDescriptions()
# <p>Read in all the information about the variables in this NetCDF file,
# i.e. variable name, type, dimensions, attributes. Each variable has an
# associated table of information specifying attributes of that variable,
# i.e., the ID of the variable (uniquely specifies this variable - like a
# pointer to that variable), the data type of the data stored in the variable
# (char, float, etc), the number of dimensions of this variable (does it
# contain a scalar, an array, a matrix), the name and size of each dimension,
# and how many other attributes there are, and what they are, i.e. a long
# name for the variable, the units the data in the variable are in, how missing
# is defined, etc.
#
# @input  $ncid - the file id of the input file.
# @input  $nvars
# @input  $ARGV - the name of the input file.  Used for error reporting only.
#
# @output  %{$name} Returns a hash for each variable name that contains:
# <ul>
#    <li> $name = latitude,
#    <li> $var{latitude}{varid} = 22;
#    <li> $var{latitude}{datatype} = float,
#    <li> $var{latitude}{ndims} = 1 ( ${latitude}{recNum}=8446 )
#    <li> $var{latitude}{dimname}[0] = recNum;
#    <li> $var{latitude}{natts} = 5
#    <li> $var{latitude}{long_name} = "latitude"
#    <li> $var{latitude}{units} = "degree_north"
#    <li> $var{latitude}{_FillValue} = 3.40282346638529e+38
#    <li> $var{latitude}{missing_value} = -9999
#    <li> $var{latitude}{reference} = "station table"
# </ul>
#
##--------------------------------------------------------------------
sub getVariableDescriptions {
    my $ncid = shift;
    my $nvars = shift;

    # The NetCDF files encode the data types as a char. Use this hash to
    # unencode these types.
    my %data_types = (2,'char',3,'short',4,'int',5,'float',6,'double');

    # Create a hash for
    # this variable to store all the information about the variable.
    my %var = ();

    # Loop through all the variables in the NetCDF input file.
    for (my $varid = 0;$varid <$nvars; $varid++) {
        my @dimids;

        # Given the NetCDF file ID and the variable ID, find out the variable
        # name, the data type the data is stored as (float, etc), the number
        # of dimensions of this variable, an array of pointers to the dimensions
        # and the number of variable attributes assigned to this variable.
        NetCDF::varinq($ncid,$varid, my $name, my $datatype,my $ndims,\@dimids,
                        my $natts);

        # Now that $name contains the name of the variable, assign all the
        # information about the variable to the hash $var{$name}
        $var{$name}{varid} = $varid;
        $var{$name}{datatype} = $data_types{$datatype};
        $var{$name}{ndims} = $ndims;

        #if (&DEBUGgetV) {print "variable # $varid:\n\tname = $name,";}
        #if (&DEBUGgetV) {print "\n\tdata type = $var{$name}{datatype},\n\t";}
        #if (&DEBUGgetV) {print "number of dimensions = $var{$name}{ndims} ( ";}

        # Loop through each of the dimensions of the variable and determine
        # the dimension name and size.
        for (my $dim = 0;$dim <$var{$name}{ndims}; $dim++) {
            NetCDF::diminq($ncid,$dimids[$dim],my $dimname,my $dimsize);
	      $var{$name}{dimname}[$dim] = $dimname;
	      $var{$name}{$dimname} = $dimsize;
	      #if (&DEBUGgetV) {print "$dimname=$var{$name}{$dimname} ";}
        }

        # Assign the information on the number of attributes to the hash.
        $var{$name}{natts} = $natts;
        #if (&DEBUGgetV) {print ")\n\tNumber of attributes = $var{$name}{natts}\n";}

        # Loop through each of the attributes assigned to this variable and
        # determine the attribute name, type, length, and value.
        for (my $attnum = 0;$attnum <$var{$name}{natts}; $attnum++) {

            # determine attribute name
            my $attname;
            if (NetCDF::attname($ncid,$varid,$attnum,$attname) == -1) {
                die "Can't inquire of attribute name of $ARGV:$!\n";
            }

            # determine attribute data type and length
            my ($atttype, $attlen);
            if (NetCDF::attinq($ncid,$varid,$attname,$atttype,$attlen) == -1) {
                die "Can't inquire of attribute type of $ARGV:$!\n";
            }
            #if (&DEBUGgetV) {print "\t$attname length = $attlen\n";}
            $var{$name}{$attname}{attlen} = $attlen;

            # Convert the attribute type from a number to a descriptive string.
            $var{$name}{atttype} = $data_types{$atttype};

            # determine the attribute value.  The values is read in as an array
            # of numbers.  If the attribute contains a string, the numbers
            # represent chars and we need to pack the chars together to get the
            # string.  If the attribute contains a number, then it should be the
            # first value in the array.
            my @value;
            if (NetCDF::attget($ncid,$varid,$attname,\@value) == -1) {
                die "Can't inquire of value of attribute of $ARGV:$!\n";
            }

            if ($var{$name}{atttype} eq "char") {
                my $str = pack("C*",@value);
                $var{$name}{$attname} = $str;
                #if (&DEBUGgetV)
                #    {print "\t$attname = \"$var{$name}{$attname}\"\n";}
            } elsif ($var{$name}{atttype} eq "int" ||
                     $var{$name}{atttype} eq "short" ||
                     $var{$name}{atttype} eq "float" ||
                     $var{$name}{atttype} eq "double") {
                if ($var{$name}{$attname}{attlen} == 1)
                    {$var{$name}{$attname} = $value[0];}
                else
                    {
                    print "WARNING: Attribute has length > 1: ";
                    print "$var{$name}{$attname}{attlen}\n";
                    exit(1);
                    }
                #if (&DEBUGgetV) {print "\t$attname = $var{$name}{$attname}\n";}
            } else {
                print "WARNING: Unknown attribute type $var{$name}{atttype}\n";
                exit(1);
            }

	    $var{$name}{"units"} =~ s/degC/C/;
        }
    }
    return(%var);
}

##--------------------------------------------------------------------
# @signature void getData()
# <p>Read in all the data for each variable that occurs in the QCF output
# i.e. time, station info, temperature, dewpoint, etc.
# Ignore other variables
#
# @output  %var{$variable}{values} Adds the array values to the hash for
#       each variable
##--------------------------------------------------------------------
sub getData {
    my $ncid = shift;
    my $recDimName = shift;
    my $var = shift;

    my $variable;

    foreach $variable (&getFields) {

        #---------------------------------------------------------------------
        # If the variable has only one dimension, and that dimension is the
        # record dimension, then read it in.
        #---------------------------------------------------------------------
        if ($var->{$variable}{ndims} == 1 &&
            $var->{$variable}{dimname}[0] eq $recDimName) {
            my @values;
            my @flags;
            for (my $recnum=0;$recnum<$var->{$variable}{$recDimName};$recnum++) {
                if (NetCDF::varget($ncid,$var->{$variable}{varid},$recnum,1,
                                   \@values) == -1) {
                    die "Can't get data for variable $variable:$!\n";
                }
                $var->{$variable}{values}[$recnum] = $values[0];
                #if (&DEBUGgetData) {
                #   print "$recnum $variable $var->{$variable}{values}[$recnum]\n";
                #}
            }
	    
        #---------------------------------------------------------------------
        # If the variable has two dimensions, the type is character, and the
        # first dimension is the record dimension, then assume the second
        # dimension is the string length and read it in.
        #---------------------------------------------------------------------
        } elsif ($var->{$variable}{ndims} == 2 &&
                 $var->{$variable}{datatype} eq "char" &&
                 $var->{$variable}{dimname}[0] eq $recDimName){
            my $varLenName = $var->{$variable}{dimname}[1];
            my $varLen = $var->{$variable}{$varLenName};
            my @values = "";
            for (my $recnum=0;$recnum <$var->{$variable}{$recDimName};$recnum++) {
                if (NetCDF::varget($ncid,$var->{$variable}{varid},[$recnum,0],
                                  [1,$varLen], \@values) == -1) {
                    die "Can't get data for variable $variable:$!\n";
                }
                # In Perl, the command:
                #
                #    pack("C", $x)
                #
                # where $x is either less than 0 or more than 255 returns the
                # error "Character in 'C' format wrapped in pack at madis.pl line
                # ####." ; the "C" format is only for encoding native operating
                # system characters (ASCII, EBCDIC, and so on) and not for Unicode
                # characters, so Perl behaved as if you meant
                #
                #        pack("C", $x & 255)
                # If you actually want to pack Unicode codepoints, use the "U"
                # format instead.
                # This information was downloaded from the Perl Diagnostics
                # webpage (perldiag) http://perldoc.perl.org/perldiag.html
                # accessed 11/2/2006.
                my $teststr = pack("U*",@values);
                my $str = pack("C*",@values);
                if ($teststr ne $str) {print STDERR $str."\n";}
                $var->{$variable}{values}[$recnum] = $str;
                #if (&DEBUGgetData) {
                #    print
                #    "$recnum $variable $var->{$variable}{values}[$recnum]\n";
                #}
            }

        #---------------------------------------------------------------------
        # Otherwise, warn the user that we don't know how to read in the
        # variable.
        #---------------------------------------------------------------------
        } else {
            print "CRITICAL ERROR: Don't know how to read in variable: $variable(";
            for (my $dim = 0;$dim <$var->{$variable}{ndims}; $dim++) {
                my $dimname = $var->{$variable}{dimname}[$dim];
              print "$var->{$variable}{dimname}[$dim]=$var->{$variable}{$dimname} ";
            }
            print ")\n";
        }
    }
}
