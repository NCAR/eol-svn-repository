#! /usr/bin/perl -w

##Module-----------------------------------------------------------------
# <p>The Averager module is used to average CLASS formatted sounding data.
# It takes the <code>(n - 1)/2</code> points before and the <code>(n - 1)/2 
# </code> points after the current point and uses the <code>n</code> number
# of points to create the averaged record.</p>
#
# <p>The Averager averages the following values.  All other values are
# calculated from these values.</p>
# <ul>
#   <li>Time</li>
#   <li>Pressure</li>
#   <li>Temperature</li>
#   <li>Relative Humidity</li>
#   <li>U Wind Component</li>
#   <li>V Wind Component</li>
# </ul>
#
# <p>It also makes a number of assumptions about the data.</p>
# <ol>
#   <li>The first (n - 1)/2 points are correct.  These points cannot be
# averaged since there are not enough points before them to create a
# correctly averaged value.</li>
#   <li>The last (n - 1)/2 points are not worth keeping.  These points
# also cannot be averaged since there are not enough points after them
# to create a correctly averaged value.  They are dropped because they 
# are so high in the atmosphere that the values are no longer that 
# important.</li>
# </ol>
#
# @author Joel Clawson
# @version 1.0  Original Creation
##Module-----------------------------------------------------------------
package Averager;
use strict;
use lib "/work/software/conversion_modules/Version3";
use DpgCalculations qw(:DEFAULT);
use Sounding::ClassRecord;
$| = 1;

##-----------------------------------------------------------------------
# @signature void average(String infile, String outfile, int points, int freq)
# <p>Average the data in the infile and put the results in the outfile using
# the number of points for the range to average over.</p>
#
# @input $infile The name of the file to be averaged.
# @input $outfile The name of the file to contain the averaged data.
# @input $points The number of points to use in the average.
# @warning This assumes that the number of points is odd.
# @warning This assumes that the first (points - 1)/2 points are correct, 
# since they cannot be averaged.
# @warning This assumes that the last (points - 1)/2 points are bad and
# will not be included in the outfile since they cannot be averaged.
##-----------------------------------------------------------------------
sub average {
    my ($self,$infile,$outfile,$points,$freq) = @_;

    if ($points % 2 == 0) { 
	printf("Averager cannot average with even number of points %d\n",$points);
	exit(1);
    }

    open(my $IN,sprintf("<%s",$infile)) or die("Cannot read $infile\n");
    open(my $OUT,sprintf(">%s",$outfile)) or die("Cannot open $outfile\n");
    open(my $LOG,sprintf(">>%s",$self->{"logfile"})) or die("Cannot open logfile\n");

    # Copy the header to the output file.
    $self->print_header($IN,$OUT);

    # Define variables that will be needed while looping through the data lines.
    my @record_list = ();
    my $last_out_record;
    my $last_alt_record;
    my $expected_time = 0;

    # Define the function as a variable to prevent from having to pass a function
    # several input parameters.
    my $function = sub {
	my $record = shift;
	
	push(@record_list,$record);
	
	if (scalar(@record_list) < (($points + 1) / 2)) {

	    if ($record->getAltitude() == 99999 && defined($last_alt_record)) {
		$record->setAltitude($self->calculate_altitude($last_alt_record,$record,$LOG),"m");
	    }
	    
	    print($OUT $record->toString());
	    $last_out_record = $record;
	    $last_alt_record = $record if ($record->getPressure() != 9999 &&
					   $record->getTemperature() != 999 &&
					   $record->getAltitude() != 99999);
	} elsif (scalar(@record_list) == $points) {
	    
	    my $averaged_record = $self->create_averaged_record($LOG,$outfile,
								$last_out_record,
								$last_alt_record,@record_list);

#	    printf("Raw : %s",$record_list[$points / 2]->toString());
#	    printf("Avrg: %s\n",$averaged_record->toString());
	    print($OUT $averaged_record->toString());
	    $last_out_record = $averaged_record;
	    $last_alt_record = $averaged_record if ($averaged_record->getPressure() != 9999 &&
						    $averaged_record->getTemperature() != 999 &&
						    $averaged_record->getAltitude() != 99999);
	    
	    shift(@record_list);
	}
    };

    # Loop through all of the data lines
    foreach my $line (<$IN>) {
	my $record = $self->create_record($LOG,$line,$last_out_record,$last_alt_record,$infile);
	
	# Times that are okay to use as they are
	if ($record->getTime() < 0 || $record->getTime() == $expected_time) {
	    $expected_time += $freq if ($record->getTime() >= 0);
	    &$function($record);
	} else {
	    printf($LOG "%s: Unexpected time (%s) for expected time (%s)\n",$infile,
		   $record->getTime(),$expected_time);

	    # Continue through missing times.
	    while ($expected_time < $record->getTime()) {
		my $new_record = Sounding::ClassRecord->new($LOG,$infile);
		$new_record->setTime($expected_time);
		printf($LOG "%s: Creating all missing record for time %s\n",$infile,
		       $new_record->getTime());
		&$function($new_record);
		$expected_time += $freq;
	    }
	    
	    # Handle the case where the missing fill moves the read line to a valid value.
	    if ($expected_time == $record->getTime()) {
		&$function($record);
		$expected_time += $freq;
	    }
	}
    }

    close($LOG);
    close($OUT);
    close($IN);
}

##-----------------------------------------------------------------------
# @signature float average_values(float[] values, float missing)
# <p>Find the average of the values in the array unless a value is missing,
# then return the missing value.</p>
#
# @input values[] The list of values to be averaged.
# @input $missing The missing value to use for the array of values.
# @output $average The average of the values.
##-----------------------------------------------------------------------
sub average_values {
    my ($self,@values) = @_;
    my $missing = $values[-1];

    my $sum = 0;
    for (my $i = 0; $i < scalar(@values) - 1; $i++) {
	return $missing if ($values[$i] == $missing);
	$sum += $values[$i];
    }
    return ($sum / (scalar(@values) - 1));
}

##-----------------------------------------------------------------------
# @signature float calculate_altitude(ClassRecord last, ClassRecord this, FileHandle LOG)
# <p>Calculate the altitude from this and the last record.</p>
#
# @input $last The previous record to use for the calculation.
# @input $this The current record to use for the calculation.
# @input $LOG The FileHandle where errors are to be logged.
##-----------------------------------------------------------------------
sub calculate_altitude {
    my ($self,$last,$this,$LOG) = @_;

#    printf("Last: %sThis: %s",$last->toString(),$this->toString());

    my $value = calculateAltitude($last->getPressure() == 9999 ? undef() : sprintf("%.1f",$last->getPressure()),
			     $last->getTemperature() == 999 ? undef() : sprintf("%.1f",$last->getTemperature()),
			     $last->getDewPoint() == 999 ? undef() : sprintf("%.1f",$last->getDewPoint()),
			     $last->getAltitude() == 99999 ? undef() : sprintf("%.1f",$last->getAltitude()),
			     $this->getPressure() == 9999 ? undef() : sprintf("%.1f",$this->getPressure()),
			     $this->getTemperature() == 999 ? undef() : sprintf("%.1f",$this->getTemperature()),
			     $this->getDewPoint() == 999 ? undef() : sprintf("%.1f",$this->getDewPoint()),
			     1,$LOG);

 #   printf("Last: %f %f %f %f\n",$last->getPressure(),$last->getTemperature(),$last->getDewPoint(),$last->getAltitude());
#    printf("This: %f %f %f %s\n",$this->getPressure(),$this->getTemperature(),$this->getDewPoint(),defined($value) ? $value : "Not Defined");
    return $value;
}

##-----------------------------------------------------------------------
# @signature ClassRecord create_averaged_record(FileHandle LOG, String filename, ClassRecord last_out_record, ClassRecord last_alt_record, ClassRecord[] record_list)
# <p>Create a new record that is the result of averaging the values in 
# record_list.</p>
#
# @input $LOG The FileHandle to the log file.
# @input $filename The filename for the averaged record.
# @input $last_out_record The averaged record that occured right before 
# this record.
# @input $last_alt_record The most recent averaged record that occured 
# before this record that contains all of the data needed to calculate
# the altitude.
# @input record_list[] The list of <i>raw_records</i> that will be used
# to create the averaged record.
# @output $averaged_record The resultant record of averaged values.
##-----------------------------------------------------------------------
sub create_averaged_record {
    my ($self,$LOG,$filename,$last_out_record,$last_alt_record,@record_list) = @_;
    my $averaged_record = Sounding::ClassRecord->new($LOG,$filename,$last_out_record);

    # Create a hash of arrays that contain the values to be averaged.
    my %values;
    foreach my $rec (@record_list) { 
	push(@{$values{"times"}},$rec->getTime());
	push(@{$values{"pressures"}},$rec->getPressure());
	push(@{$values{"temps"}},$rec->getTemperature());
	push(@{$values{"rhs"}},$rec->getRelativeHumidity());
	push(@{$values{"uwinds"}},$rec->getUWindComponent());
	push(@{$values{"vwinds"}},$rec->getVWindComponent());
    }

    # Set the values in the averaged record with the averaged values
    $averaged_record->setTime($self->average_values(@{$values{"times"}},9999));
    $averaged_record->setPressure($self->average_values(@{$values{"pressures"}},9999),"mbar");
    $averaged_record->setTemperature($self->average_values(@{$values{"temps"}},999),"C");
    $averaged_record->setRelativeHumidity($self->average_values(@{$values{"rhs"}},999));
    $averaged_record->setUWindComponent($self->average_values(@{$values{"uwinds"}},9999),"m/s");
    $averaged_record->setVWindComponent($self->average_values(@{$values{"vwinds"}},9999),"m/s");
    
    # Calculate the Altitude manually since the ClassRecord module doesn't do it.
    $averaged_record->setAltitude($self->calculate_altitude($last_alt_record,
							    $averaged_record,$LOG),"m");

    return $averaged_record;
}

##-----------------------------------------------------------------------
# @signature ClassRecord create_record(FileHandle LOG, String line, ClassRecord prev_record, ClassRecord last_alt_record, String filename)
# <p>Create the ClassRecord from a line of data.</p>
#
# @input $LOG The FileHandle to the log file.
# @input $line The line containing the data for the record.
# @input $prev_record The record that occurred directly before this record.
# @input $last_alt_record The most recent record that has the values need for computing an
# altitude.
# @input $filename The filename of the file the record belongs to.
# @output $record The ClassRecord for the data.  (Only contains the values that cannot
# be calculated directly be the record.)
##-----------------------------------------------------------------------
sub create_record {
    my $self = shift;
    my $LOG = shift;
    my @data = split(' ',shift);
    my $prev_record = shift;
    my $last_alt_record = shift;
    my $filename = shift;
    my $record = Sounding::ClassRecord->new($LOG,$filename,$prev_record);

    # Load the data from the data line.
    $record->setTime($data[0]);
    $record->setPressure($data[1],"mbar");
    $record->setTemperature($data[2],"C");
    $record->setRelativeHumidity($data[4]);
    $record->setUWindComponent($data[5],"m/s");
    $record->setVWindComponent($data[6],"m/s");

    my $lon_fmt = $data[10] < 0 ? "-" : "";
    while (length($lon_fmt) < length($data[10])) { $lon_fmt .= "D"; }
    my $lat_fmt = $data[11] < 0 ? "-" : "";
    while (length($lat_fmt) < length($data[11])) { $lat_fmt .= "D"; }

    $record->setLongitude($data[10],$lon_fmt);
    $record->setLatitude($data[11],$lat_fmt);

    # Only want to set the altitude manually if it is the first record,
    # otherwise it should be calculated.
    if (!defined($prev_record)) {
	$record->setAltitude($data[14],"m");
#    } else {
#	$record->setAltitude($self->calculate_altitude($last_alt_record,
#						       $record,$LOG),"m");
    }

    return $record;
}

##-----------------------------------------------------------------------
# @signature Averager new(String log_file)
# <p>Create a new Averager with the specified log file for warnings.</p>
#
# @input $log_file The name of the log file for warnings.
##-----------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = $invocant || ref($invocant);
    bless($self,$class);

    ($self->{"logfile"}) = @_;
    open(my $LOG,sprintf(">%s",$self->{"logfile"})) or
	die(sprintf("Cannot open the log file: %s\n",$self->{"logfile"}));
    close($LOG);

    return $self;
}

##-----------------------------------------------------------------------
# @signature void print_header(FileHandle IN, FileHandle OUT);
# <p>Print the header from the input file handle to the output file
# handle.</p>
#
# @input $IN The file handle to be read.
# @input $OUT The file handle to be written to.
##-----------------------------------------------------------------------
sub print_header {
    my ($self,$IN,$OUT) = @_;

    for (my $i = 0; $i < 15; $i++) {
	my $line = <$IN>;
	print($OUT $line);
    }
}
