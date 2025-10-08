#! /usr/bin/perl -w

package AFRL_5mb_Extracter;



use strict;
use lib "/net/work/software/TREX/library/conversion_modules/Version6";
use lib "/work/software/TREX/library/conversion_modules/Version6";
use Sounding::ClassConstants;
use Sounding::ClassHeader;
use Sounding::ClassRecord;

&main();

sub main {
    my $extracter = AFRL_5mb_Extracter->new();

    open(my $WARN,">".$extracter->{"warning_file"}) or die("Can't open warning file\n");
    open(my $PRESS,">".$extracter->{"press_warn_file"}) or die("Can't open pressure warning file.\n");

    foreach my $file ($extracter->get_class_files()) {
	printf("Processing file: %s\n",$file);

	my @headers = (); my @records = ();
	$extracter->read_file($PRESS,$file,\@headers,\@records);

	$extracter->interpolate_pressures(\@records);

	$extracter->output_pressure_file($file,\@headers,\@records);

	$extracter->create_5mb_file($WARN,$file,\@headers,\@records);
    }

    close($PRESS);
    close($WARN);
}

sub new {
    my $invocant = shift;
    my $class = ref($invocant) || $invocant;
    my $self = {};
    bless($self,$class);

    $self->{"INT_DIR"} = "../pressure_files/AFRL";
    $self->{"RAW_DIR"} = "../data/AFRL";
    $self->{"OUT_DIR"} = "../5mb/AFRL";
    $self->{"press_warn_file"} = $self->{"INT_DIR"}."/warning.log";
    $self->{"warning_file"} = $self->{"OUT_DIR"}."/5mb_warning.log";

    $self->{"FLAG_PREC"}->{sprintf("%d",$UNCHECKED_FLAG)} = 6;
    $self->{"FLAG_PREC"}->{sprintf("%d",$MISSING_FLAG)} = 5;
    $self->{"FLAG_PREC"}->{sprintf("%d",$ESTIMATE_FLAG)} = 4;
    $self->{"FLAG_PREC"}->{sprintf("%d",$BAD_FLAG)} = 3;
    $self->{"FLAG_PREC"}->{sprintf("%d",$QUESTIONABLE_FLAG)} = 2;
    $self->{"FLAG_PREC"}->{sprintf("%d",$GOOD_FLAG)} = 1;

    return $self;
}

sub create_5mb_file {
    my ($self,$WARN,$file,$headers,$records) = @_;

    $file =~ s/\.cls/\.05mb/;
    $file =~ s/\.gz$//;
    open(my $OUT,sprintf(">%s/%s",$self->{"OUT_DIR"},$file)) or die("Can't create $file\n");

    foreach my $line (@{$headers}) {
	print($OUT $line);
    }

    print($OUT $records->[0]->toString());
    my $next5mbPressure = (sprintf("%d",$records->[0]->getPressure())) -
	$records->[0]->getPressure() % 5;

    while ($next5mbPressure >= $records->[-1]->getPressure()) {
	my $record = Sounding::ClassRecord->new($WARN,$file);
	$record->setPressure($next5mbPressure,"mb");

	$record->setPressureFlag($self->interpolate_pressure_flag($next5mbPressure,$records));

	$record->setAltitude($self->interpolate_altitude($next5mbPressure,$records),"m");

	my ($temp,$temp_flag) = $self->interpolate_temperature($next5mbPressure,$records);
	$record->setTemperature($temp,"C");
	$record->setTemperatureFlag($temp_flag);

	my ($rh,$rh_flag) = $self->interpolate_relative_humidity($next5mbPressure,$records);
	$record->setRelativeHumidity($rh);
	$record->setRelativeHumidityFlag($rh_flag);

	my ($uwind,$vwind,$wind_flag) = $self->interpolate_winds($next5mbPressure,$records);
	$record->setUWindComponent($uwind,"m/s");
	$record->setUWindComponentFlag($wind_flag);
	$record->setVWindComponent($vwind,"m/s");
	$record->setVWindComponentFlag($wind_flag);
	

	print($OUT $record->toString());

	$next5mbPressure -= 5;
    }

    close($OUT);
}

sub determine_flag_precedence {
    my ($self,$current,$high,$low) = @_;

    $current = sprintf("%d",$current);
    $high = sprintf("%d",$high);
    $low = sprintf("%d",$low);

    my $worst = $self->{"FLAG_PREC"}->{$high} > $self->{"FLAG_PREC"}->{$low} ? $high : $low;

    if ($current != $UNCHECKED_FLAG) {
	$worst = $self->{"FLAG_PREC"}->{$current} > $self->{"FLAG_PREC"}->{$worst} ? $current : $worst;
    }

    return $worst;
}

sub get_class_files {
    my ($self) = @_;

    opendir(my $RAW,$self->{"RAW_DIR"}) or die("Can't open raw data directory.");
    my @files = grep(/\.cls(\.gz)?$/,readdir($RAW));
    closedir($RAW);

    return @files;
}

sub interpolate_altitude {
    my ($self,$pressure,$records) = @_;

    for (my $i = 0; $i < @{$records}; $i++) {

	# There is an existing record on the 5mb pressure.
	if ($records->[$i]->getPressure() == $pressure && $records->[$i]->getAltitude() != 99999) {
	    return $records->[$i]->getAltitude();
	}

	# The record has passed the 5mb pressure and has a non-missing altitude, 
	# so this will be the low end of the weighted average calculation.
	elsif ($records->[$i]->getPressure() < $pressure && $records->[$i]->getAltitude() != 99999) {

	    my ($high,$low);
	    $low = $records->[$i];

	    # With the low pressure value found, search the records in the reverse order from
	    # the low record to find the first record with a higher pressure than the 5mb pressure
	    # with a non-missing altitude.
	    for (my $h = $i - 1; $h >= 0; $h--) {
		if ($records->[$h]->getPressure() > $pressure && $records->[$h]->getAltitude() != 99999) {
		    $high = $records->[$h];
		    last;
		}
	    }

	    # Now that the two records have been found, the weighted average can be calculated.
	    return ($high->getAltitude()*(1 - (($high->getPressure() - $pressure)/($high->getPressure() - $low->getPressure()))) + $low->getAltitude()*(1 - (($pressure - $low->getPressure()) / ($high->getPressure() - $low->getPressure()))));

	}

	# The 5mb pressure has not been reached so ignore the record.
	else {}
    }
}

sub interpolate_pressure_flag {
    my ($self,$pressure,$records) = @_;

    for (my $i = 0; $i < @{$records}; $i++) {

	# There is an existing record on the 5mb pressure.
	if ($records->[$i]->getPressure() == $pressure) {
	    return $records->[$i]->getPressureFlag();
	}

	# The record has passed the 5mb pressure and has a non-missing temperature, 
	# so this will be the low end of the weighted average calculation.
	elsif ($records->[$i]->getPressure() < $pressure && $records->[$i]->getTemperature() != 999) {

	    my ($high,$low);
	    $low = $records->[$i];

	    # With the low pressure value found, search the records in the reverse order from
	    # the low record to find the first record with a higher pressure than the 5mb pressure
	    # with a non-missing temperature.
	    for (my $h = $i - 1; $h >= 0; $h--) {
		if ($records->[$h]->getPressure() > $pressure && $records->[$h]->getTemperature() != 999) {
		    $high = $records->[$h];
		    last;
		}
	    }

#	    printf($low->toString());
#	    printf($high->toString());

	    # Finally, determine what the temperature flag should be.
	    return $self->determine_flag_precedence($high->getPressure() - $low->getPressure() > 5 ? $QUESTIONABLE_FLAG : $UNCHECKED_FLAG,$high->getPressureFlag(),$low->getPressureFlag());
	}

	# The 5mb pressure has not been reached so ignore the record.
	else {}
    }
}

sub interpolate_pressures {
    my ($self,$records) = @_;

    for (my $i = 0; $i < @{ $records}; $i++) {
	if ($records->[$i]->getPressure() == 9999) {

	    # Determine the indicies of the records to be used in the interpolation
	    my $highIndex = $i - 1;
	    my $lowIndex = $i + 1;
	    if (!defined($records->[$lowIndex])) {
		delete($records->[$i]);
		return;
	    }

	    while ($records->[$lowIndex]->getPressure() == 9999) { 
		$lowIndex++; 
		if (!defined($records->[$lowIndex])) {
		    for (my $j = $i; $j < @{ $records}; $j++) {
			delete($records->[$j]);
		    }
		    return;
		}
	    }

	    # Set the records being used in the interpolation.
	    my $high = $records->[$highIndex];
	    my $current = $records->[$i];
	    my $low = $records->[$lowIndex];

	    # Interpolate the pressure using a weighted average:
            #
            # A = Altitude, P = Pressure, c = current, l = low, h = high
	    #
	    #           (    (Ac - Ah))        (    (Al - Al))
            # Pc = Ph * (1 - (-------)) + Pl * (1 - (-------))
            #           (    (Al - Ah))        (    (Al - Ah))
            #
	    $current->setPressure($high->getPressure()*(1 - ($current->getAltitude() - $high->getAltitude()) / ($low->getAltitude() - $high->getAltitude())) + $low->getPressure()*(1 - ($low->getAltitude() - $current->getAltitude()) / ($low->getAltitude() - $high->getAltitude())),"mb");
	    
	    # Now there is a pressure for this record, change the flag so it is no longer missing
	    $current->setPressureFlag($UNCHECKED_FLAG);

	    # Determine the new flag for the pressure.
	    if ($high->getPressure() - $low->getPressure() > 5) {
		$current->setPressureFlag($QUESTIONABLE_FLAG);
	    }

	    # Determine the flag that should be set for the interpolated pressure.
	    $current->setPressureFlag($self->determine_flag_precedence($current->getPressureFlag(),$high->getPressureFlag(),$low->getPressureFlag()));	    
	}
    }
}

sub interpolate_relative_humidity {
    my ($self,$pressure,$records) = @_;

    for (my $i = 0; $i < @{$records}; $i++) {

	# There is an existing record on the 5mb pressure.
	if ($records->[$i]->getPressure() == $pressure && $records->[$i]->getRelativeHumidity() != 999) {
	    return ($records->[$i]->getRelativeHumidity(),$records->[$i]->getRelativeHumidityFlag());
	}

	# The record has passed the 5mb pressure and has a non-missing rh, 
	# so this will be the low end of the weighted average calculation.
	elsif ($records->[$i]->getPressure() < $pressure && $records->[$i]->getRelativeHumidity() != 999) {

	    my ($high,$low);
	    $low = $records->[$i];

	    # With the low pressure value found, search the records in the reverse order from
	    # the low record to find the first record with a higher pressure than the 5mb pressure
	    # with a non-missing relative humidity.
	    for (my $h = $i - 1; $h >= 0; $h--) {
		if ($records->[$h]->getPressure() > $pressure && $records->[$h]->getRelativeHumidity() != 999) {
		    $high = $records->[$h];
		    last;
		}
	    }

	    # Now that the two records have been found, the weighted average can be calculated.
	    my $rh = ($high->getRelativeHumidity()*(1 - (($high->getPressure() - $pressure)/($high->getPressure() - $low->getPressure()))) + $low->getRelativeHumidity()*(1 - (($pressure - $low->getPressure()) / ($high->getPressure() - $low->getPressure()))));

	    # Finally, determine what the temperature flag should be.
	    my $rh_flag = $self->determine_flag_precedence($high->getPressure() - $low->getPressure() > 5 ? $QUESTIONABLE_FLAG : $UNCHECKED_FLAG,$high->getRelativeHumidityFlag(),$low->getRelativeHumidityFlag());

	    return ($rh,$rh_flag);
	}

	# The 5mb pressure has not been reached so ignore the record.
	else {}
    }
}

sub interpolate_temperature {
    my ($self,$pressure,$records) = @_;

    for (my $i = 0; $i < @{$records}; $i++) {

	# There is an existing record on the 5mb pressure.
	if ($records->[$i]->getPressure() == $pressure && $records->[$i]->getTemperature() != 999) {
	    return ($records->[$i]->getTemperature(),$records->[$i]->getTemperatureFlag());
	}

	# The record has passed the 5mb pressure and has a non-missing temperature, 
	# so this will be the low end of the weighted average calculation.
	elsif ($records->[$i]->getPressure() < $pressure && $records->[$i]->getTemperature() != 999) {

	    my ($high,$low);
	    $low = $records->[$i];

	    # With the low pressure value found, search the records in the reverse order from
	    # the low record to find the first record with a higher pressure than the 5mb pressure
	    # with a non-missing temperature.
	    for (my $h = $i - 1; $h >= 0; $h--) {
		if ($records->[$h]->getPressure() > $pressure && $records->[$h]->getTemperature() != 999) {
		    $high = $records->[$h];
		    last;
		}
	    }

	    # Now that the two records have been found, the weighted average can be calculated.
	    my $temp = ($high->getTemperature()*(1 - (($high->getPressure() - $pressure)/($high->getPressure() - $low->getPressure()))) + $low->getTemperature()*(1 - (($pressure - $low->getPressure()) / ($high->getPressure() - $low->getPressure()))));

	    # Finally, determine what the temperature flag should be.
	    my $temp_flag = $self->determine_flag_precedence($high->getPressure() - $low->getPressure() > 5 ? $QUESTIONABLE_FLAG : $UNCHECKED_FLAG,$high->getTemperatureFlag(),$low->getTemperatureFlag());

	    return ($temp,$temp_flag);
	}

	# The 5mb pressure has not been reached so ignore the record.
	else {}
    }
}

sub interpolate_winds {
    my ($self,$pressure,$records) = @_;

    for (my $i = 0; $i < @{$records}; $i++) {

	# There is an existing record on the 5mb pressure.
	if ($records->[$i]->getPressure() == $pressure && $records->[$i]->getUWindComponent() != 9999 && $records->[$i]->getVWindComponent() != 9999) {
	    return ($records->[$i]->getUWindComponent(),
		    $records->[$i]->getVWindComponent(),
		    $self->determine_flag_precedence($UNCHECKED_FLAG,
						     $records->[$i]->getUWindComponentFlag(),
						     $records->[$i]->getVWindComponentFlag()));
	}

	# The record has passed the 5mb pressure and has a non-missing wind component values, 
	# so this will be the low end of the weighted average calculation.
	elsif ($records->[$i]->getPressure() < $pressure && $records->[$i]->getUWindComponent() != 9999 && $records->[$i]->getVWindComponent() != 9999) {

	    my ($high,$low);
	    $low = $records->[$i];

	    # With the low pressure value found, search the records in the reverse order from
	    # the low record to find the first record with a higher pressure than the 5mb pressure
	    # with non-missing wind component values.
	    for (my $h = $i - 1; $h >= 0; $h--) {
		if ($records->[$h]->getPressure() > $pressure && $records->[$h]->getUWindComponent() != 9999 && $records->[$h]->getVWindComponent != 9999) {
		    $high = $records->[$h];
		    last;
		}
	    }

	    # Couldn't find a higher pressure record with non-missing winds, so return with null to automatically set it to missing.
	    return (undef(),undef(),undef()) if (!defined($high));

	    # Now that the two records have been found, the weighted average can be calculated.
	    my $uwind = ($high->getUWindComponent()*(1 - (($high->getPressure() - $pressure)/($high->getPressure() - $low->getPressure()))) + $low->getUWindComponent()*(1 - (($pressure - $low->getPressure()) / ($high->getPressure() - $low->getPressure()))));
	    my $vwind = ($high->getVWindComponent()*(1 - (($high->getPressure() - $pressure)/($high->getPressure() - $low->getPressure()))) + $low->getVWindComponent()*(1 - (($pressure - $low->getPressure()) / ($high->getPressure() - $low->getPressure()))));

	    # Finally, determine what the wind flag should be.
	    my $uwind_flag = $self->determine_flag_precedence($high->getPressure() - $low->getPressure() > 5 ? $QUESTIONABLE_FLAG : $UNCHECKED_FLAG,$high->getUWindComponentFlag(),$low->getUWindComponentFlag());
	    my $vwind_flag = $self->determine_flag_precedence($high->getPressure() - $low->getPressure() > 5 ? $QUESTIONABLE_FLAG : $UNCHECKED_FLAG,$high->getVWindComponentFlag(),$low->getVWindComponentFlag());

	    my $wind_flag = $self->determine_flag_precedence($UNCHECKED_FLAG,$uwind_flag,$vwind_flag);

	    return ($uwind,$vwind,$wind_flag);
	}

	# The 5mb pressure has not been reached so ignore the record.
	else {}
    }
}

sub output_pressure_file {
    my ($self,$file,$headers,$records) = @_;

    $file =~ s/\.gz$//;
    open(my $OUT,sprintf(">%s/%s",$self->{"INT_DIR"},$file)) or die("Can't create pressure file: $file\n");
    
    foreach my $line (@{ $headers} ) {
	print($OUT $line);
    }

    foreach my $record (@{ $records}) {
	print($OUT $record->toString()) if (defined($record));
    }

    close($OUT);
}

sub read_file {
    my ($self,$WARN,$file,$headers,$records) = @_;
    my $gzip = 0;

    if ($file =~ /(.+)\.gz$/) {
	$gzip = 1;
	system(sprintf("gunzip %s/%s",$self->{"RAW_DIR"},$file)) == 0 or die("Can't gunzip $file\n");
	$file = $1;
    }

    open(my $FILE,sprintf("%s/%s",$self->{"RAW_DIR"},$file)) or die("Can't read file: $file\n");

    for (my $i = 0; $i < 15; $i++) {
	my $line = <$FILE>;
	push(@{ $headers},$line);
    }

    while ((my $line = <$FILE>)) {
	chomp($line);
	my @data = split(' ',$line);

	my $record = Sounding::ClassRecord->new($WARN,$file);
	$record->setTime($data[0]);
	$record->setPressure($data[1],"mb");
	$record->setTemperature($data[2],"C");
	$record->setRelativeHumidity($data[4]);
	$record->setUWindComponent($data[5],"m/s");
	$record->setVWindComponent($data[6],"m/s");
	$record->setAltitude($data[14],"m");
	$record->setPressureFlag($data[15]);
	$record->setTemperatureFlag($data[16]);
	$record->setRelativeHumidityFlag($data[17]);
	$record->setUWindComponentFlag($data[18]);
	$record->setVWindComponentFlag($data[19]);

	push(@{ $records},$record);
    }

    close($FILE);

    if ($gzip == 1) {
	system(sprintf("gzip %s/%s",$self->{"RAW_DIR"},$file)) == 0 or die("Can't gzip file: $file\n");
    }
}
