#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The ClassSounding.pm module is the representation of a sounding in CLASS
# format.  It is a single ascension with multiple readings after the sounding
# began.</p>
#
# @author Joel Clawson
# @version 1.0 Original Creation
##Module------------------------------------------------------------------------
package ClassSounding;
use strict;
use Conversions;
use Station;


##------------------------------------------------------------------------------
# @signature String getActualReleaseDate()
# <p>Get the date of the time when the sounding was actually released.</p>
#
# @output $date The release date of the sonde.
##------------------------------------------------------------------------------
sub getActualReleaseDate {
    my $self = shift;
    return defined($self->{"act_date"}) ? $self->{"act_date"} : "9999/99/99";
}

##------------------------------------------------------------------------------
# @signature String getActualReleaseTime()
# <p>Get the time of the date when the sounding was actually released.</p>
#
# @output $time The release time of the sonde.
##------------------------------------------------------------------------------
sub getActualReleaseTime {
    my $self = shift;
    return defined($self->{"act_time"}) ? $self->{"act_time"} : "99:99:99";
}

##------------------------------------------------------------------------------
# @signature float getAltitude(float time)
# <p>Get the altitude of the sounding at the specified time.</p>
#
# @input $time The time after launch..
# @output $alt The altitude of the sounding.  Default 99999.0
##------------------------------------------------------------------------------
sub getAltitude {
    my $self = shift;

    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"altitude"}) ?
	$self->{"data"}->{$index}->{"altitude"} : 99999.0;
}

##------------------------------------------------------------------------------
# @signature float getAscensionRate(float time)
# <p>Get the ascension rate of the sounding.  This can be negative after the
# balloon pops or if the sounding is a dropsonde.</p>
# <p>If the ascension rate is not set, the function will attempt to calculate
# the rate based off of the difference in height and the change in time since
# the last reading.</p>
#
# @input $time The time after launch.
# @output $rate The ascension rate of the sounding.  Default 999.0
# @warning If this function is called before all of the sounding data has been
# loaded into the module, it is possible for it to return an invalid value.
# This would happen if the module does not know about the real previous record
# because it was not yet loaded.
##------------------------------------------------------------------------------
sub getAscensionRate {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $rate = $self->{"data"}->{$index}->{"ascension_rate"};

    # Don't need to try and calculate the rate.
    if (defined($rate) && $rate != 999) { return $rate; }

    my @times = @{ $self->{"data"}->{"times"}};

    # Calculate the rate.
    if ($index > 0 && $self->getAltitude($times[$index]) != 99999 &&
	$self->getAltitude($times[$index-1]) != 99999 &&
	$times[$index] != $times[$index-1]) {
	
	return (($self->getAltitude($times[$index]) - $self->getAltitude($times[$index-1])) /
		($times[$index] - $times[$index-1]));
    }
    return 999.0;
}

##------------------------------------------------------------------------------
# @signature float getAscensionRateFlag(float time)
# <p>Get the QC flag for the ascension rate.</p>
#
# @input $time The time after launch.
# @output $flag The QC flag for the ascension rate.  Default: 99.0
##------------------------------------------------------------------------------
sub getAscensionRateFlag {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"ascension_rate_flag"}) ?
	$self->{"data"}->{$index}->{"ascension_rate_flag"} : 99.0;
}

##------------------------------------------------------------------------------
# @signature float getDewPoint(float time)
# <p>Get the dew point of the sounding at the specified time after launch.</p>
# <p>If the dew point has not been set or is set to the missing value, the 
# function will try to calculate it from the relative humidity and temperature.</p>
#
# @input $time The time after launch.
# @output $dp The dew point in &deg;C.  Default 999.0;
##------------------------------------------------------------------------------
sub getDewPoint {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $dp = $self->{"data"}->{$index}->{"dew_point"};

    # Don't need to calculate.
    if (defined($dp) && $dp != 999) { return $dp; }

    # Try to calculate the dew point.
    my $rh = $self->{"data"}->{$index}->{"rel_humid"};
    if ($self->getTemperature($_[0]) != 999 && defined($rh) && $rh != 999) {
	return Conversions::calculateDewPoint($self->getTemperature($_[0]),$rh,
					      1,$self->{"warn"});
    }
    return 999.0
}

##------------------------------------------------------------------------------
# @signature String getId()
# <p>Get the station id that is used for the station that launched this sounding.</p>
#
# @output $stn_id The station id.
##------------------------------------------------------------------------------
sub getId {
    my $self = shift;
    return defined($self->{"stn_id"}) ? $self->{"stn_id"} : 
	$self->{"station"}->getStationId();
}

##------------------------------------------------------------------------------
# @signature float getLatitude(float time)
# <p>Get the latitude of the sounding at the specified time after launch.</p>
#
# @input $time The time after launch.
# @output $lat The latitude in degrees.  Default 999.0
##------------------------------------------------------------------------------
sub getLatitude {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return (defined($self->{"data"}->{$index}->{"latitude"})) ?
	$self->{"data"}->{$index}->{"latitude"} : 999.0;
}

##------------------------------------------------------------------------------
# @signature float getLongitude(float time)
# <p>Get the longitude of the sounding at the specified time after launch.</p>
#
# @input $time The time after launch.
# @output $long The longitude in degrees.  Default 9999.0
##------------------------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return (defined($self->{"data"}->{$index}->{"longitude"})) ?
	$self->{"data"}->{$index}->{"longitude"} : 9999.0;
}

##------------------------------------------------------------------------------
# @signature String getNominalReleaseDate()
# <p>Get the nominal release date for the sounding.</p>
#
# @output $date The nominal release date.
##------------------------------------------------------------------------------
sub getNominalReleaseDate {
    my $self = shift;
    return (defined($self->{"nom_date"})) ? $self->{"nom_date"} : "9999/99/99";
}

##------------------------------------------------------------------------------
# @signature String getNominalReleaseTime()
# <p>Get the nominal release time for the sounding.</p>
#
# @output $time The nominal release time.
##------------------------------------------------------------------------------
sub getNominalReleaseTime {
    my $self = shift;
    return defined($self->{"nom_time"}) ? $self->{"nom_time"} : "99:99:99";
}

##------------------------------------------------------------------------------
# @signature float getPressure(float time)
# <p>Get the pressure of the sounding at the specified time after launch.</p>
#
# @input $time The time after launch.
# @output $press The pressure in mb.  Default 9999.0
##------------------------------------------------------------------------------
sub getPressure {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"pressure"}) ? 
	$self->{"data"}->{$index}->{"pressure"} : 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getPressureFlag(float time)
# <p>Get the QC flag for the pressure of the sounding.</p>
#
# @input $time The time after launch.
# @output $flag The QC flag for the pressure.  Default 99.0
##------------------------------------------------------------------------------
sub getPressureFlag {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"pressure_flag"}) ?
	$self->{"data"}->{$index}->{"pressure_flag"} : 99.0;
}

##------------------------------------------------------------------------------
# @signature String getProjectId()
# <p>Get the identifier for the project for this sounding.</p>
#
# @output $id The project id.
##------------------------------------------------------------------------------
sub getProjectId {
    my $self = shift;
    return defined($self->{"proj_id"}) ? $self->{"proj_id"} : "";
}

##------------------------------------------------------------------------------
# @signature float getRelativeHumidity(float time)
# <p>Get the relative humidity of the sounding at the specified time after
# launch.</p>
# <p>If the relative humidity has not been set or has been set to missing, this
# function will try to calculate it from the temperature and the dew point.</p>
#
# @input $time The time after launch.
# @output $rh The relative humidity in %.  Default 999.0
##------------------------------------------------------------------------------
sub getRelativeHumidity {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $rh = $self->{"data"}->{$index}->{"rel_humid"};
    if (defined($rh) && $rh != 999) { return $rh; }

    my $dp = $self->{"data"}->{$index}->{"dew_point"};
    if ($self->getTemperature($_[0]) != 999 && defined($dp) && $dp != 999) {
	return Conversions::calculateRelativeHumidity($self->getTemperature($_[0]),$dp,1,
						      $self->{"warn"});
    }
    return 999.0
}

##------------------------------------------------------------------------------
# @signature float getRelativeHumidityFlag(float time)
# <p>Get the QC flag for hte relative humidity of the sounding.</p>
#
# @input $time The time after launch.
# @output $flag The QC flag for the relative humidity.  Default 99.0
##------------------------------------------------------------------------------
sub getRelativeHumidityFlag {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"rel_humid_flag"}) ?
	$self->{"data"}->{$index}->{"rel_humid_flag"} : 99.0;
}

##------------------------------------------------------------------------------
# @signature float getReleaseAltitude()
# <p>Get the altitude from which the sounding was released.</p>
#
# @output $alt The altitude in meters.
##------------------------------------------------------------------------------
sub getReleaseAltitude {
    my $self = shift;
    return defined($self->{"elevation"}) ? $self->{"elevation"} :
	$self->{"station"}->getElevation();
}

##------------------------------------------------------------------------------
# @signature float getReleaseLatitude()
# <p>Get the latitude from which the sounding was released.</p>
#
# @output $lat The latitude in degrees.
##------------------------------------------------------------------------------
sub getReleaseLatitude {
    my $self = shift;
    return defined($self->{"latitude"}) ? $self->{"latitude"} :
	$self->{"station"}->getLatitude();
}

##------------------------------------------------------------------------------
# @signature float getReleaseLocation()
# <p>Get the longitude from which the sounding was released.</p>
#
# @output $lon The longitude in degrees.
##------------------------------------------------------------------------------
sub getReleaseLongitude {
    my $self = shift;
    return defined($self->{"longitude"}) ? $self->{"longitude"} :
	$self->{"station"}->getLongitude();
}

##------------------------------------------------------------------------------
# @signature String getReleaseSite()
# <p>Get the name and information of the site where the sounding was released.</p>
#
# @output $site The site information.
##------------------------------------------------------------------------------
sub getReleaseSite {
    my $self = shift;
    return defined($self->{"site"}) ? $self->{"site"} :
	sprintf("%s %s",$self->{"station"}->getStationId(),
		$self->{"station"}->getStationName());
}

##------------------------------------------------------------------------------
# @signature float getTemperature(float time)
# <p>Get the temperature for the sounding at the specified time after launch.</p>
#
# @input $time The time after launch.
# @output $temp The temperature in &deg;C.  Default 999.0
##------------------------------------------------------------------------------
sub getTemperature {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"temperature"}) ? 
	$self->{"data"}->{$index}->{"temperature"} : 999.0;
}

##------------------------------------------------------------------------------
# @signature float getTemperatureFlag(float time)
# <p>Get the QC flag for the temperature of the sounding.</p>
#
# @input $time The time after launch.
# @output $flag The QC flag for the temperature.  Default 99.0
##------------------------------------------------------------------------------
sub getTemperatureFlag {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"temperature_flag"}) ?
	$self->{"data"}->{$index}->{"temperature_flag"} : 99.0;
}

##------------------------------------------------------------------------------
# @signature String getType()
# <p>Get the type of data in the class file.</p>
#
# @output $type The type of the data.
##------------------------------------------------------------------------------
sub getType {
    my $self = shift;
    return defined($self->{"type"}) ? $self->{"type"} : "";
}

##------------------------------------------------------------------------------
# @signature float getUWindComponent(float time)
# <p>Get the U wind component of the sounding at the specified time after
# launch.</p>
# <p>This function will try to calculate the U wind component from wind speed
# and wind direction if the value was not set or was set to missing.</p>
#
# @input $time The time after launch.
# @output $uwind The U wind component in m/s.  Default 9999.0
##------------------------------------------------------------------------------
sub getUWindComponent {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $uwind = $self->{"data"}->{$index}->{"uwind"};

    if (defined($uwind) && $uwind != 9999) { return $uwind; }

    my $spd = $self->{"data"}->{$index}->{"wind_spd"};
    my $dir = $self->{"data"}->{$index}->{"wind_dir"};

    if (defined($spd) && $spd != 999 && defined($dir) && $dir != 999) {
	return (Conversions::calculateUVfromWind($spd,$dir))[0];
    }
    return 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getUWindComponentFlag(float time)
# <p>Get the QC flag for the U wind component.</p>
#
# @input $time The time after launch.
# @output $flag The QC flag for the U wind component.  Default 99.0
##------------------------------------------------------------------------------
sub getUWindComponentFlag {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"uwind_flag"}) ?
	$self->{"data"}->{$index}->{"uwind_flag"} : 99.0;
}

##------------------------------------------------------------------------------
# @signature String getVariableParameterName(int index)
# <p>Get the name of the specified variable parameter.</p>
#
# @input $index The index of the variable parameter.
# @output $name The name of the parameter.  Default ""
##------------------------------------------------------------------------------
sub getVariableParameterName {
    my $self = shift;
    my $name = $self->{"var_param_name_".$_[0]};
    return (defined($name)) ? $name : "";
}

##------------------------------------------------------------------------------
# @signature String getVariableParameterUnit(int index)
# <p>Get the unit name of the specified variable parameter.</p>
#
# @input $index The index of the variable parameter.
# @output $unit The unit name of the parameter.  Default ""
##------------------------------------------------------------------------------
sub getVariableParameterUnit {
    my $self = shift;
    my $unit = $self->{"var_param_unit_".$_[0]};
    return (defined($unit)) ? $unit : "";
}

##------------------------------------------------------------------------------
# @signature float getVariableParameterValue(float time, int index)
# <p>Get the value of a the specified variable parameter at the specified time.</p>
#
# @input $time The time after launch.
# @input $index The index of the variable parameter.
# @output $value The value of the parameter.  Default 999.0
##------------------------------------------------------------------------------
sub getVariableParameterValue {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $val = $self->{"data"}->{$index}->{"var_param_".$_[1]};
    return defined($val) ? $val : 999.0;
}

##------------------------------------------------------------------------------
# @signature float getVWindComponent(float time)
# <p>Get the V wind component for the sounding.</p>
# <p>If the V wind component is not set or is set to missing, this function
# will try to calculate the value from the wind speed and wind direction.</p>
#
# @input $time The time after launch.
# @output $vwind The V wind component in m/s.  Default 9999.0
##------------------------------------------------------------------------------
sub getVWindComponent {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $vwind = $self->{"data"}->{$index}->{"vwind"};

    # Has a value, don't need to calculate.
    if (defined($vwind) && $vwind != 9999) { return $vwind; }

    my $spd = $self->{"data"}->{$index}->{"wind_spd"};
    my $dir = $self->{"data"}->{$index}->{"wind_dir"};

    # Calculate only on non-missing values.
    if (defined($spd) && $spd != 999 && defined($dir) && $dir != 999) {
	return (Conversions::calculateUVfromWind($spd,$dir))[1];
    }
    return 9999.0;
}

##------------------------------------------------------------------------------
# @signature float getVWindComponentFlag(float time)
# <p>Get the QC flag for the V wind component.</p>
#
# @input $time The time after launch.
# @output $flag The QC flag for the V wind component.  Default 99.0
##------------------------------------------------------------------------------
sub getVWindComponentFlag {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    return defined($self->{"data"}->{$index}->{"vwind_flag"}) ?
	$self->{"data"}->{$index}->{"vwind_flag"} : 99.0;
}

##------------------------------------------------------------------------------
# @signature float getWindDirection(float time)
# <p>Get the wind direction for the sounding at the specified time after launch.</p>
# <p>This function will try to calculate the wind direction from the U and V 
# wind components if the value was not set or was set to missing.</p>
#
# @input $time The time after launch.
# @output $dir The wind direction in degrees.  Default 999.0
##------------------------------------------------------------------------------
sub getWindDirection {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $dir = $self->{"data"}->{$index}->{"wind_dir"};
    
    # Valid value, don't need to calculate.
    if (defined($dir) && $dir != 999) { return $dir; }

    my $uwind = $self->{"data"}->{$index}->{"uwind"};
    my $vwind = $self->{"data"}->{$index}->{"vwind"};

    # Only calculate if components are not missing.
    if (defined($uwind) && $uwind != 9999 && defined($vwind) && $vwind != 9999) {
	return (Conversions::calculateWindFromUV($uwind,$vwind))[1];
    }
    return 999.0;
}

##------------------------------------------------------------------------------
# @signature float getWindSpeed(float time)
# <p>Get the wind speed for the sounding at the specified time after launch.</p>
# <p>This function will try to calculate the wind speed from the U and V wind
# components if the value was not set or was set to missing.</p>
#
# @input $time The time after launch.
# @output $spd The wind speed in m/s.  Default 999.0
##------------------------------------------------------------------------------
sub getWindSpeed {
    my $self = shift;
    my $index = $self->{"data"}->{"timeindex"}->{$_[0]};
    my $spd = $self->{"data"}->{$index}->{"wind_spd"};
    
    # Valid value, don't need to calculate.
    if (defined($spd) && $spd != 999) { return $spd; }
    
    my $uwind = $self->{"data"}->{$index}->{"uwind"};
    my $vwind = $self->{"data"}->{$index}->{"vwind"};

    # Only calculate if components are not missing.
    if (defined($uwind) && $uwind != 9999 && defined($vwind) && $vwind != 9999) {
	return (Conversions::calculateWindFromUV($uwind,$vwind))[0];
    }
    return 999.0;
}

##------------------------------------------------------------------------------
# @signature ClassSounding new(FILE* warn, --Station station--)
# <p>Create a new ClassSounding object.</p>
#
# @input $warn The file that will contain the warning information.
# @input $station <b>Optional</b> The station where the sounding was taken.
# @output $self The new ClassSounding object.
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"warn"} = $_[0];
    if (defined($_[1])) { $self->{"station"} = $_[1]; }
    else { $self->{"station"} = Station->new(); }

#    $self->{"data"}->{"times"} = ();

    return $self;
}

##--------------------------------------------------------------------------------
# @signature void saveHeader()
# <p>Generate the mandatory lines of the header from the information stored
# in the object.</p>
##--------------------------------------------------------------------------------
sub saveHeader {
    my $self = shift;

    $self->{"header"}->{1}->{"label"} = "Data Type";
    $self->{"header"}->{1}->{"value"} = $self->getType();

    $self->{"header"}->{2}->{"label"} = "Project ID";
    $self->{"header"}->{2}->{"value"} = $self->getProjectId();

    $self->{"header"}->{3}->{"label"} = "Release Site Type/Site ID";
    $self->{"header"}->{3}->{"value"} = $self->getReleaseSite();

    my $lat = $self->getReleaseLatitude();
    my $lat_fmt = $lat < 0 ? "-" : "";
    while (length($lat_fmt) < length($lat)) { $lat_fmt .= "D"; }

    my $lon = $self->getReleaseLongitude();
    my $lon_fmt = $lon < 0 ? "-" : "";
    while (length($lon_fmt) < length($lon)) { $lon_fmt .= "D"; }

    my ($lat_deg, $lat_min, $lat_sec) = Conversions::convertLatLong($lat,$lat_fmt,"DMS");
    my ($lon_deg, $lon_min, $lon_sec) = Conversions::convertLatLong($lon,$lon_fmt,"DMS");
    
    $lat_min += $lat_sec / 60;
    $lon_min += $lon_sec / 60;

    my $lat_unit = $lat < 0 ? "S" : "N";
    my $lon_unit = $lon < 0 ? "W" : "E";

    $self->{"header"}->{4}->{"label"} = "Release Location (lon,lat,alt)";
    $self->{"header"}->{4}->{"value"} =
	sprintf("%3d %05.2f'%s, %2d %05.2f'%s, %5.1f, %5.1f, %6.1f",
		abs($lon_deg),$lon_min,$lon_unit,abs($lat_deg),$lat_min,$lat_unit,
		$lon,$lat,$self->getReleaseAltitude());

    $self->{"header"}->{5}->{"label"} = "UTC Release Time (y,m,d,h,m,s)";
    $self->{"header"}->{5}->{"value"} = sprintf("%04d, %02d, %02d, %s",
						split(/\//,$self->getActualReleaseDate()),
						$self->getActualReleaseTime());

    $self->{"header"}->{12}->{"label"} = "Nominal Release Time (y,m,d,h,m,s)";
    $self->{"header"}->{12}->{"value"} = sprintf("%04d, %02d, %02d, %s",
						 split(/\//,$self->getNominalReleaseDate()),
						 $self->getNominalReleaseTime());
}

##------------------------------------------------------------------------------
# @signature void saveToFile(--String save_dir--)
# <p>Save the sounding data in this object to its class file.  If the save_dir
# is omitted the file will be placed in the current working directory.</p>
#
# @input $save_dir <b>Optional</b> The location where the class file should be
# placed.
##------------------------------------------------------------------------------
sub saveToFile {
    my $self = shift;
    my $save_dir = shift;
    my $WARN = $self->{"warn"};

    $self->saveHeader();

    # Define the file name
    my @line = split(/[,:]/,$self->{"header"}->{12}->{"value"});
    my $file = sprintf("%s/%s_%04d%02d%02d%02d%02d.cls",$save_dir,$self->getId(),@line);

    # Create a warning and do not create an output file if there is no data.
    if (scalar(keys(%{$self->{"data"}})) == 0) {
	printf($WARN "No data, not creating output file: %s.\n",$file);
	return;
    }

    if (!defined($save_dir)) { $save_dir = cwd(); }

    my $FILE;
    my $read_fmt = "%6.1f %6.1f %5.1f %5.1f %5.1f %6.1f %6.1f %5.1f %5.1f %5.1f %8.3f %7.3f %5.1f %5.1f %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f %4.1f\n";


    open($FILE,">$file") or die("Cannot open class file to write the data to.\n");

    # Create the header
    for(my $i = 1; $i < 13; $i++) {
	if (defined($self->{"header"}->{$i})) {
	    my $label = sprintf("%s:",$self->{"header"}->{$i}->{"label"});
	    printf($FILE "%-35s%s\n",$label,$self->{"header"}->{$i}->{"value"});
	} else {
	    printf($FILE "/\n");
	}
    }

    printf($FILE " Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat  %5s %5s   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n",$self->getVariableParameterName(1),$self->getVariableParameterName(2));
    printf($FILE "  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg  %5s %5s    m    code code code code code code\n","%",$self->getVariableParameterUnit(1),$self->getVariableParameterUnit(2));
    printf($FILE "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n");

    # Print the lines of data
    foreach my $time (@{ $self->{"data"}->{"times"}}) {
	printf($FILE $read_fmt,
	       $time,
	       $self->getPressure($time),
	       $self->getTemperature($time),
	       $self->getDewPoint($time),
	       $self->getRelativeHumidity($time),
	       $self->getUWindComponent($time),
	       $self->getVWindComponent($time),
	       $self->getWindSpeed($time),
	       $self->getWindDirection($time),
	       $self->getAscensionRate($time),
	       $self->getLongitude($time),
	       $self->getLatitude($time),
	       $self->getVariableParameterValue($time,1),
	       $self->getVariableParameterValue($time,2),
	       $self->getAltitude($time),
	       $self->getPressureFlag($time),
	       $self->getTemperatureFlag($time),
	       $self->getRelativeHumidityFlag($time),
	       $self->getUWindComponentFlag($time),
	       $self->getVWindComponentFlag($time),
	       $self->getAscensionRateFlag($time));
    }

    close($FILE);
}

##------------------------------------------------------------------------------
# @signature void setActualReleaseTime(String date, String date_fmt, String time, String time_fmt, int offset)
# <p>Set the actual date and time the sounding was released from the station.</p>
#
# @input $date The date of the release.
# @input $date_fmt The format of the date.
# @input $time The time of the release.
# @input $time_fmt The format of the time.
# @input $offset The offset of the time from UTC.
##------------------------------------------------------------------------------
sub setActualReleaseTime {
    my $self = shift;
    my $min_offset = $_[5];
    if (!defined($min_offset)) { $min_offset = 0; }
    (my $date, my $time) =
      Conversions::adjustDateTime(Conversions::formatDate($_[0], $_[1]),
                                Conversions::formatTime($_[2], $_[3]),
                                  0, $_[4], $min_offset);

    if (length($time) == 5) { $time = sprintf("%s:00",$time); }

    $self->{"act_date"} = $date;
    $self->{"act_time"} = $time;
}

##------------------------------------------------------------------------------
# @signature void setAltitude(float time, float alt, String unit)
# <p>Set the altitude of the sounding at a time after launch.</p>
#
# @input $time The time after launch.
# @input $alt The altitude value.
# @input $unit The unit of length of the altitude value.
##------------------------------------------------------------------------------
sub setAltitude {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"altitude"} = Conversions::convertLength($_[1],$_[2],"m");
}

##------------------------------------------------------------------------------
# @signature void setAscensionRate(float time, float rate, String unit)
# <p>Set the ascension rate of the sounding at a time after launch.</p>
# 
# @input $time The time after launch.
# @input $rate The rate of ascension.
# @inptu $unit The unit of velocity of the rate.
##------------------------------------------------------------------------------
sub setAscensionRate {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"ascension_rate"} = Conversions::convertVelocity($_[1],$_[2],"m/s");
}

##------------------------------------------------------------------------------
# @signature void setAscensionRateFlag(float time, float flag)
# <p>Set the QC flag for the ascension rate.</p>
#
# @input $time The time after launch.
# @input $flag The QC flag for the rate.
##------------------------------------------------------------------------------
sub setAscensionRateFlag {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"ascension_rate_flag"} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setDewPoint(float time, float value, String flag)
# <p>Set the dew point of the sounding at a time after launch.</p>
#
# @input $time The time after launch.
# @input $value The dew point.
# @input $unit The temperature unit of the dew point value.
##------------------------------------------------------------------------------
sub setDewPoint {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"dew_point"} = Conversions::convertTemperature($_[1],$_[2],"C");
}

##------------------------------------------------------------------------------
# @signature void setHeaderLine(int line, String label, String value)
# <p>Set header information for a non-mandatory header line.</p>
#
# @input $line The header line number.
# @input $label The label for the header line.
# @input $value The data for the header label.
# @warning This function will die if the line number is less than 6 or greater
# than 11.
##------------------------------------------------------------------------------
sub setHeaderLine {
    my $self = shift;
    if ($_[0] < 6 || $_[0] > 11) { die("Can only define header lines 6-11.\n"); }
    $self->{"header"}->{$_[0]}->{"label"} = $_[1];
    $self->{"header"}->{$_[0]}->{"value"} = $_[2];
}

##------------------------------------------------------------------------------
# @signature void setId(String id)
# <p>Set the station id to use for the file name.</p>
#
# @input $id The station id.
##------------------------------------------------------------------------------
sub setId {
    my $self = shift;
    $self->{"stn_id"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setLatitude(float time, float lat, String fmt)
# <p>Set the latitude of the sounding at a time after launch.</p>
#
# @input $time The time after launch.
# @input $lat The latitude at the time.
# @input $fmt The format of the latitude value.
##------------------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"latitude"} = Conversions::convertLatLong($_[1],$_[2]);
}

##------------------------------------------------------------------------------
# @signature void setLongitude(float time, float long, String fmt)
# <p>Set the longitude of the sounding at a time after launch.</p>
#
# @input $time The time after launch.
# @input $long The longitude at the time.
# @input $fmt The format of the longitude value.
##------------------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"longitude"} = Conversions::convertLatLong($_[1],$_[2]);
}

##------------------------------------------------------------------------------
# @signature void setNominalReleaseTime(String date, String date_fmt, String time, String time_fmt, int offset)
# <p>Set the nominal date and time the sounding was released from the station.</p>
#
# @input $date The date of the release.
# @input $date_fmt The format of the date.
# @input $time The time of the release.
# @input $time_fmt The format of the time.
# @input $offset The offset of the time from UTC.
##------------------------------------------------------------------------------
sub setNominalReleaseTime {
    my $self = shift;
    my $min_offset = $_[5];
    if (!defined($min_offset)) { $min_offset = 0; }
    (my $date, my $time) =
      Conversions::adjustDateTime(Conversions::formatDate($_[0], $_[1]),
                                Conversions::formatTime($_[2], $_[3]),
                                  0, $_[4], $min_offset);

    if (length($time) == 5) { $time = sprintf("%s:00",$time); }

    $self->{"nom_date"} = $date;
    $self->{"nom_time"} = $time;
}

##------------------------------------------------------------------------------
# @signature void setPressure(float time, float press, String unit)
# <p>Set the pressure of the sounding at a time after launch.</p>
#
# @input $time The time after launch.
# @input $press The pressure value.
# @input $unit The unit of pressure of the pressure value.
##------------------------------------------------------------------------------
sub setPressure {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"pressure"} = Conversions::convertPressure($_[1],$_[2],"mb");
}

##------------------------------------------------------------------------------
# @signature void setPressureFlag(float time, float flag)
# <p>Set the QC flag for the pressure value.</p>
#
# @input $time The time after launch.
# @input $flag The QC flag for the pressure.
##------------------------------------------------------------------------------
sub setPressureFlag {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"pressure_flag"} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setProjectId(String proj)
# <p>Set the project id that is displayed in the header.</p>
#
# @input $proj The project id.
##------------------------------------------------------------------------------
sub setProjectId {
    my $self = shift;
    $self->{"proj_id"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setRelativeHumidity(float time, float rh)
# <p>Set the relative humidity for the sounding for a time after launch.</p>
#
# @input $time The time after launch.
# @input $rh The relative humidity in %.
##------------------------------------------------------------------------------
sub setRelativeHumidity {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"rel_humid"} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setRelativeHumidtyFlag(float time, float flag)
# <p>Set the QC flag for the relative humidity value.</p>
#
# @input $time The time after launch.
# @input $flag The QC flag for the relative humidity.
##------------------------------------------------------------------------------
sub setRelativeHumidityFlag {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"rel_humid_flag"} = $_[1];
}

sub setReleaseLocation {
    my $self = shift;
    die("Do not use setReleaseLocation anymore.\n");
}

##------------------------------------------------------------------------------
# @signature void setReleaseAltitude(float alt, String unit)
# <p>Set the altitude where the sounding was released.</p>
#
# @input $alt The altitude value.
# @input $unit The unit of length of the altitude value.
##------------------------------------------------------------------------------
sub setReleaseAltitude {
    my $self = shift;
    $self->{"elevation"} = Conversions::convertLength($_[0],$_[1],"m");
}

##------------------------------------------------------------------------------
# @signature void setReleaseLatitude(String lat, String format)
# <p>Set the latitude where the sounding was released.</p>
#
# @input $lat The latitude.
# @input $format The format of the latitude.
##------------------------------------------------------------------------------
sub setReleaseLatitude {
    my $self = shift;
    $self->{"latitude"} = Conversions::convertLatLong($_[0],$_[1],"D");
}

##------------------------------------------------------------------------------
# @signature void setReleaseLongitude(String lon, String format)
# <p>Set the longitude where the sounding was released.</p>
#
# @input $lon The longitude.
# @input $format The format of the longitude.
##------------------------------------------------------------------------------
sub setReleaseLongitude {
    my $self = shift;
    $self->{"longitude"} = Conversions::convertLatLong($_[0],$_[1],"D");
}

##------------------------------------------------------------------------------
# @signature void setReleaseSite(String site)
# <p>Set the name of the release site of the sounding.</p>
#
# @input $site The name of the release site.
##------------------------------------------------------------------------------
sub setReleaseSite {
    my $self = shift;
    $self->{"site"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setTemperature(float time, float temp, String unit)
# <p>Set the temperature of the sounding at a time after launch.
#
# @input $time The time after launch
# @input $temp The temperature value
# @input $unit The temperature unit of the temperature value.
##------------------------------------------------------------------------------
sub setTemperature {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"temperature"} = 
      Conversions::convertTemperature($_[1],$_[2],"C");
}

##------------------------------------------------------------------------------
# @signature void setTemperature(float time, float flag)
# <p>Set the QC flag for the temperature value.</p>
#
# @input $time The time after launch.
# @input $flag The QC flag for the temperature.
##------------------------------------------------------------------------------
sub setTemperatureFlag {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"temperature_flag"} = $_[1];
}

##------------------------------------------------------------------------------
# @signagure void setType(String type)
# <p>Set the data type of the sounding.</p>
#
# @input $type The data type of the sounding.
##------------------------------------------------------------------------------
sub setType {
    my $self = shift;
    $self->{"type"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setUWindComponent(float time, float uwind, String unit)
# <p>Set the U wind component of the sounding at a time after launch.</p>
#
# @input $time The time after launch
# @input $uwind The U wind component.
# @input $unit The velocity unit of the U wind component value.
##------------------------------------------------------------------------------
sub setUWindComponent {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"uwind"} = Conversions::convertVelocity($_[1],$_[2],"m/s");
}

##------------------------------------------------------------------------------
# @signature void setUWindComponentFlag(float time, float flag)
# <p>Set the QC flag for the U wind component.</p>
#
# @input $time The time after launch.
# @input $flag The QC flag for the U wind component.
##------------------------------------------------------------------------------
sub setUWindComponentFlag {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"uwind_flag"} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setVariableParameter(int index, String name, String unit)
# <p>Define the name and unit of measurement for a variable parameter.</p>
#
# @input $index The index of the variable parameter.
# @input $name The name of the variable parameter.
# @input $unit The unit of measurement of the variable parameter.
##------------------------------------------------------------------------------
sub setVariableParameter {
    my $self = shift;
    $self->{"var_param_name_".$_[0]} = $_[1];
    $self->{"var_param_unit_".$_[0]} = $_[2];
}

##------------------------------------------------------------------------------
# @signature void setVariableParameterValue(float time, int index, float value)
# <p>Set the value of a variable parameter.</p>
#
# @input $time The time after launch.
# @input $index The index of the variable parameter.
# @input $value The value of the variable parameter.
##------------------------------------------------------------------------------
sub setVariableParameterValue {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"var_param_".$_[1]} = $_[2];
}

##------------------------------------------------------------------------------
# @signature setVWindComponent(float time, float vwind, String unit)
# <p>Set the V wind component of the sounding for a time after launch.</p>
#
# @input $time The time after launch.
# @input $vwind The V wind component value.
# @input $unit The unit of velocity of the V wind component.
##------------------------------------------------------------------------------
sub setVWindComponent {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"vwind"} = Conversions::convertVelocity($_[1],$_[2],"m/s");
}

##------------------------------------------------------------------------------
# @signature void setVWindComponentFlag(float time, float flag)
# <p>Set the QC flag for the V wind component.</p>
#
# @input $time The time after launch.
# @input $flag The QC flag for the V wind component.
##------------------------------------------------------------------------------
sub setVWindComponentFlag {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"vwind_flag"} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setWindDirection(float time, float dir)
# <p>Set the wind direction of the sounding for a time after launch.</p>
#
# @input $time The time after launch.
# @input $dir The wind direction value in degrees.
##------------------------------------------------------------------------------
sub setWindDirection {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"wind_dir"} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setWindSpeed(float time, float spd, String unit)
# <p>Set the wind speed of the sounding for a time after launch.</p>
# 
# @input $time The time after launch.
# @input $spd The wind speed value.
# @input $unit The unit of velocity of the wind speed value.
##------------------------------------------------------------------------------
sub setWindSpeed {
    my $self = shift;
    my $index = $self->getTimeIndex($_[0]);
    $self->{"data"}->{$index}->{"wind_spd"} = Conversions::convertVelocity($_[1],$_[2],"m/s");
}

sub getTimeIndex {
    my $self = shift;
    my $time = shift;

    my @times = ();
    if (defined($self->{"data"}->{"times"})) {
	@times = @{ $self->{"data"}->{"times"}};
    }

    if (!defined($times[0])) {
	push(@{ $self->{"data"}->{"times"}},$time); 
    	$self->{"data"}->{"timeindex"}->{$time} = scalar(@{ $self->{"data"}->{"times"}}) - 1;
    } elsif ($times[-1] ne $time) {
	push(@{ $self->{"data"}->{"times"}},$time);
	$self->{"data"}->{"timeindex"}->{$time} = scalar(@{ $self->{"data"}->{"times"}}) - 1;
    }

    return $self->{"data"}->{"timeindex"}->{$time};
}

1;









