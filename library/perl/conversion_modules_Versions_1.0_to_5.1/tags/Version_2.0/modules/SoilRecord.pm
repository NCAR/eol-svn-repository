#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The <code>SoilRecord</code> is a <code>SurfaceRecord</code> that has
# a height.</p>
#
# @author Joel Clawson
# @version 2.1 This is a new module to handle CEOP data.
##Module--------------------------------------------------------------------
package SoilRecord;
use strict;
use lib ".";
use Conversions;
use Record;
our @ISA("Record");

##--------------------------------------------------------------------------
# @signature float getSensorHeight();
# <p>Get the height of the sensor for the Record.</p>
#
# @output $height The height of the sensor.
##--------------------------------------------------------------------------
sub getSensorHeight {
    my $self = shift;
    $self->checkDefined($self->{"sensor_height"}, $self->getMissing());
}

##--------------------------------------------------------------------------
# @signature float getSoilMoisture()
# <p>Get the percent of the moisture in the soil.</p>
#
# @output $moist The amount of moisture in the soil.
##--------------------------------------------------------------------------
sub getSoilMoisture {
    my $self = shift;
    $self->checkDefined($self->{"soil_moist"}, $self->getMissing());
}

##--------------------------------------------------------------------------
# @signature float getSoilTemperature()
# <p>Get the temperature of the soil.</p>
#
# @output $temp The temperature of the soil.
##--------------------------------------------------------------------------
sub getSoilTemperature {
    my $self = shift;
    $self->checkDefined($self->{"soil_temp"}, $self->getMissing());
}

##--------------------------------------------------------------------------
# @signature void setSensorHeight(float height, String unit.)
# <p>Set the height of the sensor for the Record.</p>
#
# @input $height The height of the sensor.
# @input $unit The unit of measurement of the sensor height.
##--------------------------------------------------------------------------
sub setSensorHeight {
    my $self = shift;
    $self->{"sensor_height"} = Conversions::convertLength($_[0], $_[1], "m");
}

##--------------------------------------------------------------------------
# @signature void setSoilMoisture(float moist, int percent)
# <p>Set the moisture of the soil for the Record.</p>
#
# @input $moist The soil moisture.
# @input $percent 1 if the moisture is in percent, 0 otherwise.
##--------------------------------------------------------------------------
sub setSoilMoisture {
    my $self = shift;
    if ($percent) { $self->{"soil_moist"} = $_[0]; }
    else { $self->{"soil_moist"} = 100 * $_[0]; }
}

##--------------------------------------------------------------------------
# @signature void setSoilTemperature(float temp, String unit)
# <p>Set the temperature of the soil for the Record.</p>
#
# @input $temp The temperature of the soil.
# @input $unit The unit of measurement of the soil temperature.
##--------------------------------------------------------------------------
sub setSoilTemperature {
    my $self = shift;
    $self->{"soil_temp"} = Conversions::convertTemperature($_[0],$_[1],"C");
}
