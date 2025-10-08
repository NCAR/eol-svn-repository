#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The <code>TowerRecord</code> is a <code>SurfaceRecord</code> that has
# a height.</p>
#
# @author Joel Clawson
# @version 2.1 This is a new module to handle CEOP data.
##Module--------------------------------------------------------------------
package TowerRecord;
use strict;
use lib ".";
use Conversions;
use SurfaceRecord;
our @ISA("SurfaceRecord");

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







