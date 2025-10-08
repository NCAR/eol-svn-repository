#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The <code>FluxRecord</code> is a <code>Record</code>.</p>
#
# @author Joel Clawson
# @version 2.1 This is a new module to handle CEOP data.
##Module--------------------------------------------------------------------
package FluxRecord;
use strict;
use lib ".";
use Conversions;
use Record;
our @ISA("Record");

##--------------------------------------------------------------------------
# @signature float getCO2Flux()
# <p>Get the CO2 flux of the Record.</p>
#
# @output $flux The CO2 flux.
##--------------------------------------------------------------------------
sub getCO2Flux {
    my $self = shift;
    $self->checkDefined($self->{"co2_flux"}, $self->getMissing());
}

##--------------------------------------------------------------------------
# @signature float getLatentHeatFlux()
# <p>Get the latent heat flux of the Record.</p>
#
# @output $flux The latent heat flux.
##--------------------------------------------------------------------------
sub getLatentHeatFlux {
    my $self = shift;
    $self->checkDefined($self->{"latent_heat_flux"}, $self->getMissing());
}

##--------------------------------------------------------------------------
# @signature float getSensibleHeatFlux()
# <p>Get the sensible heat flux for the Record.</p>
#
# @output $flux The sensible heat flux.
##--------------------------------------------------------------------------
sub getSensibleHeatFlux {
    my $self = shift;
    $self->checkDefined($self->{"sensible_heat_flux"}, $self->getMissing());
}

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
# @signature float getSoilHeatFlux()
# <p>Get the soil heat flux for the Record.</p>
#
# @output $flux The soil heat flux.
##--------------------------------------------------------------------------
sub getSoilHeatFlux {
    my $self = shift;
    $self->checkDefined($self->{"soil_heat_flux"}, $self->getMissing());
}

##--------------------------------------------------------------------------
# @signature void setCO2Flux(float flux)
# <p>Set the CO2 flux for the Record.  The flux must be in umol/m2/s units.</p>
#
# @input $flux The CO2 flux.
##--------------------------------------------------------------------------
sub setCO2Flux {
    my $self = shift;
    $self->{"co2_flux"} = $_[0];
}

##--------------------------------------------------------------------------
# @signature void setLatentHeatFlux(float flux, String unit)
# <p>Set the latent heat flux for the Record.</p>
#
# @input $flux The latent heat flux.
# @input $unit The unit of radiation of the flux value.
##--------------------------------------------------------------------------
sub setLatentHeatFlux {
    my $self = shift;
    $self->{"latent_heat_flux"} = 
      Conversions::convertRadiation($_[0], $_[1], "W/m2");
}

##--------------------------------------------------------------------------
# @signature void setSensibleHeatFlux(float flux, String unit)
# <p>Set the sensible heat flux for the Record.</p>
#
# @input $flux The sensible heat flux.
# @input $unit The unit of radiation of the flux value.
##--------------------------------------------------------------------------
sub setSensibleHeatFlux {
    my $self = shift;
    $self->{"sensible_heat_flux"} =
      Conversion::convertRadiation($_[0], $_[1], "W/m2");
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
# @signature void setSoilHeatFlux(float soil, String unit)
# <p>Set the soil heat flux for the Record.</p>
#
# @input $soil The heat flux for the soil.
# @input $unit The unit of radiation of the soil heat flux.
##--------------------------------------------------------------------------
sub setSoilHeatFlux {
    my $self = shift;
    $self->{"soil_heat_flux"} = 
      Conversions::convertRadiation($_[0], $_[1], "W/m2");
}

1;

