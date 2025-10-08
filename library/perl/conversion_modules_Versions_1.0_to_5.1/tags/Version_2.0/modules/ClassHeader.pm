#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The ClassHeader.pm module is a representation of the header portion of
# a CLASS format file.  This only creates the header and does not contain any
# data for a sounding.</p>
# <p>If the ClassHeader->new($station) constructor is used, the station 
# information for the header will be determined from the Station object unless
# it is manually set by the user.</p>
#
# @author Joel Clawson
# @version 1.0 Original Creation
##Module------------------------------------------------------------------------
package ClassHeader;
use strict;
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version2";
use Conversions;
use Station;

##------------------------------------------------------------------------------
# @signature String getActualDate()
# <p>Get the actual date the sounding was released.</p>
#
# @output $date The actual release date of the sounding.
##------------------------------------------------------------------------------
sub getActualDate {
    my $self = shift;
    return defined($self->{"act_date"}) ? $self->{"act_date"} : "9999/99/99";
}

##------------------------------------------------------------------------------
# @signature String getActualTime()
# <p>Get the actual time the sounding was released.</p>
#
# @output $time The actual release time of the sounding.
##------------------------------------------------------------------------------
sub getActualTime {
    my $self = shift;
    return defined($self->{"act_time"}) ? $self->{"act_time"} : "99:99:99";
}

##------------------------------------------------------------------------------
# @signature float getAltitude()
# <p>Get the altitude/elevation of the station where the sounding was released.</p>
#
# @output $alt The altitude of the station.
##------------------------------------------------------------------------------
sub getAltitude {
    my $self = shift;
    return defined($self->{"altitude"}) ? $self->{"altitude"} : 
	$self->{"station"}->getElevation();
}

##------------------------------------------------------------------------------
# @signature String getId()
# <p>Get the id of the station where the sounding was released.</p>
#
# @output $id The station identifier.
##------------------------------------------------------------------------------
sub getId {
    my $self = shift;
    return defined($self->{"id"}) ? $self->{"id"} : $self->{"station"}->getStationId();
}

##------------------------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the station where the sounding was released.</p>
#
# @output $lat The latitude of the station.
##------------------------------------------------------------------------------
sub getLatitude {
    my $self = shift;
    return defined($self->{"latitude"}) ? $self->{"latitude"} :
	$self->{"station"}->getLatitude();
}

##------------------------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the station where the sounding was released.</p>
#
# @output $lon The longitude of the station.
##------------------------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    return defined($self->{"longitude"}) ? $self->{"longitude"} :
	$self->{"station"}->getLongitude();
}

##------------------------------------------------------------------------------
# @signature String getNominalDate()
# <p>Get the nominal release date of the sounding.</p>
#
# @output $date The nominal release date.
##------------------------------------------------------------------------------
sub getNominalDate {
    my $self = shift;
    return defined($self->{"nom_date"}) ? $self->{"nom_date"} : $self->getActualDate();
}

##------------------------------------------------------------------------------
# @signature String getNominalTime()
# <p>Get the nominal release time of the sounding.</p>
#
# @output $time The nominal release time.
##------------------------------------------------------------------------------
sub getNominalTime {
    my $self = shift;
    return defined($self->{"nom_time"}) ? $self->{"nom_time"} : $self->getActualTime();
}

##------------------------------------------------------------------------------
# @signature String getProject()
# <p>Get the name of the project this sounding is associated with.</p>
#
# @output $project The name of the project.
##------------------------------------------------------------------------------
sub getProject {
    my $self = shift;
    return defined($self->{"project"}) ? $self->{"project"} : "";
}

##------------------------------------------------------------------------------
# @signature String getSite()
# <p>Get the description of the station where the sounding was released.</p>
#
# @output $site The station description.
##------------------------------------------------------------------------------
sub getSite {
    my $self = shift;
    return defined($self->{"site"}) ? $self->{"site"} :
	sprintf("%s %s",$self->{"station"}->getStationId(),
		$self->{"station"}->getStationName());
}

##------------------------------------------------------------------------------
# @signature String getType()
# <p>Get what type of sounding this release is.  This is like National Weather
# Service Sounding.</p>
#
# @output $type The type of sounding.
##------------------------------------------------------------------------------
sub getType {
    my $self = shift;
    return defined($self->{"type"}) ? $self->{"type"} : "";
}

##------------------------------------------------------------------------------
# @signature String getVariableParameterName(int index)
# <p>Get the name of one of the variable parameters.</p>
#
# @input $index The index of the variable parameter.
# @output $name The name of the parameter.
##------------------------------------------------------------------------------
sub getVariableParameterName {
    my $self = shift;
    my $key = sprintf("var_name_%d",$_[0]);
    return defined($self->{$key}) ? $self->{$key} : ("Ele","Azi")[$_[0] - 1];
}

##------------------------------------------------------------------------------
# @signature Strin getVariableParameterUnit(int index)
# <p>Get the unit of one of the variable parameters.</p>
#
# @input $index The index of the variable parameter.
# @output $unit The unit of the parameter.
##------------------------------------------------------------------------------
sub getVariableParameterUnit {
    my $self = shift;
    my $key = sprintf("var_unit_%d",$_[0]);
    return defined($self->{$key}) ? $self->{$key} : "deg";
}

##------------------------------------------------------------------------------
# @signature ClassHeader new(--Station station--)
# <p>Create a new ClassHeader object.</p>
#
# @input $station <b>Optional</b> The station where the sounding was released.</p>
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->{"station"} = defined($_[0]) ? $_[0] : Station->new();

    return $self;
}

##------------------------------------------------------------------------------
# @signature void setActualRelease(String date, String date_fmt, String time, String time_fmt, int hour_offset, --int min_offset--)
# <p>Set the actual date and time of the release of the sounding.</p>
#
# @input $date The date of the release.
# @input $date_fmt The format of the date.
# @input $time The time of the release.
# @input $time_fmt The format of the time.
# @input $hour_offset The hour offset to shift the date and time to UTC.
# @input $min_offset <b>Optional</b> The minute offset to shift the date and time to UTC.
##------------------------------------------------------------------------------
sub setActualRelease {
    my $self = shift;
    my $min_offset = defined($_[5]) ? $_[5] : 0;

    ($self->{"act_date"},$self->{"act_time"}) =
      Conversions::adjustDateTime(Conversions::formatDate($_[0],$_[1]),
				Conversions::formatTime($_[2],$_[3]),0,$_[4],$min_offset);

    $self->{"act_time"} .= ":00" if (length($self->{"act_time"}) == 5);
}

##------------------------------------------------------------------------------
# @signature void setAltitude(float alt, String unit)
# <p>Set the altitude/elevation of the station where the sounding was released.</p>
#
# @input $alt The altitude value.
# @input $unit The unit of the altitude.
##------------------------------------------------------------------------------
sub setAltitude {
    my $self = shift;
    $self->{"altitude"} = Conversions::convertLength($_[0],$_[1],"m");
}

##------------------------------------------------------------------------------
# @signature void setId(String id)
# <p>Set the identifier for the station where the sounding was released.</p>
#
# @input $id The station id.
##------------------------------------------------------------------------------
sub setId {
    my $self = shift;
    $self->{"id"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setLatitude(float lat, String fmt)
# <p>Set the latitude of the station where the sounding was released.</p>
#
# @input $lat The latitude of the station.
# @input $fmt The format of the latitude.
##------------------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    $self->{"latitude"} = Conversions::convertLatLong($_[0],$_[1],"D") 
	unless ($_[0] >= 999);
}

##------------------------------------------------------------------------------
# @signature void setLine(String label, String value)
# <p>Set the label and value of an optional header line.</p>
#
# @input $label The label of the header line.
# @input $value The value associated with the label.
##------------------------------------------------------------------------------
sub setLine {
    my $self = shift;
    $self->{"header"}->{$_[0]} = $_[1];
}

##------------------------------------------------------------------------------
# @signature void setLongitude(float lon, String fmt)
# <p>Set the longitude of the station where the sounding was released.</p>
#
# @input $lon The longitude of the station.
# @input $fmt The format of the longitude.
##------------------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    $self->{"longitude"} = Conversions::convertLatLong($_[0],$_[1],"D")
	unless ($_[0] >= 9999);
}

##------------------------------------------------------------------------------
# @signature void setNominalRelease(String date, String date_fmt, String time, String time_fmt, int hour_offset, --int min_offset--)
# <p>Set the nominal date and time when the sounding was released.</p>
#
# @input $date The date of the release.
# @input $date_fmt The format of the date.
# @input $time The time of the release.
# @input $time_fmt The format of the time.
# @input $hour_offset The hour offset to shift the date and time to UTC.
# @input $min_offset <b>Optional</b> The minute offset to shift the date and time to UTC.
##------------------------------------------------------------------------------
sub setNominalRelease {
    my $self = shift;
    my $min_offset = defined($_[5]) ? $_[5] : 0;

    ($self->{"nom_date"},$self->{"nom_time"}) =
      Conversions::adjustDateTime(Conversions::formatDate($_[0],$_[1]),
				Conversions::formatTime($_[2],$_[3]),0,$_[4],$min_offset);

    $self->{"nom_time"} .= ":00" if (length($self->{"nom_time"}) == 5);
}

##------------------------------------------------------------------------------
# @signature void setProject(String project)
# <p>Set the project name for the sounding.</p>
#
# @input $project The project name.
##------------------------------------------------------------------------------
sub setProject {
    my $self = shift;
    $self->{"project"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setSite(String site)
# <p>Set the description of the station where the sounding was released.</p>
#
# @input $site The station description.
##------------------------------------------------------------------------------
sub setSite {
    my $self = shift;
    $self->{"site"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setType(String type)
# <p>Set the type of sounding this sounding is.</p>
#
# @input $type The type of sounding.
##------------------------------------------------------------------------------
sub setType {
    my $self = shift;
    $self->{"type"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature String toString()
# <p>Create the String output of the header information.</p>
#
# @output $header The header formated for the CLASS format header.
##------------------------------------------------------------------------------
sub toString {
    my $self = shift;
    my @lines = ();

    # Create formats for latitude and longitude.
    my $lat_fmt = $self->getLatitude() < 0 ? "-" : "";
    while (length($lat_fmt) < length($self->getLatitude())) { $lat_fmt .= "D"; }
    my $lon_fmt = $self->getLongitude() < 0 ? "-" : "";
    while (length($lon_fmt) < length($self->getLongitude())) { $lon_fmt .= "D"; }
    
    # Convert the latitude and longitude to degrees and minute pieces.
    my ($lat_deg,$lat_min,$lat_sec) = 
      Conversions::convertLatLong($self->getLatitude(),$lat_fmt,"DMS");
    my ($lon_deg,$lon_min,$lon_sec) = 
      Conversions::convertLatLong($self->getLongitude(),$lon_fmt,"DMS");
    $lat_min += $lat_sec / 60;
    $lon_min += $lon_sec / 60;
    my $lat_dir = $lat_deg < 0 ? "S" : "N";
    my $lon_dir = $lon_deg < 0 ? "W" : "E";

    # Define the mandatory header lines.
    $lines[0] = sprintf("%-35s%s","Data Type:",$self->getType());
    $lines[1] = sprintf("%-35s%s","Project ID:",$self->getProject());
    $lines[2] = sprintf("%-35s%s","Release Site Type/Site ID:",$self->getSite());
    $lines[3] = sprintf("%-35s%03d %4.2f'%s, %02d %4.2f'%s, %.3f, %.3f, %.1f",
			"Release Location (lon,lat,alt):",abs($lon_deg),$lon_min,$lon_dir,
			abs($lat_deg),$lat_min,$lat_dir,$self->getLongitude(),
			$self->getLatitude(),$self->getAltitude());
    $lines[4] = sprintf("%-35s%04d, %02d, %02d, %s","UTC Release Time (y,m,d,h,m,s):",
			split(/\//,$self->getActualDate()),$self->getActualTime());
    $lines[11]= sprintf("%-35s%04d, %02d, %02d, %s","Nominal Release Time (y,m,d,h,m,s):",
			split(/\//,$self->getNominalDate()),$self->getNominalTime());
    $lines[12]= sprintf(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   %4s  %4s   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ",$self->getVariableParameterName(1),$self->getVariableParameterName(2));
    $lines[13]= sprintf("  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg   %4s  %4s    m    code code code code code code","%",$self->getVariableParameterUnit(1),$self->getVariableParameterUnit(2));
    $lines[14]= "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n";

    # Define the set optional header lines.
    my $index = 5;
    foreach my $key (sort(keys(%{$self->{"header"}}))) {
	$lines[$index++] = sprintf("%-35s%s",$key,$self->{"header"}->{$key});
    }

    # Fill the remaining optional header lines with empty / values.
    for (my $i = 5; $i < scalar(@lines); $i++) {
	$lines[$i] = "/" unless (defined($lines[$i]));
    }

    # Put the lines together with '\n' characters.
    return join("\n",@lines);
}

1;











