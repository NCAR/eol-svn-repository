#! /usr/bin/perl -w

##Module------------------------------------------------------------------------
# <p>The ClassHeader.pm module is a representation of the header portion of
# a CLASS format file.  This only creates the header and does not contain any
# data for a sounding.</p>
# <p>If the ClassHeader->new($station) constructor is used, the station 
# information for the header will be determined from the Station object unless
# it is manually set by the user.</p>
#
# @author L. Cully
# Updated lat/lon output format to have preceeding zero fill for header.
# Update specifically for VOCALS Ron Brown upsondes, but for all. Change
# was around line 490 from
#   $lines[3] = sprintf("%-35s%03d %4.2f'%s, %02d %4.2f'%s, %.3f, %.3f, %.1f",
# to
#   $lines[3] = sprintf("%-35s%03d %05.2f'%s, %02d %05.2f'%s, %.3f, %.3f, %.1f",
#
# @author Joel Clawson
# @version 4.0 <p>Added in a check to make sure that the nominal date is within
# three hours of the actual release time.  This included the changing of the
# constructor to require a parameter for an open FileHandle for warnings.</p>
#
# @author Joel Clawson
# @version 3.0 <p>Checks on the parameter lists' size were added.  This is to
# try help find errors quicker by forcing the function to only accept lists of
# a certain size.  The functions do not check the values, only ensures that the
# number of parameters is acceptable.</p>
#
# @author Joel Clawson
# @version 2.0 Original Creation
##Module------------------------------------------------------------------------
package ClassHeader;
use strict;
use lib "/net/work/lib/perl/Station";
use lib "/net/work/lib/perl/Utilities";
use DpgConversions qw(:DEFAULT);
use DpgDate qw(:DEFAULT);
use Station;

##------------------------------------------------------------------------------
# @signature void check_dates()
# <p>Compare the actual release time and the nominal release time to see if 
# they line up.</p>
##------------------------------------------------------------------------------
sub check_dates {
    my $self = shift;

    my $act_date = formatDate($self->getActualDate(),"YYYY, MM, DD","YYYYMMDD");
    my $nom_date = formatDate($self->getNominalDate(),"YYYY, MM, DD","YYYYMMDD");
    my $act_time = formatTime($self->getActualTime(),"HH:MM:SS","HHMMSS");
    my $nom_time = formatTime($self->getNominalTime(),"HH:MM:SS","HHMMSS");

    my $small = sprintf("%s%s",adjustDateTime($act_date,"YYYYMMDD",$act_time,"HHMMSS",0,-3,0,0));
    my $large = sprintf("%s%s",adjustDateTime($nom_date,"YYYYMMDD",$nom_time,"HHMMSS",0,3,0,0));
    my $nom = sprintf("%s%s",$nom_date,$nom_time);

    unless ($small <= $nom && $nom <= $large) {
	printf({$self->{"warn"}} "The nominal release of %s %s is not within 3 hours of the actual time %s %s\n",$self->getNominalDate(),$self->getNominalTime(),$self->getActualDate(),$self->getActualTime());
    }
}

##------------------------------------------------------------------------------
# @signature String getActualDate()
# <p>Get the actual date the sounding was released.</p>
#
# @output $date The actual release date of the sounding.
##------------------------------------------------------------------------------
sub getActualDate {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getActualDate\n"); }
    return defined($self->{"act_date"}) ? $self->{"act_date"} : $self->getNominalDate();
}

##------------------------------------------------------------------------------
# @signature String getActualTime()
# <p>Get the actual time the sounding was released.</p>
#
# @output $time The actual release time of the sounding.
##------------------------------------------------------------------------------
sub getActualTime {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getActualTime\n"); }
    return defined($self->{"act_time"}) ? $self->{"act_time"} : $self->getNominalTime();
}

##------------------------------------------------------------------------------
# @signature float getAltitude()
# <p>Get the altitude/elevation of the station where the sounding was released.</p>
#
# @output $alt The altitude of the station.
##------------------------------------------------------------------------------
sub getAltitude {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getAltitude\n"); }

    return defined($self->{"altitude"}) ? $self->{"altitude"} : 
	$self->{"station"}->getElevation() < -9999 ? 999.0 : 
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
    if (scalar(@_) != 0) { die("Invalid parameters to getId\n"); }

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
    if (scalar(@_) != 0) { die("Invalid parameters to getLatitude\n"); }

    return defined($self->{"latitude"}) ? $self->{"latitude"} :
	$self->{"station"}->getLatitude() <= -99 ? 99.0 : 
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
    if (scalar(@_) != 0) { die("Invalid parameters to getLongitude\n"); }

    return defined($self->{"longitude"}) ? $self->{"longitude"} :
	$self->{"station"}->getLongitude() <= -999 ? 999.0 :
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
    if (scalar(@_) != 0) { die("Invalid parameters to getNominalDate\n"); }

    return defined($self->{"nom_date"}) ? $self->{"nom_date"} : "9999, 99, 99";
}

##------------------------------------------------------------------------------
# @signature String getNominalTime()
# <p>Get the nominal release time of the sounding.</p>
#
# @output $time The nominal release time.
##------------------------------------------------------------------------------
sub getNominalTime {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getNominalTime\n"); }

    return defined($self->{"nom_time"}) ? $self->{"nom_time"} : "99:99:99";
}

##------------------------------------------------------------------------------
# @signature String getProject()
# <p>Get the name of the project this sounding is associated with.</p>
#
# @output $project The name of the project.
##------------------------------------------------------------------------------
sub getProject {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getProject\n"); }

    return defined($self->{"project"}) ? $self->{"project"} : "";
}

##------------------------------------------------------------------------------
# @signature String getReleaseDirection()
# <p>Get the direction the sounding was released.</p>
#
# @output $direction The direction the sounding was released.
##------------------------------------------------------------------------------
sub getReleaseDirection {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getReleaseDirection.\n"); }

    return defined($self->{"direction"}) ? $self->{"direction"} : "Unspecified";
}

##------------------------------------------------------------------------------
# @signature String getSite()
# <p>Get the description of the station where the sounding was released.</p>
#
# @output $site The station description.
##------------------------------------------------------------------------------
sub getSite {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to getSite\n"); }

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
    if (scalar(@_) != 0) { die("Invalid parameters to getType\n"); }

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
    if (scalar(@_) != 1) { die("Invalid parameters to getVariableParameterName\n"); }

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
    if (scalar(@_) != 1) { die("Invalid parameters to getVariableParameterUnit\n"); }

    my $key = sprintf("var_unit_%d",$_[0]);
    return defined($self->{$key}) ? $self->{$key} : "deg";
}

##------------------------------------------------------------------------------
# @signature ClassHeader new(FileHandle WARN, --Station station--)
# <p>Create a new ClassHeader object.</p>
#
# @input $WARN The FileHandle where warnings are to be printed.
# @input $station <b>Optional</b> The station where the sounding was released.</p>
##------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    if (scalar(@_) > 2) { die("Invalid parameters to ClassHeader->new\n"); }

    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);
    
    $self->{"warn"} = $_[0];
    $self->{"station"} = defined($_[1]) ? $_[1] : Station->new();

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
    if (scalar(@_) < 5 || scalar(@_) > 6) { 
	die("Invalid parameters to setActualDate\n"); 
    }

    my $min_offset = defined($_[5]) ? $_[5] : 0;

    ($self->{"act_date"},$self->{"act_time"}) =
      adjustDateTime(formatDate($_[0],$_[1],"YYYY, MM, DD"),"YYYY, MM, DD",
		     formatTime($_[2],$_[3],"HH:MM:SS"),"HH:MM:SS",0,$_[4],$min_offset,0);
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
    if (scalar(@_) != 2) { die("Invalid parameters to setAltitude\n"); }

    $self->{"altitude"} = convertLength($_[0],$_[1],"m");
}

##------------------------------------------------------------------------------
# @signature void setId(String id)
# <p>Set the identifier for the station where the sounding was released.</p>
#
# @input $id The station id.
##------------------------------------------------------------------------------
sub setId {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setId\n"); }

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
    if (scalar(@_) != 2) { die("Invalid parameters to setLatitude\n"); }

    $self->{"latitude"} = (convertLatLong($_[0],$_[1],"D"))[0]
	unless ($_[1] !~ /[mMsS]/ && $_[0] >= 9999);
}

##------------------------------------------------------------------------------
# @signature void setLine(int index, String label, String value)
# <p>Set the label and value of an optional header line.</p>
#
# @input $index The index of the line the data is for.
# @input $label The label of the header line.
# @input $value The value associated with the label.
##------------------------------------------------------------------------------
sub setLine {
    my $self = shift;
    if (scalar(@_) != 3) { die("Invalid parameters to setLine\n"); }

    $self->{"header"}->{$_[0]}->{"key"} = $_[1];
    $self->{"header"}->{$_[0]}->{"value"} = $_[2];
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
    if (scalar(@_) != 2) { die("Invalid parameters to setLongitude\n"); }

    $self->{"longitude"} = (convertLatLong($_[0],$_[1],"D"))[0]
	unless ($_[1] !~ /[mMsS]/ && $_[0] >= 9999);
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
    if (scalar(@_) < 5 || scalar(@_) > 6) { 
	die("Invalid parameters to setNominalRelease\n"); 
    }

    my $min_offset = defined($_[5]) ? $_[5] : 0;

    ($self->{"nom_date"},$self->{"nom_time"}) =
	adjustDateTime(formatDate($_[0],$_[1],"YYYY, MM, DD"),"YYYY, MM, DD",
		       formatTime($_[2],$_[3],"HH:MM:SS"),"HH:MM:SS",0,$_[4],$min_offset,0);
}

##------------------------------------------------------------------------------
# @signature void setProject(String project)
# <p>Set the project name for the sounding.</p>
#
# @input $project The project name.
##------------------------------------------------------------------------------
sub setProject {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setProject\n"); }

    $self->{"project"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setReleaseDirection(String direction)
# <p>Set the release direction for the sounding.  (If it is an upsonde, dropsonde,
# tethersonde, etc.)</p>
#
# @param $direction The direction of the sounding.
##------------------------------------------------------------------------------
sub setReleaseDirection {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setReleaseDirection.\n"); }

    $self->{"direction"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setSite(String site)
# <p>Set the description of the station where the sounding was released.</p>
#
# @input $site The station description.
##------------------------------------------------------------------------------
sub setSite {
    my $self = shift;
    if (scalar(@_) != 1) { die("Invalid parameters to setSite\n"); }

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
    if (scalar(@_) != 1) { die("Invalid parameters to setType\n"); }

    $self->{"type"} = $_[0];
}

##------------------------------------------------------------------------------
# @signature void setVariableParameter(int index, String name, String unit)
# <p>Set the name and unit of a variable parameter.</p>
#
# @input $index The index of the variable parameter.
# @input $name The name of the variable parameter.
# @input $unit The unit for the variable parameter.
##------------------------------------------------------------------------------
sub setVariableParameter {
    my $self = shift;
    if (scalar(@_) != 3) { die("Invalid parameters to setVariableParamter\n"); }

    ($self->{"var_name_".$_[0]},$self->{"var_unit_".$_[0]}) = @_[1..2];
}

##------------------------------------------------------------------------------
# @signature String toString()
# <p>Create the String output of the header information.</p>
#
# @output $header The header formated for the CLASS format header.
##------------------------------------------------------------------------------
sub toString {
    my $self = shift;
    if (scalar(@_) != 0) { die("Invalid parameters to toString\n"); }

    $self->check_dates();

    my @lines = ();

    # Create formats for latitude and longitude.
    my $lat_fmt = $self->getLatitude() < 0 ? "-" : "";
    while (length($lat_fmt) < length($self->getLatitude())) { $lat_fmt .= "D"; }
    my $lon_fmt = $self->getLongitude() < 0 ? "-" : "";
    while (length($lon_fmt) < length($self->getLongitude())) { $lon_fmt .= "D"; }
    
    # Convert the latitude and longitude to degrees and minute pieces.
    my ($lat_deg,$lat_min,undef()) = convertLatLong($self->getLatitude(),$lat_fmt,"DM");
    my ($lon_deg,$lon_min,undef()) = convertLatLong($self->getLongitude(),$lon_fmt,"DM");
    my $lat_dir = $lat_deg < 0 ? "S" : "N";
    my $lon_dir = $lon_deg < 0 ? "W" : "E";

    # Define the mandatory header lines.
    $lines[0] = sprintf("%-35s%s/%s","Data Type:",$self->getType(),$self->getReleaseDirection());

    $lines[1] = sprintf("%-35s%s","Project ID:",$self->getProject());

    $lines[2] = sprintf("%-35s%s","Release Site Type/Site ID:",$self->getSite());

    $lines[3] = sprintf("%-35s%03d %05.2f'%s, %02d %05.2f'%s, %.3f, %.3f, %.1f",
			"Release Location (lon,lat,alt):",abs($lon_deg),$lon_min,$lon_dir,
			abs($lat_deg),$lat_min,$lat_dir,$self->getLongitude(),
			$self->getLatitude(),$self->getAltitude());

    $lines[4] = sprintf("%-35s%s, %s","UTC Release Time (y,m,d,h,m,s):",
			$self->getActualDate(),$self->getActualTime());

    $lines[11]= sprintf("%-35s%s, %s","Nominal Release Time (y,m,d,h,m,s):",
			$self->getNominalDate(),$self->getNominalTime());

    $lines[12]= sprintf(" Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   %4s  %4s   Alt    Qp   Qt   Qrh  Qu   Qv   QdZ",$self->getVariableParameterName(1),$self->getVariableParameterName(2));
    $lines[13]= sprintf("  sec    mb     C     C     %s     m/s    m/s   m/s   deg   m/s      deg     deg   %4s  %4s    m    code code code code code code","%",$self->getVariableParameterUnit(1),$self->getVariableParameterUnit(2));
    $lines[14]= "------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n";

    # Define the set optional header lines.
    foreach my $index (sort(keys(%{$self->{"header"}}))) {
	$lines[$index] = sprintf("%-35s%s",$self->{"header"}->{$index}->{"key"},$self->{"header"}->{$index}->{"value"});
    }

    # Fill the remaining optional header lines with empty / values.
    for (my $i = 5; $i < scalar(@lines); $i++) {
	$lines[$i] = "/" unless (defined($lines[$i]));
    }

    # Put the lines together with '\n' characters.
    return join("\n",@lines);
}

1;











