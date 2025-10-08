#! /usr/bin/perl -w

##Module---------------------------------------------------------------
# <p>The <code>Record</code> class is a generic record that handles basic
# information such as dates/times, latitude, longitude, elevation, and
# other non-data related information.</p>
#
# @author Joel Clawson
# @version 2.0 This is a refactored version of the Version1 Record to 
# only hold the non-data related information.
##Module---------------------------------------------------------------
package Record;
use strict;
use lib ".";
use Conversions;
use Station;

##-------------------------------------------------------------------------
# @signature String checkDefined(String value, String default)
# <p>This is a simple selection function that returns the default value 
# for undefined values.</p>
#
# @input $value The value to determine if it is defined
# @input $default The default value to return if the value is undefined.
# @output $value The value if defined, otherwise the default value
##-------------------------------------------------------------------------
sub checkDefined {
    my $self = shift;
    if (defined($_[0])) { return $_[0]}
    else { return $_[1]; }
}

##------------------------------------------------------------------------
# @signature String getActualDate()
# <p>Get the date the reading was taken.</p>
#
# @output $act_date The date in YYYY/MM/DD format, 9999/99/99 default.
##------------------------------------------------------------------------
sub getActualDate {
    my $self = shift;
    return $self->checkDefined($self->{"act_date"}, "9999/99/99");
}

##------------------------------------------------------------------------
# @signature String getActualTime()
# <p>Get the time the reading was taken.</p>
#
# @output $act_time The time in HH:MM format, 99:99 default.
##------------------------------------------------------------------------
sub getActualTime {
    my $self = shift;
    return $self->checkDefined($self->{"act_time"}, "99:99");
}

##-----------------------------------------------------------------------
# @signature String getCSEId()
# <p>Get the CSE for the Record.</p>
#
# @output $cse_id The identifier for the CSE, "CSE" default.
##-----------------------------------------------------------------------
sub getCSEId {
    my $self = shift;
    $self->checkDefined($self->{"cse_id"}, $self->{"station"}->getCSE());
}

##----------------------------------------------------------------------
# @signature float getElevation()
# <p>Get the elevation of the station the record was taken at.</p>
#
# @output $elevation The elevation of the record, default -999.99
##----------------------------------------------------------------------
sub getElevation {
    my $self = shift;
    my $elev = $self->checkDefined($self->{"elevation"}, 
				   $self->{"station"}->getElevation());
    if ($elev < Conversions::getMissing()) {return Conversions::getMissing();}
    else { return $elev; }
}

##----------------------------------------------------------------------
# @signature float getLatitude()
# <p>Get the latitude of the station where the record was taken.</p>
#
# @output $lat The latitude of the station, default -999.99999
##----------------------------------------------------------------------
sub getLatitude {
    my $self = shift;
    return $self->checkDefined($self->{"latitude"},
			       $self->{"station"}->getLatitude());
}

##----------------------------------------------------------------------
# @signature float getLongitude()
# <p>Get the longitude of the station where the record was taken.</p>
#
# @output $long The longitude of the station, default -999.99999
##----------------------------------------------------------------------
sub getLongitude {
    my $self = shift;
    return $self->checkDefined($self->{"longitude"},
			       $self->{"station"}->getLongitude());
}

##----------------------------------------------------------------------
# @signature String getNetworkId()
# <p>Get the name of the network the record's station is a member of</p>
#
# @output $net The network of the records's station, default Network
##----------------------------------------------------------------------
sub getNetworkId {
    my $self = shift;
    return $self->checkDefined($self->{"net_id"}, 
			       $self->{"station"}->getNetworkName());
}

##-----------------------------------------------------------------------
# @signature String getNominalDate()
# <p>Get the rounded date of the record.</p>
#
# @output $date The nominal date, default ActualDate
##-----------------------------------------------------------------------
sub getNominalDate {
    my $self = shift;
    return $self->checkDefined($self->{"nom_date"}, $self->getActualDate());
}

##-----------------------------------------------------------------------
# @signature String getNominalTime()
# <p>Get the rounded time of the record.</p>
#
# @output $time The nominal time, default ActualTime
##----------------------------------------------------------------------
sub getNominalTime {
    my $self = shift;
    return $self->checkDefined($self->{"nom_time"}, $self->getActualTime());
}

##-----------------------------------------------------------------------
# @signature int getOccurence()
# <p>Get the occurence value of the record</p>
#
# @output $occurence The occurence value, default 0
##-----------------------------------------------------------------------
sub getOccurence {
    my $self = shift;
    return $self->checkDefined($self->{"occurence"}, 0);
}

##-----------------------------------------------------------------------
# @signature String getReferenceSiteId()
# <p>Get the identifier for the reference site.
#
# @output $ref_site_id The identifier for the reference site, "Ref_Site"
# default.
##-----------------------------------------------------------------------
sub getReferenceSiteId {
    my $self = shift;
    $self->checkDefined($self->{"ref_site_id"}, 
			$self->{"station"}->getReferenceSiteId());
}

##-----------------------------------------------------------------------
# @signature String getStationId()
# <p>Get the station identifier of the Record.</p>
#
# @output $id The station identifier, default Station
##-----------------------------------------------------------------------
sub getStationId {
    my $self = shift;
    return $self->checkDefined($self->{"stn_id"}, 
			       $self->{"station"}->getStationId());
}

##-----------------------------------------------------------------------
# @signature int getVerbose()
# <p>Get if this record should be verbose in its warning generation.</p>
#
# @output $verbose The verbose value, default 1.
##-----------------------------------------------------------------------
sub getVerbose {
    my $self = shift;
    return $self->checkDefined($self->{"verbose"},
			       $self->{"station"}->getVerbose());
}

##-----------------------------------------------------------------------
# @signature int isSpecial()
# <p>Determine if this record is a special record.</p>
# <p>A special record is any record that has a nominal time not on the 
# frequency.  i.e. A record with time 13:13 will be special for hourly
# frequency, but 13:00 will not be special.</p>
#
# @output $special 1 if the record is special, 0 otherwise.
##-----------------------------------------------------------------------
sub isSpecial {
    my $self = shift;
    my $freq = $self->{"station"}->getReportingFrequency();

    return $self->{"isSpecial"} if (defined($self->{"isSpecial"}));

    if ($freq eq "hourly") {
	return (substr($self->getNominalTime(), 3, 2) eq "00") ? 0 : 1;
    } elsif ($freq eq "1 minute" || $freq =~ /x+/) {
	return 0;
    } else {
	die("isSpecial does not recognize the frequency: ".$freq);
    }
}

##-----------------------------------------------------------------------
# @signature Record new(FILE* warning, --Station stn--)
# <p>Create a new <code>Record</code>.</p>
#
# @input $warning The file where warnings are going to printed.
# @input $stn <b>Optional</b> The <code>Station</code> the 
# <code>Record</code> was taken at.  If not sent, it will create a 
# <code>Record</code> with the default values.
# @output $rec The new <code>Record</code>.
##-----------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self, $class);
    
    $self->{"warn"} = $_[0];

    if (defined($_[1])) { $self->{"station"} = $_[1]; }
    else { $self->{"station"} = Station->new(); }
   
    return $self;
}

##-----------------------------------------------------------------------
# @signature void setActualDate(String date, String format)
# <p>Set the actual date the reading was taken.</p>
#
# @input $date The actual date of the reading.
# @input $format The format of the date.
##-----------------------------------------------------------------------
sub setActualDate {
    my $self = shift;
    $self->{"act_date"} = Conversions::formatDate($_[0], $_[1]);
}

##-----------------------------------------------------------------------
# @signature void setActualTime(String time, String format)
# <p>Set the actual time the reading was taken.</p>
#
# @input $time The actual time of the reading.
# @input $format The format of the time.
##-----------------------------------------------------------------------
sub setActualTime {
    my $self = shift;
    $self->{"act_time"} = Conversions::formatTime($_[0], $_[1]);
}

##-----------------------------------------------------------------------
# @signature void setCSEId(String cse_id)
# <p>Set the identifier for the CSE of the Record.</p>
#
# @input $cse_id The CSE's identifier.
##-----------------------------------------------------------------------
sub setCSEId {
    my $self = shift;
    $self->{"cse_id"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setElevation(float elev, String unit)
# <p>Set the elevation of the reading.</p>
#
# @input $elev The elevation value.
# @input $unit The units of measurement for the elevation.
##-----------------------------------------------------------------------
sub setElevation {
    my $self = shift;
    $self->{"elevation"} = Conversions::convertLength($_[0], $_[1], "m");
}

##-----------------------------------------------------------------------
# @signature void setIsSpecial(int spec)
# <p>Define this record to be a special record or not.</p>
#
# @input $spec A boolean int if the record is special.
##-----------------------------------------------------------------------
sub setIsSpecial {
    my $self = shift;
    $self->{"isSpecial"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setLatitude(String lat, String format)
# <p>Set the latitude of the reading.</p>
#
# @input $lat The latitude of the reading.
# @input $format The format of the latitude.
##-----------------------------------------------------------------------
sub setLatitude {
    my $self = shift;
    $self->{"latitude"} = Conversions::convertLatLong($_[0], $_[1]);
}

##-----------------------------------------------------------------------
# @signature void setLongitude(String long, String format)
# <p>Set the longitude of the reading.</p>
#
# @input $long The longitude of the reading.
# @input $format The format of the longitude.
##-----------------------------------------------------------------------
sub setLongitude {
    my $self = shift;
    $self->{"longitude"} = Conversions::convertLatLong($_[0], $_[1]);
}

##-----------------------------------------------------------------------
# @signature void setNetworkId(String id)
# <p>Set the identifier for the network of the reading.</p>
#
# @input $id The network identifier.
##-----------------------------------------------------------------------
sub setNetworkId {
  my $self = shift;
  $self->{"net_id"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setNominalDate(String date, String format)
# <p>Set the nominal date for the reading.</p>
#
# @input $date The nominal date of the reading.
# @input $format The format of the date.
##-----------------------------------------------------------------------
sub setNominalDate {
    my $self = shift;
    $self->{"nom_date"} = Conversions::formatDate($_[0], $_[1]);
}

##-----------------------------------------------------------------------
# @signature void setNominalTime(String time, String format)
# <p>Set the nominal time for the reading.</p>
#
# @input $time The nominal time of the reading.
# @input $format The format of the time.
##-----------------------------------------------------------------------
sub setNominalTime {
    my $self = shift;
    $self->{"nom_time"} = Conversions::formatTime($_[0], $_[1]);
}

##-----------------------------------------------------------------------
# @signature void setOccurence(int occ)
# <p>Set the occurence value of the reading.</p>
#
# @input $occ The occurence value.
##-----------------------------------------------------------------------
sub setOccurence {
  my $self = shift;
  $self->{"occurence"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setReadingTime(String date, String date_format, String time, String time_format, int hour_offset, --int min_offset--)
# <p>Set the reading time of the record.</p>
#
# @input $date The date of the reading.
# @input $date_format The format of the date.
# @input $time The time of the reading.
# @input $time_format The format of the time.
# @input $hour_offset The number of hours to offset the reading time.
# @input $min_offset <b>Optional</b> The number of minutes to offset the
# reading time.
##-----------------------------------------------------------------------
sub setReadingTime {
    my $self = shift;
    my $min_offset = $_[5];
    if (!defined($min_offset)) { $min_offset = 0; }
    (my $date, my $time) = 
      Conversions::adjustDateTime(Conversions::formatDate($_[0], $_[1]),
				Conversions::formatTime($_[2], $_[3]), 
				  0, $_[4], $min_offset);
    $self->setActualDate($date, "YYYY/MM/DD");
    $self->setActualTime($time, "HH:MM");
}

##-----------------------------------------------------------------------
# @signature void setReferenceSiteId(String ref_site_id)
# <p>Set the identifier for the reference site of the Record.</p>
#
# @input $ref_site_id The reference site's identifier.
##-----------------------------------------------------------------------
sub setReferenceSiteId {
    my $self = shift;
    $self->{"ref_site_id"} = $_[0];
}

##-----------------------------------------------------------------------
# @signature void setStationId(String id)
# <p>Set the station identifier for the reading.</p>
#
# @input $id The station identifier.
##-----------------------------------------------------------------------
sub setStationId {
    my $self = shift;
    $self->{"stn_id"} = $_[0];
}

##--------------------------------------------------------------------------
# @signature void setVerbose(int verbose)
# <p>Set the verbose flag for the record.</p>
#
# @input $verbose The verbose flag for the record.
##--------------------------------------------------------------------------
sub setVerbose {
    my $self = shift;
    $self->{"verbose"} = $_[0];
}

1;



