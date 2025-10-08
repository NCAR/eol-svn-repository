#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDatabase.pm>Link to MySqlDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDataset.pm>Link to MySqlDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
# <p>The MySqlDataset.pm module is a partial representation of an entry from
# the dataset table of the database.  It is only able to update the begin and
# end dates, and min/max lat/lon of the dataset.  It does not handle inserting
# because that should be done through a different program.</p>
# <p>This is meant to be used in conjunction with the inserting of files into
# the database so the file dates will update the dataset at the same time.</p>
#
# @author Joel Clawson
##Module----------------------------------------------------------------------
package MySqlDataset;
use strict;
use DBI;
use MySqlDatabase;

##----------------------------------------------------------------------------
# @signature String getBeginDate()
# <p>Get the begin date for the dataset.</p>
# @output $date The begin date.
##----------------------------------------------------------------------------
sub getBeginDate {
    my $self = shift;
    return $self->{"begin_date"};
}

##----------------------------------------------------------------------------
# @signature String getDatasetId()
# <p>Get the old-style id for the dataset, i.e. the archive_ident (natural key).</p>
# @output $id The archive identifier.
##----------------------------------------------------------------------------
sub getDatasetId {
    my $self = shift;
    warnings::warnif("deprecated", "getDatasetId is deprecated, use getDatabaseId or getArchiveIdent instead (returning the archive_ident for backwards-compatibility)");
    return $self->{"archive_ident"};
}

##----------------------------------------------------------------------------
# @signature String getDatabaseId()
# <p>Get the database id (primary key) for the dataset.</p>
# @output $id The database id.
##----------------------------------------------------------------------------
sub getDatabaseId {
    my $self = shift;
    return $self->{"database_id"};
}

##----------------------------------------------------------------------------
# @signature String getArchiveIdent()
# <p>Get the archive identifier (natural key; the old dataset_id e.g. "1.33") for the dataset.</p>
# @output $id The dataset id.
##----------------------------------------------------------------------------
sub getArchiveIdent {
    my $self = shift;
    return $self->{"archive_ident"};
}

##----------------------------------------------------------------------------
# @signature String getEndDate()
# <p>Get the end date for the dataset.</p>
# @output $date The end date.
##----------------------------------------------------------------------------
sub getEndDate {
    my $self = shift;
    return $self->{"end_date"};
}

##-----------------------------------------------------------------------------
# @signature String getMinlat()
# <p>Get the minimum latitude of the file.</p>
#
# @output $minLat.
##-----------------------------------------------------------------------------
sub getMinlat {
        my $self = shift;
        return $self->{"minimum_latitude"}
}

##-----------------------------------------------------------------------------
# @signature String getMaxlat()
# <p>Get the maximum latitude of the file.</p>
#
# @output $maxLat.
##-----------------------------------------------------------------------------
sub getMaxlat {
        my $self = shift;
        return $self->{"maximum_latitude"}
}

##-----------------------------------------------------------------------------
# @signature String getMinlon()
# <p>Get the minimum lonitude of the file.</p>
#
# @output $minLong.
##-----------------------------------------------------------------------------
sub getMinlon {
        my $self = shift;
        return $self->{"minimum_longitude"}
}

##-----------------------------------------------------------------------------
# @signature String getMaxlon()
# <p>Get the maximum lonitude of the file.</p>
#
# @output $minLong.
##-----------------------------------------------------------------------------
sub getMaxlon {
        my $self = shift;
        return $self->{"maximum_longitude"}
}


##----------------------------------------------------------------------------
# @signature MySqlDataset new(unsigned long/String id)
# <p>Create a new dataset.</p>
#
# @input $id The dataset id.
# @output $self A MySqlDataset the represents the specified dataset id.
##----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    if (!defined($_[0])) {
	die("MySqlDataset->new requires a dataset id.\n");
    }

    if ($_[0] =~ m/^\d+$/) {
      $self->{"database_id"} = $_[0];
      $self->{"archive_ident"} = undef;
    } else {
      $self->{"database_id"} = undef;
      $self->{"archive_ident"} = $_[0];
    }

    return $self;
}

##----------------------------------------------------------------------------
# @signature String selectDataset(MySqlDatabase db)
# <p>Load the values in this MySqlDataset with the values in the database.</p>
#
# @input $db- An open database connection where the file should be inserted. 
#
# @output $err An error message or the empty string.
##----------------------------------------------------------------------------
sub selectDataset {
    my $self = shift;
    my $db = shift;
    my $err = '';

    if (defined($self->getDatabaseId())) {

    ($err,$self->{'begin_date'},$self->{'end_date'},$self->{'minimum_latitude'},$self->{'maximum_latitude'},$self->{'minimum_longitude'},$self->{'maximum_longitude'},$self->{'database_id'},$self->{'archive_ident'}) =
	$db->select('dataset','begin_date,end_date,minimum_latitude,maximum_latitude,minimum_longitude,maximum_longitude,id,archive_ident',
		    sprintf("id=%d",$self->{'database_id'}));

    } elsif (defined($self->getArchiveIdent())) {

    ($err,$self->{'begin_date'},$self->{'end_date'},$self->{'minimum_latitude'},$self->{'maximum_latitude'},$self->{'minimum_longitude'},$self->{'maximum_longitude'},$self->{'database_id'},$self->{'archive_ident'}) =
	$db->select('dataset','begin_date,end_date,minimum_latitude,maximum_latitude,minimum_longitude,maximum_longitude,id,archive_ident',
		    sprintf("archive_ident='%s'",$self->{'archive_ident'}));

    } else {
        $err = "No database ID or archive ID.\n";
    }

    return $err;
}

##----------------------------------------------------------------------------
# @signature void setBeginDate(int year, int month, int day, int hour, int min, int sec)
# <p>Set the begin date for the dataset.</p>
#
# @input $year The year of the date.
# @input $month The month of the date.
# @input $day The day of the date.
# @input $hour The hour of the time.
# @input $min The minute of the time.
# @input $sec The second of the time.
##----------------------------------------------------------------------------
sub setBeginDate {
    my $self = shift;
    $self->{"begin_date"} = sprintf("%04d-%02d-%02d %02d:%02d:%02d",@_);
}

##----------------------------------------------------------------------------
# @signature void setEndDate(int year, int month, int day, int hour, int min, int sec)
# <p>Set the end date for the dataset.</p>
#
# @input $year The year of the date.
# @input $month The month of the date.
# @input $day The day of the date.
# @input $hour The hour of the time.
# @input $min The minute of the time.
# @input $sec The second of the time.
##----------------------------------------------------------------------------
sub setEndDate {
    my $self = shift;
    $self->{"end_date"} = sprintf("%04d-%02d-%02d %02d:%02d:%02d",@_);
}

##-----------------------------------------------------------------------------
# @signature String setMinlat(int minLat)
# <p>Set the minimum latitude of the file.</p>
#
# @input $minLat.
##-----------------------------------------------------------------------------
sub setMinlat {
        my $self = shift;
        $self->{"minimum_latitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature String setMaxlat(int maxLat)
# <p>Set the maximum latitude of the file.</p>
#
# @input $maxLat.
##-----------------------------------------------------------------------------
sub setMaxlat {
        my $self = shift;
        $self->{"maximum_latitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature String setMinlon(int minLong)
# <p>Set the minimum lonitude of the file.</p>
#
# @input $minLong.
##-----------------------------------------------------------------------------
sub setMinlon {
        my $self = shift;
        $self->{"minimum_longitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature String setMaxlon(int maxLong)
# <p>Set the maximum lonitude of the file.</p>
#
# @input $maxLong.
##-----------------------------------------------------------------------------
sub setMaxlon {
        my $self = shift;
        $self->{"maximum_longitude"} = $_[0];
}


##----------------------------------------------------------------------------
# @signature String updateDataset(MySqlDatabase db)
# <p>Update the database with the current values in this dataset.</p>
#
# @input $db- An open database connection where the file should be inserted. 
#
# @output $err An error message or the empty string.
##----------------------------------------------------------------------------
sub updateDataset {
    my $self = shift;
    my $db = shift;

    my $command = "";
    foreach my $key (keys(%{$self})) {
         next if ($key eq "database_id");
         next if ($key eq "archive_ident");

         $command .= sprintf("%s='%s',", $key, $self->{$key});
    }
    $command =~ s/,$//;
    
    return $db->update("dataset",$command,sprintf("archive_ident='%s'",$self->{"archive_ident"}));
}

1;


