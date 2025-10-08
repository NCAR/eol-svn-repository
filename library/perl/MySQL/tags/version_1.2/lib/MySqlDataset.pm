#! /usr/bin/perl -w

##Module----------------------------------------------------------------------
# <p>The MySqlDataset.pm module is a partial representation of an entry from
# the dataset table of the database.  It is only able to update the begin and
# end dates of the dataset.  It does not handle inserting because that should
# be done through a different program.</p>
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
# <p>Get the id for the dataset.</p>
# @output $id The dataset id.
##----------------------------------------------------------------------------
sub getDatasetId {
    my $self = shift;
    return $self->{"dataset_id"};
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

##----------------------------------------------------------------------------
# @signature MySqlDataset new(String id)
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

    $self->{"dataset_id"} = $_[0];

    return $self;
}

##----------------------------------------------------------------------------
# @signature String selectDataset()
# <p>Load the values in this MySqlDataset with the values in the database.</p>
#
# @output $err An error message or the empty string.
##----------------------------------------------------------------------------
sub selectDataset {
    my $self = shift;
    my $db = shift;
    my $err = "";

    ($err,$self->{"begin_date"},$self->{"end_date"}) =
	$db->select("dataset","begin_date,end_date",
		    sprintf("dataset_id='%s'",$self->{"dataset_id"}));
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

##----------------------------------------------------------------------------
# @signature String updateDataset()
# <p>Update the database with the current values in this dataset.</p>
#
# @output $err An error message or the empty string.
##----------------------------------------------------------------------------
sub updateDataset {
    my $self = shift;
    my $db = shift;

    return $db->update("dataset",sprintf("begin_date='%s',end_date='%s'",
					 $self->{"begin_date"},$self->{"end_date"}),
		       sprintf("dataset_id='%s'",$self->{"dataset_id"}));
}

1;


