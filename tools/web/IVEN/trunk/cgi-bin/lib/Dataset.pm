##Module-----------------------------------------------------------------------
# <p>The Dataset.pm module contains the Dataset class which is simply a 
#  container used to hold a record of the datasets table.</p>
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package Dataset;

use strict;

##-----------------------------------------------------------------------------
# @signature Dataset new()
# <p>Creates a new instance of the Dataset class</p>
#
# @output $self reference to the Dataset instance.
##-----------------------------------------------------------------------------
sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	return $self;
}

##-----------------------------------------------------------------------------
# @signature void checkDefined()
# <p>Simple function to set all undefined/NULL values to empty character 
#  strings so they are printable.  Avoids a lot of runtime errors.</p>
##-----------------------------------------------------------------------------
sub checkDefined
{
	my $self = shift;

	foreach my $param ("dsname","pdname","pjname","storm_id","user_processing",
                     "raw_data","final_data","station_info","software","plots",
                     "status_notes","admin_notes","status","how_to","readme")
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : "";
	}

	foreach my $param ("id_type", "platform_type")
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : undef();
	}

	foreach my $param ("exclude", "questions", "best_sw" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : 0;
	}
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref $row )
#  <p>Populates this Dataset with the given row of data from the datasets table.
#  Each value is removed from the array and stored in the order the datsets
#  table was created.  For use with a <code>Select * ...</code> type query.
#  Calls the checkDefined() function.
#
# @input $row a reference to the array of data retrieved from the database.
##-----------------------------------------------------------------------------
sub set
{
	my $self = shift;
	my $row = shift;

	$self->{dsname} = shift( @$row );
	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{storm_id} = shift( @$row );
	$self->{user_processing} = shift( @$row );
	$self->{raw_data} = shift( @$row );
	$self->{final_data} = shift( @$row );
	$self->{station_info} = shift( @$row );
	$self->{software} = shift( @$row );
	$self->{plots} = shift( @$row );
	$self->{id_type} = shift( @$row );
	$self->{platform_type} = shift( @$row );
	$self->{status_notes} = shift( @$row );
	$self->{admin_notes} = shift( @$row );
	$self->{status} = shift( @$row );
	$self->{exclude} = shift( @$row );
	$self->{questions} = shift( @$row );
	$self->{how_to} = shift( @$row );
	$self->{readme} = shift( @$row );
	$self->{best_sw} = shift( @$row );

	$self->checkDefined();
}

##-----------------------------------------------------------------------------
# @signature void dbPrepare()
# <p>Prepares this Dataset for entry into the database, seting empty string 
#  characters and undefined values to NULL.
##-----------------------------------------------------------------------------
sub dbPrepare
{
	my $self = shift;

	foreach my $param ("storm_id", "user_processing",
                     "raw_data", "final_data", "station_info", "software", "plots",
                     "status_notes", "admin_notes", "status", "how_to", "readme" )
	{
		$self->{$param} = defined( $self->{$param} ) && $self->{$param} ne "" ? 
				$self->{$param} : undef();
	}

	foreach my $param ("id_type", "platform_type" )
	{
		$self->{$param} = defined( $self->{$param} ) && $self->{$param} ne "" ? 
				$self->{$param} : "NULL";
	}

	foreach my $param ("exclude", "questions", "best_sw" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : 0;
	}
}

1;
