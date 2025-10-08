##Module-----------------------------------------------------------------------
# <p>The Project.pm module contains the Project class which is a simple 
#  container used to hold a record of the <i>projects</i> table.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package Project;

use strict;

##-----------------------------------------------------------------------------
# @signature Project new()
# <p>Creates a new instance of the Project class.</p>
#
# @output $self the new Project
##-----------------------------------------------------------------------------
sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	$self->{products} = undef();

	return $self;
}

##-----------------------------------------------------------------------------
# @signature void checkDefined()
# <p>Simple function to set all undefined/NULL values to empty character 
#  strings.  Avoids many runtime errors.<p>
##-----------------------------------------------------------------------------
sub checkDefined
{
	my $self = shift;

	foreach my $param ("pjname","full_name","storm_id_prefix","begin_date",
										 "end_date","minlat","maxlat","minlon","maxlon",
										 "admin_notes","link_source","link_target","charge_num")
	{
		$self->{$param} = ( defined( $self->{$param} ) )? $self->{$param} : "";
	}
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref $row )
# <p>Populates this Project with the given row of data from the projects table.
#  Each value is removed from the array (using shift) and stored in the order
#  the projects table was created.  For user with SELECT * FROM projects
#  type qureies.  Calls the checkDefined() function. </p>
##-----------------------------------------------------------------------------
sub set
{
	my $self = shift;
	my $row = shift;

	$self->{pjname} = shift( @$row );
	$self->{full_name} = shift( @$row );
	$self->{storm_id_prefix} = shift( @$row );
	$self->{begin_date} = shift( @$row );
	$self->{end_date} = shift( @$row );
	$self->{minlat} = shift( @$row );
	$self->{maxlat} = shift( @$row );
	$self->{minlon} = shift( @$row );
	$self->{maxlon} = shift( @$row );
	$self->{admin_notes} = shift( @$row );
	$self->{link_source} = shift( @$row );
	$self->{link_target} = shift( @$row );
	$self->{charge_num} = shift( @$row );

	$self->checkDefined();
}

##-----------------------------------------------------------------------------
# @signature void dbPrepare()
# <p>Prepares the Project to be inserted (or updated) into the database.  
#  Sets all empty character strings to NULL values.</p>
##-----------------------------------------------------------------------------
sub dbPrepare
{
	my $self = shift;

	foreach my $param ("pjname","full_name","storm_id_prefix","begin_date",
										 "end_date", "admin_notes","link_source","link_target","charge_num")
	{
		$self->{$param} = ( $self->{$param} && $self->{$param} ne "" )? 
			$self->{$param} : undef();
	}

	# Make the min/max lat/longs equal to "NULL" if not entered	
	foreach my $param ( "minlat","maxlat","minlon","maxlon")
	{
		$self->{$param} = ( $self->{$param} && $self->{$param} ne "" )? 
			$self->{$param} : "NULL";
	}

	# This is a little shortcut, the storm_id_prefix in the database cannot be NULL
	#  There is no reason as to why it can't be NULL, but now's not the time to be messing with the database - Dan
	if( !defined( $self->{storm_id_prefix} ) )
	{ $self->{storm_id_prefix} = "99"; }
}
1;
