##Module-----------------------------------------------------------------------
# <p>The User.pm module contains the User class which is a simple container
#  class to hold one row of the <i>users</i> table.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package User;

use strict;

##-----------------------------------------------------------------------------
# @signature User new()
# <p>Creates a new instance of the User class.</p>
#
# @output $self reference to the new User.
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
#  <p>Sets all undefined/NULL values to empty character strings so they are
#  printable.  Avoids many runtime errors.</p>
##-----------------------------------------------------------------------------
sub checkDefined
{
	my $self = shift;

	$self->{uid} = ( defined( $self->{uid} ) )? $self->{uid} : -1;
	$self->{fname} = ( defined( $self->{fname} ) )? $self->{fname} : "";
	$self->{lname} = ( defined( $self->{lname} ) )? $self->{lname} : "";
	$self->{email} = ( defined( $self->{email} ) )? $self->{email} : "";
	$self->{active} = ( defined( $self->{active} ) )? $self->{active} : 0;
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref, $row )
# <p>Populates this user with the values in the array. </p>
#
# @input $row reference to the array of data.
##-----------------------------------------------------------------------------
sub set
{
	my $self = shift;
	my $row = shift;

	$self->{uid} = shift( @$row );
	$self->{fname} = shift( @$row );
	$self->{lname} = shift( @$row );
	$self->{email} = shift( @$row );
	$self->{active} = shift( @$row );

	$self->checkDefined();
}

sub dbPrepare
{
	my $self = shift;

	$self->{fname} = ( $self->{fname} && $self->{fname} ne "" )? $self->{fname} : undef();
	$self->{lname} = ( $self->{lname} && $self->{lname} ne "" )? $self->{lname} : undef();
	$self->{email} = ( $self->{email} && $self->{email} ne "" )? $self->{email} : undef();
	$self->{active} = ( defined( $self->{active} ) )? $self->{active} : 1;
}

1;
