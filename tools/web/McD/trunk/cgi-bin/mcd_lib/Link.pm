package Link;

# --------------------------------------------------------------
# Link.pm:
#  Contains the Link class used to perform operations on the link
#  table in the database.  A single instance of Link corresponds
#  to a record in the link table.
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
# sub new() - creates a new instance of the Link class.
#
# sub set( @values ) - sets the fields in this Link with the given values.
#
# sub checkDefined() - sets default values for undefs returned
#  by the database
#
# --------------------------------------------------------------

use lib ".";

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{name} = "";
	$self->{url} = "";
	$self->{target} = "";

	return $self;
}

sub set
{
	my $self = shift;
	my @arr = @_;

	$self->{id} = $arr[0];
	$self->{name} = $arr[1];
	$self->{url} = $arr[2];
	$self->{target} = $arr[3];

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = defined( $self->{id} )? $self->{id} : -1;
	$self->{name} = defined( $self->{name} )? $self->{name} : "";
	$self->{url} = defined( $self->{url} )? $self->{url} : "";
	$self->{target} = undef() if( defined( $self->{target} ) && $self->{target} eq "" );
}

1;
