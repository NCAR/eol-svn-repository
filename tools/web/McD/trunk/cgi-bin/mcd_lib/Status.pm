package Status;

# --------------------------------------------------------------
# Status.pm:
#  Contains the Status class used to perform operations on the
#  database.  A single instance of the Status class corresponds
#  to an entry in the status table.
# 
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - returns a new instance of the Status class
#
# sub set( @vals ) - assigns all of the fields in this Status
#  with the given values.
#
# sub checkDefined() - set default values to fields with undef()
#
# sub dbPrepare() - Prepares the Status to be entered into
#  the database.  In general sets blank text fields to undef();
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{dataset} = 0;
	$self->{documented} = 0;
	$self->{checked} = 0;
	$self->{loaded} = 0;
	$self->{in_progress} = 0;
	$self->{updated} = 0;
	$self->{new} = 0;

	return $self;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{dataset} = shift(@$row);
	$self->{documented} = shift(@$row);
	$self->{checked} = shift(@$row);
	$self->{loaded} = shift(@$row);
	$self->{in_progress} = shift(@$row);
	$self->{new} = shift(@$row);
	$self->{updated} = shift(@$row);

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{dataset} = defined( $self->{dataset} )? $self->{dataset} : 0;

	# leave the rest
}

sub dbPrepare
{
	my $self = shift;

	$self->{dataset} = defined( $self->{dataset} )? $self->{dataset} : 0;

	$self->{documented} = ( $self->{documented} )? 1 : 0;
	$self->{checked} = ( $self->{checked} )? 1 : 0;
	$self->{loaded} = ( $self->{loaded} )? 1 : 0;
	$self->{in_progress} = ( $self->{in_progress} )? 1 : 0;
	$self->{updated} = ( $self->{updated} )? 1 : 0;
	$self->{new} = ( $self->{new} )? 1 : 0;
}
1;
