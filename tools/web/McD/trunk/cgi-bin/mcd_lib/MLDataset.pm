package MLDataset;

use lib ".";
use Dataset;
@ISA = ("Dataset");  # Inherit the Dataet class

# --------------------------------------------------------------
# MLDataset.pm:
#  Contains the MLDataset class
#
# Author - Dan Sullivan
# Date - July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - Returns a new instane of the MLDataset class
#
# sub set( $arr_ref ) - Sets the instance variables in this MLDataset with the
#  given values.  Array should contain all values from first the dataset
#  table and then the mlview table.
#
# sub checkDefined() - Sets default values for undefined values 
#  returned by the database
#
# sub dbPrepare() - Prepares the MLDataset to be entered into
#  the database.  In general sets blank text fields to undef();
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = $class->SUPER::new(@_);

	$self->{date} = "";
	$self->{hide} = 0;
	$self->{doc_url} = "";

	return $self;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->SUPER::set( $row );
	
	shift(@$row);
	$self->{date} = shift(@$row);
	$self->{hide} = shift(@$row);
	$self->{doc_url} = shift(@$row);

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{date} = defined( $self->{date})? $self->{date} : "0000-00-00";	
	$self->{hide} = defined( $self->{hide})? $self->{hide} : 0;	
	$self->{doc_url} = defined( $self->{doc_url})? $self->{doc_url} : "";	
}

sub dbPrepare
{
	my $self = shift;
	$self->SUPER::dbPrepare();

	$self->{date} = (defined( $self->{date} ) && $self->{date} ne "")? $self->{date} : undef();
	$self->{hide} = ($self->{hide})? $self->{hide} : 0;
	$self->{doc_url} = (defined( $self->{doc_url} ) && $self->{doc_url} ne "" )? $self->{doc_url} : undef();
}
1;
