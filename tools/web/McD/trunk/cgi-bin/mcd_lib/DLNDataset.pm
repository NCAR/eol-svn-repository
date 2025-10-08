package DLNDataset;

use lib ".";
use Dataset;
@ISA = ("Dataset");  # Inherit the Dataet class

# --------------------------------------------------------------
# DLNDataset.pm:
#  Contains the DLNDataset class
#
# Author - Dan Sullivan
# Date - July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - Returns a new instane of the DLNDataset class
#
# sub set( $arr_ref ) - Sets the instance variables in this DLNDataset with the
#  given values.  Array should contain all values from first the dataset
#  table and then the dlnview table.
#
# sub checkDefined() - Sets default values for undefined values 
#  returned by the database
#
# sub dbPrepare() - Prepares the DLNDataset to be entered into
#  the database.  In general sets blank text fields to undef();
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = $class->SUPER::new(@_);

	$self->{date} = "";
	$self->{loader} = "";
	$self->{checker} = "";
	$self->{int_contact} = "";
	$self->{ext_contact} = "";
	$self->{ext_email} = "";
	$self->{ingest} = "";
	$self->{archive} = "";
	$self->{notes} = "";

	return $self;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->SUPER::set( $row );
	
	shift(@$row);
	$self->{date} = shift(@$row);
	$self->{loader} = shift(@$row);
	$self->{checker} = shift(@$row);
	$self->{int_contact} = shift(@$row);
	$self->{ext_contact} = shift(@$row);
	$self->{ext_email} = shift(@$row);
	$self->{ingest} = shift(@$row);
	$self->{archive} = shift(@$row);
	$self->{notes} = shift(@$row);

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->SUPER::checkDefined();

	$self->{date} = defined( $self->{date})? $self->{date} : "0000-00-00";	
	$self->{loader} = defined( $self->{loader})? $self->{loader} : "";	
	$self->{checker} = defined( $self->{checker})? $self->{checker} : "";	
	$self->{int_contact} = defined( $self->{int_contact})? $self->{int_contact} : "";	
	$self->{ext_contact} = defined( $self->{ext_contact})? $self->{ext_contact} : "";	
	$self->{ext_email} = defined( $self->{ext_email})? $self->{ext_email} : "";	
	$self->{ingest} = defined( $self->{ingest})? $self->{ingest} : "";	
	$self->{archive} = defined( $self->{archive})? $self->{archive} : "";	
	$self->{notes} = defined( $self->{notes})? $self->{notes} : "";	
}

sub dbPrepare
{
	my $self = shift;
	$self->SUPER::dbPrepare();

	$self->{date} = (defined( $self->{date} ) && $self->{date} ne "")? $self->{date} : undef();
	$self->{loader} = (defined( $self->{loader} ) && $self->{loader} ne "" )? $self->{loader} : undef();
	$self->{checker} = (defined( $self->{checker} ) && $self->{checker} ne "" )? $self->{checker} : undef();
	$self->{int_contact} = (defined( $self->{int_contact} ) && $self->{int_contact} ne "" )? $self->{int_contact} : undef();
	$self->{ext_contact} = (defined( $self->{ext_contact} ) && $self->{ext_contact} ne "" )? $self->{ext_contact} : undef();
	$self->{ext_email} = (defined( $self->{ext_email} ) && $self->{ext_email} ne "" )? $self->{ext_email} : undef();
	$self->{ingest} = (defined( $self->{ingest} ) && $self->{ingest} ne "" )? $self->{ingest} : undef();
	$self->{archive} = (defined( $self->{archive} ) && $self->{archive} ne "" )? $self->{archive} : undef();
	$self->{notes} = (defined( $self->{notes} ) && $self->{notes} ne "" )? $self->{notes} : undef();
}
1;
