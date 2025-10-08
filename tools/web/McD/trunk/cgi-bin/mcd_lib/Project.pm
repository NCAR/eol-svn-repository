package Project;

# --------------------------------------------------------------
# Project.pm:
#  Contains the Project class used to perform operations on the
#  database.  A single instance of the Project class corresponds
#  to an entry in the project table.
# 
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - returns a new instance of the Project class
#
# sub set( @vals ) - assigns all of the fields in this Project
#  with the given values.
#
# sub checkDefined() - set default values to fields with undef()
#
# sub dbPrepare() - prepare the Project to be added/updated to 
#  database
# --------------------------------------------------------------

use lib ".";
use McDDB;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{name} = "";
	$self->{long_name} = "";
	$self->{active} = 1;
	$self->{internal_contact} = "";
	$self->{storm_id_prefix} = "";

	return $self;
}

sub set
{
	my $self = shift;
	my $row = shift;

	$self->{id} = shift( @$row );
	$self->{name} = shift( @$row );
	$self->{long_name} = shift( @$row );
	$self->{active} = shift( @$row );
	$self->{internal_contact} = shift( @$row );
	$self->{storm_id_prefix} = shift( @$row );

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{name} = ( defined( $self->{name} ) )? $self->{name}: "";
	$self->{long_name} = ( defined( $self->{long_name} ) )? $self->{long_name}: "";
	$self->{active} = ( defined( $self->{active} ) )? $self->{active}: 0;
	$self->{internal_contact} = ( defined( $self->{internal_contact} ) )? $self->{internal_contact}: "";
	$self->{storm_id_prefix} = ( defined( $self->{storm_id_prefix} ) )? $self->{storm_id_prefix}: "";
} 

sub dbPrepare
{
	my $self = shift;

	$self->{name} = ( $self->{name} ne "" )? $self->{name}: "";
	$self->{long_name} = ( $self->{long_name} ne "" )? $self->{long_name}: "";
	$self->{active} = ( $self->{active}  )? 1 : 0;
	$self->{internal_contact} = ( $self->{internal_contact}  ne "" )? $self->{internal_contact}: undef();
	$self->{storm_id_prefix} = ( $self->{storm_id_prefix} ne "" )? $self->{storm_id_prefix} : undef;
}

1;
