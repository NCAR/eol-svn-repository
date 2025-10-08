package Category;

# --------------------------------------------------------------
# Category.pm:
#  Holds the Category class to query and update the category
#   table in the ml database.  A single instance of this class
#   corresponds to one record in the category table.  
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# 
# Subroutines:
# 
# sub new() - constructor, returns a new instance of Category
#
# sub set( @vals ) - sets all fields in this Category with the given
#  values.  
#
# sub checkDefined() - Set default values for undef vales returned
#  from the database.  This prevents trying to do operations on
#  undef values.
# --------------------------------------------------------------


use lib ".";
use CategoryNode;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{name} = "";
	$self->{parent} = undef();
	$self->{project} = -1;

	return $self;
}

sub set
{
	my $self = shift;
	my @row = @_;

	$self->{id} = $row[0];	
	$self->{name} = $row[1];	
	$self->{parent} = $row[2];	
	$self->{project} = $row[3];	

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = ( defined( $self->{id} ) )? $self->{id} : -1;
	$self->{name} = ( defined( $self->{name} ) )? $self->{name} : "";
	$self->{parent} = ( defined( $self->{parent} ) )? $self->{parent} : undef();
	$self->{project} = ( defined( $self->{project} ) )? $self->{project} : -1;
}

1;
