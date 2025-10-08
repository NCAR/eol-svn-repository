##Module-----------------------------------------------------------------------
# <p>The Product.pm module contains the Product class which is a simple 
#  container class to hold one row of the <i>products</i> table.
#
# @author Dan Sullivan 
##Module-----------------------------------------------------------------------

package Product;

use strict;

##-----------------------------------------------------------------------------
# @signature Product new()
#  <p>Creates a new instance of the Extraction class.</p>
#
# @output $self reference to the Extraction.
##-----------------------------------------------------------------------------
sub new
{
	my $class = shift;	
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();
	
	$self->{datasets} = undef();
	$self->{extractions} = undef();

	return $self;
}

##-----------------------------------------------------------------------------
# @signature void checkDefined()
# <p>Sets all undefined/NULL values to empty character strings so they are
#  printable.  Avoids many runtime errors.</p>
##-----------------------------------------------------------------------------
sub checkDefined
{
	my $self = shift;

	$self->{pdname} = defined( $self->{pdname} )? $self->{pdname} : "";
	$self->{pjname} = defined( $self->{pjname} )? $self->{pjname} : "";
	$self->{status} = defined( $self->{status} )? $self->{status} : "";
	$self->{note} = defined( $self->{note} )? $self->{note} : "";
	$self->{product_link} = defined( $self->{product_link} )? $self->{product_link} : "";
	$self->{product_link_url} = defined( $self->{product_link_url} )? $self->{product_link_url} : "";
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref $row )
#  <p>Populates this Product with the values in the array.  Each attribute
#  value is popped off the array in the order the <i>products</i> table was
#  created.
#
# @input $row reference to the array of data.
##-----------------------------------------------------------------------------
sub set
{
	my $self = shift;
	my $row = shift;

	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{status} = shift( @$row );
	$self->{note} = shift( @$row );
	$self->{product_link} = shift( @$row );
	$self->{product_link_url} = shift( @$row );

	$self->checkDefined();
}

##-----------------------------------------------------------------------------
# @signature void dbPrepare()
#  <p>Prepares the Product for insertion into the database</p>
##-----------------------------------------------------------------------------
sub dbPrepare
{
	my $self = shift;
	
	foreach my $param ("note", "product_link", "product_link_url")
	{
		$self->{$param} = ( defined($self->{$param}) && $self->{$param} ne "" )? $self->{$param} : undef();
	}

	$self->{status} = ( defined($self->{status}) && $self->{status} ne "" )? $self->{status} : "tbd";
}

1;
