##Module-----------------------------------------------------------------------
# <p>The Extraction.pm module contains the Extraction class which is a simple
#  container class to hold one record of the <i>extractions</i> table.</p>
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package Extraction;

use strict;

##-----------------------------------------------------------------------------
# @signature Extraction new()
# <p>Creates a new instance of the Extraction class.</p>
#
# @output $self reference to the new Extraction.
##-----------------------------------------------------------------------------
sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	$self->{dataset_object} = undef();

	return $self;
}

##-----------------------------------------------------------------------------
# @signature void checkDefined()
#  <p>Sets all undefined/NULL values to empty character strings so they are
#  printable.  Avoids runtime errors.</p>
##-----------------------------------------------------------------------------
sub checkDefined
{
	my $self = shift;

	foreach my $p ("dsname", "pdname", "pjname", "pdsource", "pjsource")
	{
		$self->{$p} = defined( $self->{$p} )? $self->{$p} : "";
	}
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref $row )
# <p>Populates this Extraction with the values contained in the array.  Each
#  attribute value is popped of the array in the order the extractions table
#  was created.  For use with <code>Select * FROM extractions</code> queries.
# </p>
#
# @input $row reference to the row of attribute values.
##-----------------------------------------------------------------------------
sub set
{
	my $self = shift;
	my $row = shift;

	$self->{dsname} = shift( @$row );
	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{pdsource} = shift( @$row );
	$self->{pjsource} = shift( @$row );

	$self->checkDefined();
}

##-----------------------------------------------------------------------------
# @signature void dbPrepare()
#  <p>Prepares the extraction for insertion into the database.  Currently this
#  function does nothing but is included for consistency.</p>
##-----------------------------------------------------------------------------
sub dbPrepare
{
	my $self = shift;
}

1;
