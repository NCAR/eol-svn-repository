##Module-----------------------------------------------------------------------
# <p>The ExtractoinDBA.pm module contains all the functions needed to manipulate
#  the <i>extractions</i> table.  All functions use the Extraction class to
#  insert, update, delete and select records.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package ExtractionDBA;

use strict;
use lib ".";
use Extraction;
use IvenDB;
use User;

##-----------------------------------------------------------------------------
# @signature array_ref getByProduct( string $pdname, string $pjname )
# <p>Returns a reference to an array of Extractions associated with the given
#  product.</p>
#
# @input $pdname the name of the product to match
# @input $pjname the name of the project to match
#
# @output $exts reference to an array of Extraction objects. 
##-----------------------------------------------------------------------------
sub getByProduct
{
	my $pdname = shift;
	my $pjname = shift;
	my @exts;
	my $dbh = IvenDB::connect();

	my $sql = "SELECT * FROM datasets, extractions, users " . 
						"WHERE " . 
						"extractions.pdname=" . $dbh->quote( $pdname ) . " AND " .
						"extractions.pjname=" . $dbh->quote( $pjname ) . " AND " . 
						"extractions.dsname=datasets.dsname AND " . 
						"extractions.pdname=datasets.pdname AND " . 
						"extractions.pjname=datasets.pjname AND " . 
						"datasets.user_processing=users.uid";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $ds = Dataset->new();
		$ds->set( \@row );
		my $ext = Extraction->new();
		$ext->set( \@row );
		my $us = User->new();
		$us->set( \@row );
		$ds->{user_processing_object} = $us;
		$ext->{dataset_object} = $ds;
		$exts[scalar(@exts)] = $ext;
	}
	return \@exts;
}


##-----------------------------------------------------------------------------
# @signature array_ref getByProject( string $pjname )
# <p>Returns a reference to an array of extractions in the given project.</a>
##-----------------------------------------------------------------------------
sub getByProject
{
	my $pjname = shift;
	my $exclude = shift;

	my @exts;

	my $dbh = IvenDB::connect();

	my $sql = "SELECT extractions.* FROM datasets, extractions " . 
						"WHERE " .
						"datasets.dsname=extractions.dsname AND " . 
						"datasets.pdname=extractions.pdname AND " . 
						"datasets.pjname=extractions.pjname AND " . 
						"extractions.pjname=" . $dbh->quote( $pjname );

	if( defined( $exclude ) )
	{ $sql = $sql . " AND datasets.exclude=$exclude"; }

	my $sth = $dbh->prepare($sql);
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $ext = Extraction->new();
		$ext->set( \@row );
		$exts[scalar(@exts)] = $ext;
	}

	return \@exts;
}

##-----------------------------------------------------------------------------
# @signature array_ref getByUser( string $uid, string $pjname )
#  <p>Returns a reference to an array of extractions beining processed by the 
#  given user.</p>
#
# @input $uid user id to match in the datasets table
# @input $pjname the project name to match
#
# @output a reference to an array of Extractions objects which contains an 
#  instance to a Dataset object referenced by 'dataset_object'.
##-----------------------------------------------------------------------------
sub getByUser
{
	my $uid = shift;
	my $pjname = shift;
	my @exts;
	my $dbh = IvenDB::connect();

	my $sql = "SELECT * FROM datasets, extractions, users " . 
						"WHERE " . 
						"extractions.pjname=" . $dbh->quote( $pjname ) . " AND " . 
						"datasets.user_processing=" . $dbh->quote( $uid ) . " AND " . 
						"extractions.dsname=datasets.dsname AND " . 
						"extractions.pdname=datasets.pdname AND " . 
						"extractions.pjname=datasets.pjname AND " . 
						"datasets.user_processing=users.uid";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $ds = Dataset->new();
		$ds->set( \@row );
		my $ext = Extraction->new();
		$ext->set( \@row );
		my $us = User->new();
		$us->set( \@row );
		$ds->{user_processing_object} = $us;
		$ext->{dataset_object} = $ds;
		$exts[scalar(@exts)] = $ext;
	}

	return \@exts;
}

##-----------------------------------------------------------------------------
# @signature Extraction getFromDB(string $dsname,string $pdname,string $pjname)
# <p>Returns an Extraction which matches the given dataset, product, and 
#  project names.</p>
#
# @input $dsname the dataset name to match
# @input $pdname the product name to match
# @input $pjname the project name to match
#
# @output $ext an Extraction containing an instance of Dataset referenced by 
#  'dataset_object'.  Returns undef() if no match is found. 
##-----------------------------------------------------------------------------
sub getFromDB
{
	my $dsname = shift;
	my $pdname = shift;
	my $pjname = shift;
	my $ext = undef();
	my $dbh = IvenDB::connect();

	my $sql = "SELECT * FROM datasets, extractions, users " . 
						"WHERE " . 
						"extractions.pdname=" . $dbh->quote( $pdname ) . " AND " .
						"extractions.pjname=" . $dbh->quote( $pjname ) . " AND " . 
						"extractions.dsname=" . $dbh->quote( $dsname ) . " AND " . 
						"extractions.dsname=datasets.dsname AND " . 
						"extractions.pdname=datasets.pdname AND " . 
						"extractions.pjname=datasets.pjname AND " . 
						"datasets.user_processing=users.uid";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	
	my @row = $sth->fetchrow();

	if( @row )
	{
		$ext = Extraction->new();
		my $ds = Dataset->new();
		my $us = User->new();

		$ds->set( \@row );
		$ext->set( \@row );
		$us->set( \@row );
		$ds->{user_processing_object} = $us;
		$ext->{dataset_object} = $ds;
	}

	return $ext;
}

##-----------------------------------------------------------------------------
# @signature hash_ref getByProductHash( string $pdname, string $pjname )
# <p>Creates a hash of the extractions associed with the given product.  
#  The key of the hash is the dsname of the extraction.  This is a little 
#  workaround to determine if a dataset is an extraction.  This could be
#  done more efficiently if MySQL allowed for sub-queries (not until 5.x)
#
# @input $pdname the product name to match
# @input $pjname the project name to match
#
# @output $ehash reference to a hash table containg the Extractions returned by
#  the getByProduct() function.
#
##-----------------------------------------------------------------------------
sub getByProductHash
{
	my $pdname = shift;
	my $pjname = shift;
	my %hash;

	my $exts = getByProduct( $pdname, $pjname );

	foreach my $ext (@$exts)
	{
		$hash{$ext->{dsname}} = $ext;
	}

	return \%hash;
}

##-----------------------------------------------------------------------------
# @signature hash_ref getByUserHash( string $uid, string $pjname )
# <p>Creates a hash of the extractions associed with the given user.  
#  The key of the hash is the dsname of the extraction.  This is a little 
#  workaround to determine if a dataset is an extraction.  This could be
#  done more efficiently if MySQL allowed for sub-queries (not until 5.x)
#
# @input $uid the user to match in datasets:user_processing
# @input $pjname the project name to match
#
# @output $ehash reference to a hash table containg the Extractions returned by
#  the getByUser() function.
##-----------------------------------------------------------------------------
sub getByUserHash
{
	my $uid = shift;
	my $pjname = shift;
	my %hash;

	my $exts = getByUser( $uid, $pjname );

	foreach my $ext (@$exts)
	{
		$hash{$ext->{dsname}} = $ext;
	}

	return \%hash;
}

##-----------------------------------------------------------------------------
# @signature void insertDB( Extraction $ex )
# <p>Inserts the the given extraction into the database.</p>
#
# @input $ex instance of Extraction to insert
##-----------------------------------------------------------------------------
sub insertDB
{
	my $ex = shift;
	my $dbh = IvenDB::connect();

	$ex->dbPrepare();

	my $sql = "INSERT INTO extractions VALUES (" . 
						$dbh->quote( $ex->{dsname} ) . ", " .
						$dbh->quote( $ex->{pdname} ) . ", " .
						$dbh->quote( $ex->{pjname} ) . " , " .
						$dbh->quote( $ex->{pdsource} ) . " , " .
						$dbh->quote( $ex->{pjsource} ) . ")";

	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void deleteFromDB( Extraction $ex )
# <p>Deletes the given extraction from the database.</p>
#
# @input $ex instance of Extraction to delete.
##-----------------------------------------------------------------------------
sub deleteFromDB
{
	my $ex = shift;
	my $dbh = IvenDB::connect();

	my $sql = "DELETE FROM extractions WHERE " . 
						"dsname = " . $dbh->quote( $ex->{dsname} ) . " AND " . 
						"pdname = " . $dbh->quote( $ex->{pdname} ) . " AND " . 
						"pjname = " . $dbh->quote( $ex->{pjname} );

	$dbh->do( $sql );
}

1;
