##Module-----------------------------------------------------------------------
# <p>The ProductDBA.pm module contains all functions needed to manipulate the
#  <i>products</i> table in the database.  All functions use the Product class
#  to insert, update, delete and select records.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------


package ProductDBA;

use strict;
use IvenDB;
use Product;
use DatasetDBA;

##-----------------------------------------------------------------------------
# @signature array_ref getByProduct( string $pjname, string $order )
# <p>Returns a reference to an array of Products associated with the given
#  project.</p>
#
# @input $pjname project name to match
# @input $order string to use in the WHERE clause - optional, default is
#   products.status.
#
# @output $products reference to an array of Products.
##-----------------------------------------------------------------------------
sub getByProject
{
	my $project = shift;
	my $order = shift;
	my @products;
	my $dbh = IvenDB::connect();

	#$order = "pdname" if( !$order );
	$order = "status" if( !$order );

	my $sql = "SELECT * FROM products WHERE pjname = " . $dbh->quote( $project ) . 
						" ORDER BY $order";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( my @row = $sth->fetchrow() )
	{
		my $p = Product->new();
		$p->set( \@row );
		$p->{datasets} = DatasetDBA::getByProduct( $p->{pdname}, $p->{pjname} );		
		$products[scalar(@products)] = $p;
	}	

	return \@products;
}

##-----------------------------------------------------------------------------
# @signature Product buildProductView( string $pjname, string $pdname )
# <p>Queries the database and builds the needed data structure for the 
#  product_view.  Returns an instance of Product with of array of Datasets
#  returned by DatasetDBA::getByProduct referenced by 'datasets'.
# 
# @input $pjname project name to match
# @input $pdname product name to match
#
# @output $prod Product populated from the database
##-----------------------------------------------------------------------------
sub buildProductView
{
	my $pjname = shift;
	my $pdname = shift;
	my $dbh = IvenDB::connect();
	my $pd;

	$pd = getFromDB( $pdname, $pjname );
	
	if( $pd )
	{
		$pd->{datasets} = DatasetDBA::getByProduct( $pd->{pdname}, $pd->{pjname} );		
	}
	else
	{
		$pd = undef();
	}

	return $pd;	
}

##-----------------------------------------------------------------------------
# @signature Product getFromDB( string $pdname, string $pjname )
# <p>Returns the Product with the given name an project populated from the 
# database.</p>
#
# @input $pdname product to match
# @input $pjname project to match
#
# @output $product instance of Product, returns undef() if no match found
#
##-----------------------------------------------------------------------------
sub getFromDB
{
	my $pdname = shift;
	my $pjname = shift;
	my $pd = undef();
	my $dbh = IvenDB::connect();

	my $sql = "SELECT * FROM products WHERE " .
						"pdname = " . $dbh->quote( $pdname ) . " AND " .
						"pjname = " . $dbh->quote( $pjname );

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @row = $sth->fetchrow();

	if( @row )
	{
		$pd = Product->new();
		$pd->set( \@row );
	}

	return $pd;
}

##-----------------------------------------------------------------------------
# @signature void insertDB( Product $pd )
# <p>Inserts the given product into the database.</p>
#
# @input $pd product to insert
#
##-----------------------------------------------------------------------------
sub insertDB
{
	my $pd = shift;
	$pd->dbPrepare();
	my $dbh = IvenDB::connect();

	my $sql = "INSERT INTO products VALUES ( " . 
						$dbh->quote( $pd->{pdname} ) . ", " .
						$dbh->quote( $pd->{pjname} ) . ", " .
						$dbh->quote( $pd->{status} ) . ", " .
						$dbh->quote( $pd->{note} ) . ", " .
						$dbh->quote( $pd->{product_link} ) . ", " .
						$dbh->quote( $pd->{product_link_url} ) . ")";

	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void updateDB( Product $pd, string $source_pdname )
# <p>Updates the given product in the database.  The source_pdname is the 
# original product name retrieved from the database, pdname in $pd could have
# been modified by the user.
#
# @input $pd Prdouct to update
# @input $srouce_pdname product name to match in WHERE clause
#
##-----------------------------------------------------------------------------
sub updateDB
{
	my $pd = shift;
	$pd->dbPrepare();
	my $source_pdname = shift;
	my $dbh = IvenDB::connect();

	my $sql = "UPDATE products SET " .
						"pdname = " . $dbh->quote( $pd->{pdname} ) . ", " .
						"status = " . $dbh->quote( $pd->{status} ) . ", " .
						"note = " . $dbh->quote( $pd->{note} ) . ", " .
						"product_link = " . $dbh->quote( $pd->{product_link} ) . ", " .
						"product_link_url = " . $dbh->quote( $pd->{product_link_url} ) . 
						" WHERE " . 
						"pjname = " . $dbh->quote( $pd->{pjname} ) . " AND " .
						"pdname = " . $dbh->quote( $source_pdname );

	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void deleteFromDB( Product $pd )
# <p>Deletes the given product from the database.</p>
#
# @input $pd Product to delete.
##-----------------------------------------------------------------------------
sub deleteFromDB
{
	my $pd = shift;
	my $dbh = IvenDB::connect();

	my $sql = "DELETE FROM products WHERE " .
						"pjname = " . $dbh->quote( $pd->{pjname} ) . " AND " .
						"pdname = " . $dbh->quote( $pd->{pdname} );

	$dbh->do( $sql );
}

1;
