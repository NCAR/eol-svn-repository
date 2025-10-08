package ProductDBA;

# all procedures considered 'static'

use strict;
use IvenDB;
use Product;
use DatasetDBA;

# Returns an array reference
sub getByProject
{
	my $project = shift;
	my $order = shift;
	my @products;
	my $dbh = IvenDB::connect();

	$order = "pdname" if( !$order );

	my $sql = "SELECT * FROM products WHERE pjname = " . $dbh->quote( $project ) . " ORDER BY $order";
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


sub buildProductView
{
	my $pjname = shift;
	my $pdname = shift;
	my $dbh = IvenDB::connect();
	my $pd;
	
	my $sql = "SELECT * FROM products WHERE " .
						"pjname = " . $dbh->quote( $pjname ) . " AND " .
						"pdname = " . $dbh->quote( $pdname );

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @row = $sth->fetchrow();
	if( @row )
	{
		$pd = Product->new();
		$pd->set( \@row );
		$pd->{datasets} = DatasetDBA::getByProduct( $pd->{pdname}, $pd->{pjname} );		
	}
	else
	{
		$pd = undef();
	}

	return $pd;	
}

1;
