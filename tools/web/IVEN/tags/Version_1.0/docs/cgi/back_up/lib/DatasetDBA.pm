package DatasetDBA;

use strict;
use IvenDB;
use Dataset;
use User;
use ExtractionDBA;

# the pdname and pjname are necessary
# returns array ref
sub getByProduct
{
	my $pdname = shift;
	my $pjname = shift;
	my $order = shift;
	my $dbh = IvenDB::connect();
	my @dss;

	# This could be done more efficiently if MySQL could do subqueries!!
	my $ehash = ExtractionDBA::getByProductHash( $pdname, $pjname );

	#$order = "dsname" if( !$order );
	$order = "status DESC" if( !$order );

	my $sql = "SELECT * FROM datasets, users  WHERE " . 
						"datasets.pdname = " . $dbh->quote( $pdname ) . " AND " .  	
						"datasets.pjname = " . $dbh->quote( $pjname ) . " AND " . 
						"datasets.user_processing = users.uid " . 
						"ORDER BY $order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $ds = Dataset->new();
		$ds->set( \@row );
		my $us = User->new();
		$us->set( \@row );
		$ds->{user_processing_object} = $us; 

		if( defined( $ehash->{$ds->{dsname}} ) )
		{
			$ds->{is_extraction} = 1;
			$ds->{extraction_object} = $ehash->{$ds->{dsname}};
		}

		$dss[scalar(@dss)] = $ds;
	}

	return \@dss;
}

sub getFromDB
{
	my $dsname = shift;
	my $pdname = shift;
	my $pjname = shift;
	my $dbh = IvenDB::connect();

	my $ds = undef();

	my $ehash = ExtractionDBA::getByProductHash( $pdname, $pjname );

	my $sql = "SELECT * FROM datasets, users WHERE " . 
						"datasets.dsname = " . $dbh->quote( $dsname ) . " AND " .  	
						"datasets.pdname = " . $dbh->quote( $pdname ) . " AND " .  	
						"datasets.pjname = " . $dbh->quote( $pjname ) . " AND " . 
						"datasets.user_processing = users.uid ";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @row = $sth->fetchrow();
	if( @row )
	{
		$ds = Dataset->new();
		$ds->set( \@row );
		my $us = User->new();
		$us->set( \@row );

		if( defined( $ehash->{$ds->{dsname}} ) )
		{
			$ds->{is_extraction} = 1;
			$ds->{extraction_object} = $ehash->{$ds->{dsname}};
		}

		$ds->{user_processing_object} = $us; 
	}  

	return $ds;
}


1;
