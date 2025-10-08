package ExtractionDBA;

use strict;
use lib ".";
use Extraction;
use IvenDB;
use User;

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

1;
