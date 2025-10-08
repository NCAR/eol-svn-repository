##Module-----------------------------------------------------------------------
# <p>The DatasetDBA.pm module contains functions used to manipulate the 
#  <i>datasets</i> table.  All database queries are performed by using
#  instances of the Dataset class.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------


package DatasetDBA;

use strict;
use IvenDB;
use Dataset;
use User;
use ExtractionDBA;

##-----------------------------------------------------------------------------
# @signature array_ref getByProduct(string $pdname,string $pjname,string $ordr)
# <p>Quries the <i>datasets</i> table for datasets in product $pdname and 
#  $pjname.  The sort order can be specified by the order parameter.</p>
# 
# @input $pdname the product name to match
# @input $pjname the project name to match
# @input $order the field(s) to use in the ORDER BY clause of the query 
#   (optional).
#
# @output $dss reference to an array of Dataset objects. 
##-----------------------------------------------------------------------------
sub getByProduct
{
	my $pdname = shift;
	my $pjname = shift;
	my $order = shift;
	my $dbh = IvenDB::connect();
	my @dss;

	# This could be done more efficiently if MySQL could do subqueries!!
	my $ehash = ExtractionDBA::getByProductHash( $pdname, $pjname );

	$order = "datasets.status, datasets.dsname" if( !$order );

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

# the uid and pjname are necessary
# returns array ref
##-----------------------------------------------------------------------------
# @signature array_ref getByUser( string $uid, string $pjname, string $order )
# <p>Queries the <i>datasets</i> table for datasets being processed by $uid
#  and in project $pjname.
#
# @input $uid the user id to match
# @input $pjname the project to match
# @input $order the field(s) to use in the ORDER BY clause (optional).
#
# @output $dss reference to the array of Dataset objects.
##-----------------------------------------------------------------------------
sub getByUser
{
	my $uid = shift;
	my $pjname = shift;
	my $order = shift;
	my $dbh = IvenDB::connect();
	my @dss;

	# This could be done more efficiently if MySQL could do subqueries!!
	my $ehash = ExtractionDBA::getByUserHash( $uid, $pjname );

	$order = "datasets.status" if( !$order );

	my $sql = "SELECT * FROM datasets  WHERE " . 
						"datasets.user_processing = " . $dbh->quote( $uid ) . " AND " .  	
						"datasets.pjname = " . $dbh->quote( $pjname ) .  
						" ORDER BY $order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $ds = Dataset->new();
		$ds->set( \@row );

		if( defined( $ehash->{$ds->{dsname}} ) )
		{
			$ds->{is_extraction} = 1;
			$ds->{extraction_object} = $ehash->{$ds->{dsname}};
		}

		$dss[scalar(@dss)] = $ds;
	}

	return \@dss;
}

##-----------------------------------------------------------------------------
# @signature Dataset getFromDB(string $dsname, string $pdname, string $pjname) 
# <p>Queries the <i>datasets</i> table for the given dataset.  If none is found
#  returns undef().  Determines if the dataset is an extraction, and stores
#  the user_processing in user_processing_object. 
#
# @input $dsname name of the dataset
# @input $pdname product dataset is in
# @input $pjname project dataset is in
#
# @output $ds instance of the Dataset class, undef() if no record is found.
##-----------------------------------------------------------------------------
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

##-----------------------------------------------------------------------------
# @signature void insertDB( Dataset $ds )
# <p>Inserts the given dataset into the <i>datasets</i> table.
#
# @input $ds instance of the Dataset class.
#
##-----------------------------------------------------------------------------
sub insertDB
{
	my $ds = shift;
	$ds->dbPrepare();

	my $dbh = IvenDB::connect();

	my $sql = "INSERT INTO datasets VALUES ( " .
						$dbh->quote( $ds->{dsname} ) . ", " .
						$dbh->quote( $ds->{pdname} ) . ", " .
						$dbh->quote( $ds->{pjname} ) . ", " .
						$dbh->quote( $ds->{storm_id} ) . ", " .
						$dbh->quote( $ds->{user_processing} ) . ", " .
						$dbh->quote( $ds->{raw_data} ) . ", " .
						$dbh->quote( $ds->{final_data} ) . ", " .
						$dbh->quote( $ds->{station_info} ) . ", " .
						$dbh->quote( $ds->{software} ) . ", " .
						$dbh->quote( $ds->{plots} ) . ", " .
						$ds->{id_type} . ", " .
						$ds->{platform_type} . ", " .
						$dbh->quote( $ds->{status_notes} ) . ", " .
						$dbh->quote( $ds->{admin_notes} ) . ", " .
						$dbh->quote( $ds->{status} ) . ", " .
						$ds->{exclude} . ", " .
						$ds->{questions} . ", " .
						$dbh->quote( $ds->{how_to} ) . ", " .
						$dbh->quote( $ds->{readme} ) . ", " .
						$ds->{best_sw} . 
						")"; 
	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void deleteFromDB( Dataset $ds )
# <p>Deletes the given Dataset from the database.</p>
#
# @input $ds Dataset to delete.
##-----------------------------------------------------------------------------
sub deleteFromDB
{
	my $ds = shift;

	my $dbh = IvenDB::connect();

	my $sql = "DELETE FROM datasets WHERE " .
						"dsname=" . $dbh->quote( $ds->{dsname} ) . " AND " .
						"pdname=" . $dbh->quote( $ds->{pdname} ) . " AND " .
						"pjname=" . $dbh->quote( $ds->{pjname} );

	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void updateDBAdmin( Dataset $ds, string $dsname, string $pdname )
# <p>Updates the administrative attributes of the dataset (also see
#  updateDBStatus).  The provided $dsname and $pdname correspond to the dataset
#  and product names originally retrieved from the, it is possible the user has
#  modified these attributes and differ in $ds.
#
# @input $ds Dataset to update.
# @input $dsname dataset name to match in the WHERE clause.
# @input $pdname product name to match in the WHERE clause.
##-----------------------------------------------------------------------------
sub updateDBAdmin
{
	my $ds = shift;
	my $dsname = shift;
	my $pdname = shift;

	my $dbh = IvenDB::connect();

	my $sql = "UPDATE datasets SET " .
						"dsname = " . $dbh->quote( $ds->{dsname} ) . ", " .
						"pdname = " . $dbh->quote( $ds->{pdname} ) . ", " .
						"storm_id = " . $dbh->quote( $ds->{storm_id} ) . ", " .
						"user_processing = " . $dbh->quote( $ds->{user_processing} ) . ", " .
						"admin_notes = " . $dbh->quote( $ds->{admin_notes} ) . ", " .
						"status = " . $dbh->quote( $ds->{status} ) . ", " .
						"exclude = $ds->{exclude}, " .
						"questions = $ds->{questions} " .
						"WHERE " .
						"dsname = " . $dbh->quote( $dsname ) . " AND " .
						"pdname = " . $dbh->quote( $pdname ) . " AND " .
						"pjname = " . $dbh->quote( $ds->{pjname} );

	$dbh->do( $sql );
	
}

##-----------------------------------------------------------------------------
# @signature void updateDBStatus( Dataset $ds, string $dsname_source )
# <p>Updates the status information of the dataset (see updateDBAdmin).  The
# $dsname_source corresponds to the dataset name originally retrieved from the
# database, this could be modified by the user and differ in $ds.
#
# @input $ds Dataset to update.
# @input $dsname_source the dataset name to match in WHERE clause. 
##-----------------------------------------------------------------------------
sub updateDBStatus
{
	my $ds = shift;
	my $dsname_source = shift;

	$ds->dbPrepare();

	my $dbh = IvenDB::connect();

	my $sql = "UPDATE datasets SET " .
						"dsname = " . $dbh->quote( $ds->{dsname} ) . ", " .
						"raw_data = " . $dbh->quote( $ds->{raw_data} ) . ", " .
						"final_data = " . $dbh->quote( $ds->{final_data} ) . ", " .
						"station_info = " . $dbh->quote( $ds->{station_info} ) . ", " .
						"software = " . $dbh->quote( $ds->{software} ) . ", " .
						"plots = " . $dbh->quote( $ds->{plots} ) . ", " .
						"id_type = " . $ds->{id_type} . ", " .
						"platform_type = " . $ds->{platform_type} . ", " .
						"status_notes = " . $dbh->quote( $ds->{status_notes} ) . ", " . 
						"how_to = " . $dbh->quote( $ds->{how_to} ) . ", " . 
						"readme = " . $dbh->quote( $ds->{readme} ) .", " . 
						"best_sw = " . $ds->{best_sw} .
						" WHERE " .
						"dsname = " . $dbh->quote( $dsname_source ) . " AND " .
						"pdname = " . $dbh->quote( $ds->{pdname} ) . " AND " .
						"pjname = " . $dbh->quote( $ds->{pjname} );

	$dbh->do( $sql );
}

1;
