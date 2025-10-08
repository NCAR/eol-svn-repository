package Dataset;

# --------------------------------------------------------------
# Dataset.pm:
#  Contains the Dataset class to perform all the needed database
#   operations on the dataset table.  A single instance of the
#   Dataset class corresponds to one record in the dataset table.
#
# Author - Dan Sullivan
# Date - July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - Returns a new instane of the Dataset class
# sub set() - Sets the instance variables in this Dataset with the
#  given values.
#
# sub checkDefined() - Sets default values for undefined values 
#  returned by the database
#
# sub getByCategory( $cat_id ) - Static method, returns a reference to an
#  array of datasets linked to the given category.
# 
# sub getDatasetList( $cat_tree ) - Static method, returns a tree of
#   DatasetListNodes.  This tree is populated (from the db) with all
#   of the datasets that are linked to each category in the given 
#   category tree (see Category::getTree). 
# 
# sub setFromDB( $ds_id ) - Populate this Dataset from the db with
#   entry corresponding to the given dataset id.
#
# sub insertDB() - Insert a new entry into the database with the
#  values in this Dataset.
#
# sub updateDB() - update the record in the database which corresponds
#  to this Dataset.
#
# sub getCategories() - returns an array of category:id values 
#  which are linked to this Dataset in the dscatlink table.
#
# sub deleteByCategory( $cat_id ) - deletes all of the datasets
#  (from the database) linked to the given category.
#
# sub updateCategories( $cats_arr_ref ) - updates the entries
#  in the dscatlink table for this Dataset.  Deletes all of 
#  the entries in the dscatlink table and then replaces them
#  with entries corresponding to the categories in the given
#  array.
#
# sub deleteDB() - delete the entry in the dataset table 
#  corresponding to this Dataset.
#
# sub getMaxId() - Static method, returns the max id used
#  in the dataset table.
#
# --------------------------------------------------------------

use lib ".";
use DatasetListNode;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{title} = "";
	$self->{date} = "";
	$self->{storm_id} = "99.999";
	$self->{url} = "";
	$self->{doc_url} = "";
	$self->{in_progress} = 0;
	$self->{hide} = 0;
	$self->{project} = -1;

	return $self;
}

sub set
{
	my $self = shift;
	my @row = @_;

	$self->{id} = $row[0];
	$self->{title} = $row[1];
	$self->{storm_id} = $row[2];
	$self->{date} = $row[3];
	$self->{url} = $row[4];
	$self->{doc_url} = $row[5];
	$self->{in_progress} = $row[6];
	$self->{hide} = $row[7];
	$self->{project} = $row[8];

	$self->checkDefined();
}

sub getByCategory
{
	my $category = shift;
	my $hide = shift;
	my @datasets;
	my $count = 0;

	my $hide_sql = "";
	$hide_sql = "AND dataset.hide=$hide" if( defined( $hide ) );

	my $dbh = MLDB->connect();
	my $sql = "SELECT dataset.* FROM dataset, dscatlink WHERE dscatlink.category=$category AND dataset.id=dscatlink.dataset $hide_sql ORDER BY title";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @arr = $sth->fetchrow()) )
	{
		my $ds = Dataset->new();
		set( $ds, @arr );
		$datasets[$count++] = $ds;	
	}

	$sth->finish();
	$dbh->disconnect();
	return \@datasets;
}

sub getDatasetList
{
	# Category tree!!
	my $tree = shift;
	my $hide = shift;
	my @list;
	my $count = 0;

	if( $tree )
	{	
		my @tree = @{ $tree };


		foreach my $node (@tree)
		{
			$list[$count++] = DatasetListNode->new( $node->{category}, 
																						Dataset::getByCategory( $node->{category}->{id}, $hide ),
																						Dataset::getDatasetList( $node->{childern} ), $hide );		
		}
	}
	else
	{
		return undef();
	}

	return \@list;
	
}

sub setFromDB
{
	my $self = shift;
	my $id = shift;

	my $dbh = MLDB->connect();
	my $sql = "SELECT * FROM dataset WHERE id=$id";
	my $sth = $dbh->prepare( $sql );
	$sth->execute;

	my @row = $sth->fetchrow();
	if( !@row )
	{
		$self = Dataset->new();
	}
	else
	{
		$self->set( @row );
	}

	$sth->finish();
	$dbh->disconnect();
}

sub getCategories
{
	my $self = shift;
	my $id = $self->{id};
	my @cats;
	my $count = 0;
	my $dbh = MLDB->connect();
	my $sql = "SELECT category FROM dscatlink WHERE dataset=$id";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		$cats[$count++] = $row[0];
	}

	$sth->finish();
	$dbh->disconnect();
	return @cats;
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = ( defined( $self->{id} ) )? $self->{id} : -1;
	$self->{title} = ( defined( $self->{title} ) )? $self->{title} : "";
	$self->{storm_id} = ( defined( $self->{storm_id} ) )? $self->{storm_id} : "";
	$self->{date} = ( defined( $self->{date} ) )? $self->{date} : "0000-00-00";
	$self->{url} = ( defined( $self->{url} ) )? $self->{url} : "";
	$self->{doc_url} = ( defined( $self->{doc_url} ) )? $self->{doc_url} : "";
	$self->{in_progress} = ( defined( $self->{in_progress} ) )? $self->{in_progress} : 0;
	$self->{hide} = ( defined( $self->{hide} ) )? $self->{hide} : 0;
	$self->{project} = ( defined( $self->{project} ) )? $self->{project} : -1;
}

sub updateDB
{
	my $self = shift;

	my $dbh = MLDB->connect();

	# Update the dataset
	my $sql = "UPDATE dataset SET " .
						"title = " . $dbh->quote( $self->{title} ) . ", " .
						"storm_id = " . $dbh->quote( $self->{storm_id} ) . ", " .
						"date = " . $dbh->quote( $self->{date} ) . ", " .
						"url = " . $dbh->quote( $self->{url} ) . ", " .
						"doc_url = " . $dbh->quote( $self->{doc_url} ) . ", " .
						"in_progress = " .  $self->{in_progress} . ", " .
						"hide = " . $self->{hide} . " WHERE id=$self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub insertDB
{
	my $self = shift;

	my $dbh = MLDB->connect();

	my $sql = "INSERT INTO dataset VALUES ( 0," .
						$dbh->quote( $self->{title} ) . ", " .
						$dbh->quote( $self->{storm_id} ) . ", " .
						$dbh->quote( $self->{date} ) . ", " .
						$dbh->quote( $self->{url} ) . ", " .
						$dbh->quote( $self->{doc_url} ) . ", " .
						$self->{in_progress} . ", " .
						$self->{hide} . ", " .
						$self->{project} . ")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$self->{id} = $dbh->{'mysql_insertid'};

	$dbh->disconnect();
}

sub updateCategories
{
	my $self = shift;
	my $cats = shift;

	my $dbh = MLDB->connect();

	# Remove the links in the dscatlink table for this dataset
	my $sql = "DELETE FROM dscatlink WHERE dataset=$self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	# Add the new entries to dscatlink
	if( defined( $cats ) )
	{
		foreach $cat (@{ $cats })
		{
			$sql = "INSERT INTO dscatlink VALUES ( $cat, $self->{id} )";

			$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
		}
	}

	$dbh->disconnect();
}

sub deleteDB
{
	my $self = shift;

	my $dbh = MLDB->connect();
	my $sql = "DELETE FROM dataset WHERE id=$self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub getMaxId
{
	my $dbh = MLDB::connect();
	my $sql = "SELECT MAX(id) FROM dataset";
	my $sth = $dbh->prepare( $sql );

	my @arr = $sth->fetchrow();

	$sth->finish();
	$dbh->disconnect();
	return $arr[0];
}

sub deleteByCategory
{
	my $category = shift;

	my $dbh = MLDB::connect();
	my $sql = "SELECT dataset.id FROM dataset, dscatlink WHERE dataset.id=dscatlink.dataset AND dscatlink.category=$category";
	
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my $sql2 = "DELETE FROM dataset WHERE id IN(";
	
	my @arr = $sth->fetchrow();
	if( @arr )
	{
		do
		{
			$sql2 = $sql2 . "$arr[0]";
			@arr = $sth->fetchrow();
			$sql2 = $sql2 . "," if( @arr );
		}
		while( @arr );
	}

	$sql2 = $sql2 . ")";

	$dbh->do( $sql2 ) || die( "doing: ", $dbh->errstr );

	$sth->finish();
	$dbh->disconnect();
}
1;
