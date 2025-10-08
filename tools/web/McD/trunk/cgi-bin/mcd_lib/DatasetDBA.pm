package DatasetDBA;

# --------------------------------------------------------------
# DatasetDBA.pm:
#
# Author - Dan Sullivan
# Date - July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - Returns a new instane of the DatasetDBA class
#
# sub getByCategory( $cat_id ) - Returns a reference to an
#  array of datasets linked to the given category.
#
# sub getByProject( $proj_id, $order ) - Returns a reference to an
#  array of datasets linked to the given project.  $order is the
#  field in the dataset table by which to sort the query.
# 
# sub getDatasetList( $cat_tree, $hide, $sort ) - Returns a tree of
#   DatasetListNodes.  This tree is populated (from the db) with all
#   of the datasets that are linked to each category in the given 
#   category tree (see CategoryDBA::getTree).
#   $hide = the value of the hide field
#   $sort = the field by which to sort: title, date, storm_id 
# 
# sub getFromDB( $ds_id ) - Returns a Dataset populated from the 
#  database with the given id number.
#
# sub getProjects( $ds_id ) - Returns an array of projects linked to 
#  the given dataset id number.
#
# sub insertDB( $dataset ) - Insert a new entry into the database with the
#  values in the given Dataset.
#
# sub updateDB( $dataset ) - update the record in the database which corresponds
#  to the given Dataset.
#
# sub getCategories( $dataset ) - returns an array of category:id values 
#  which are linked to the given Dataset in the dscat table.
#
# sub deleteByCategory( $cat_id ) - deletes all of the datasets
#  (from the database) linked to the given category.
#
# sub updateCategories( $dataet, $cats_arr_ref ) - updates the entries
#  in the dscat table for the given Dataset.  Deletes all of 
#  the entries in the dscat table and then replaces them
#  with entries corresponding to the categories in the given
#  array.
#
# sub deleteDB() - delete the entry in the dataset table 
#  corresponding to the given Dataset.
#
# sub getMaxId() - Static method, returns the max id used
#  in the dataset table.
#
# --------------------------------------------------------------

use lib ".";
use DatasetListNode;
use Dataset;
use Project;
use StatusDBA;
use McDDB;
use EQuery;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	return $self;
}

sub getByCategory
{
	my $self = shift;
	my $category = shift;
	my $hide = shift;
	my $order = shift;
	my @datasets;
	my $count = 0;

	my $hide_sql = "";
	$hide_sql = "AND dataset.hide=$hide" if( defined( $hide ) );

	$order = "dataset.title" if( !defined( $order ) );

	if( $order eq "dataset.date" )
	{
		$order = $order . " DESC, dataset.storm_id";
	}	
	elsif( $order =~ /^dataset/ )
	{
		$order = $order . ", dataset.date DESC";
	}


	my $dbh = McDDB::connect();
	my $sql = "SELECT dataset.*, status.* " .
						"FROM dataset, status, dscat " .
						"WHERE " .
						"dscat.category=$category AND " .
						"dataset.id=dscat.dataset AND " .
						"status.dataset=dataset.id $hide_sql " .
						"ORDER BY $order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @arr = $sth->fetchrow) )
	{
		my $ds = Dataset->new();
		$ds->set( \@arr );
		$datasets[$count++] = $ds;	
	}

	$sth->finish();
	$dbh->disconnect();
	return \@datasets;
}

sub getDatasetList
{
	# Category tree!!
	my $self = shift;
	my $tree = shift;
	my $hide = shift;
	my $sort = shift;
	my @list;
	my $count = 0;

	if( $tree )
	{	
		my @tree = @{ $tree };


		foreach my $node (@tree)
		{
			$list[$count++] = DatasetListNode->new( $node->{category}, 
																						$self->getByCategory( $node->{category}->{id}, $hide, $sort ),
																						$self->getDatasetList( $node->{childern} ), $hide, $sort );		
		}
	}
	else
	{
		return undef();
	}

	return \@list;
}

sub getFromDB
{
	my $self = shift;
	my $id = shift;
	my $dataset = Dataset->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT dataset.*, status.* " .
						"FROM dataset, status " .
						"WHERE dataset.id=status.dataset AND dataset.id=$id";
	my $sth = $dbh->prepare( $sql );
	$sth->execute;

	my @row = $sth->fetchrow();
	if( @row )
	{
		$dataset->set( \@row );
	}

	$sth->finish();
	$dbh->disconnect();

	return $dataset;
}

sub getCategories
{
	my $self = shift;
	my $dataset = shift;
	my $id = $dataset->{id};
	my @cats;
	my $count = 0;
	my $dbh = McDDB::connect();
	my $sql = "SELECT category FROM dscat WHERE dataset=$id";
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

sub updateDB
{
	my $self = shift;
	my $dataset = shift;

	$dataset->dbPrepare();

	my $dbh = McDDB::connect();

	# Update the dataset
	my $sql = "UPDATE dataset SET " .
						"storm_id = " . $dbh->quote( $dataset->{storm_id} ) . ", " .
						"title = " . $dbh->quote( $dataset->{title} ) . ", " .
						"remote_url = " . $dbh->quote( $dataset->{remote_url} ) . 
						" WHERE id=$dataset->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
	
	$self->updateStatus( $dataset->{status} );

	$dbh->disconnect();
}

sub updateStatus
{
	my $self = shift;
	my $status = shift;

	my $dba = StatusDBA->new();
	$dba->updateDB( $status );	
}

sub insertDB
{
	my $self = shift;
	my $dataset = shift;
	
	$dataset->dbPrepare();

	my $dbh = McDDB::connect();

	my $sql = "INSERT INTO dataset VALUES ( " .
						"0, " .
						$dbh->quote( $dataset->{storm_id} ) . ", " .
						$dbh->quote( $dataset->{title} ) . ", " .
						"CURDATE()" . ", " .
						$dbh->quote( $dataset->{remote_url} ) . 
						")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dataset->{id} = $dbh->{'mysql_insertid'};
	$dataset->{status}->{dataset} = $dataset->{id};
	
	$self->insertStatus( $dataset->{status} );

	$dbh->disconnect();
}

sub insertStatus
{
	my $self = shift;
	my $status = shift;
	
	my $dba = StatusDBA->new();
	$dba->insertDB( $status );
}

sub updateCategories
{
	my $self = shift;
	my $dataset = shift;
	my $cats = shift;

	my $dbh = McDDB::connect();

	# Remove the links in the dscat table for this dataset
	my $sql = "DELETE FROM dscat WHERE dataset=$dataset->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	# Add the new entries to dscat
	if( defined( $cats ) )
	{
		foreach $cat (@{ $cats })
		{
			$sql = "INSERT INTO dscat VALUES ( $dataset->{id}, $cat )";

			$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
		}
	}

	$dbh->disconnect();
}

sub deleteDB
{
	my $self = shift;
	my $dataset = shift;

	my $dbh = McDDB::connect();
	my $sql = "DELETE FROM dataset WHERE id=$dataset->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub getMaxId
{
	my $dbh = McDDB::connect();
	my $sql = "SELECT MAX(id) FROM dataset";
	my $sth = $dbh->prepare( $sql );

	my @arr = $sth->fetchrow();

	$sth->finish();
	$dbh->disconnect();
	return $arr[0];
}

sub deleteByCategory
{
	my $self = shift;
	my $category = shift;

	my $dbh = McDDB::connect();
	my $sql = "SELECT dataset.id FROM dataset, dscat WHERE dataset.id=dscat.dataset AND dscat.category=$category";
	
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

sub getByProject
{
	my $project = shift;
	my $order = shift;
	$order = "title" if( !defined( $order ) );

	my @datasets;
	my $c = 0;

	my $dbh = McDDB::connect();
	my $sql = "SELECT dataset.*, status.* FROM dataset, dsproj, status WHERE " . 
						"dsproj.dataset=dataset.id AND dsproj.project=$project AND status.dataset=dataset.id ORDER BY dataset.$order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $ds = Dataset->new();
		$ds->set( \@row );
		$datasets[$c++] = $ds;
	}

	$sth->finish();
	$dbh->disconnect();
	return \@datasets;	
}

sub getProjects
{
	my $self = shift;
	my $ds_id = shift;
	my @projs;

	my $dbh = McDDB::connect();
	my $sql = "SELECT project.* FROM project, dsproj WHERE dsproj.project = project.id AND dsproj.dataset = $ds_id";
	$sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $project = Project->new();
		$project->set( \@row );
		$projs[scalar(@projs)] = $project;
	}

	return @projs;
}

sub linkToProject
{
	my $self = shift;
	my $ds_id = shift;
	my $proj_id = shift;

	my $dbh = McDDB::connect();
	my $sql = "INSERT INTO dsproj VALUES( $ds_id, $proj_id )";
	$dbh->do( $sql ) || die( "Unable to link project to dataset\n" );

	$dbh->disconnect();
}

sub getDocUrls_list
{
	my $self = shift;
	my $list = shift;
	my %hash;
	my $in = "(";
	$self->listToHash( $list, \%hash, \$in );	

	$self->getDocUrls( \%hash, $in );	
}

sub getDocUrls_array
{
	my $self = shift;
	my $array = shift;
	my %hash;
	my $in = "(";
	$self->arrayToHash( $array, \%hash, \$in );

	$self->getDocUrls( \%hash, $in );	
}

sub getDocUrls
{
	my $self = shift;
	my $hash = shift;
	my $in = shift;

	chop( $in );

	if( $in ne "" )
	{
		$in = $in . ")";
		my $eq = EQuery->new( "catalog_db" );
		$eq->query( "SELECT storm_id, dir_path, file_name FROM dataset WHERE storm_id IN $in" );
		
		while( (my %row = $eq->getRow()) )
		{
			if( $row{dir_path} ne "" && $row{file_name} ne "" )
			{
				my $url = 	"http://www.joss.ucar.edu" .
										substr( $row{dir_path}, length( "/web" ) ) .
										"/$row{file_name}";	
				$hash->{$row{storm_id}}->{doc_url} = $url;	
			}	
		}	
	}	
}


sub listToHash
{
	my $self = shift;
	my $list = shift;
	my $hash = shift;
	my $in = shift;

	if( $list )
	{
		foreach my $node (@$list)
		{
			foreach my $ds (@{$node->{datasets}})
			{
				if( $ds->{storm_id} && !$ds->{doc_url} )
				{
					if( ! exists( $hash->{$ds->{storm_id}} ) )
					{
						$$in = $$in . "$ds->{storm_id},";
						$hash->{$ds->{storm_id}} = $ds; 
					}
				}
			}

			$self->listToHash( $node->{childern}, $hash, $in );
		}
	}
}

sub arrayToHash
{
	my $self = shift;
	my $array = shift;	
	my $hash = shift;
	my $in = shift;

	foreach my $ds (@{$array} )
	{
		if( $ds->{storm_id} && !$ds->{doc_url} )
		{
			$$in = $$in . "$ds->{storm_id},";
			$hash->{$ds->{storm_id}} = $ds; 
		}
	}
}

1;
