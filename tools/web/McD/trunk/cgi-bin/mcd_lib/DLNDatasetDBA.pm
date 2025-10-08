package DLNDatasetDBA;
use lib ".";
use DatasetListNode;
use DLNDataset;
use DatasetDBA;
use StatusDBA;
@ISA = ("DatasetDBA"); # Inherit the DatasetDBA class

# --------------------------------------------------------------
# DLNDatasetDBA.pm:
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
# sub getByProject( $proj_id, $hide_arr_ref, $order ) - Returns a reference to an
#  array of datasets linked to the given project.  $order is the
#  field in the dataset table by which to sort the query.
#
# sub getByLoader( $user_id, $hide_arr, $order ) - Retruns a reference to an array
#  of datasets with the give loader id.
# 
# sub getByInternalContact( $user_id, $hide_arr, $order )
#
# sub getByChecker( $user_id, $hide_arr, $order )
#
# sub getFromDB( $ds_id ) - Returns a DLNDataset populated from the 
#  database with the given id number.  If no entry in dlnview table for
#  the given id, undef is returned.
#
# sub insertDB( $dataset ) - Insert a new entry into the database with the
#  values in the given DLNDataset.
#
# sub updateDB( $dataset ) - update the record in the database which corresponds
#  to the given DLNDataset.
#
# --------------------------------------------------------------


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

	if( $order eq "dlnview.date" )
	{
		$order = $order . " DESC, dataset.storm_id";
	}	
	elsif( $order =~ /^dataset/ || $order =~ /^dlnview/ )
	{
		$order = $order . ", dlnview.date DESC";
	}

	my $dbh = McDDB::connect();

	my $sql = "SELECT dataset.*, status.*, dlnview.* " .
						"FROM dataset, dlnview, status, dscat " .
						"WHERE " .
						"dscat.category=$category AND " . 
						"dataset.id=dscat.dataset AND " . 
						"dlnview.dataset=dataset.id " .
						"status.dataset=dataset.id $hide_sql " . 
						"ORDER BY $order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @arr = $sth->fetchrow()) )
	{
		my $ds = DLNDataset->new();
		$ds->set( \@arr );
		$datasets[$count++] = $ds;	
	}

	$sth->finish();
	$dbh->disconnect();
	return \@datasets;
}

sub getFromDB
{
	my $self = shift;
	my $id = shift;
	my $dataset = DLNDataset->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT dataset.*, status.*, dlnview.* FROM dataset, status, dlnview WHERE " .
						"dataset.id=$id AND dlnview.dataset=dataset.id AND status.dataset=dataset.id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute;

	my @row = $sth->fetchrow();
	if( @row )
	{
		$dataset->set( \@row );
	}
	else
	{
		$dataset = undef();
	}

	$sth->finish();
	$dbh->disconnect();

	return $dataset;
}

sub updateDB
{
	my $self = shift;
	my $ds = shift;

	$self->SUPER::updateDB( $ds );

	my $dbh = McDDB::connect();

	# Update the dlnview table
	my $sql = "UPDATE dlnview SET " .
							"date = " . $dbh->quote( $ds->{date} ) . ", " . 
							"loader = " . $dbh->quote( $ds->{loader} ) . ", " . 
							"checker = " . $dbh->quote( $ds->{checker} ) . ", " . 
							"int_contact = " . $dbh->quote( $ds->{int_contact} ) . ", " . 
							"ext_contact = " . $dbh->quote( $ds->{ext_contact} ) . ", " . 
							"ext_email = " . $dbh->quote( $ds->{ext_email} ) . ", " . 
							"ingest = " . $dbh->quote( $ds->{ingest} ) . ", " . 
							"archive = " . $dbh->quote( $ds->{archive} ) . ", " . 
							"notes = " . $dbh->quote( $ds->{notes} ) . " " . 
							"WHERE dataset = $ds->{id}"; 

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub updateStatus
{
	my $self = shift;
	my $status = shift;

	my $dba = StatusDBA->new();
	$dba->updateDB( $status, "documented", "checked", "loaded" );
}

sub insertDB
{
	my $self = shift;
	my $ds = shift;

	$self->SUPER::insertDB( $ds );

	my $dbh = McDDB::connect();

	# Insert a new entry into dlnview table
	my $sql = "INSERT INTO dlnview VALUES ( " .
						$ds->{id} . ", " .
						$dbh->quote( $ds->{date} ) . ", " .
						$dbh->quote( $ds->{loader} ) . ", " .
						$dbh->quote( $ds->{checker} ) . ", " .
						$dbh->quote( $ds->{int_contact} ) . ", " .
						$dbh->quote( $ds->{ext_contact} ) . ", " .
						$dbh->quote( $ds->{ext_email} ) . ", " .
						$dbh->quote( $ds->{ingest} ) . ", " .
						$dbh->quote( $ds->{archive} ) . ", " .
						$dbh->quote( $ds->{notes} ) . 
						")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub insertBlank
{
	my $self = shift;
	my $ds_id = shift;

	my $dbh = McDDB::connect();

	# Insert a new entry into dlnview table
	my $sql = "INSERT INTO dlnview (dataset, date, loader, checker, int_contact ) " . 
						"VALUES ( $ds_id, '0000-00-00', 'unassigned', 'unassigned', 'unassigned' )";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

# order options:
#  dataset.<field>
#  dlnview.<field>
#  loader
#  checker
#  int_contact 
sub getByProject
{
	my $self = shift;
	my $project = shift;
	my $hide = shift;
	my $order = shift;

	$order = "dataset.title" if( !defined( $order ) );

	if( $order eq "dlnview.date" )
	{
		$order = $order . " DESC, dataset.storm_id";
	}	
	elsif( $order =~ /^dataset/ || $order =~ /^dlnview/ )
	{
		$order = $order . ", dlnview.date DESC";
	}

	my $sql_hide = "";
	if( $hide )
	{
		foreach my $h (@$hide)
		{
			if( $h eq "remote_url" )
			{
				$sql_hide = $sql_hide . "AND (dataset.$h IS NULL || dataset.$h = '') ";
			}
			else
			{
				$sql_hide = $sql_hide . "AND NOT status.$h ";
			}
		}
	}

	my @datasets;
	my $c = 0;

	my $dbh = McDDB::connect();

	my $sql;	
	if( $order =~ /^dataset/ || $order =~ /^dlnview/ )
	{
		 $sql = "SELECT dataset.*, status.*, dlnview.* FROM dataset, status, dsproj, dlnview WHERE " . 
						"dsproj.dataset=dataset.id AND dsproj.project =$project AND dlnview.dataset=dataset.id " . 
						"AND status.dataset=dataset.id $sql_hide ORDER BY $order";
	}
 	else
	{
		 $sql = "SELECT dataset.*, status.*, dlnview.* FROM dataset, status, dsproj, dlnview, user WHERE " . 
						"dsproj.dataset=dataset.id AND dsproj.project =$project AND dlnview.dataset=dataset.id AND " .
						"user.id=dlnview.$order AND status.dataset=dataset.id $sql_hide ORDER BY user.display_name, dlnview.date DESC";
	}

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $ds = DLNDataset->new();
		$ds->set( \@row );
		$datasets[$c++] = $ds;
	}

	$sth->finish();
	$dbh->disconnect();
	return \@datasets;	
}

sub getByLoader
{
	my $self = shift;
	my $loader = shift;
	my $hide = shift;
	my $order = shift;

	return $self->getByUser( $loader, "loader", $hide, $order );
}

sub getByChecker
{
	my $self = shift;
	my $checker = shift;
	my $hide = shift;
	my $order = shift;

	return $self->getByUser( $checker, "checker", $hide, $order );
}

sub getByInternalContact
{
	my $self = shift;
	my $int_contact = shift;
	my $hide = shift;
	my $order = shift;

	return $self->getByUser( $int_contact, "int_contact", $hide, $order );
}

# order options:
#  dataset.<field>
#  dlnview.<field>
#  project
#  loader
#  checker
#  int_contact
sub getByUser
{
	my $self = shift;
	my $user = shift;
	my $field = shift;
	my $hide = shift;
	my $order = shift;
	my @dss;

	$order = "dataset.title" if( !defined( $order ) );

	if( $order eq "project" )
	{
		$order = "project.name";
	}

	if( $order eq "dlnview.date" )
	{
		$order = $order . " DESC, dataset.storm_id";
	}	
	elsif( $order =~ /^dataset/ || $order =~ /^dlnview/  || $order =~ /^project/ )
	{
		$order = $order . ", dlnview.date DESC";
	}

	my $hide_sql = "";
	if( $hide )
	{
		foreach my $h (@{$hide})
		{
			if( $h eq "remote_url" )
			{
				$sql_hide = $sql_hide . "AND NOT dataset.$h AND dataset.$h != '' ";
			}
			else
			{
				$hide_sql = $hide_sql . "AND NOT status.$h ";
			}
		}
	}

	my $dbh = McDDB::connect();

	my $sql;
	if( $order =~ /^dataset/ || $order =~ /^dlnview/  || $order =~ /^project/ )
	{
		 $sql = "SELECT dataset.*, status.*, dlnview.*, project.name FROM dataset, status, dlnview, project, dsproj WHERE " . 
						"$field=" . $dbh->quote( $user ) . " AND " . 
						"project.id=dsproj.project AND dataset.id=dsproj.dataset AND " .
						"dataset.id=dlnview.dataset AND status.dataset=dataset.id $hide_sql ORDER BY $order";
	}
	else
	{
		 $sql = "SELECT dataset.*, status.*, dlnview.*, project.name FROM dataset, status, dlnview, project, dsproj, user WHERE " . 
						"$field=" . $dbh->quote( $user ) . " AND " . 
						"project.id=dsproj.project AND dataset.id=dsproj.dataset AND " .
						"dataset.id=dlnview.dataset AND dlnview.$order = user.id " .
						"AND status.dataset=dataset.id $hide_sql ORDER BY user.display_name, dlnview.date DESC";
	}

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $ds = DLNDataset->new();
		$ds->set( \@row );
		$ds->{project} = shift( @row );
		$dss[scalar(@dss)] = $ds;
	}

	$sth->finish();
	$dbh->disconnect();

	return \@dss;
}
1;
