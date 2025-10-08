package MLDatasetDBA;
use lib ".";
use DatasetListNode;
use MLDataset;
use DatasetDBA;
@ISA = ("DatasetDBA"); # Inherit the DatasetDBA class

# --------------------------------------------------------------
# MLDatasetDBA.pm:
#
# Author - Dan Sullivan
# Date - July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - Returns a new instane of the MLDatasetDBA class
#
# sub getByCategory( $cat_id ) - Returns a reference to an
#  array of datasets linked to the given category.
#
# sub getByProject( $proj_id, $order, $hide ) - Returns a reference to an
#  array of datasets linked to the given project.  $order is the
#  field in the dataset table by which to sort the query. $hide is a
#  the value of the hide flag in mlview table, if none given all
#  datasets are returned
#
# sub getFromDB( $ds_id ) - Returns a MLDataset populated from the 
#  database with the given id number.  If no entry in mlview table
#  exists for the given id, undef is returned.
#
# sub insertDB( $dataset ) - Insert a new entry into the database with the
#  values in the given MLDataset.
#
# sub updateDB( $dataset ) - update the record in the database which corresponds
#  to the given MLDataset.
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

	my $dbh = McDDB::connect();

	$order = "dataset.title" if( !defined( $order ) );

	if( $order eq "mlview.date" )
	{
		$order = $order . " DESC, dataset.storm_id";
	}	
	elsif( $order =~ /^dataset/ || $order =~ /^mlview/ )
	{
		$order = $order . ", mlview.date DESC";
	}

	my $hide_sql = "";
	$hide_sql = "AND dataset.hide=$hide" if( defined( $hide ) );

	my $sql = "SELECT dataset.*, status.*, mlview.* " .
						"FROM dataset, mlview, dscat,status " . 
						"WHERE " .
						"dscat.category=$category AND " .
						"dataset.id=dscat.dataset AND " .
						"mlview.dataset = dataset.id AND " .
						"dataset.id=status.dataset $hide_sql" .
						"ORDER BY $order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @arr = $sth->fetchrow()) )
	{
		my $ds = MLDataset->new();
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
	my $dataset = MLDataset->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT dataset.*, status.*, mlview.* FROM dataset, mlview, status WHERE " . 
						"dataset.id=$id AND mlview.dataset=dataset.id AND status.dataset=dataset.id";

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

	# Update the mlview table
	my $sql = "UPDATE mlview SET " .
							"date = " . $dbh->quote( $ds->{date} ) . ", " . 
							"hide = " . $ds->{hide} . ", " . 
							"doc_url = " . $dbh->quote( $ds->{doc_url} ) . 
							"WHERE dataset = $ds->{id}"; 

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub updateStatus
{
	my $self = shift;
	my $status = shift;

	my $dba = StatusDBA->new();
	$dba->updateDB( $status, "in_progress", "updated", "new" );
}

sub insertDB
{
	my $self = shift;
	my $ds = shift;

	$self->SUPER::insertDB( $ds );

	my $dbh = McDDB::connect();

	# Insert a new entry into mlview table
	my $sql = "INSERT INTO mlview VALUES ( " .
						$ds->{id} . ", " .
						$dbh->quote( $ds->{date} ) . ", " .
						$ds->{hide} . ", " .
						$dbh->quote( $ds->{doc_url} ) . 
						")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub insertBlank
{
	my $self = shift;
	my $ds_id = shift;
	my $dbh = McDDB::connect();

	my $sql = "INSERT INTO mlview (dataset, date, hide) VALUES ($ds_id, '0000-00-00', 1)";		

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
	$dbh->disconnect();
}

# order options:
#  dataset.<field>
#  mlview.<field>
sub getByProject
{
	my $self = shift;
	my $project = shift;
	my $hide = shift;
	my $order = shift;

	$order = "dataset.title" if( !defined( $order ) );

	if( $order eq "mlview.date" )
	{
		$order = $order . " DESC, dataset.storm_id";
	}	
	elsif( $order =~ /^dataset/ || $order =~ /^mlview/ )
	{
		$order = $order . ", mlview.date DESC";
	}

	my $sql_hide = "";
	if( $hide )
	{
		foreach my $h (@$hide)
		{
			$sql_hide = $sql_hide . "AND NOT status.$h ";
		}
	}

	my @datasets;
	my $c = 0;

	my $dbh = McDDB::connect();

	my $sql = "SELECT dataset.*, status.*, mlview.* FROM dataset, status, dsproj, mlview WHERE " . 
						"dsproj.dataset=dataset.id AND dsproj.project =$project AND mlview.dataset=dataset.id " . 
						"AND status.dataset=dataset.id $sql_hide ORDER BY $order";
	

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $ds = MLDataset->new();
		$ds->set( \@row );
		$datasets[$c++] = $ds;
	}

	$sth->finish();
	$dbh->disconnect();
	return @datasets;	
}

1;
