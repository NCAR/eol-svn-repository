package Search;

use strict;
use CGI;
use URI;

use lib ".";
use lib "../mcd_lib"; # This could be a problem - should be ../../mcd_lib???
use McDDB;
use DLNDataset;

my $dbh = McDDB::connect();

my @possible_fields = ("title", "notes", "archive", "ingest", "remote_url", "storm_id" );
my %field_table = ( "title" => "dataset", "notes" => "dlnview", 
										"archvie" => "dlnview", "ingest" => "dlnview", "remote_url" => "dataset",
										"storm_id" => "dataset" );

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{string} = undef();
	$self->{project} = undef();
	$self->{fields} = [];

	return $self;
}

sub setFromParams
{
	my $self = shift;
	my $cgi = shift;

	$self->{string} = $cgi->param( "string" );
	$self->{project} = $cgi->param( "project" );	

	my $c = 0;	
	$self->{fields} = [];
	foreach my $field (@possible_fields)
	{
		my $p = $cgi->param( $field );
		$self->{fields}->[$c++] = $field if( defined( $p ) );
	}	
}

sub getListFromDB
{
	my $self = shift;
	my $sort = shift;

	if( $sort eq "project" )
	{
		$sort = "project.name";
	}

	if( $sort eq "dlnview.date" )
	{
		$sort = $sort . " DESC, dataset.storm_id";
	}	
	elsif( $sort =~ /^dataset/ || $sort =~ /^dlnview/  || $sort =~ /^project/ )
	{
		$sort = $sort . ", dlnview.date DESC";
	}

	my $sql;

	if( $self->{project} == -1 )
	{
		$sql = "SELECT dataset.*, status.*, dlnview.*, project.name FROM dlnview, dataset, status, project, dsproj " .
					 "WHERE dataset.id=dlnview.dataset AND status.dataset=dataset.id AND " . 
					 "dsproj.dataset=dataset.id AND dsproj.project=project.id ";	
	}
	else
	{
		$sql = "SELECT dataset.*, status.*, dlnview.*, project.name " .
					 "FROM dlnview, dataset, dsproj, status, project " . 
					 "WHERE dsproj.project=project.id AND dataset.id=dsproj.dataset AND status.dataset=dataset.id AND " .
					 "dsproj.project=$self->{project} AND dataset.id=dlnview.dataset ";
	}

	$sql = $sql . "AND ( ";
	foreach my $field (@{$self->{fields}})
	{
		my $like = $dbh->quote( "\%$self->{string}\%" );

		if( $field eq "storm_id" ) 
		{ $like = $dbh->quote( $self->{string} ); }

		$sql = $sql . " $field_table{$field}.$field LIKE " . $dbh->quote( "\%$self->{string}\%" ) . " OR ";
	}

	# Get rid of the last OR
	chop($sql);
	chop($sql);
	chop($sql);
	$sql = $sql . ")";

	$sql = $sql . " ORDER BY $sort";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @dss;
	my $c = 0;
	while( (my @row = $sth->fetchrow() ) )
	{
		my $ds = DLNDataset->new();
		$ds->set( \@row );
		$ds->{project} = shift( @row );
		$dss[$c++] = $ds;
	}

	return \@dss;
}

sub getUrlString
{
	my $self = shift;

	my $url = "project=$self->{project}&" . 
						"string=" . URI::Escape::uri_escape($self->{string});

	foreach my $field (@{$self->{fields}})
	{
		$url = $url . "&$field=1";
	}	

	return $url;
}

sub getHiddenFieldString
{
	my $self = shift;

	my $str = "<input type=hidden name=project value=\"$self->{project}\">\n" . 
						"<input type=hidden name=string value=\"" . URI::Escape::uri_escape($self->{project}) . "\">\n";

	foreach my $field (@{$self->{fields}})
	{
		$str = $str . "<input type=hidden name=$field value=1>\n";
	} 

	return $str;
}

1;
