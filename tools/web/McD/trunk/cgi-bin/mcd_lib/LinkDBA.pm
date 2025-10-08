package LinkDBA;

# --------------------------------------------------------------
# LinkDBA.pm:
#   Database Access class for the link table.
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
# sub new() - creates a new instance of the Link class.
#
# sub getCategoryLinks( $cat_id ) - Returns an
#  array of Links associated to the given category.
#
# sub getProjectLinks( $proj_id ) - Returns
#  array of Links associated to the given project.
#  
# sub getFromDB( $id ) - queries the database and returns a new
#  Link.
#
# sub insertDB( $link ) - insert a new record into the database with the
#  values in the given Link.
#
# sub updateDB( $link ) - update the record in the database corresponding
#  to the given Link.
#
# sub deleteDB( $link ) - delete the record in the database corresponding to
#  the given Link.
#
# sub linkToCategory( $link, $cat_id ) - link the Link to the given category
#  by adding a record to the linkcat table.
#  
# sub linkToProject( $link, $proj_id ) - link the Link to the given project
#  by adding a record to the linkproj table.
#
# sub deleteByCategory( $cat_id ) - Deletes all of
#  the links associated with the given category. 
# --------------------------------------------------------------

use lib ".";
use McDDB;
use Link;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	return $self;
}

sub getCategoryLinks
{
	my $self = shift;
	my $cat = shift;
	my @links;
	my $count = 0;

	my $dbh = McDDB::connect();
	my $sql = "SELECT link.* FROM link, linkcat WHERE linkcat.category = $cat AND linkcat.link = link.id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @arr = $sth->fetchrow()) )
	{
		my $link = Link->new();
		$link->set( @arr );
		$links[$count++] = $link;
	}

	$sth->finish();
	$dbh->disconnect();
	return @links;
}

sub getProjectLinks
{
	my $self = shift;
	my $proj = shift;
	my @links;

	my $dbh = McDDB::connect();
	my $sql = "SELECT link.* FROM link, linkproj WHERE linkproj.project = $proj AND linkproj.link = link.id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @arr = $sth->fetchrow()) )
	{
		my $link = Link->new();
		$link->set( @arr );
		$links[scalar(@links)] = $link;
	}
	$sth->finish();
	$dbh->disconnect();
	return @links;
}

sub getFromDB
{
	my $self = shift;
	my $id = shift;
	my $link = Link->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT * FROM link WHERE id=$id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @arr = $sth->fetchrow();

	$link->set( @arr );

	$sth->finish();
	$dbh->disconnect();

	return $link;
}

sub insertDB
{
	my $self = shift;
	my $link = shift;

	my $dbh = McDDB::connect();
	my $sql = "INSERT INTO link VALUES( 0, " . 
						$dbh->quote( $link->{name} ) . ", " . 
						$dbh->quote( $link->{url} ) . ", " . 
						$dbh->quote( $link->{target} ) . ")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$link->{id} = $dbh->{'mysql_insertid'};

	$dbh->disconnect();
}

sub updateDB
{
	my $self = shift;
	my $link = shift;

	my $dbh = McDDB::connect();

	my $sql = "UPDATE link SET ".
						"name = " . $dbh->quote( $link->{name} ) . ", " .
						"url = " . $dbh->quote( $link->{url} ) . ", " .
						"target = " . $dbh->quote( $link->{target} ) . 
						" WHERE id = $link->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
	$dbh->disconnect();

}

sub deleteDB
{
	my $self = shift;
	my $link = shift;

	my $dbh = McDDB::connect();

	my $sql = "DELETE FROM link WHERE id = $link->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
	$dbh->disconnect();
}

sub linkToCategory
{
	my $self = shift;
	my $link = shift;
	my $cat = shift;

	my $dbh = McDDB::connect();
	my $sql = "INSERT INTO linkcat VALUES ($link->{id}, $cat)";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub linkToProject
{
	my $self = shift;
	my $link = shift;
	my $proj = shift;

	my $dbh = McDDB::connect();
	my $sql = "INSERT INTO linkproj VALUES ($link->{id}, $proj)";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
}

sub deleteByCategory
{
	my $self = shift;
	my $cat = shift;

	my $sql = "SELECT link.id FROM link, linkcat WHERE linkcat.link=link.id AND linkcat.category=$cat";
	my $dbh = McDDB::connect();
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @arr = $sth->fetchrow();
	
	my $sql2 = "DELETE FROM link WHERE id IN(";
	if( @arr )
	{
		do
		{
			$sql2 = $sql2 . "$arr[0] ";
			@arr = $sth->fetchrow();
			$sql2 = $sql2 . "," if( @arr );
		}
		while( @arr );
	
		$sql2 = $sql2 . ")";

		$dbh->do( $sql2 );
	}

	$sth->finish();
	$dbh->disconnect();
}

1;
