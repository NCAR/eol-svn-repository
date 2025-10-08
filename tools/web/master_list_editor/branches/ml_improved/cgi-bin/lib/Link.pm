package Link;

# --------------------------------------------------------------
# Link.pm:
#  Contains the Link class used to perform operations on the link
#  table in the database.  A single instance of Link corresponds
#  to a record in the link table.
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
# sub new() - creates a new instance of the Link class.
#
# sub set( @values ) - sets the fields in this Link with the given values.
#
# sub checkDefined() - sets default values for undefs returned
#  by the database
#
# sub getCategoryLinks( $cat_id ) - Static method, returns an
#  array of Links associated to the given category.
#
# sub getProjectLinks( $proj_id ) - Static method, returns
#  array of Links associated to the given project.
#  
# sub setFromDB( $id ) - queries the database and populates this
#  Link.
#
# sub insertDB() - insert a new record into the database with the
#  values in this Link.
#
# sub updateDB() - update the record in the database corresponding
#  to this Link.
#
# sub deleteDB() - delete the record in the database corresponding to
#  this Link.
#
# sub linkToCategory( $cat_id ) - link this Link to the given category
#  by adding a record to the linkcatlink table.
#  
# sub linkToProject( $proj_id ) - link this Link to the given project
#  by adding a record to the linkprojlink table.
#
# sub deleteByCategory( $cat_id ) - Static method, deletes all of
#  the links associated with the given category. 
# --------------------------------------------------------------

use lib ".";
use MLDB;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{name} = "";
	$self->{url} = "";
	$self->{target} = "";

	return $self;
}

sub set
{
	my $self = shift;
	my @arr = @_;

	$self->{id} = $arr[0];
	$self->{name} = $arr[1];
	$self->{url} = $arr[2];
	$self->{target} = $arr[3];

	$self->checkDefined();
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = defined( $self->{id} )? $self->{id} : -1;
	$self->{name} = defined( $self->{name} )? $self->{name} : "";
	$self->{url} = defined( $self->{url} )? $self->{url} : "";
	$self->{target} = undef() if( defined( $self->{target} ) && $self->{target} eq "" );
}

sub getCategoryLinks
{
	my $cat = shift;
	my @links;
	my $count = 0;

	my $dbh = MLDB::connect();
	my $sql = "SELECT link.* FROM link, linkcatlink WHERE linkcatlink.category = $cat AND linkcatlink.link = link.id";

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
	my $proj = shift;
	my @links;

	my $dbh = MLDB::connect();
	my $sql = "SELECT link.* FROM link, linkprojlink WHERE linkprojlink.project = $proj AND linkprojlink.link = link.id";

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

sub setFromDB
{
	my $self = shift;
	my $id = shift;

	my $dbh = MLDB::connect();
	my $sql = "SELECT * FROM link WHERE id=$id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @arr = $sth->fetchrow();

	$self->set( @arr );

	$sth->finish();
	$dbh->disconnect();
}

sub insertDB
{
	my $self = shift;

	my $dbh = MLDB::connect();
	my $sql = "INSERT INTO link VALUES( 0, " . 
						$dbh->quote( $self->{name} ) . ", " . 
						$dbh->quote( $self->{url} ) . ", " . 
						$dbh->quote( $self->{target} ) . ")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$self->{id} = $dbh->{'mysql_insertid'};

	$dbh->disconnect();
}

sub updateDB
{
	my $self = shift;

	my $dbh = MLDB::connect();

	my $sql = "UPDATE link SET ".
						"name = " . $dbh->quote( $self->{name} ) . ", " .
						"url = " . $dbh->quote( $self->{url} ) . ", " .
						"target = " . $dbh->quote( $self->{target} ) . 
						" WHERE id = $self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
	$dbh->disconnect();

}

sub deleteDB
{
	my $self = shift;

	my $dbh = MLDB::connect();

	my $sql = "DELETE FROM link WHERE id = $self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
	$dbh->disconnect();
}

sub linkToCategory
{
	my $self = shift;
	my $cat = shift;

	my $dbh = MLDB::connect();
	my $sql = "INSERT INTO linkcatlink VALUES ($self->{id}, $cat)";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub linkToProject
{
	my $self = shift;
	my $proj = shift;

	my $dbh = MLDB::connect();
	my $sql = "INSERT INTO linkprojlink VALUES ($self->{id}, $proj)";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
}

sub deleteByCategory
{
	my $cat = shift;

	my $sql = "SELECT link.id FROM link, linkcatlink WHERE linkcatlink.link=link.id AND linkcatlink.category=$cat";
	my $dbh = MLDB::connect();
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
