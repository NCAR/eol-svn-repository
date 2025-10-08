package CategoryDBA;

# --------------------------------------------------------------
# CategoryDBA.pm:
#  Category Database Access class.  Performs all of the needed
#   Select, Update, Insert and Delete options for the category
#   table.
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# 
# Subroutines:
# 
# sub new() - constructor, returns a new instance of CategoryDBA
#
# sub getTree( $project_id ) - Returns the category tree for the 
#  given project.  A tree consists of an array of CategoryNodes.  
#  each node contains a Category and an array of CategoryNode 
#  childern. 
#
# sub getRoots( $project_id ) - Returns an array of all categories
#  with a NULL parent for the given project.  Used by the getTree
#  subroutine. 
#
# sub getSubCats( $category_id ) - Returns the sub-categories of 
#  the given category.  If no sub-categories exist undef is returned.
#
# sub getSubTree( $category_id ) - Returns a tree similar to getTree
#  but for the given category.  The returned array contains only one
#  element but can be used in the same manner as an array returned
#  by getTree.
#
# sub updateDB( $cat ) - update the database with the values in the
#   given Category.
#
# sub insertDB( $cat ) - insert a new entry into the database with the 
#  values in the given Category.
#
# sub deleteDB( $cat ) - delete the entry in the database with the 
#  Category's id number.
# --------------------------------------------------------------

use lib ".";
use CategoryNode;
use Category;
use McDDB;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	return $self;
}

sub getTree
{
	my $self = shift;
	my $project = shift;
	my @roots = $self->getRoots( $project );
	my @all;
	my $count = 0;

	foreach my $cat (@roots)
	{
		$all[$count++] = CategoryNode->new( $cat, $self->getSubCats( $cat->{id} ) );
	}

	return @all;	
}

sub getRoots
{
	my $self = shift;
	my $project = shift;
	my $dbh = McDDB::connect();
	my @roots;
	my $count = 0;
	my $sql = "SELECT * FROM category WHERE project=$project AND parent is NULL ORDER BY name";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $cat = Category->new();
		$cat->set( @row );

		$roots[$count++] = $cat;
	}
	$sth->finish();
	$dbh->disconnect();

	return @roots;
}

# Returns a tree of CategoryNodes 
sub getSubCats
{
	my $self = shift;
	my $id = shift;
	my @list;
	my $count = 0;

	my $dbh = McDDB::connect();

	my $sql = "SELECT * FROM category WHERE parent = $id ORDER BY name";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @row = $sth->fetchrow();

	if( @row )
	{
		do
		{
			my $cat = Category->new();
			$cat->set( @row );
			$list[$count++] = CategoryNode->new( $cat, $self->getSubCats( $cat->{id} ) );
		}
		while( (@row = $sth->fetchrow() ) );
	}
	else
	{
		$sth->finish();
		$dbh->disconnect();
		return undef();
	}

	$sth->finish();
	$dbh->disconnect();

	return \@list;
}

sub getSubTree
{
	my $self = shift;
	my $id = shift;

	my $dbh = McDDB::connect();
	my $sql = "SELECT * FROM category WHERE id=$id";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my $cat = Category->new();

	$cat->set( $sth->fetchrow() );

	$sth->finish();
	$dbh->disconnect();
	return CategoryNode->new( $cat, $self->getSubCats( $id ) );
}


sub getFromDB
{
	my $self = shift;
	my $id = shift;
	my $cat = Category->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT * FROM category WHERE id=$id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	$cat->set( $sth->fetchrow() );

	$sth->finish();
	$dbh->disconnect();

	return $cat;
}

sub updateDB
{
	my $self = shift;
	my $cat = shift;

	my $dbh = McDDB::connect();

	my $parent = $cat->{parent};
	$parent = "NULL" if( ! defined( $parent ) );
	
	my $sql = "UPDATE category SET name = " . $dbh->quote( $cat->{name} ) . ", " .
						"parent = $parent, project = $cat->{project} WHERE id = $cat->{id} ";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub insertDB
{
	my $self = shift;
	my $cat = shift;

	my $dbh = McDDB::connect();

	my $parent = $cat->{parent};
	$parent = "NULL" if( ! defined( $parent ) );

	my $sql = "INSERT INTO category VALUES ( 0, " . 
						$dbh->quote( $cat->{name} ) . ", " .
						"$parent, $cat->{project} )";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$cat->{id} = $dbh->{'mysql_insertid'};

	$dbh->disconnect();
}

sub deleteDB
{
	my $self = shift;
	my $cat = shift;

	my $dbh = McDDB::connect();

	my $sql = "DELETE FROM category WHERE id=$cat->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

1;
