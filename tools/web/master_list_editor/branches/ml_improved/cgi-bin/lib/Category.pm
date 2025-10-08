package Category;

# --------------------------------------------------------------
# Category.pm:
#  Holds the Category class to query and update the category
#   table in the ml database.  A single instance of this class
#   corresponds to one record in the category table.  
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# 
# Subroutines:
# 
# sub new() - constructor, returns a new instance of Category
#
# sub set( @vals ) - sets all fields in this Category with the given
#  values.  
#
# sub getTree( $project_id ) - Returns the category tree for the 
#  given project.  A tree consists of an array of CategoryNodes.  
#  each node contains a Category and an array of CategoryNode 
#  childern. This is a static method. 
#
# sub getRoots( $project_id ) - Returns an array of all categories
#  with a NULL parent for the given project.  Used by the getTree
#  subroutine.  This is a static method.
#
# sub getSubCats( $category_id ) - Returns the sub-categories of 
#  the given category.  If no sub-categories exist undef is returned.
#  This is a static method.
#
# sub getSubTree( $category_id ) - Returns a tree similar to getTree
#  but for the given category.  The returned array contains only one
#  element but can be used in the same manner as an array returned
#  by getTree.  This is a static method.
#
# sub setFromDB( $category_id ) - query the database and populate
#  this Category.
#   
# sub updateDB() - update the database with the values in this
#   Category.
#
# sub insertDB() - insert a new entry into the database with the 
#  values in this Category.
#
# sub deleteDB() - delete the entry in the database with this 
#  Category's id number.
#
# sub checkDefined() - Set default values for undef vales returned
#  from the database.  This prevents trying to do operations on
#  undef values.
# --------------------------------------------------------------


use lib ".";
use CategoryNode;
use MLDB;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{name} = "";
	$self->{parent} = undef();
	$self->{project} = -1;

	return $self;
}

sub set
{
	my $self = shift;
	my @row = @_;

	$self->{id} = $row[0];	
	$self->{name} = $row[1];	
	$self->{parent} = $row[2];	
	$self->{project} = $row[3];	

	$self->checkDefined();
}

sub getTree
{
	my $project = shift;
	my @roots = getRoots( $project );
	my @all;
	my $count = 0;

	foreach my $cat (@roots)
	{
		$all[$count++] = CategoryNode->new( $cat, &getSubCats( $cat->{id} ) );
	}

	return @all;	
}

sub getRoots
{
	my $project = shift;
	my $dbh = MLDB->connect();
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
	my $id = shift;
	my @list;
	my $count = 0;

	my $dbh = MLDB->connect();

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
			$list[$count++] = CategoryNode->new( $cat, &getSubCats( $cat->{id} ) );
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
	my $id = shift;

	my $dbh = MLDB->connect();
	my $sql = "SELECT * FROM category WHERE id=$id";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my $cat = Category->new();

	$cat->set( $sth->fetchrow() );

	$sth->finish();
	$dbh->disconnect();
	return CategoryNode->new( $cat, getSubCats( $id ) );
}


sub setFromDB
{
	my $self = shift;
	my $id = shift;

	my $dbh = MLDB->connect();
	my $sql = "SELECT * FROM category WHERE id=$id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	$self->set( $sth->fetchrow() );

	$sth->finish();
	$dbh->disconnect();
}

sub updateDB
{
	my $self = shift;

	my $dbh = MLDB->connect();

	my $parent = $self->{parent};
	$parent = "NULL" if( ! defined( $parent ) );
	
	my $sql = "UPDATE category SET name = " . $dbh->quote( $self->{name} ) . ", " .
						"parent = $parent, project = $self->{project} WHERE id = $self->{id} ";

	$dbh->do( $sql ) || die( "doing: ", $dhb->errstr );

	$dbh->disconnect();
}

sub insertDB
{
	my $self = shift;

	my $dbh = MLDB->connect();

	my $parent = $self->{parent};
	$parent = "NULL" if( ! defined( $parent ) );

	my $sql = "INSERT INTO category VALUES ( 0, " . 
						$dbh->quote( $self->{name} ) . ", " .
						"$parent, $self->{project} )";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$self->{id} = $dbh->{'mysql_insertid'};

	$dbh->disconnect();
}

sub deleteDB
{
	my $self = shift;

	my $dbh = MLDB->connect();

	my $sql = "DELETE FROM category WHERE id=$self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );

	$dbh->disconnect();
}

sub checkDefined
{
	my $self = shift;

	$self->{id} = ( defined( $self->{id} ) )? $self->{id} : -1;
	$self->{name} = ( defined( $self->{name} ) )? $self->{name} : "";
	$self->{parent} = ( defined( $self->{parent} ) )? $self->{parent} : undef();
	$self->{project} = ( defined( $self->{project} ) )? $self->{project} : -1;
}

1;
