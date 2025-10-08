package UserDBA;

# --------------------------------------------------------------
# UserDBA.pm: 
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new()
#
# sub getAllUsers( $sort, $active ) - return array of all users
#
# sub getFromDB( $user ) - return the user with the given id
#
# --------------------------------------------------------------

use lib ".";
use User;
use McDDB;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	return $self;
}

sub getAllUsers
{
	my $self = shift;
	my $sort = shift;
	my $active = shift;

	my @users;

	my $where = "";
	$where = "WHERE active=$active" if( defined( $active ) );

	my $order_by = "";
	$order_by = "ORDER BY $sort" if( defined( $sort ) );

	my $dbh = McDDB::connect();
	my $sql = "SELECT * FROM user $where $order_by";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $user = User->new();
		$user->set( \@row );
		$users[scalar(@users)] = $user;
	}

	return @users;
}

sub getFromDB
{
	my $self = shift;
	my $id = shift;
	my $user = User->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT * FROM user WHERE id=" . $dbh->quote( $id );
	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	
	my @row = $sth->fetchrow();
	if( @row )
	{
		$user->set( \@row );
	}

	return $user;	
}

1; 
