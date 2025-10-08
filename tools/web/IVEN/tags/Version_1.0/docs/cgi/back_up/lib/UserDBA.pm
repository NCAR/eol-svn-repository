package UserDBA;

use strict;
use IvenDB;
use User;

sub getAllUsers
{
	my $order = shift;
	my @users;
	
	$order = "lname, fname" if( !defined( $order ) );

	my $sql = "SELECT * FROM users ORDER BY $order";
	my $dbh = IvenDB::connect();
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow()) )
	{
		my $user = User->new();
		$user->set( \@row );
		$users[scalar(@users)] = $user;
	}

	return \@users;
}

1;
