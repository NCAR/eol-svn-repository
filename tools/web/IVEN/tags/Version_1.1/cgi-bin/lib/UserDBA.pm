##Module-----------------------------------------------------------------------
# <p>The UserDBA.pm module contains all the functions needed to manipulate the
#  <i>users</i> table in the database.  All functions user the User class
#  to insert, update, delete and selete records.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------


package UserDBA;

use strict;
use IvenDB;
use User;
use DatasetDBA;

##-----------------------------------------------------------------------------
# @signature array_ref getAllUsers( string $order )
# <p>Pulls all the users from the database and stores into an array.</p>
#
# @input $order the string to use in the WHERE clause, default is 
#  "lname, fname".
#
# @output $users a reference to an array of Users.
##-----------------------------------------------------------------------------
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

##-----------------------------------------------------------------------------
# @signature User getFromDB( string $uid )
# <p>Pulls the user from the DB matching the given uid.
#
# @input $uid uid to match
#
# @output $user User populated from the database, returns undef() if no match.
##-----------------------------------------------------------------------------
sub getFromDB
{
	my $uid = shift;
	my $user;
	my $dbh = IvenDB::connect();

	my $sql = "SELECT * FROM users WHERE uid = " . $dbh->quote( $uid );

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @row = $sth->fetchrow();

	if( @row )
	{
		$user = User->new();
		$user->set( \@row );
	}
	else
	{
		$user = undef();
	}

	return $user;
}

##-----------------------------------------------------------------------------
# @signature User buildUserView( string $uid )
# <p>Builds the needed data structure for the user_view.</p>
#
# @input $uid the uid to match in DB.
#
# @output an instance of user with the datasets the user is processing 
#  referenced by 'datasets' obtained by a call to DatasetDBA::getByUser().
##-----------------------------------------------------------------------------
sub buildUserView
{
	my $uid = shift;
	my $pjname = shift;

	my $user = getFromDB( $uid );

	if( $user )
	{
		$user->{datasets} = DatasetDBA::getByUser( $uid, $pjname );
	}

	return $user;
}

1;
