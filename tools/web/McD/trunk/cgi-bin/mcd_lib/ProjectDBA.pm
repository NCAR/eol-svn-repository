package ProjectDBA;

# --------------------------------------------------------------
# ProjectDBA.pm:
#  Contains the ProjectDBA class used to perform operations on the
#  database.  
# 
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - returns a new instance of the ProjectDBA class
#
# sub getAllProjects( $active ) - Returns an array of all the projects 
#  in the database.  Pulls only the projects with the given active 
#  value, if $active is not given (undef) all projects are returned.
#
# sub getFromDB( $proj_id ) - query the database and return the
#  Project associated with the given id.
#
# sub updateDB( $proj ) - update the record in the project table 
#  corresponding to the given Project.
#
# sub deleteDB( $proj ) - delete the record in the project table 
#  correspondign to the given Project.
#
# sub insertDB($proj) - insert a new record into the database with
#   the given Project's values.
# --------------------------------------------------------------

use lib ".";
use McDDB;
use Project;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	return $self;
}

sub getAllProjects
{
	my $self = shift;
	my $active = shift;
	my @projects;
	my $count = 0;
	$active = 1 if( !defined( $active ) );

	my $dbh = McDDB::connect();

	my $sql = "SELECT * FROM project WHERE active = $active ORDER BY name";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $proj = Project->new();
		$proj->set( \@row );
		$projects[$count++] = $proj;
	}

	$sth->finish();	
	$dbh->disconnect();

	return @projects;
}


sub getFromDB
{
	my $self = shift;
	my $id = shift;
	my $proj = Project->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT * FROM project WHERE id = $id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @row = $sth->fetchrow();
	$proj->set( \@row );

	$sth->finish();
	$dbh->disconnect();
	return $proj;
}

sub updateDB
{
	my $self = shift;
	my $proj = shift;
	$proj->dbPrepare();

	my $dbh = McDDB::connect();
	my $sql = "UPDATE project SET " . 
						"name = " . $dbh->quote( $proj->{name} ) . ", " . 
						"long_name = " . $dbh->quote( $proj->{long_name} ) . ", " .
						"active = " . $proj->{active} . ", " .
						"internal_contact = " . $dbh->quote( $proj->{internal_contact} ) . ", ". 
						"storm_id_prefix = " . $dbh->quote( $proj->{storm_id_prefix} ) . 
						" WHERE id=$proj->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$dbh->disconnect();
}
sub deleteDB
{
	my $self = shift;
	my $proj = shift;

	my $dbh = McDDB::connect();
	my $sql = "DELETE FROM project WHERE id=$proj->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$dbh->disconnect();
}
sub insertDB
{
	my $self = shift;
	my $proj = shift;
	$proj->dbPrepare();

	my $dbh = McDDB::connect();
	my $sql = "INSERT INTO project VALUES( 0, " .
						$dbh->quote( $proj->{name} ) . ", " .
						$dbh->quote( $proj->{long_name} ) . ", " .
						$proj->{active} . ", " .
						$dbh->quote( $proj->{internal_contact} ) . ", " . 
						$dbh->quote( $proj->{storm_id_prefix} ) . 
						")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$proj->{id} = $dbh->{'mysql_insertid'};
	
	$dbh->disconnect();
}
1;

