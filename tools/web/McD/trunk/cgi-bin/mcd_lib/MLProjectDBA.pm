package MLProjectDBA;

# --------------------------------------------------------------
# MLProjectDBA.pm:
#  Contains the MLProjectDBA class used to perform operations on the
#  database.  
# 
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - returns a new instance of the MLProjectDBA class
#
# sub getAllProjects( $active ) - Returns an array of all the projects 
#  in the database.  Pulls only the projects with the given active 
#  value, if $active is not given (undef) all projects are returned.
#
# sub getFromDB( $proj_id ) - query the database and return the
#  MLProject associated with the given id.
#
# sub getFromDBByName( $proj_name ) - query the database and return the
#  MLProject associated with the given project name.
#
# sub updateDB( $proj ) - update the record in the project table 
#  corresponding to the given MLProject.
#
# sub insertDB($proj) - insert a new record into the database with
#   the given MLProject's values.
# --------------------------------------------------------------

use lib ".";
use McDDB;
use MLProject;
use ProjectDBA;
@ISA = ("ProjectDBA"); # Inherit the ProjectDBA class

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

	my $sql = "SELECT project.*, projmlview.* FROM project, projmlview WHERE " . 
						"project.id=projmlview.project AND project.active = $active ORDER BY project.name";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $proj = MLProject->new();
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
	my $proj = MLProject->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT project.*, projmlview.* FROM project, projmlview WHERE " . 
						"project.id=projmlview.project AND project.id = $id";

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
	$self->SUPER::updateDB( $proj );

	my $dbh = McDDB::connect();

	my $sql = "UPDATE projmlview SET " . 
						"logo_src = " . $dbh->quote( $proj->{logo_src} ) . ", " .
						"left_css_src = " . $dbh->quote( $proj->{left_css_src} ) . ", " .
						"body_css_src = " . $dbh->quote( $proj->{body_css_src} ) . 
						" WHERE project=$proj->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$dbh->disconnect();
}

sub insertDB
{
	my $self = shift;
	my $proj = shift;
	$self->SUPER::insertDB( $proj );

	my $dbh = McDDB::connect();
	my $sql = "INSERT INTO projmlview VALUES( $proj->{id}, " .
						$dbh->quote( $proj->{logo_src} ) . ", " .
						$dbh->quote( $proj->{left_css_src} ) . ", " .
						$dbh->quote( $proj->{body_css_src} ) . " " .
						")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$dbh->disconnect();
}

sub getFromDBByName
{
	my $self = shift;
	my $name = shift;
	my $proj = MLProject->new();

	my $dbh = McDDB::connect();
	my $sql = "SELECT project.*, projmlview.* FROM project, projmlview WHERE " . 
						"project.id=projmlview.project AND project.name = " . $dbh->quote($name);

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @row = $sth->fetchrow();
	if( !@row )
	{
		$proj = undef();
	}
	else
	{
		$proj->set( \@row );
	}
	$sth->finish();
	$dbh->disconnect();
	return $proj;
}

1;

