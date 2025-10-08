package Project;

# --------------------------------------------------------------
# Project.pm:
#  Contains the Project class used to perform operations on the
#  database.  A single instance of the Project class corresponds
#  to an entry in the project table.
# 
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new() - returns a new instance of the Project class
#
# sub set( @vals ) - assigns all of the fields in this Project
#  with the given values.
#
# sub getAll( $active ) - Static method, returns an array of all the projects 
#  in the database.  Pulls only the projects with the given active 
#  value, if $active is not given (undef) all projects are returned.
#
# sub getAllDatasets( $proj_id, $order_by ) - Static method,
#  returns all of the datasets associated with the given project.
#  Datasets are ordered by the field defined by $order_by, if
#  $order_by is not given the default is title.
#
# sub setFromDB( $id ) - query the database and populate this project
#
# sub checkDefined() - set default values to fields with undef()
#
# sub updateDB() - update the record in the project table 
#  corresponding to this Project.
#
# sub deleteDB() - delete the record in the project table 
#  correspondign to this Project.
#
# sub insertDB() - insert a new record into the database with
#   this projects values.
# --------------------------------------------------------------

use lib ".";
use MLDB;
use Utils;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = -1;
	$self->{name} = "";
	$self->{long_name} = "";
	$self->{active} = 1;
	$self->{logo_src} = "";
	$self->{left_css_src} = "";
	$self->{body_css_src} = "";
	$self->{contact_name} = "";
	$self->{contact_email} = "";

	return $self;
}

sub set
{
	my $self = shift;
	$self->{id} = shift;
	$self->{name} = shift;
	$self->{long_name} = shift;
	$self->{active} = shift;
	$self->{logo_src} = shift;
	$self->{left_css_src} = shift;
	$self->{body_css_src} = shift;
	$self->{contact_name} = shift;
	$self->{contact_email} = shift;

	$self->checkDefined();
}

sub getAll
{
	my $active = shift;
	my @projects;
	my $count = 0;
	$active = 1 if( !defined( $active ) );

	my $dbh = MLDB->connect();

	my $sql = "SELECT * FROM project WHERE active = $active ORDER BY name";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $proj = Project->new();
		$proj->set( @row );
		$projects[$count++] = $proj;
	}

	$sth->finish();	
	$dbh->disconnect();

	return @projects;
}

sub getAllDatasets
{
	my $project = shift;
	my $order = shift;
	$order = "title" if( !defined( $order ) );

	my @datasets;
	my $c = 0;

	my $dbh = MLDB->connect();
	my $sql = "SELECT * FROM dataset WHERE project =$project ORDER BY $order";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	while( (my @row = $sth->fetchrow() ) )
	{
		my $ds = Dataset->new();
		$ds->set( @row );
		$datasets[$c++] = $ds;
	}

	$sth->finish();
	$dbh->disconnect();
	return @datasets;	
}

sub setFromDB
{
	my $self = shift;
	my $id = shift;

	my $dbh = MLDB->connect();
	my $sql = "SELECT * FROM project WHERE id = $id";

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	$self->set( $sth->fetchrow() );

	$sth->finish();
	$dbh->disconnect();
}

sub checkDefined
{
	my $self = shift;

	$self->{name} = ( defined( $self->{name} ) )? $self->{name}: "";
	$self->{long_name} = ( defined( $self->{long_name} ) )? $self->{long_name}: "";
	$self->{active} = ( defined( $self->{active} ) )? $self->{active}: 0;
	$self->{logo_src} = ( defined( $self->{logo_src} ) )? $self->{logo_src}: "";
	$self->{left_css_src} = ( defined( $self->{left_css_src} ) )? $self->{left_css_src}: "";
	$self->{body_css_src} = ( defined( $self->{body_css_src} ) )? $self->{body_css_src}: "";
	$self->{contact_name} = ( defined( $self->{contact_name} ) )? $self->{contact_name}: "";
	$self->{contact_email} = ( defined( $self->{contact_email} ) )? $self->{contact_email}: "";
} 

sub updateDB
{
	my $self = shift;

	my $dbh = MLDB::connect();
	my $sql = "UPDATE project SET " . 
						"name = " . $dbh->quote( $self->{name} ) . ", " . 
						"long_name = " . $dbh->quote( $self->{long_name} ) . ", " .
						"active = " . $self->{active} . ", " .
						"logo_src = " . $dbh->quote( $self->{logo_src} ) . ", " .
						"left_css_src = " . $dbh->quote( $self->{left_css_src} ) . ", " .
						"body_css_src = " . $dbh->quote( $self->{body_css_src} ) . ", " .
						"contact_name = " . $dbh->quote( $self->{contact_name} ) .  ", " .
						"contact_email = " . $dbh->quote( $self->{contact_email} ) . 
						" WHERE id=$self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$dbh->disconnect();
}
sub deleteDB
{
	my $self = shift;

	my $dbh = MLDB::connect();
	my $sql = "DELETE FROM project WHERE id=$self->{id}";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$dbh->disconnect();
}
sub insertDB
{
	my $self = shift;

	my $dbh = MLDB::connect();
	my $sql = "INSERT INTO project VALUES( 0, " .
						$dbh->quote( $self->{name} ) . ", " .
						$dbh->quote( $self->{long_name} ) . ", " .
						$self->{active} . ", " .
						$dbh->quote( $self->{logo_src} ) . ", " .
						$dbh->quote( $self->{left_css_src} ) . ", " .
						$dbh->quote( $self->{body_css_src} ) . ", " .
						$dbh->quote( $self->{contact_email} ) . ", " .
						$dbh->quote( $self->{contact_name} ) . ")";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr() );

	$self->{id} = $dbh->{'mysql_insertid'};
	
	$dbh->disconnect();
}
1;
