##Module-----------------------------------------------------------------------
# <p>The ProjectDBA.pm module contains all functions needed to manipulate the
#  <i>projects</i> table in the database.  All functions use the Project class
#  to insert, update, delete and select records.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package ProjectDBA;

use strict;
use lib ".";
use Project;
use DatasetDBA;
use ProductDBA;
use Extraction;
use List;
use Node;

##-----------------------------------------------------------------------------
# @signature Project buildProjectView( string $pjname )
# <p>Queries the database and creates the needed data structure for the 
#  project_view.  Returns an instance of Project with the project's products
#  reference by 'products' obtainbed by a call to ProductDBA::getByProject().  
#
# @input $pjname the project name to match
#
# @output an intance of Project described above
##-----------------------------------------------------------------------------
sub buildProjectView
{
	my $proj = shift;

	my $project = getFromDB( $proj );

	if( !defined( $project ) )
	{
		$project = Project->new();
		$project->{pjname} = $proj;
	}

	$project->{products} = ProductDBA::getByProject( $proj );

	return $project;
}

##-----------------------------------------------------------------------------
# @signature Project getFromDB( string $pjname )
# <p>Selects the given project from the database.</p>
#
# @input $pjname the project name to match in the DB.
# 
# @output $proj an instance of Project.
##-----------------------------------------------------------------------------
sub getFromDB
{
	my $pjname = shift;
	my $dbh = IvenDB::connect();
	my $project;

	my $sql = "SELECT * FROM projects WHERE pjname=" . $dbh->quote( $pjname );

	my $sth = $dbh->prepare( $sql );
	$sth->execute();

	my @row = $sth->fetchrow();
	if( @row )
	{
	 	$project = Project->new();
		$project->set( \@row );
	}
	else
	{	
		$project = undef();
	}

	return $project;
}

##-----------------------------------------------------------------------------
# @signature Project buildThreadView( string $pjname )
# <p>Builds the needed data structure to show the dependencies between 
#  Products for the given project.<br>
# 
# @input $pjname the name of the project to match
#
# @output $project and instance of Project with some additions: <br>
#  <b>threads:</b> A reference to an array of List objects containing all the 
#    dependency threads within the project.  See the List.pm module.<br>
#
##-----------------------------------------------------------------------------
sub buildThreadView
{
	my $pjname = shift;
	my @lists;
	my $c = 0;

	my $project = ProjectDBA::getFromDB( $pjname );

	my $exts = ExtractionDBA::getByProject( $pjname, 0 );

	foreach my $ext (@$exts)
	{
		my $pdsource = ProductDBA::getFromDB( $ext->{pdsource}, $pjname );
		my $pdname = ProductDBA::getFromDB( $ext->{pdname}, $pjname );
		#$lists[$c++] = List->new( $ext->{pdsource}, $ext->{pdname} ); 
		$lists[$c++] = List->new( $pdsource, $pdname ); 
	}

	for( my $x = 0; $x < $c; $x++ )
	{
		my $current = $lists[$x];

		for( my $j = 0; $j < $c; $j++ )
		{
			if( $j != $x )
			{
				if( $current->{head}->{val}->{pdname} eq $lists[$j]->{tail}->{val}->{pdname} )
				{
					$lists[$j]->addToTail( $current->copy() );
					$current->{linked} = 1;
				}	
				elsif( $current->{tail}->{val}->{pdname} eq $lists[$j]->{head}->{val}->{pdname} )
				{
					$lists[$j]->addToHead( $current->copy() );
					$current->{linked} = 1;
				}	
			}
		}
	}

	my @lists2;
	foreach my $list (@lists)
	{
		@lists2[scalar(@lists2)] = $list if( !$list->{linked} );
	}
	$project->{threads} = \@lists2;

	return $project;	
}

##-----------------------------------------------------------------------------
# @signature void updateDBNotes(Project $pj)
# <p>Updates the the notes of the given project in the database.  <b>Note: 
#  </b> other functions will have to bee added to update the other attributes
#  in the <i>projects</i> table.
#
# @input $pj the project to update.
##-----------------------------------------------------------------------------
sub updateDBNotes
{
	my $pj = shift;
	my $dbh = IvenDB::connect();
	$pj->dbPrepare();

	my $sql = "UPDATE projects SET " .
						"admin_notes = " . $dbh->quote( $pj->{admin_notes} ) . 
						" WHERE " . 
						"pjname = " . $dbh->quote( $pj->{pjname} );

	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void insertDB( Project $proj )
# <p>Inserts the given project into the database.</p>
#
# @input $proj the project to insert
##-----------------------------------------------------------------------------
sub insertDB
{
	my $proj = shift;
	$proj->dbPrepare();

	my $dbh = IvenDB::connect();

	my $sql = "INSERT INTO projects " .
						"(pjname, full_name, storm_id_prefix, begin_date, end_date, minlat, " .
						" maxlat, minlon, maxlon, link_source, link_target, charge_num ) " . 
						"VALUES (" .
						$dbh->quote( $proj->{pjname} ) . ", " .
						$dbh->quote( $proj->{full_name} ) . ", " .
						$dbh->quote( $proj->{storm_id_prefix} ) . ", " .
						$dbh->quote( $proj->{begin_date} ) . ", " .
						$dbh->quote( $proj->{end_date} ) . ", " .
						"$proj->{minlat}, " .
						"$proj->{maxlat}, " .
						"$proj->{minlon}, " .
						"$proj->{maxlon}, " .
						$dbh->quote( $proj->{link_source} ) . ", " .
						$dbh->quote( $proj->{link_target} ) . ", " .
						$dbh->quote( $proj->{charge_num} ) . ") ";

	$dbh->do( $sql );
}

##-----------------------------------------------------------------------------
# @signature void updateDB( Project $proj, string $source_pjname )
# <p>Update the given project in the database.  The source_pjname is the
#  original name of the project in the database.  The value of pjname in the
#  $proj object could have been changed by the user.</p>
#
# @input $proj project with which to update the database
# @input $source_pjname the name of the project in the database to update
##-----------------------------------------------------------------------------
sub updateDB
{
	my $proj = shift;
	my $source_pjname = shift;
	my $dbh = IvenDB::connect();
	$proj->dbPrepare();

	my $sql = "UPDATE projects SET " .
						"pjname = " . $dbh->quote( $proj->{pjname} ) . ", " . 
						"full_name = " . $dbh->quote( $proj->{full_name} ) . ", " . 
						"storm_id_prefix = " . $dbh->quote( $proj->{storm_id_prefix} ) . ", " . 
						"begin_date = " . $dbh->quote( $proj->{begin_date} ) . ", " . 
						"end_date = " . $dbh->quote( $proj->{end_date} ) . ", " . 
						"minlat = " . $proj->{minlat} . ", " .
						"maxlat = " . $proj->{maxlat} . ", " .
						"minlon = " . $proj->{minlon} . ", " .
						"maxlon = " . $proj->{maxlon} . ", " .
						"link_source = " . $dbh->quote( $proj->{link_source} ) . ", " . 
						"link_target = " . $dbh->quote( $proj->{link_target} ) . ", " . 
						"charge_num = " . $dbh->quote( $proj->{charge_num} ) . 
						" WHERE pjname = " . $dbh->quote( $source_pjname );

	$dbh->do( $sql );
}
1;
