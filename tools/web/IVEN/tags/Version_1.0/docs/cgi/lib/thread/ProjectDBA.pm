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
use ThreadNode;
use ThreadGroup;

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
# @signature array_ref getAllProjects()
# <p>Queries the <i>projects</i> and returns an array of all the projects in
#  the database.
#
# @output $projects a reference to the array of Projects.
##-----------------------------------------------------------------------------
sub getAllProjects
{
	my @projects;
	my $sql = "SELECT * FROM projects";
	my $dbh = IvenDB::connect();

	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	while( (my @row = $sth->fetchrow()) )
	{
		my $proj = Project->new();
		$proj->set( \@row );
		$projects[scalar(@projects)] = $proj;
	}

	return \@projects;
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
#  <font color=red>Warning: </font>This has some bugs and does not work
#  correctly.</p>
# 
# @input $pjname the name of the project to match
#
# @output $project and instance of Project with some additions: <br>
#  <b>tgroup:</b> The group of threads showing dependencies.<br>
#  <b>no_deps:</b> The products which have no dependencies. 
#
##-----------------------------------------------------------------------------
sub buildThreadView
{
	my $pjname = shift;

	my $project = ProjectDBA::getFromDB( $pjname );

	my $products = ProductDBA::getByProject( $pjname );
	my @no_deps;
	my $tgroup = ThreadGroup->new();
	my $tcount = 0;
	my @nodes;

	foreach my $pd (@$products)
	{
		my $exts = ExtractionDBA::getByProduct( $pd->{pdname}, $pd->{pjname} );
		if( scalar( @$exts ) == 0 )
		{
			$no_deps[scalar(@no_deps)] = $pd;
		}
		else
		{
			foreach my $ext (@$exts)
			{
				my $parent = ThreadNode->new( $ext->{pdname} );
				my $child = ThreadNode->new( $ext->{pdsource} );
				$parent->addNode( $child );
				push( @nodes, $parent );
			}
		}
	}
	$tgroup->setThreadList( \@nodes );

	$project->{tgroup} = $tgroup;
	$project->{no_deps} = \@no_deps;

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

	my $sql = "UPDATE projects SET " .
						"admin_notes = " . $dbh->quote( $pj->{admin_notes} ) . 
						" WHERE " . 
						"pjname = " . $dbh->quote( $pj->{pjname} );

	$dbh->do( $sql );
}

1;
