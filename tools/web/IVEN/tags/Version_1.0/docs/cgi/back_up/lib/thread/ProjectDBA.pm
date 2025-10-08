package ProjectDBA;

use strict;
use lib ".";
use Project;
use DatasetDBA;
use ProductDBA;
use Extraction;
use ThreadNode;
use ThreadList;
use ThreadGroup;

sub buildProjectView
{
	my $proj = shift;

	# this slows it down for some reason :(
	my $project = getFromDB( $proj );
	if( !defined( $project ) )
	{
		$project = Project->new();
		$project->{pjname} = $proj;
	}

	#my $project = Project->new();
	#$project->{pjname} = $proj;

	$project->{products} = ProductDBA::getByProject( $proj );

	return $project;
}

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

sub buildThreadView
{
	my $pjname = shift;

	my $project = ProjectDBA::getFromDB( $pjname );

	my $products = ProductDBA::getByProject( $pjname );
	my @no_deps;
	my $tgroup = ThreadGroup->new();
	my $tcount = 0;

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
				my $list = ThreadList->new( $parent );
				$list->addNode( $child );

print( "\nproj: $list->{head}->{pdname}  $list->{head}->{next}->{pdname} \n" );
				$tgroup->addThreadList( $list );
			}
		}
	}

my $head = ThreadGroup::combineLists( $tgroup->{thread_lists}->[0]->{head}, $tgroup->{thread_lists}->[1]->{head} );
$tgroup->{thread_lists}->[3] = ThreadList->new( $head );
$tgroup->{tcount}++; 

	$project->{tgroup} = $tgroup;

	return $project;	
}

1;
