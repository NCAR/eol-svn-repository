#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The Project.pm module contains the Project class which is a simple 
#  container used to hold a record of the <i>projects</i> table.
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package Project;

use strict;
use Dataset;
use Note;
use IvenDB;
use Status;

sub addNote {
    my ($self, $note) = @_;
    $self->{"notes"}->{$note->getEntryDate()} = $note;
}

##-----------------------------------------------------------------------------
# @signature array_ref getAllProjects()
# <p>Queries the <i>projects</i> and returns an array of all the projects in
#  the database.
#
# @output $projects a reference to the array of Projects.
##-----------------------------------------------------------------------------
sub getAllProjects {
    my @projects;
    my $sql = "SELECT project_id, full_name FROM project ORDER BY project_id";

    my $dbh = IvenDB::getConnection();
    my $sth = $dbh->prepare( $sql );
    $sth->execute();

    while ((my @row = $sth->fetchrow())) {
	my $project = Project->new();
	$project->setProjectId($row[0]);
	$project->setName($row[1]);


	push(@projects, $project);
    }
    
    return @projects;
}

sub getBeginDate {
    my ($self) = @_;
    return $self->{"begin_date"};
}

sub getChargeNumber {
    my ($self) = @_;
    return $self->{"charge_number"};
}

sub getEndDate {
    my ($self) = @_;
    return $self->{"end_date"};
}

sub getMaxLatitude {
    my ($self) = @_;
    return $self->{"maxlat"};
}

sub getMaxLongitude {
    my ($self) = @_;
    return $self->{"maxlon"};
}

sub getMinLatitude {
    my ($self) = @_;
    return $self->{"minlat"};
}

sub getMinLongitude {
    my ($self) = @_;
    return $self->{"minlon"};
}

sub getName {
    my ($self) = @_;
    return $self->{"name"};
}

sub getProductList {
    my ($self) = @_;
    
    if (!defined($self->{"product_list"})) {
        my $statusMap = Status::getStatusMap();

	my $sql = "SELECT DISTINCT(dataset.dataset_id), name, process_status_id FROM dataset JOIN dataset_project ON dataset.dataset_id=dataset_project.dataset_id JOIN dataset_source_dataset ON dataset.dataset_id=dataset_source_dataset.dataset_id JOIN dataset_process ON dataset.dataset_id=dataset_process.dataset_id WHERE project_id=?";
	my $dbh = IvenDB::getConnection();
	my $sth = $dbh->prepare($sql);
	$sth->execute($self->getProjectId());

	while ((my @row = $sth->fetchrow())) {

    	        my $dataset = Dataset->new();
	        $dataset->setDatasetId($row[0]);
	        $dataset->setName($row[1]);

            if (!defined($self->{"datasets"}->{$dataset->getDatasetId()})) {
                $self->{"datasets"}->{$dataset->getDatasetId()} = $dataset;
            }

            $self->{"datasets"}->{$dataset->getDatasetId()}->setStatus($statusMap->{$row[2]});

            my $stmt = $dbh->prepare("SELECT source_dataset_id, name, exclude_flag FROM dataset_source_dataset JOIN dataset ON dataset_source_dataset.source_dataset_id=dataset.dataset_id WHERE dataset_source_dataset.dataset_id=?");
            $stmt->execute($dataset->getDatasetId());

            while ((my @results = $stmt->fetchrow())) {
                if (!defined($self->{"datasets"}->{$results[0]})) {
                    my $src = Dataset->new();
                    $src->setDatasetId($results[0]);
                    $src->setName($results[1]);
                    $self->{"datasets"}->{$src->getDatasetId()} = $src;
                }

                $self->{"datasets"}->{$dataset->getDatasetId()}->addSource($self->{"datasets"}->{$results[0]}, $results[2]);
            }
	}


        foreach my $dataset_id (keys(%{$self->{"datasets"}})) {
            my $dataset = $self->{"datasets"}->{$dataset_id};

            if ($dataset->isComposite()) {
                push(@{$self->{"product_list"}}, $dataset);
            }
        }
    }

    return defined($self->{"product_list"}) ? sort { $a->getName() cmp $b->getName() } @{$self->{"product_list"}} : ();
}

sub getProjectId {
    my ($self) = @_;
    return $self->{"project_id"};
}

sub getProjectNotes {
    my ($self) = @_;
    my @list = ();
    
    foreach my $key (keys(%{$self->{"notes"}})) {
	push(@list, $self->{"notes"}->{$key});
    }

    return sort { $b->getEntryDate() cmp $a->getEntryDate() } @list;
}

sub getUserMap {
    my ($self) = @_;

    if (!defined($self->{"user_map"})) {
	$self->{"user_map"} = {};
    }

    return $self->{"user_map"};
}

sub load {
    my ($project_id) = @_;
    
    my $project = Project->new();
    $project->setProjectId($project_id);

    my $sql = "SELECT full_name, begin_date, end_date, minlat, minlon, maxlat, maxlon, charge_number, active_flag FROM project WHERE project_id=?";
    my $dbh = IvenDB::getConnection();
    my $sth = $dbh->prepare($sql);
    $sth->execute($project_id);

    if (my @row = $sth->fetchrow()) {
	$project->setName($row[0]);
	$project->setBeginDate($row[1]);
	$project->setEndDate($row[2]);
	$project->setMinLatitude($row[3]);
	$project->setMinLongitude($row[4]);
	$project->setMaxLatitude($row[5]);
	$project->setMaxLongitude($row[6]);
	$project->setChargeNumber($row[7]);
    }
   
    my $stmt = $dbh->prepare("SELECT DISTINCT(note.note_id), author_id, entry_date, note_text, row_revise_time FROM note JOIN project_note ON note.note_id=project_note.note_id WHERE project_id=?");
    $stmt->execute($project_id);

    while ((my @row = $stmt->fetchrow())) {
        my $note = Note->new();
        $note->setNoteId($row[0]);
        $note->setAuthor($project->getUserMap()->{$row[1]});
        $note->setEntryDate($row[2]);
        $note->setNoteText($row[3]);
        $note->setRowReviseTime($row[4]);
        $project->addNote($note);
    }
 
    return $project;
}

sub setBeginDate {
    my ($self, $date) = @_;
    $self->{"begin_date"} = $date;
}

sub setChargeNumber {
    my ($self, $number) = @_;
    $self->{"charge_number"} = $number;
}

sub setEndDate {
    my ($self, $date) = @_;
    $self->{"end_date"} = $date;
}

sub setMaxLatitude {
    my ($self, $lat) = @_;
    $self->{"maxlat"} = $lat;
}

sub setMaxLongitude {
    my ($self, $lon) = @_;
    $self->{"maxlon"} = $lon;
}

sub setMinLatitude {
    my ($self, $lat) = @_;
    $self->{"minlat"} = $lat;
}

sub setMinLongitude {
    my ($self, $lon) = @_;
    $self->{"minlon"} = $lon;
}

sub setName {
    my ($self, $name) = @_;
    $self->{"name"} = $name;
}

sub setProjectId {
    my ($self, $project_id) = @_;
    $self->{"project_id"} = $project_id;
}










##-----------------------------------------------------------------------------
# @signature Project new()
# <p>Creates a new instance of the Project class.</p>
#
# @output $self the new Project
##-----------------------------------------------------------------------------
sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->checkDefined();

	$self->{products} = undef();

	return $self;
}

##-----------------------------------------------------------------------------
# @signature void checkDefined()
# <p>Simple function to set all undefined/NULL values to empty character 
#  strings.  Avoids many runtime errors.<p>
##-----------------------------------------------------------------------------
sub checkDefined
{
#	my $self = shift;

#	foreach my $param ("pjname","full_name","storm_id_prefix","begin_date",
#										 "end_date","minlat","maxlat","minlon","maxlon",
#										 "admin_notes","link_source","link_target","charge_num")
#	{
#		$self->{$param} = ( defined( $self->{$param} ) )? $self->{$param} : "";
#	}
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref $row )
# <p>Populates this Project with the given row of data from the projects table.
#  Each value is removed from the array (using shift) and stored in the order
#  the projects table was created.  For user with SELECT * FROM projects
#  type qureies.  Calls the checkDefined() function. </p>
##-----------------------------------------------------------------------------
sub set
{
	#my $self = shift;
	#my $row = shift;

	#$self->{pjname} = shift( @$row );
	#$self->{full_name} = shift( @$row );
	#$self->{storm_id_prefix} = shift( @$row );
	#$self->{begin_date} = shift( @$row );
	#$self->{end_date} = shift( @$row );
	#$self->{minlat} = shift( @$row );
	#$self->{maxlat} = shift( @$row );
	#$self->{minlon} = shift( @$row );
	#$self->{maxlon} = shift( @$row );
	#$self->{admin_notes} = shift( @$row );
	#$self->{link_source} = shift( @$row );
	#$self->{link_target} = shift( @$row );
	#$self->{charge_num} = shift( @$row );

	#$self->checkDefined();
}

##-----------------------------------------------------------------------------
# @signature void dbPrepare()
# <p>Prepares the Project to be inserted (or updated) into the database.  
#  Sets all empty character strings to NULL values.</p>
##-----------------------------------------------------------------------------
sub dbPrepare
{
	#my $self = shift;

	#foreach my $param ("pjname","full_name","storm_id_prefix","begin_date",
	#									 "end_date", "admin_notes","link_source","link_target","charge_num")
	#{
	#	$self->{$param} = ( $self->{$param} && $self->{$param} ne "" )? 
	#		$self->{$param} : undef();
	#}

	# Make the min/max lat/longs equal to "NULL" if not entered	
	#foreach my $param ( "minlat","maxlat","minlon","maxlon")
	#{
	#	$self->{$param} = ( $self->{$param} && $self->{$param} ne "" )? 
	#		$self->{$param} : "NULL";
	#}

	# This is a little shortcut, the storm_id_prefix in the database cannot be NULL
	#  There is no reason as to why it can't be NULL, but now's not the time to be messing with the database - Dan
	#if( !defined( $self->{storm_id_prefix} ) )
	#{ $self->{storm_id_prefix} = "99"; }
}


1;
