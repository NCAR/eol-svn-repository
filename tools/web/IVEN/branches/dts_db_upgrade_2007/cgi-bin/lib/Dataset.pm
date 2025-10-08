##Module-----------------------------------------------------------------------
# <p>The Dataset.pm module contains the Dataset class which is simply a 
#  container used to hold a record of the datasets table.</p>
#
# @author Dan Sullivan
##Module-----------------------------------------------------------------------

package Dataset;

use strict;

##-----------------------------------------------------------------------------
# @signature Dataset new()
# <p>Creates a new instance of the Dataset class</p>
#
# @output $self reference to the Dataset instance.
##-----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = {};
    bless( $self, $class );
    
    #$self->checkDefined();
    
    return $self;
}

sub addSource {
    my ($self, $src, $excluded) = @_;
    $self->{"source_datasets"}->{$src->getDatasetId()} = $src;
    $self->{"excluded"}->{$src->getDatasetId()} = $excluded;
}

sub getExcludedDatasetCount {
    my ($self) = @_;
    my $count = 0;
    foreach my $key (keys(%{$self->{"excluded"}})) {
        $count++ if ($self->{"excluded"}->{$key});
    }
    return $count;
}

sub getDatasetId {
    my ($self) = @_;
    return $self->{"dataset_id"};
}

sub getName {
    my ($self) = @_;
    return $self->{"name"};
}

sub getProcessedDatasetCount {
    my ($self) = @_;
    my $count = 0;
    foreach my $source ($self->getSourceDatasets()) {
        $count++ unless($source->isSourceOnlyDataset());
    }
    return $count;
}

sub getRecursiveCompletedDatasetCount {
    my ($self) = @_;
    unless (defined($self->{"comp_ds_count"})) {
        $self->{"comp_ds_count"} = 0;
        foreach my $source ($self->getSourceDatasets()) {
            unless ($source->isSourceOnlyDataset() || $self->isExcluded($source) || (defined($source->getStatus()) && !$source->getStatus()->isResolved())) {
                $self->{"comp_ds_count"}++;
                $self->{"comp_ds_count"} += $source->getRecursiveCompletedDatasetCount();
            }
        }
    }
    return $self->{"comp_ds_count"};
}

sub getRecursiveProcessedDatasetCount {
    my ($self) = @_;
    unless (defined($self->{"proc_ds_count"})) {
        $self->{"proc_ds_count"} = 0;
        foreach my $source ($self->getSourceDatasets()) {
            unless ($source->isSourceOnlyDataset() || $self->isExcluded($source)) {
                $self->{"proc_ds_count"}++;
                $self->{"proc_ds_count"} += $source->getRecursiveProcessedDatasetCount();
            }
        }
    }
    return $self->{"proc_ds_count"};
}

sub getSourceCount {
    my ($self) = @_;
    return scalar($self->getSourceDatasets());
}

sub getSourceDatasets {
    my ($self) = @_;
    my @list = ();

    foreach my $key (keys(%{$self->{"source_datasets"}})) {
        push(@list, $self->{"source_datasets"}->{$key});
    }

    return sort { $a->getName() cmp $b->getName()} @list;
}

sub getStatus {
    my ($self) = @_;
    return $self->{"status"};
}

sub isComposite {
    my ($self) = @_;

    if ($self->isSourceOnlyDataset()) { return 0; }

    my $count = 0;
    my $composite_count = 0;
    my $self_source = 0;
    my $src_dataset;
    foreach my $src_id (keys(%{$self->{"source_datasets"}})) {
        $count++;
        $src_dataset = $self->{"source_datasets"}->{$src_id} unless(defined($src_dataset));
        if ($src_id eq $self->getDatasetId()) { $self_source = 1; }
        elsif (!$self->{"source_datasets"}->{$src_id}->isSourceOnlyDataset()) { $composite_count++; }
    }

    if ($self_source && $count == 1) { return 1; }
    
    if ($composite_count == 0) { return 0; }

    if ($composite_count == 1 && $count == 1 && !$src_dataset->isSourceOnlyDataset()) { return 0; }

    return 1;
}

sub isExcluded {
    my ($self, $dataset) = @_;
    return $self->{"excluded"}->{$dataset->getDatasetId()};
}

sub isSourceOnlyDataset {
    my ($self) = @_;
    return !defined($self->{"source_datasets"});
}

sub setDatasetId {
    my ($self, $id) = @_;
    $self->{"dataset_id"} = $id;
}

sub setName {
    my ($self, $name) = @_;
    $self->{"name"} = $name;
}

sub setStatus {
    my ($self, $status) = @_;
    $self->{"status"} = $status;
}














##-----------------------------------------------------------------------------
# @signature void checkDefined()
# <p>Simple function to set all undefined/NULL values to empty character 
#  strings so they are printable.  Avoids a lot of runtime errors.</p>
##-----------------------------------------------------------------------------
sub checkDefined
{
	my $self = shift;

	foreach my $param ("dsname","pdname","pjname","storm_id","user_processing",
                     "raw_data","final_data","station_info","software","plots",
                     "status_notes","admin_notes","status","how_to","readme")
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : "";
	}

	foreach my $param ("id_type", "platform_type")
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : undef();
	}

	foreach my $param ("exclude", "questions", "best_sw" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : 0;
	}
}

##-----------------------------------------------------------------------------
# @signature void set( array_ref $row )
#  <p>Populates this Dataset with the given row of data from the datasets table.
#  Each value is removed from the array and stored in the order the datsets
#  table was created.  For use with a <code>Select * ...</code> type query.
#  Calls the checkDefined() function.
#
# @input $row a reference to the array of data retrieved from the database.
##-----------------------------------------------------------------------------
sub set
{
	my $self = shift;
	my $row = shift;

	$self->{dsname} = shift( @$row );
	$self->{pdname} = shift( @$row );
	$self->{pjname} = shift( @$row );
	$self->{storm_id} = shift( @$row );
	$self->{user_processing} = shift( @$row );
	$self->{raw_data} = shift( @$row );
	$self->{final_data} = shift( @$row );
	$self->{station_info} = shift( @$row );
	$self->{software} = shift( @$row );
	$self->{plots} = shift( @$row );
	$self->{id_type} = shift( @$row );
	$self->{platform_type} = shift( @$row );
	$self->{status_notes} = shift( @$row );
	$self->{admin_notes} = shift( @$row );
	$self->{status} = shift( @$row );
	$self->{exclude} = shift( @$row );
	$self->{questions} = shift( @$row );
	$self->{how_to} = shift( @$row );
	$self->{readme} = shift( @$row );
	$self->{best_sw} = shift( @$row );

	$self->checkDefined();
}

##-----------------------------------------------------------------------------
# @signature void dbPrepare()
# <p>Prepares this Dataset for entry into the database, seting empty string 
#  characters and undefined values to NULL.
##-----------------------------------------------------------------------------
sub dbPrepare
{
	my $self = shift;

	foreach my $param ("storm_id", "user_processing",
                     "raw_data", "final_data", "station_info", "software", "plots",
                     "status_notes", "admin_notes", "status", "how_to", "readme" )
	{
		$self->{$param} = defined( $self->{$param} ) && $self->{$param} ne "" ? 
				$self->{$param} : undef();
	}

	foreach my $param ("id_type", "platform_type" )
	{
		$self->{$param} = defined( $self->{$param} ) && $self->{$param} ne "" ? 
				$self->{$param} : "NULL";
	}

	foreach my $param ("exclude", "questions", "best_sw" )
	{
		$self->{$param} = defined( $self->{$param} )? $self->{$param} : 0;
	}
}

1;
