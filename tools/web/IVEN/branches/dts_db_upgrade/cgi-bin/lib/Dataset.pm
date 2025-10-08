#! /usr/bin/perl -w

package Dataset;
use strict;
use lib ".";
use Entry;
use Project;
our @ISA = ("Entry");

sub addDatasets {
    my ($self,@datasets) = @_;
    push(@{ $self->{"datasets"}},@datasets);
}

sub addProducts {
    my ($self,@products) = @_;
    push(@{ $self->{"products"}},@products);
}

sub addReadmes {
    my ($self,@readmes) = @_;
    push(@{ $self->{"readmes"}},@readmes);
}

sub getCollectionTimeNote {
    my ($self) = @_;
    return defined($self->{"collection_time_note"}) ? $self->{"collection_time_note"} : "";
}

sub getCollectionTimeNoteId {
    my ($self) = @_;
    return defined($self->{"collection_time_note_id"}) ? $self->{"collection_time_note_id"} : -1;
}

sub getDatasetType {
    my ($self) = @_;
    return defined($self->{"dataset_type"}) ? $self->{"dataset_type"} : "Processed";
}

sub getDatasets {
    my ($self) = @_;
    return defined($self->{"datasets"}) ? @{ $self->{"datasets"}} : ();
}

sub getDSTFlag {
    my ($self) = @_;
    return $self->{"dst_flag"};
}

sub getFinalDir {
    my ($self) = @_;
    return defined($self->{"final_dir"}) ? $self->{"final_dir"} : "";
}

sub getHomeDir {
    my ($self) = @_;
    return defined($self->{"home_dir"}) ? $self->{"home_dir"} : "";
}

sub getHowTo {
    my ($self) = @_;
    return defined($self->{"howto"}) ? $self->{"howto"} : "";
}

sub getNoteId {
    my ($self) = @_;
    return defined($self->{"note_id"}) ? $self->{"note_id"} : -1;
}

sub getNote {
    my ($self) = @_;
    return defined($self->{"note"}) ? $self->{"note"} : "";
}

sub getPlotDir {
    my ($self) = @_;
    return defined($self->{"plot_dir"}) ? $self->{"plot_dir"} : "";
}

sub getProcessContactId {
    my ($self) = @_;
    return defined($self->{"process_contact"}) ? $self->{"process_contact"} : -1;
}

sub getProducts {
    my ($self) = @_;
    return defined($self->{"products"}) ? @{ $self->{"products"}} : ();
}

sub getProject {
    my ($self) = @_;
    return defined($self->{"project"}) ? $self->{"project"} : Project->new();
}

sub getReadmes {
    my ($self) = @_;
    return defined($self->{"readmes"}) ? @{ $self->{"readmes"}} : ();
}

sub getReposDir {
    my ($self) = @_;
    return defined($self->{"repos_dir"}) ? $self->{"repos_dir"} : "";
}

sub getSourceTimeNote {
    my ($self) = @_;
    return defined($self->{"source_time_note"}) ? $self->{"source_time_note"} : "";
}

sub getSourceTimeNoteId {
    my ($self) = @_;
    return defined($self->{"source_time_note_id"}) ? $self->{"source_time_note_id"} : -1;
}

sub getStationList {
    my ($self) = @_;
    return defined($self->{"station_list"}) ? $self->{"station_list"} : "";
}

sub getUTCOffset {
    my ($self) = @_;
    return defined($self->{"utc_offset"}) ? $self->{"utc_offset"} : "";
}

sub hasQuestions {
    my ($self) = @_;
    return defined($self->{"questions"}) ? $self->{"questions"} : 0;
}

sub insert {
    my ($self,$db) = @_;
    $db->insertDataset($self);
}

sub isExcluded {
    my ($self) = @_;
    return defined($self->{"excluded"}) ? $self->{"excluded"} : 0;
}

sub getStatusId {
    my ($self) = @_;
    return defined($self->{"status_id"}) ? $self->{"status_id"} : -1;
}

sub setCollectionTimeNote {
    my ($self,$note) = @_;
    $self->{"collection_time_note"} = $note;
}

sub setCollectionTimeNoteId {
    my ($self,$id) = @_;
    $self->{"collection_time_note_id"} = $id;
}

sub setDatasetType {
    my ($self,$type) = @_;
    $self->{"dataset_type"} = $type;
}

sub setDSTFlag {
    my ($self,$flag) = @_;
    $self->{"dst_flag"} = $flag;
}

sub setExcludedFlag {
    my ($self,$flag) = @_;
    $self->{"excluded"} = $flag;
}

sub setFinalDir {
    my ($self,$dir) = @_;
    $self->{"final_dir"} = $dir;
}

sub setHomeDir {
    my ($self,$dir) = @_;
    $self->{"home_dir"} = $dir;
}

sub setHowTo {
    my ($self,$link) = @_;
    $self->{"howto"} = $link;
}

sub setNoteId {
    my ($self,$id) = @_;
    $self->{"note_id"} = $id;
}

sub setNote {
    my ($self,$note) = @_;
    $self->{"note"} = $note;
}

sub setPlotDir {
    my ($self,$dir) = @_;
    $self->{"plot_dir"} = $dir;
}

sub setProcessContactId {
    my ($self,$id) = @_;
    $self->{"process_contact"} = $id;
}

sub setProject {
    my ($self,$project) = @_;
    $self->{"project"} = $project;
}

sub setQuestionFlag {
    my ($self,$flag) = @_;
    $self->{"questions"} = $flag;
}

sub setReposDir {
    my ($self,$dir) = @_;
    $self->{"repos_dir"} = $dir;
}

sub setSourceTimeNote {
    my ($self,$note) = @_;
    $self->{"source_time_note"} = $note;
}

sub setSourceTimeNoteId {
    my ($self,$id) = @_;
    $self->{"source_time_note_id"} = $id;
}

sub setStationList {
    my ($self,$list) = @_;
    $self->{"station_list"} = $list;
}

sub setStatusId {
    my ($self,$id) = @_;
    $self->{"status_id"} = $id;
}

sub setUTCOffset {
    my ($self,$offset) = @_;
    $self->{"utc_offset"} = $offset;
}

sub update {
    my ($self,$db) = @_;
    $db->updateDataset($self);
}

1;
