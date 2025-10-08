#! /usr/bin/perl -w

package Product;
use strict;
use lib ".";
use Entry;
use Project;
our @ISA = ("Entry");

sub addDatasets {
    my ($self,@datasets) = @_;
    push(@{ $self->{"datasets"}},@datasets);
}

sub getDatasets {
    my ($self) = @_;
    return defined($self->{"datasets"}) ? @{ $self->{"datasets"}} : ();
}

sub getEditor() { return undef(); }

sub getProject {
    my ($self) = @_;
    return defined($self->{"project"}) ? $self->{"project"} : Project->new();
}

sub getStats { return (undef(),undef(),undef(),undef()); }

sub getStatusId {
    my ($self) = @_;
    return defined($self->{"status_id"}) ? $self->{"status_id"} : -1;
}

sub getTypedId { return undef(); }

sub getViewer() { return undef(); }

sub isGroup { return undef(); }

sub loadDatasets { return undef(); }

sub setProject {
    my ($self,$project) = @_;
    $self->{"project"} = $project;
}

sub setStatusId {
    my ($self,$id) = @_;
    $self->{"status_id"} = $id;
}

1;
