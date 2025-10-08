#! /usr/bin/perl -w

package Group;
use strict;
use lib ".";
use Product;
our @ISA = ("Product");

sub delete {
    my ($self,$db) = @_;
    $db->deleteGroup($self);
}

sub getEditor { return "edit_group"; }

sub getNote {
    my ($self) = @_;
    return defined($self->{"note"}) ? $self->{"note"} : "";
}

sub getNoteId {
    my ($self) = @_;
    return defined($self->{"note_id"}) ? $self->{"note_id"} : -1;
}

sub getStats {
    my ($self,$db) = @_;
    return $db->getGroupStats($self);
}

sub getTypedId {
    my ($self) = @_;
    return sprintf("grp%s",$self->getId());
}

sub getViewer { return "group_view"; }

sub insert {
    my ($self,$db) = @_;
    $db->insertGroup($self);
}

sub isGroup { return 1; }

sub loadDatasets {
    my ($self,$db) = @_;
    return $db->getGroupDatasets($self);
}

sub setNote {
    my ($self,$note) = @_;
    $self->{"note"} = $note;
}

sub setNoteId {
    my ($self,$id) = @_;
    $self->{"note_id"} = $id;
}

sub update {
    my ($self,$db) = @_;
    $db->updateGroup($self);
}

1;
