#! /usr/bin/perl -w

package Composite;
use strict;
use lib ".";
use Dataset;
use Product;
our @ISA = ("Dataset","Product");

sub getDatasetType { return "Composite"; }

sub getEditor { return "edit_dataset"; }

sub getStats {
    my ($self,$db) = @_;
    return $db->getCompositeStats($self);
}

sub getTypedId {
    my ($self) = @_;
    return sprintf("cmp%s",$self->getId());
}

sub getViewer { return "dataset_view"; }

sub isGroup { return 0; }

sub loadDatasets {
    my ($self,$db) = @_;
    return $db->getCompositeDatasets($self);
}

1;
