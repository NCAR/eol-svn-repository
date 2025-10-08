#! /usr/bin/perl -w

package Project;
use strict;
use lib ".";
use Entry;
our @ISA = ("Entry");

sub addProduct {
    my ($self,$product) = @_;
    push(@{ $self->{"products"}},$product);
}

sub getBeginDate {
    my ($self) = @_;
    return defined($self->{"begin_date"}) ? $self->{"begin_date"} : "";
}

sub getChargeNumber {
    my ($self) = @_;
    return defined($self->{"charge"}) ? $self->{"charge"} : 0;
}

sub getEndDate {
    my ($self) = @_;
    return defined($self->{"end_date"}) ? $self->{"end_date"} : "";
}

sub getMaxLatitude {
    my ($self) = @_;
    return defined($self->{"maxlat"}) ? $self->{"maxlat"} : 90;
}

sub getMaxLongitude {
    my ($self) = @_;
    return defined($self->{"maxlon"}) ? $self->{"maxlon"} : 180;
}

sub getMinLatitude {
    my ($self) = @_;
    return defined($self->{"minlat"}) ? $self->{"minlat"} : -90;
}

sub getMinLongitude {
    my ($self) = @_;
    return defined($self->{"minlon"}) ? $self->{"minlon"} : -180;
}

sub getNote {
    my ($self) = @_;
    return defined($self->{"note"}) ? $self->{"note"} : "";
}

sub getNoteId {
    my ($self) = @_;
    return defined($self->{"note_id"}) ? $self->{"note_id"} : -1;
}

sub getPrefix {
    my ($self) = @_;
    return defined($self->{"prefix"}) ? $self->{"prefix"} : "XX";
}

sub getProducts {
    my ($self) = @_;
    return defined($self->{"products"}) ? @{ $self->{"products"}} : ();
}

sub setBeginDate {
    my ($self,$date) = @_;
    $self->{"begin_date"} = (split(' ',$date))[0];
}

sub setChargeNumber {
    my ($self,$num) = @_;
    $self->{"charge"} = $num;
}

sub setEndDate {
    my ($self,$date) = @_;
    $self->{"end_date"} = (split(' ',$date))[0];
}

sub setMaxLatitude {
    my ($self,$lat) = @_;
    $self->{"maxlat"} = $lat;
}

sub setMaxLongitude {
    my ($self,$lon) = @_;
    $self->{"maxlon"} = $lon;
}

sub setMinLatitude {
    my ($self,$lat) = @_;
    $self->{"minlat"} = $lat;
}

sub setMinLongitude {
    my ($self,$lon) = @_;
    $self->{"minlon"} = $lon;
}

sub setNote {
    my ($self,$note) = @_;
    $self->{"note"} = $note;
}

sub setNoteId {
    my ($self,$id) = @_;
    $self->{"note_id"} = $id;
}

sub setPrefix {
    my ($self,$prefix) = @_;
    $self->{"prefix"} = $prefix;
}

sub update {
    my ($self,$db) = @_;
    $db->updateProject($self);
}

1;
