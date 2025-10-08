#! /usr/bin/perl -w

package Entry;
use strict;

sub delete {}

sub getId {
    my ($self) = @_;
    return defined($self->{"id"}) ? $self->{"id"} : "";
}

sub getName {
    my ($self) = @_;
    return defined($self->{"name"}) ? $self->{"name"} : "";
}

sub insert {}

sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = {};
    bless($self,$class);
    return $self;
}

sub setId {
    my ($self,$id) = @_;
    $self->{"id"} = $id;
}

sub setName {
    my ($self,$name) = @_;
    $self->{"name"} = $name;
}

sub update {}

1;
