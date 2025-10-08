#! /usr/bin/perl -w

package Status;
use strict;
use lib ".";
use Entry;
our @ISA = ("Entry");

sub getStyle {
    my ($self) = @_;
    return defined($self->{"style"}) ? $self->{"style"} : "";
}

sub isDone {
    my ($self) = @_;
    return defined($self->{"is_done"}) ? $self->{"is_done"} : 0;
}

sub setIsDone {
    my ($self,$flag) = @_;
    $self->{"is_done"} = $flag;
}

sub setStyle {
    my ($self,$style) = @_;
    $self->{"style"} = $style;
}

1;
