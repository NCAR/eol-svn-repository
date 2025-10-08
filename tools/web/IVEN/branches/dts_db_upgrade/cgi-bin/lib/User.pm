#! /usr/bin/perl -w

package User;
use strict;
use lib ".";
use Entry;
our @ISA = ("Entry");

sub getEmail {
    my ($self) = @_;
    return defined($self->{"email"}) ? $self->{"email"} : "";
}

sub isActive {
    my ($self) = @_;
    return defined($self->{"active"}) ? $self->{"active"} : 0;
}

sub setActive {
    my ($self,$active) = @_;
    $self->{"active"} = $active;
}

sub setEmail {
    my ($self,$email) = @_;
    $self->{"email"} = $email;
}

1;
