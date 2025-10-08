#!/bin/perl

package Schedule;

sub new
{
	my $class = shift;

	my $self = {};
	bless( $self, $class );

	$self->{events} = [];
	$self->{count} = 0;

	return $self;
}

sub addEvent
{
	my $self = shift;
	$self->{events}[$self->{count}] = shift;
	$self->{count}++;
}
1;

