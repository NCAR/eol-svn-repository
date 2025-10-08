#!/bin/perl

package Event;

# new( id, time, sector_id, sector_abrv, duration )
sub new
{
	my $class = shift;
	my $self = {};

	bless( $self, $class );

	$self->{id} = shift;
	$self->{time} = shift;
	$self->{sector} = shift;
	$self->{abrv} = shift;
	$self->{duration} = shift;

	return $self;	
}
1;
