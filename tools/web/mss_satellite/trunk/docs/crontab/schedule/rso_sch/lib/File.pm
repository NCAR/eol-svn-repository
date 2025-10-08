#!/bin/perl

package File;
use Time;
use QC;

sub new
{
	my $class = shift;
	my $time = shift;
	my $id = shift;
	
	my $self = {};
	bless( $self, $class );
	
	$self->{time} = $time;
	$self->{id} = $id;
	$self->{sector} = undef;
	$self->{qc} = undef;	
	return $self;
}

sub setSector
{
	my $self = shift;
	my $event = shift;
	my $next = shift;
	my $cp_known = 1;

	# check to make sure this event has time to complete
	my $tm = $self->{time}->add( $event->{duration} );
	my $check_diff = Time::minuteDiff( $next->{time}, $tm );

	if( $check_diff < 0 )
	{
		$self->{sector} = 11;
		$self->{qc} = $UNKNOWN;
		return;
	}
	elsif( $check_diff > 5 )
	{
		$cp_known = 0;	
	}	

	my $diff = Time::minuteDiff( $self->{time}, $event->{time} );
	if( $diff == 0 && $cp_known )
	{ $self->{qc} = $ZERO_CP; }
	elsif( $diff == 0 && !$cp_known )
	{ $self->{qc} = $ZERO_UK; }
	elsif( $diff > 0 && $cp_known )
	{ $self->{qc} = $PONE_CP; }
	elsif( $diff > 0 && !$cp_known )
	{ $self->{qc} = $PONE_UK; }
	elsif( $diff < 0 && $cp_known )
	{ $self->{qc} = $MONE_CP; }
	elsif( $diff < 0 && !$cp_known )
	{ $self->{qc} = $MONE_UK; }
	else
	{ $self->{qc} = $OTHER; }
	 
	$self->{sector} = $event->{sector};	
}
1;
