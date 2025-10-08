#!/bin/perl

package Time;

sub new
{
	my $class = shift;
	my $time = shift;

	my $self = {};
	bless( $self, $class );
	
	$self->{hour} = undef;
	$self->{min} = undef;
	$self->{sec} = undef;

	if( defined( $time ) )
	{
		$self->setTime( $time );
	}
	return $self;
}

#setTime( "01:01:59" ) or setTime( hour, min, sec )
sub setTime
{
	my $self = shift;
	my $time = shift;
	my $hour = $time;
	my $min = shift;
	my $sec = shift;
	if( !defined( $min ) )
	{
		@tm = split( /:/, $time );
		$hour = $tm[0];
		$min = $tm[1];
		$sec = $tm[2];
	}

	$self->{hour} = $hour;
	$self->{min} = $min;
	$self->{sec} = $sec;
}

# like: time1 - time2
sub minuteDiff
{
	my $t1 = shift;	
	my $t2 = shift;

	my $m1 = $t1->{hour} * 60 + $t1->{min};
	my $m2 = $t2->{hour} * 60 + $t2->{min};

	return $m1 - $m2;			
}

# returns a new time with the added time: $newtime = $time->add( $addtime )
sub add
{
	my $self = shift;
	my $add = shift;
	my $new = Time->new();

	$new->{hour} = $self->{hour} + $add->{hour};
	$new->{min} = $self->{min} + $add->{min};
	$new->{sec} = $self->{sec} + $add->{sec};

	if( $new->{sec} >= 60 )
	{
		$new->{min}++;
		$new->{sec} -= 60;
	}
	if( $new->{min} >= 60 )
	{
		$new->{hour}++;
		$new->{min} -= 60;
	}
	if( $new->{hour} >=24 )
	{
		$new->{hour} -= 24;
	}
	
	return $new;
}

sub getTime
{
	my $self = shift;
	return( sprintf( "%2.2d", $self->{hour} ) . ":" . sprintf( "%2.2d", $self->{min} ) . ":" . sprintf( "%2.2d", $self->{sec} ) );  
}
1;
