#!/bin/perl

package Table;

sub new
{
	my $class = shift;	
	my $self = {};
	bless( $self, $class );

	$self->{head} = undef;
	$self->{tail} = undef;
	$self->{count} = 0;
	$self->{id} = 0;

	return $self;
}

sub addRecord
{
	my $self = shift;
	my $rec = shift;

	if( !defined( $self->{tail} ) )
	{
		$self->{head} = $rec;
		$self->{tail} = $rec;
	}
	else
	{
		$rec->{right} = $self->{head};
		$self->{head}->{left} = $rec;
		$self->{head} = $rec;
	}

	$self->{count}++;
}

sub write
{
	my $self = shift;
	*OUT = shift;
	my $tail = $self->{tail};

	while( $tail )
	{
		$tail->write( *OUT );
		$tail = $tail->{left};
	}
}

sub addToDB
{
	my $self = shift;	
	my $dbh = shift;
	my $tail = $self->{tail};

	while( $tail )
	{
		$tail->addToDB( $dbh );
		$tail = $tail->{left};
	}
}
1;
