#!/bin/perl

package Table;

sub new
{
	my $class = shift;	
	my $self = {};
	bless( $self, $class );

	$self->{array} = ();
	$self->{count} = 0;
	$self->{id} = 0;

	return $self;
}

sub addRecord
{
	my $self = shift;
	my $rec = shift;
	$self->{array}[$self->{count}++] = $rec;
}

sub write
{
	my $self = shift;
	*OUT = shift;
	my @arr = @{$self->{array}};

	foreach my $rec (@arr)
	{
		$rec->write( *OUT );
	}
}

sub addToDB
{
	my $self = shift;	
	my $dbh = shift;

	my @arr = @{$self->{array}};

	foreach my $rec (@arr)
	{
		$rec->addToDB( $dbh );
	}
}
1;
