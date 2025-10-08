#!/bin/perl

package Day;

use lib ".";
use File;
use Time;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{files} = [];
	$self->{nxtFile} = undef;
	$self->{count} = 0;
	$self->{day} = undef;
	$self->{sat} = undef;
	$self->{res} = undef;

	return $self;
}

sub queryDB
{
	my $self = Day->new();
	my $dbh = shift;
	$self->{day} = shift;
	$self->{sat} = shift;
	$self->{res} = shift;
	$self->{files} = undef;
	$self->{count} = 0;
	$self->{nxtFile} = undef;

	my $sql = "SELECT FileTable.time, FileTable.id FROM FileTable, DayTable WHERE FileTable.day_id=DayTable.id and " .
						"DayTable.satellite=\"$self->{sat}\" and DayTable.resolution=\"$self->{res}\" and DayTable.date=$self->{day} ORDER BY FileTable.time";

	my $sth = $dbh->prepare( $sql );

	$sth->execute();
	
	my @row = $sth->fetchrow();
	if( !@row )
	{
		$sth->finish();
		return undef;
	}

	do
	{
		$self->{files}[$self->{count}] = File->new( Time->new( convertTime( $row[0] ) ), $row[1] );
		$self->{count}++;
	}
	while( (@row=$sth->fetchrow()) );
	
	$sth->finish();
	my $next = $self->{day} + 1;

	$sql = "SELECT MIN( FileTable.time ) FROM FileTable, DayTable WHERE FileTable.day_id=DayTable.id and " .
					"DayTable.satellite=\"$self->{sat}\" and DayTable.resolution=\"$self->{res}\" and DayTable.date=$next";

	$sth = $dbh->prepare( $sql );
	$sth->execute();

	@row = $sth->fetchrow();

	if( defined( $row[0] ) )
	{
		$self->{nxtFile} = File->new( Time->new( convertTime( $row[0] ) ), undef );	
	}
	else
	{
		$self->{nxtFile} = undef;
	}

	$sth->finish();

	return $self;
}

sub convertTime
{
	my $time = shift;
	my $hour = int($time / 100);
	my $min = $time - int($hour * 100);
	
	return( sprintf( "%2.2d", $hour) . ":" . sprintf( "%2.2d", $min ) . ":00\n" ); 
}

1;
