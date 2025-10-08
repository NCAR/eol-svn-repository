#!/bin/perl

package FileRecord;

use Time;
use QC;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{day_id} = undef;
	$self->{date} = undef;
	$self->{time} = undef;
	$self->{out_time} = undef;
	$self->{size} = undef;
	$self->{path} = undef;
	$self->{sector} = undef;
	$self->{qc} = undef;
	$self->{left} = undef;
	$self->{right} = undef;

	return $self;	
}

sub write
{
	my $self = shift;	
	*OUT = shift;

	print( OUT $self->{date}, "\t" );
	print( OUT $self->{out_time}, "\t" );
	printf( OUT "%d\t", $self->{size} );
	print( OUT $self->{sector}, "\t" );
	print( OUT $self->{qc}, "\t" );
	print( OUT $self->{path}, "\n" );
}

sub addToDB
{
	my $self = shift;
	my $dbh = shift;

	my $sql = "INSERT INTO FileTable (day_id, date, time, size, path, sector, sector_qc, id) " . 
						"VALUES ( $self->{day_id}, $self->{date}, $self->{out_time}, $self->{size}, \'$self->{path}\', $self->{sector}, $self->{qc}, 0 )";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
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
