#!/bin/perl

package FileRecord;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{day_id} = undef;
	$self->{date} = undef;
	$self->{time} = undef;
	$self->{size} = undef;
	$self->{path} = undef;
	$self->{left} = undef;
	$self->{right} = undef;

	return $self;	
}

sub write
{
	my $self = shift;	
	*OUT = shift;

	print( OUT $self->{date}, "\t" );
	print( OUT $self->{time}, "\t" );
	printf( OUT "%d\t", $self->{size} );
	print( OUT $self->{path}, "\n" );
}

sub addToDB
{
	my $self = shift;
	my $dbh = shift;

	my $sql = "INSERT INTO FileTable (day_id, date, time, size, path, sector, sector_qc, id) " . 
						"VALUES ( $self->{day_id}, $self->{date}, $self->{time}, $self->{size}, \'$self->{path}\', 11, 6, 0 )";

	$dbh->do( $sql ) || die( "doing: ", $dbh->errstr );
}
1;
