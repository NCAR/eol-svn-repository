#!/bin/perl

package DayRecord;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = undef;
	$self->{satellite} = undef;
	$self->{resolution} = undef;
	$self->{date} = undef;
	$self->{nfiles} = undef;
	$self->{size} = undef;
	$self->{path} = undef;
	$self->{missing} = 0;
	$self->{nxtFile} = undef;	

	$self->{left} = undef;
	$self->{right} = undef;

	return $self;
}

sub write
{
	my $self = shift;
	*OUT = shift;

	print( OUT $self->{date}, "\t" );
	print( OUT $self->{satellite}, "\t" );
	print( OUT $self->{resolution}, "\t" );
	print( OUT $self->{nfiles}, "\t" );
	printf( OUT "%d\t", $self->{size} );
	print( OUT $self->{path}, "\n" );
}

sub addToDB
{
	my $self = shift;
	my $dbh = shift;

	my $sql = "INSERT INTO DayTable ( id, date, satellite, resolution, nfiles, size, path, missing ) " .
						"VALUES ( $self->{id}, $self->{date}, \'$self->{satellite}\', \'$self->{resolution}\', ".
						"$self->{nfiles}, $self->{size}, \'$self->{path}\', $self->{missing} )";

	$dbh->do( $sql ) || die ("doing: ", $dbh->errstr );
}

1;
