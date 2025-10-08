#!/bin/perl

package FileRecord;

#----------------------------------------------------
#FileRecord.pm:
#	This module holds the data for the FileTable table in
#	the suldan MySQL database.  It is used with the query*
#	script to query the GOES data on mass store.
#
#Subroutines:
#	new - construct a new FileRecord
#	write - write the FileRecord to the given file handle,
#		tab delimited
#	read - read the FileRecord from the given file handle,
#		tab delimited
#	setValues - set the values of the FileRecord using
#		the given array
#	getHtmlRow - format the FileRecord with the needed HTML
#		tags for the query* script
#	getTableHeader - get the HTML table header for the 
#		query* script
#
#Variables:
#	day_id - id number that corresponds to this file's
#		DayRecord
#	date - the date in YYYYJJJ
#	time - the time of scan in HHMM format
#	size - the size of the file
#	path - the path of the file on mass store
#	left, right - used with Table.pm to create a 	
#		doubly linked list of records		
#----------------------------------------------------
use lib ".";
use Util;

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
	$self->{sector} = undef;
	$self->{sector_qc} = undef;
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

sub read
{
	my $self = shift;
	*INPUT = shift;

	my $line = <INPUT>;
	my @data = split( /\t/, $line );

	$self->{date} = $data[0];
	$self->{time} = $data[1];
	$self->{size} = $data[2];
	$self->{path} = $data[3];
}

sub setValues
{
	my $self = $_[0];
	my @data = @$_[1];

	$self->{day_id} = $data[0];
	$self->{date} = $data[1];
	$self->{time} = $data[2];
	$self->{size} = $data[3];
	$self->{path} = $data[4];	
}

sub getHtmlRow 
{
	my $self = shift;
	my $color = shift;

	if( !$color ) { $color = "#FFFFFF"; }

	my $str = "<tr bgcolor=$color>\n";
		$str = $str . "<td align=center><font size=-1>$self->{date}</td>\n";
		$str = $str . "<td align=center><font size=-1>" . substr( $self->{time}, 0, 2 ) . ":" . substr( $self->{time}, 2, 2) . "</td>\n";
		$str = $str . "<td align=right><font size=-1>" . commify( $self->{size} ) . "</td>\n";
		if( $self->{sector} )
		{
			my $tcol = "black";

			if( $self->{sector} eq "unknown" )
			{
				$tcol = "red";
			}			
			$str = $str . "<td align=center><font size=-1 color=$tcol>$self->{sector}</td>\n";
		}
		else
		{
			$str = $str . "<td align=center><font size=-1>unknown</td>\n";
		}
		$str = $str . "<td align=center><font size=-1>$self->{path}</td>\n";
		my $qc = $self->{sector_qc};
		if( $qc >= 3 && qc <= 5 )
		{ $str = $str . "<td align=center bgcolor=white><font size=-1 color=red><b>*</b>\n"; }
	$str = $str . "</tr>\n";

	return $str;
}

sub getTableHeader
{
	my $self = shift;

	my $str = "<table border=0 cellpadding=3 cellspacing=3>\n";
	$str = $str . "<tr bgcolor=#6600FF>\n";
		$str = $str . "<td nowrap align=center><font size=-1 color=white><b>Date</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Time</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Size</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Sector</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Path</b></td>\n";
	$str = $str . "</tr>";

	return $str;
}
1;
