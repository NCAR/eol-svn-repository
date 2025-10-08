#!/bin/perl

package DayRecord;

#----------------------------------------------------
#DayRecord.pm:
#	This module is used to hold data from the DayTable 
#	table in the suldan MySQL database.  It is used with
#	the query* script to query the GOES data on mass store.
#	A DayRecord holds the needed info for one day of one 
#	satellite (G10 or G08) of one resolution (1KM or 4KM).
#
#Subroutines:
#	new - constructs a new DayRecord
#	write - writes the contents of the record to the given
#		file handle - tab delimited	
#	read -  reads the contents of a record from the given file
#		handle - tab delimited
#	setValues - sets the field values using the given array 
#	getHtmlRow - prints the contents of the Record with the
#		needed HTML tags for the query* script
#	getTableHeader - gets the HTML table header
#
#Variables:
#	id - primary key of the table
#	date - the date in YYYYJJJ format
#	satellite - G10 or G08
#	resolution - 1KM or 4KM
#	nfiles - the number of files for this day
#	size - the total size for this day - the sum of
#		all the individual file sizes
#	missing - true/false, whether or not this day could	
#		have missing data
#	left, right - used with the Table.pm module to form
#		a doubly linked list of records		
#----------------------------------------------------

use lib ".";
use Util;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{id} = undef;
	$self->{date} = undef;
	$self->{satellite} = undef;
	$self->{resolution} = undef;
	$self->{nfiles} = undef;
	$self->{size} = undef;
	$self->{path} = undef;
	$self->{missing} = undef;
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

sub read
{
	my $self = shift;
	*INPUT = shift;
	my $line = <INPUT>;

	my @data = split( /\t/, $line );
	$self->{date} = $data[0];
	$self->{satellite} = $data[1];
	$self->{resolution} = $data[2];
	$self->{nfiles} = $data[3];
	$self->{size} = $data[4];
	$self->{path} = $data[5];
}

sub setValues
{
	my $self = $_[0];
	my @data = @$_[1];

	$self->{id} = $data[0];
	$self->{date} = $data[1];
	$self->{satellite} = $data[2];
	$self->{resolution} = $data[3];
	$self->{nfiles} = $data[4];
	$self->{size} = $data[5];
	$self->{path} = $data[6];
}

sub getHtmlRow 
{
	my $self = shift;
	my $color = shift;
	my $link = shift;

	if( !$color ) { $color = "#FFFFFF"; }

	my $str = "<tr bgcolor=$color>\n";
		if( $self->{missing} )
		{
#			$str = $str . "<td bgcolor=#FFFFFF><font color=red size=+1>&bull;</font></td>\n";
#			$str = $str . "<td bgcolor=#FFFFFF><font color=red size=+1>#&8226;</font></td>\n";
			$str = $str . "<td bgcolor=#FFFFFF><b><font color=red size=-2>&gt;&gt;</font></td>\n";
		}
		else
		{
			$str = $str . "<td bgcolor=#FFFFFF>&nbsp;</td>\n";
		}

		if( $link )
		{
			$str = $str . "<td align=center><font size=-1>$link" . getCalendar( $self->{date} ) . "</a></td>\n";
		}
		else
		{
			$str = $str . "<td align=center><font size=-1>" . getCalendar( $self->{date} ) . "</td>\n";
		}
		$str = $str . "<td align=center><font size=-1>$self->{date}</td>\n";
		$str = $str . "<td align=center><font size=-1>$self->{satellite}</td>\n";
		$str = $str . "<td align=center><font size=-1>$self->{resolution}</td>\n";
		$str = $str . "<td align=right><font size=-1>$self->{nfiles}</td>\n";
		$str = $str . "<td align=right><font size=-1>" . commify( $self->{size} ) . "</td>\n";
		$str = $str . "<td align=center><font size=-1>$self->{path}</td>\n";
	$str = $str . "</tr>\n";

	return $str;
}

sub getTableHeader
{
	my $self = shift;

	my $str = "<table border=0 cellpadding=3 cellspacing=3>\n";
	$str = $str . "<tr bgcolor=#6600FF>\n";
		$str = $str . "<td bgcolor=#FFFFFF>&nbsp;</td>\n";
		$str = $str . "<td nowrap align=center><font size=-1 color=white><b>Date</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Jul. Date</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Satellite</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Resolution</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b># Files</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Tot. Size</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Path</b></td>\n";
	$str = $str . "</tr>";

	return $str;
}
1;
