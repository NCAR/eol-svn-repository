#!/bin/perl

package AllRecord;

#----------------------------------------------------
#
#Subroutines:
#	new - constructs a new AllRecord
#	getHtmlRow - prints the contents of the Record with the
#		needed HTML tags for the query* script
#	getTableHeader - gets the HTML table header
#
#----------------------------------------------------

use lib ".";
use Util;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{date} = undef;
	$self->{satellite} = undef;
	$self->{resolution} = undef;
	$self->{time} = undef;
	$self->{size} = undef;
	$self->{path} = undef;
	$self->{sector} = undef;
	$self->{left} = undef;
	$self->{right} = undef;

	return $self;
}

sub getHtmlRow 
{
	my $self = shift;
	my $color = shift;
	my $link = shift;

	if( !$color ) { $color = "#FFFFFF"; }

	my $str = "<tr bgcolor=$color>\n";
		$str = $str . "<td align=center><font size=-1>" . getCalendar( $self->{date} ) . "</td>\n";
		$str = $str . "<td align=center><font size=-1>$self->{date}</td>\n";
		$str = $str . "<td align=center><font size=-1>$self->{satellite}</td>\n";
		$str = $str . "<td align=center><font size=-1>$self->{resolution}</td>\n";
		$str = $str . "<td align=center><font size=-1>" . substr( $self->{time}, 0, 2 ) . ":" . substr( $self->{time}, 2, 2) . "</td>\n";
		$str = $str . "<td align=right><font size=-1>" . commify( $self->{size} ) . "</td>\n";

		#$str = $str . "<td align=center><font size=-1>$self->{sector}</td>\n";
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
	$str = $str . "</tr>\n";

	return $str;
}

sub getTableHeader
{
	my $self = shift;

	my $str = "<table border=0 cellpadding=3 cellspacing=3>\n";
	$str = $str . "<tr bgcolor=#6600FF>\n";
		$str = $str . "<td nowrap align=center><font size=-1 color=white><b>Date</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Jul. Date</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Satellite</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Resolution</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Time</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Size</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Sector</b></td>\n";
		$str = $str . "<td nowrap><font size=-1 color=white><b>Path</b></td>\n";
	$str = $str . "</tr>";

	return $str;
}
1;
