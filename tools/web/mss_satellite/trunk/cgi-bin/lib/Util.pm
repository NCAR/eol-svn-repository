#!/bin/perl

package Util;

require Exporter;
use vars qw(@ISA @EXPORT $VERSION );

@ISA = qw(Exporter);
@EXPORT = qw( &getCalendar &println &commify %col1 %col2 %sect_refs); 

sub getCalendar
{
	my $jdate = shift;

	$year = substr( $jdate, 0, 4 );
	$day = substr( $jdate, 4, 3 );

	$off = 0;
	if( $year % 4 == 0 )
	{
		$off = 1;
	}

	if( $day <= 31 )
	{
		$month = "01";
		$day = $day;
	}
	elsif( ( $day <= 59 && $day >= 32 ) || ( $day == 60 && $off ) )
	{
		$month = "02";
		$day = $day - 31;
	}
	elsif( $day >= 60 + $off && $day <=90 + $off )
	{
		$month = "03";
		$day = $day - 59 - $off;
	}
	elsif( $day >= 91 + $off && $day <= 120 + $off )
	{
		$month = "04";
		$day = $day - 90 - $off;
	}
	elsif( $day >= 121 + $off && $day <= 151 + $off )
	{
		$month = "05";
		$day = $day - 120  - $off;
	}
	elsif( $day >= (152 + $off) && $day <= 181 + $off )
	{
		$month = "06";
		$day = $day - 151  - $off;
	}
	elsif( $day >= 182 + $off && $day <= 212 + $off )
	{
		$month = "07";
		$day = $day - 181  - $off;
	}
	elsif( $day >= 213 + $off && $day <= 243 + $off )
	{
		$month = "08";
		$day = $day - 212  - $off;
	}
	elsif( $day >= 244 + $off && $day <= 273 + $off )
	{
		$month = "09";
		$day = $day - 243  - $off;
	}
	elsif( $day >= 274 + $off && $day <= 304 + $off )
	{
		$month = "10";
		$day = $day - 273  - $off;
	}
	elsif( $day >= 305 + $off && $day <= 334 + $off )
	{
		$month = "11";
		$day = $day - 304  - $off;
	}
	elsif( $day >= 335 + $off && $day <= 365 + $off )
	{
		$month = "12";
		$day = $day - 334  - $off;
	}

	$dy = $year . "-" . $month . "-" . sprintf( "%2.2d", $day );

	return $dy; 
}

sub println
{
	print( @_, "\n" );
}




sub commify
{
	my $text = reverse( $_[0] );
	
	$text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
	return scalar reverse $text
}

%col1 = ( "G08", "#FFFFFF",
					"G10", "#FFFFFF",
					"G12", "#FFFFFF",
					"G08/RSO", "#FFFFCC",
					"G10/RSO", "#FFFFCC",
					"G12/RSO", "#FFFFCC",
					"G08/SRSO", "#FFCCCC",
					"G10/SRSO", "#FFCCCC",
					"G12/SRSO", "#FFCCCC",
					"NONE", "#FFFFFF");

%col2 = ( "G08", "#EBEBF5",
					"G10", "#EBEBF5",
					"G12", "#EBEBF5",
					"G08/RSO", "#FFFF99",
					"G10/RSO", "#FFFF99",
					"G12/RSO", "#FFFF99",
					"G08/SRSO", "#FF9999",
					"G10/SRSO", "#FF9999",
					"G12/SRSO", "#FF9999",
					"NONE", "#EBEBF5");

%sect_refs;
$sect_refs{"1"} = [6,12,26];
$sect_refs{"2"} = [1];
$sect_refs{"3"} = [3];
$sect_refs{"4"} = [5];
$sect_refs{"5"} = [14];
$sect_refs{"6"} = [2,13,28];
$sect_refs{"7"} = [27];
$sect_refs{"8"} = [4,15,29];
$sect_refs{"9"} = [7,16,21];
$sect_refs{"10"} = [9];
$sect_refs{"11"} = [19,23];
$sect_refs{"12"} = [8,18];
$sect_refs{"13"} = [17,22];
$sect_refs{"14"} = [24];
$sect_refs{"15"} = [10,20,25];

$sect_refs{"16"} = [35,36,40];
$sect_refs{"17"} = [30];
$sect_refs{"18"} = [32];
$sect_refs{"19"} = [34];
$sect_refs{"20"} = [38];
$sect_refs{"21"} = [31,42,37];
$sect_refs{"22"} = [41];
$sect_refs{"23"} = [33,39,43];

1;
