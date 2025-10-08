#!/bin/perl

open( IN, "<goes08_srso.htm" );
open( OUT, ">goes08_srso2.htm" );

while( !eof( IN ) )
{
	my $line1 = <IN>;
	my $line2 = <IN>;
	my $line3 = <IN>;
	chop( $line2 );
	$line3 = trim( $line3 );

	print( OUT $line1 );
	$line2 = $line2 . $line3;
	print( OUT $line2, "\n" );
}

sub trim
{
	my $str = $_[0];

	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	return $str;
}
