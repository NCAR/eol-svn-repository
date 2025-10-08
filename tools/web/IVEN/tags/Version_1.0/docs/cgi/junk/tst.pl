#!/bin/perl

use HTML::Entities;
my $str = "dan's > sign";
my $s = HTML::Entities::encode( $str );
print( $s, "\n\n" );

my $link_source = "/ork/IHOP";
my $path = "/work/IHOP/GWMD5";

if( $path =~ /^$link_source/ )
{
	print( "yes: ", $path, "\n" );
}
else
{
	print( "no: ", $path, "\n" );
}
