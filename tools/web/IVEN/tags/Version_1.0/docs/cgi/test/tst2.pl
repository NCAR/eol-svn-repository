#!/bin/perl

use lib ".";
use LTime;

#---------------------
my @b;
my $bcount = 0;

LTime::restart();
for( my $x = 0; $x < 10000; $x++ )
{
	$b[$bcount++] = $x;
}
LTime::log( "Using counter" );
#---------------------

#---------------------
my @a;
LTime::restart();
for( my $x = 0; $x < 10000; $x++ )
{
	$a[scalar($a)] = $x;
}
LTime::log( "Using scalar" );
#---------------------

