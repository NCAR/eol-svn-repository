#!/bin/perl

use Benchmark qw(cmpthese timethis timediff timestr);
my $x = 3;

my $t1 = timethis( 0, a );
my $t2 = timethis( 0, b );
#cmpthese( 0, { a => a, b => b } );
print( timestr( timediff($t1, $t2) ) );
sub a 
{
	my $y = $x*$x*$x;
}

sub b
{
	my $y = $x**3;
}
