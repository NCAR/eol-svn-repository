#!/bin/perl -w

use lib "lib";
use ProjectDBA;

my $proj = ProjectDBA::buildThreadView( "IHOP_2002" );

print( $proj->{tgroup}->{tcount}, "\n" );

foreach my $list (@{$proj->{tgroup}->{thread_lists}})
{
	print( "Thread: \n" );
	traverse( $list, 0 );
}

sub traverse
{
	my $list = shift;
	my $count = shift;

	if( !$list )
	{
		return;
	}
	for( my $x = 0; $x<$count; $x++ )
	{
		print( "  " );
	}
	print( $list->{pdname}, "\n" );
	traverse( $list->{next}, $count+1 );
}
