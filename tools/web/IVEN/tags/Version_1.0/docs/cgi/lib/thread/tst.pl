#!/bin/perl -w

# I don't know!!!!

use lib "lib";
use ProjectDBA;

my $proj = ProjectDBA::buildThreadView( "IHOP_2002" );
my @heads;
#print( $proj->{tgroup}->{tcount}, "\n" );

foreach my $list (@{$proj->{threads}})
{
	print( "Thread: \n" );
	traverse( $list->{head}, 0 );
	$heads[scalar(@heads)] = $list->{head};
}

my $root = buildTree( @heads );

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
	print( $list->{val}, "\n" );
	traverse( $list->{next}, $count+1 );
}

sub buildTree
{
	my @lists = @_;
	my @heads;


	my $root = TreeLevel->new();
	$root->addNode( TreeNode->new( $lists[0]->{val}, undef() ) );
	$this_level[0] = $lists[0]->{next};	
	
	my $size = @lists;

	for( int x= 1; $x < $size; $x++ )
	{
		if( $lists[$x]->{head}->{val}	
	}

	foreach my $head (@lists)
	{
		my $found = 0;
		foreach my $h (@heads)
		{
			foreach my $val (@$h)
			{
				if( $val eq $head->{val} )
				{
					$found = 1;
					$h->[scalar(@$h)] = $head;
				}
			}
		}

		if( !$found )
		{
			my $s = scalar(@heads)];
			$heads[$s] = [];
			$heads[$s]->[0] = $head;
		}
	}

}
