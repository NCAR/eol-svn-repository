#!/bin/perl -w

use lib ".";
use lib "../lib";
use IvenDB;

use List;

my @lists;
my $c = 0;

my $dbh = IvenDB::connect();

my $sql = "SELECT pdsource, pdname FROM extractions WHERE pjname='IHOP_2002'";

my $sth = $dbh->prepare( $sql );
$sth->execute();

while( (my @row = $sth->fetchrow()) )
{
	$lists[$c++] = List->new( $row[0], $row[1] );
}

for( my $x = 0; $x < $c; $x++ )
{
	my $current = $lists[$x];
	for( my $j = 0; $j < $c; $j++ )
	{
		if( $j != $x )
		{
			if( $current->{head}->{val} eq $lists[$j]->{tail}->{val} )
			{
				$lists[$j]->addToTail( $current->copy() );
				$current->{linked} = 1;
			}	
			elsif( $current->{tail}->{val} eq $lists[$j]->{head}->{val} )
			{
				$lists[$j]->addToHead( $current->copy() );
				$current->{linked} = 1;
			}	
		}
	}

	foreach $list (@lists)
	{
		printList( $list );
	}
	my $x = <STDIN>;
}

foreach $list (@lists)
{
	printList( $list ) if( !$list->{linked} );
}

sub printList
{
	my $list = shift;
	my $head = $list->{head};

	while( $head )
	{
		print( $head->{val} );
		print( "-->" ) if( $head->{next} );
		$head = $head->{next};
	}

	print( "\n" );
}
