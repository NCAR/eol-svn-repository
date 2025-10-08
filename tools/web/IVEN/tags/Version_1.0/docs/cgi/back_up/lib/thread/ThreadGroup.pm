package ThreadGroup;

use lib ".";
use ThreadNode;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{thread_lists} = [];
	$self->{tcount} = 0;

	return $self;
}

sub addThreadList
{
	my $self = shift;
	my $tlist = shift;

	if( $self->{tcount} == 0 )
	{
		$self->{thread_lists}->[$self->{tcount}] = $tlist;
		$self->{tcount}++;
	}
	else
	{
		my $success = 0;
		for( my $x = 0; $x < $self->{tcount}; $x++ )
		{
			my $head = combineLists( $tlist->{head}, $self->{thread_lists}->[$x]->{head} );
			if( $head )
			{
				#$self->{thread_lists}->[$x] = ThreadList->new( $head ); 
				$self->{thread_lists}->[$x]->{head} = $head; 
				$success = 1;
			}
		}
		if( !$success )
		{
print( "adding new list...\n" );
			$self->{thread_lists}->[$self->{tcount}] = $tlist;
			$self->{tcount}++;
		}

		for( my $x = 0; $x < $self->{tcount}; $x++ )
		{
			print( "List $x: \n" );
			traverse( $self->{thread_lists}->[$x]->{head}, 0 );
		}
	}
}

sub combineLists
{
	my $new_list = shift;
	my $list = shift;

	my $rlist = undef();

#print( "l: ", $list->{pdname}, "\n" ) if( $list );
#print( "n: ", $new_list->{pdname}, "\n" );
	if( !defined( $list ) )
	{
		$rlist =  undef();
	}	
	elsif( $list->{next} && $list->{next}->{pdname} eq $new_list->{pdname} )
	{
print( "node2 = node2 case:\n" );
		my $tmp_node = ThreadNode->new( $new_list->{pdname} );
		my $tmp_node2 = ThreadNode->new( $new_list->{next}->{pdname} );
		my $tmp_list = ThreadList->new( $tmp_node );
		$tmp_list->addNode( $tmp_node2 ); 	
		my $tmp_head = $tmp_list->{head};
		$tmp_head->{next}->{next} = $list->{next}->{next};
		$list->{next} = $tmp_head;
	
		#my $nlnext = $new_list->{next};
		#$new_list->{next}->{next} = $list->{next};
		#$list->{next} = $new_list;	
print( "  $list->{pdname}  $list->{next}->{pdname}  $list->{next}->{next}->{pdname} \n" );
		$rlist =  $list;
	}
	elsif( $new_list->{next}->{pdname} eq $list->{pdname} )
	{
print( "node1 = node1 case:\n" );
		$rlist = ThreadNode->new( $new_list->{pdname} );
		$rlist->{next} = $list;
		#$new_list->{next}->{next} = $list->{next};
		#$rlist =  $new_list;
print( "  $rlist->{pdname}  $rlist->{next}->{pdname}  $rlist->{next}->{next}->{pdname} \n" );
	}
	else
	{
print( "going next..\n" );
		$rlist = combineLists( $new_list, $list->{next} );
		if( $rlist )
		{
print( " ---rlist: $list->{pdname}\n" );
			$list->{next} = $rlist;
			return $list;
			#return $rlist;
			#$rlist = $list;
		}
		else
		{
			#return $rlist;
		}
	}

	return $rlist;
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
1;
