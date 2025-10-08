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

sub setThreadList
{
	my $self = shift;
	my $nodes = shift;
	my @nodes = @$nodes;
	$self->{thread_lists} = [];
	$self->{tcount} = 0;
	my $continue = 1;

	#foreach my $node (@nodes)
	while( $continue )
	{
		$node = shift( @nodes );
			
		#$self->addThreadList( $node );

		if( !$node )
		{
			$continue = 0;
		}
		elsif( $self->{tcount} == 0 )
		{
			$self->{thread_lists}->[$self->{tcount}] = $node;
			$self->{tcount}++;
		}
		elsif( $node->{visits} == 3 )
		{
			$node->{visits} = 0;
			foreach my $n2 (@nodes)
			{
				$n2->{visits} = 0;
			}

			$self->{thread_lists}->[$self->{tcount}] = $node;
			$self->{tcount}++;
		}
		else
		{
			my $tmp = $self->{thread_lists}->[$self->{tcount}-1];  
			if( $tmp->{pdname} eq $node->{next}->{pdname} )
			{
				$node->{next} = $tmp;
				$self->{thread_lists}->[$self->{tcount}-1] = $node;  
			}
			else
			{
				my $head = combineLists( $node, $self->{thread_lists}->[$self->{tcount}-1] );  
				if( $head )
				{
					$self->{thread_lists}->[$self->{tcount}-1] = $head;
				}
				else
				{
					$node->{visits}++;

					push( @nodes, $node ); 
					#$nodes[scalar(@nodes)] = $node;
				}
			}
		}
	}
}

sub addThreadList
{
	my $self = shift;
	my $node = shift;

	if( $self->{tcount} == 0 )
	{
		$self->{thread_lists}->[$self->{tcount}] = $node;
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

	if( !defined( $list ) )
	{
		$rlist =  undef();
	}	
	elsif( $list->{next} && $list->{next}->{pdname} eq $new_list->{pdname} )
	{
		my $tmp_node = ThreadNode->new( $new_list->{pdname} );
		my $tmp_node2 = ThreadNode->new( $new_list->{next}->{pdname} );
		$tmp_node->addNode( $tmp_node2 );

		$tmp_node->{next}->{next} = $list->{next}->{next};
		$list->{next} = $tmp_node;
	
		$rlist =  $list;
	}
	#elsif( $new_list->{next}->{pdname} eq $list->{pdname} )
	#{
	#	$rlist = ThreadNode->new( $new_list->{pdname} );
	#	$rlist->{next} = $list;
	#}
	else
	{
		if( !$list->{next} )
		{
			$list = undef();
		}
		else
		{
			$rlist = combineLists( $new_list, $list->{next}->{next} );
			if( $rlist )
			{
				$list->{next}->{next} = $rlist;
				$rlist = $list;
			}
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
