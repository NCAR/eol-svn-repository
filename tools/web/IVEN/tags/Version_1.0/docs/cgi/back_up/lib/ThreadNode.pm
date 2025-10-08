package ThreadNode;

sub new
{
	my $class = shift;
	my $self = {};

	bless( $self, $class );

	$self->{pdname} = shift;
	$self->{next} = undef();
	$self->{visits} = 0;
	return $self;
}

# Add the node to this node's tree
sub addNode
{
	my $self = shift;
	my $node = shift;
	my $head = $self;

	if( !$head )
	{
		$self->{head} = $node;
	}
	else
	{
		while( $head->{next} ) 
		{
			$head = $head->{next};
		} 
		$head->{next} = $node;
	}
}

1;
