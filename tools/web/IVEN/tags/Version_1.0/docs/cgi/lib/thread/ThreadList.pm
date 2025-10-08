package ThreadList;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{head} = shift;

	return $self;
}

sub addNode
{
	my $self = shift;
	my $node = shift;
	my $head = $self->{head};

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
