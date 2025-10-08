package Node;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );
	my $val = shift;

	$self->{val} = $val;
	$self->{next} = undef();
	$self->{prev} = undef();

	return $self;
}

1;
