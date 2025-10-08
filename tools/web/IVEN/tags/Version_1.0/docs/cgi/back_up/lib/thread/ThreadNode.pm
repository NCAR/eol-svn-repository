package ThreadNode;

sub new
{
	my $class = shift;
	my $self = {};

	bless( $self, $class );

	$self->{pdname} = shift;
	$self->{next} = undef();

	return $self;
}

1;
