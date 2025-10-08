package TreeLevel;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{nodes} = [];
	$self->{node_count} = 0;	

	return $self;
}

sub addNode
{
	my $self = shift;
	my $node = shift;

	$self->{nodes}->[$self->{node_count}++] = $node;
}
1;
