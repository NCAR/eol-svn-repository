package TreeNode;

use strict;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{val} = shift;
	$self->{next} = shift;
}

1;
