package List;

use lib ".";
use Node;

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );
	my $head = shift;
	my $tail = shift;

	$self->{head} = Node->new( $head );
	$self->{tail} = Node->new( $tail );

	$self->{head}->{next} = $self->{tail};
	$self->{tail}->{prev} = $self->{head};

	$self->{linked} = 0;

	return $self;
}

# Assume $self->{head} == $list->{tail};
sub addToHead
{
	my $self = shift;
	my $list = shift;
	
	$list->{tail} = $list->{tail}->{prev};
	$list->{tail}->{next} = $self->{head};
	$self->{head}->{prev} = $list->{tail};

	$self->{head} = $list->{head};
}

sub addToTail
{
	my $self = shift;
	my $list = shift;

	$self->{tail} = $self->{tail}->{prev};
	$self->{tail}->{next} = $list->{head};
	$list->{head}->{prev} = $self->{tail};
	
}

sub copy
{
	my $self = shift;
	my $copy = List->new( $self->{head}->{val}, $self->{tail}->{val} );

	my $head = $self->{head};
	my $chead = Node->new( $head->{val} );
	my $ctail = $chead;
	$head = $head->{next};
	while( $head )
	{
		my $new = Node->new( $head->{val} );
		$new->{prev} = $ctail;
		$ctail->{next} = $new;
		
		$ctail = $new;
		$head = $head->{next};
	}

	$copy->{head} = $chead;
	$copy->{tail} = $ctail;
	return $copy;	
}
1;
