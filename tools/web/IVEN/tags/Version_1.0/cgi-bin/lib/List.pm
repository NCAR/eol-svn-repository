##Module----------------------------------------------------------------------
#
# The List class maintains a doubly linked list of Nodes (see the Node.pm)
#  module.  This class is used to create the dependency threads in the 
#  dependencies view (thread_view).
#  
# @author Dan Sullivan
##Module----------------------------------------------------------------------

package List;

use lib ".";
use Node;

##----------------------------------------------------------------------------
# @signature List new( scalar $head, scalar $tail )
# <p>Creates a new instance of the List class.  The provided $head and $tail
#  values are used to create the first two nodes in the list.  The $head and
#  $tail must be scalars, but can be references to other objects.  
#
# @input $head the value of the head node
# @input $tail the value of the tail node
# @output $self the new List
##----------------------------------------------------------------------------
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

##----------------------------------------------------------------------------
# @signature void addToHead( List $list )
# <p>Adds the given List to the head of this List. This is a bit tricky it
#  seems for some reason the function assumes <code>$self->{head} == $list->{tail}.</code>
# </p> 
#
# @input $list the list to add to the head
##----------------------------------------------------------------------------
sub addToHead
{
	my $self = shift;
	my $list = shift;
	
	$list->{tail} = $list->{tail}->{prev};
	$list->{tail}->{next} = $self->{head};
	$self->{head}->{prev} = $list->{tail};

	$self->{head} = $list->{head};
}

##----------------------------------------------------------------------------
# @signature void addToTail( List $list )
# <p>Adds the given List to the tail of this List. 
# </p> 
#
# @input $list the list to add to the tail
##----------------------------------------------------------------------------
sub addToTail
{
	my $self = shift;
	my $list = shift;

	$self->{tail} = $self->{tail}->{prev};
	$self->{tail}->{next} = $list->{head};
	$list->{head}->{prev} = $self->{tail};
	
}

##----------------------------------------------------------------------------
# @signature List copy()
# <p>Makes a copy of this list.</p>
#
# @output $list a copy of this list
##----------------------------------------------------------------------------
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
