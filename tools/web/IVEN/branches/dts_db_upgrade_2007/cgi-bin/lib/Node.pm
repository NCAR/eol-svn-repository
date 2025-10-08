##Module----------------------------------------------------------------------
# <p>Node.pm defines the Node class used by the List class.  Each Node has
#  a value (val), a reference to the (next) node, and a reference to the
#  previous node (prev).</p>
#
# @author Dan Sullivan
##Module----------------------------------------------------------------------

package Node;

##----------------------------------------------------------------------------
# @signature Node new( scalar $val )
# <p>Creates a new Node with value $val.  The next and prev references are
#  set to undefined.</p>
#
# @input $val the value of the node
# @output $self the new node
##----------------------------------------------------------------------------
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
