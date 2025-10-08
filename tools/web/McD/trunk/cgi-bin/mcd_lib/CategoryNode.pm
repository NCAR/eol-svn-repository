package CategoryNode;

# --------------------------------------------------------------
# CategoryNode.pm
#  Contains the CategoryNode class which forms the category
#   tree data structure obtained from Category::getTree( $proj ).
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
#
# sub new( $category, $childern_ref ) - Creates a new CategoryNode
#  with the given Category and reference to an array of CategoryNodes.
#   $node->{category} - the Category
#   $node->{childern} - the reference to the array of childern.
#  
# sub inTree( $category ) - Checks to see if the given instance of
#  Category is a descendent of this node. 
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{category} = shift;
	my $ch = shift;

	if( defined( $ch ) )
	{
		$self->{childern} = $ch;
	}	
	else
	{
		$self->{childern} = $ch;
	}

	return $self;
}

sub inTree
{
	my $self = shift;
	my $find = shift;

	if( $self->{childern} )
	{
		foreach my $cat (@{$self->{childern}})
		{
			return 1 if( $cat->{category}->{id} == $find );
	
			return 1 if( $cat->inTree( $find ) );
		}
	}

	return 0;
}
1;
