package DatasetListNode;

# --------------------------------------------------------------
# DatasetListNode.pm:
#  Contains the DatasetListNode class which makes up the 
#  datasetList tree returned by the Dataset::getDatasetList method.
#
# Author: Dan Sullivan
# Date: July, 2003
#
# Subroutines:
#
# sub new() - returns a new instance of a DatasetListNode.
#  $node->{category} - an instance of Category
#  $node->{datasets} - a ref to an array of Datasets
#  $node->{childern} - a ref to an array of DatasetListNodes. 
# --------------------------------------------------------------

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	$self->{category} = shift;
	$self->{datasets} = shift;
	$self->{childern} = shift;

	return $self;
}
1;
