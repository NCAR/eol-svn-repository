package State;

# --------------------------------------------------------------
# State.pm:
#  Contains the State class which helps maintain the current 
#   state the editor is in.  Contains subroutines to automatically
#   read in the current state and write the current state.
#
# Current 'states': currently, the  pages (scripts) the 
#  user would need to be returned to are:
#   list_table - alpha list of datasets
#   category_table  - datasets by category
# The following variables are used:
#   "list_table_proj" - the project id to display in the list_table
#   "category_table_proj" - the project id to display in the 
#      category_table
#   "category_table_cat" - the category id to dispaly in the 
#      category_table.
#   list_table_proj or category_table_proj must be defined, this
#   dictates which page to send the user to.  If category_table_proj
#   is defined category_table_cat can also be defined but this
#   is optional.
#
# Author: Dan Sullivan
# Date: July, 2003
#                      -  -  -  -  - 
# Subroutines:
# 
# sub CATEGORY_TABLE - static method, returns the constant 
#   corresponding to the category_table script.
#
# sub LIST_TABLE - static method, returns the constnat
#   corresponding to the list_table script.
#
# sub new() - returns a new instance of the State class
#
# sub getUrlParams() - returns a string of the needed url params
#  that correspond to the current state, example;
#   category_table_proj=2&category_table_cat=3
#
# sub getUrl() - returns a url that points to the correct script
#  and includes the needed params
#
# sub getHidden() - returns a string containing all of the needed
#   hidden input tags needed for an html form, example:
#    <input type=hidden name=category_table_proj value=2>
#
# sub setFromParams() - populates this State by reading in the
#  needed values from the passed params - either from a form or
#  a url.
# --------------------------------------------------------------

use CGI qw(:standard :html3 );
my $LIST_TABLE = 99;
my $CATEGORY_TABLE = 98;

sub CATEGORY_TABLE
{
	return $CATEGORY_TABLE;
}

sub LIST_TABLE
{
	return $LIST_TABLE
}

sub new
{
	my $class = shift;
	my $self = {};
	bless( $self, $class );

	my $type = shift;

	$self->{vals} = {};

	if( defined( $type ) )	
	{
		if( $type == $LIST_TABLE )
		{
			$self->{vals}->{list_table_proj} = shift;
		}
		elsif( $type = $CATEGORY_TABLE )
		{
			$self->{vals}->{category_table_proj} = shift;
			my $cat = shift;
			$self->{vals}->{category_table_cat} = $cat if( defined( $cat ) && $cat > 0 );
		}
	}

	return $self;	
}

sub getUrlParams
{
	my $self = shift;
	my $url = "";

	foreach $p (keys %{ $self->{vals} })
	{
		$url = $url . "&" . "$p=$self->{vals}->{$p}";  
	}

	return $url;
}

sub getUrl
{
	my $self = shift;
	my $url = "";
	my %vals = %{ $self->{vals} };

	if( exists( $vals{list_table_proj} ) )
	{
		$url = "list_table?project=$vals{list_table_proj}";
		$url = $url . "&sort=$vals{list_table_sort}" if( exists( $vals{list_table_sort} ) );
	}
	elsif( exists( $vals{category_table_proj} ) )
	{
		$url = "category_table?project=$vals{category_table_proj}";
		$url = $url . "&category=$vals{category_table_cat}" if( exists( $vals{category_table_cat} ) ); 
	}

	return $url;
}

sub getHidden
{
	my $self = shift;
	my $hide = "";

	foreach $p (keys %{ $self->{vals} })
	{
		$hide = $hide . "<input type=hidden name=\"$p\" value=\"$self->{vals}->{$p}\">\n";
	}

	return $hide;
}

sub setFromParams
{
	my $self = shift;

	foreach my $p ("list_table_proj", "category_table_proj", "category_table_cat" )
	{
		my $in = param( $p );
		$self->{vals}->{$p} = $in if( defined( $in ) );
	}
}
1;
