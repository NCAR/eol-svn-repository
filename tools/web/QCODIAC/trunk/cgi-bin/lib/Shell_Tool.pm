#
# Shell_Tool.pm
#  Author: Dan Sullivan
#  Date: 2003-06-23
#
#  Rev. 11/11/2015
#  Fix whitespace.
#  - Eric Dattore
#

#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# new()
#   * Creates and instance of the On_Line_Phys_Listing Tool
#
# shortDesc()
#   * Returns simple text title/description of the tool
#
# longDesc()
#   * Returns a long text description of the function of this tool
#
# linkTitle()
#   * Returns a smaller title to display in the left frame
#
# (my $title, my $form) = queryForm()
#   * Returns the tool title and the text for an HTML form
#
# (my $title, my $result ) = queryResult( $url_params )
#   * Input URLparams Instance
#     Output Queried Result, and a Title
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Shell_Tool;
use Utils;
use EQuery;

sub new
{
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "shell";
    $self->{group} = "project";

    return $self;
}

sub shortDesc
{
    return "Testing Tool";
}

sub longDesc
{
    return "This tool does blah...";
}

sub linkTitle
{
    return "Shell Tool";
}

sub queryForm
{
    my $title = "Shell Tool Title";

    my $form = "HTML STUFF AND A QUERY FORM HERE";

    return ( $title, $form );
}

sub queryResult
{
    my $params = $_[1];

    my $title = "Shell Tool Title";

    my $result = "HTML STUFF AND QUERY RESULT HERE";

    return ( $title, $result );
}



1;

