#
# Logical_Fmt.pm
#  Author: Phillip Dressen
#  Date: 2003-07-24
#
#  Rev. 11/11/2015
#  Fix whitespace.
#  - Eric Dattore
#
#   Clean up code - 10/2020
#   J. Scannell
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

package Logical_Fmt;
use Utils;
use EQuery;

sub new
{
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "log_fmt";
    $self->{group} = "general";

    return $self;
}

sub shortDesc
{
    return "Logical Formats";
}

sub longDesc
{
    return "Displays all of the valid logical format types.";
}

sub linkTitle
{
    return "Logical Formats";
}

sub queryForm
{
    my $self = shift;
    my $params = shift;
    return $self->queryResult( $params );
}

sub queryResult
{
    my $self = shift;
    my $params = shift;

    my $title = "Logical Formats";
    my @logical_fmts;

    my $sort = ( defined( $params ) && $params->exists( "sort" ) )? $params->get( "sort" ) : "logical_fmt";

    @logical_fmts = getLogicalFormats( $sort );

    my $result = cr(
        "",
        "<table border=0 cellpadding=5 cellspacing=2 bgcolor=#e9e9e9 width=75%>",
        "<tr>",
        "<td align=center bgcolor=#e9e9e9 id=sort_link>",
        "<a href=supervisor?qform=$self->{identifier}&sort=logical_fmt><b>Id</b></a>",
        "</td>",
        "<td align=center bgcolor=#e9e9e9 id=sort_link>",
        "<a href=supervisor?qform=$self->{identifier}&sort=fmt_name><b>Name</b></a>",
        "</td>",
        "" );

    for( my $x = 0; $x < @logical_fmts; $x += 2) {
        my @l_key = $logical_fmts[$x];
        $result = $result . cr(
            "<tr bgcolor=white>",
            "<td>",
            $logical_fmts[$x],
            "</td>",
            "<td>",
            $logical_fmts[$x + 1],
            "</td>",
            "</tr>",
            "" );

    }

    $result = $result . cr(
        "",
        "</table>",
        "");

    return ( $title, $result );
}

sub getLogicalFormats
{
    my $sort = shift;

    my @fmts;

    my $eq = EQuery->new( "phys_dir_db" );

    $eq->query( "SELECT * FROM logical_format SORT BY $sort" );

    while( (my %row = $eq->getRow()) )
    {
        my @temp = ( $row{logical_fmt}, $row{fmt_name} );
        push( @fmts, @temp );
    }

    return @fmts;
}

1;

