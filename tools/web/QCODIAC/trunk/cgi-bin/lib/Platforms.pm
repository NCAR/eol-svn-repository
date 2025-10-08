#! /usr/bin/perl -w

#
# Platforms.pm
#  Author: Dan Sullivan
#  Date: 2003-06-26
#
#  Rev. 11/11/2015
#  Fix whitespace and remove dead code.
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
#   # Expected Params
#      -sort (optional) sort field by which to sort the table, if none specified
#         default is id.
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Platforms;
use lib ".";
use Utils;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "platforms";
    $self->{group} = "general";

    return $self;
}

sub shortDesc {
    return "Platform Types";
}

sub longDesc {
    return "Displays all of the valid observing platform types.";
}

sub linkTitle {
    return "Platforms";
}

sub queryForm {
    my $self = shift;
    my $params = shift;
    return $self->queryResult( $params );
}

sub queryResult {
    my $self = shift;
    my $params = shift;
    my $title = "Platform Types";

    my $sort = ( defined( $params ) && $params->exists( "sort" ) )? $params->get( "sort" ) : "platform_id";

    my $query = EQuery->new();

    my $sql = "SELECT id as platform_id,platform.* FROM platform ORDER BY \'$sort\'";

    $query->query( $sql );

    my $result = cr("<table border=0 cellpadding=3 cellspacing=2 bgcolor=#e9e9e9 width=$table_width>",
        "<tr>",
        "<td align=center bgcolor=#e9e9e9 id=sort_link>",
        "<a href=supervisor?qform=$self->{identifier}&sort=platrom_id><b>Platform Id</b></a></td>",
        "<td align=center bgcolor=#e9e9e9 id=sort_link>",
        "<a href=supervisor?qform=$self->{identifier}&sort=name><b>Name</b></a></td>",
        "</tr>" );

    while( (my %row = $query->getRow() ) ) {
        $result = $result . cr("<tr>",
            "<td bgcolor=white>$row{platform_id}</td>",
            "<td bgcolor=white nowrap>$row{name}</td>",
            "</tr>" );
    }

    $result = $result . cr( "</table>" );

    return ( $title, $result );
}


1;

