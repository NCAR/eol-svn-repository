#! /usr/bin/perl -w

#
# Xlinks.pm
#  Author: Amanda Orin
#  Date: 2012-02-13
#
#  Rev 11/11/2015
#  Remove dead code and fix whitespace
#  - Eric Dattore
#
#   Fix hidden field marking - 10/2020
#   J. Scannell
#
#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# new()
#   * Creates and instance of the Xlinks Tool
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
#   * Expected params:
#      -href - href in database (e.g. catalog.eol.ucar.edu)
#      -purpose - of xlink
#      -title - of xlink (e.g. Bering Sea Integrated Ecosystem Research Program)
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Xlinks;
use Utils;
use EQuery;

# Default fields to display
my @def_fields = ( "xlink_id", "href", "title" );

# The field titles to display at the top of the table
my %field_titles = ("xlink_id" => "ID",
    "href" => "Href",
    "title" => "Title",
    "purpose" => "Purpose",
    "hide" => "Hidden",
    "datasets" => "Datasets?",
    "projects" => "Projects?" );

# The field tags to use in the body of the table
my %field_tags = ( "xlink_id" => "<td align=center nowrap>",
    "href" => "<td align=center>",
    "title" => "<td align=center>",
    "purpose" => "<td align=center nowrap>",
    "hide" => "<td align=center>",
    "datasets" => "<td align=center>",
    "projects" => "<td align=center>" );

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "xlinks";
    $self->{group} = "general";

    return $self;
}

sub shortDesc {
    return "External Links";
}

sub longDesc {
    return cr("<table border=0 cellpadding=0 cellspacing=0><tr><td>",
        "Displays the contents of the xlinks table for one or more projects, output includes: ",
        "<ul><li>Href and Title</li>",
        "<li>Purpose</li>",
        "<li>Other optional fields</li>",
        "</ul></td></tr></table>" );
}

sub linkTitle {
    return "External Links";
}

sub queryForm {
    my $self = shift;

    my $form = cr(
        "<form method=POST action=supervisor>",
        "<table border=0 cellpadding=8 cellspacing=0 bgcolor=white width=$table_width>",
        "<tr>",
        "<td colspan=3>&nbsp;</td>",
        "<tr>",
        "<td colspan=3><b>Enter one of the following fields, can be a regular expression:</td>",
        "</tr>",
        "<tr>",
        "<td width=10% nowrap align=right>",
        "<b>Xlink ID: </b>",
        "</td>",
        "<td colspan=2>",
        "<input type=number min=0 name=xlink_id>",
        "</td>",
        "<tr>",
        "<td width=10% nowrap align=right>",
        "<b>Xlink Href: </b>",
        "</td>",
        "<td colspan=2>",
        "<input type=text size=50 name=href>", &nbsp(4),
        "</tr>",
        "<tr>",
        "<td align=right><b>Xlink Title: </b>",
        "<td colspan=2><input type=text size=30 name=title></td>",
        "</tr>",
        "<tr><td colspan=2>&nbsp;</td></tr>",
        "<tr>",
        "<td colspan=3>",
        &nbsp(5), "<font size=-1><b>To view ALL users/contacts leave all fields blank and click Submit Query.",
        "</td>");

    $form = $form . cr(
        "<tr bgcolor=white>",
        "<td colspan=2>",
        "<b>Select which fields to display:</b>",
        "</td>",
        "</tr>" ,
        "<tr bgcolor=white>",
        "<td align=center colspan=2><font size=-1><b>",
        "<input hidden type=checkbox checked name=fields value=xlink_id>" . &nbsp(3),
        "<input hidden type=checkbox checked name=fields value=href>" . &nbsp(3),
        "<input hidden type=checkbox checked name=fields value=title>" . &nbsp(3),
        "<input type=checkbox checked name=fields value=datasets>Linked to Datasets" . &nbsp(3),
        "<input type=checkbox checked name=fields value=projects>Linked to Projects." . &nbsp(3),
        "<input type=checkbox checked name=fields value=purpose>Purpose" . &nbsp(3),
        "<input type=checkbox name=fields value=hide>Hidden" . &nbsp(3),
        "</td>",
        "</tr>",
        "<tr><td align=right colspan=2>",
        "<input type=hidden name=results value=$self->{identifier}>",
        "<input type=submit value=\"Submit Query\" name=query>",
        "</tr></td>",
        "<tr><td colspan=2>&nbsp;</td></tr>",
        "</table></form>" );


    my $title = "External Links: Query";
    return ( $title, $form );
}

sub queryResult {
    my $self = shift;
    my $params = shift;
    my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
    my $nfields = @fields+3;

    # Instantiate the query object
    my $query = EQuery->new();

    my $xlink_id = $params->exists( "xlink_id" ) ? $params->get( "xlink_id" ) : 0;
    my $href = $params->exists( "href" ) ? $params->get( "href" ) : "";
    my $xlink_title = $params->exists( "title" ) ? $params->get( "title" ) : "";
    my $sort = $params->exists( "sort" )? $params->get( "sort" ) : "xlink.id";

    my $sql = "SELECT xlink.id as xlink_id, xlink.* FROM xlink WHERE 1=1";
    $sql = $sql . " AND xlink.id = ".$xlink_id if( $xlink_id != 0 );
    $sql = $sql . " AND href LIKE ".$query->quote("%".$href."%") if( $href ne "" );
    $sql = $sql . " AND title LIKE ".$query->quote("%".$xlink_title."%") if( $xlink_title ne "" );

    $sql = $sql . " ORDER BY $sort";
    $query->query( $sql );

    my $sort_url = "supervisor?results=$self->{identifier}";
    $sort_url = $sort_url . "&xlink_id=$xlink_id" if( $xlink_id != 0 );
    $sort_url = $sort_url . "&href=". &escape($href) if( $href ne "" );
    $sort_url = $sort_url . "&title=". &escape($xlink_title) if( $xlink_title ne "" );

    foreach my $f (@fields) {
        $sort_url = $sort_url . "&fields=". &escape($f) if ($f ne "");
    }

    # Write the table header
    my $result = cr(
        "<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
        "<tr bgcolor=white>",);
    # Write column name for each field
    foreach my $f (@fields)
    {
        if( defined( $field_titles{$f} ) )
        {
            $result = $result . cr ( "<td align=center bgcolor=#e9e9e9><a href='$sort_url&sort=$f'><b>$field_titles{$f}</b></a></td>" );
        }
    }
    $result = $result . cr(	"</tr>" );

    # Loop through each row
    while( (my %row = $query->getRow() ) )
    {
        # Retrieve the linked datasets and projects
        my $ds_list = getDatasets($row{xlink_id});
        my $prj_list = getProjects($row{xlink_id});

        # Show the record, only the fields user specified
        $result = $result . cr(
            "<tr bgcolor=white>",);
        foreach my $f (@fields)
        {
            if( defined( $field_titles{$f} ) )
            {
                if( $f eq "xlink_id" )
                { $row{$f} = "<a href='https://data.eol.ucar.edu/zinc/xlink/show/$row{$f}'>$row{$f}</a>"; }
                elsif( $f eq "href" )
                { $row{$f} = "<a href='$row{$f}'>$row{$f}</a>"; }
                elsif( $f eq "title" )
                { $row{$f} = $row{$f}; }
                elsif( $f eq "datasets" )
                { $row{$f} = ($ds_list eq "None") ? "<font color=red>$ds_list</font>" : $ds_list; }
                elsif( $f eq "projects" )
                { $row{$f} = ($prj_list eq "None") ? "<font color=red>$prj_list</font>" : $prj_list; }
                elsif( $f eq "purpose" )
                { $row{$f} = $row{$f}; }
                elsif ($f eq "hide" )
                { 
                   my $val = "<font color=blue>N</font>";
                   if ($row{visible} == 0) {
                      $val = "<font color=red>Y</font>";
                   }
                   $row{$f} = $val;
                }
                $result = $result . cr ( "$field_tags{$f}$row{$f}</td>" );
            }
        }

        $result = $result . cr( "</tr>" );
    }

    $result = $result . cr("<tr><td colspan=$nfields><b>External Links Found: ", $query->getRowCount(), "</td></tr>",
        "</table>" );

    my $title = "External Links";
    return ( $title, $result );
}

sub getDatasets {
    my $xlink_id = shift;
    my $query = EQuery->new();
    my $sql = "SELECT dataset.id, dataset.archive_ident FROM dataset,dataset_xlink,xlink ";
    $sql = $sql . "WHERE dataset_xlink.xlink_id=xlink.id ";
    $sql = $sql . "AND dataset_xlink.dataset_id=dataset.id ";
    $sql = $sql . "AND xlink.id = " .$xlink_id. " ";
    $sql = $sql . "ORDER BY dataset.archive_ident";

    $query->query( $sql );

    my $result = $query->getRowCount();
    $result = "None" if $result < 1;

    return $result;
}

sub getProjects {
    my $xlink_id = shift;
    my $query = EQuery->new();
    my $sql = "SELECT project.id, project.name FROM project,project_xlink,xlink ";
    $sql = $sql . "WHERE project_xlink.xlink_id=xlink.id ";
    $sql = $sql . "AND project_xlink.project_id=project.id ";
    $sql = $sql . "AND xlink.id = " .$xlink_id. " ";
    $sql = $sql . "ORDER BY project.name";

    $query->query( $sql );

    my $result = $query->getRowCount();
    $result = "None" if $result < 1;

    return $result;
}

1;

