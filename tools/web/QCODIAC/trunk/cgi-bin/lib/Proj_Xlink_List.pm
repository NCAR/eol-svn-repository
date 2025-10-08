#! /usr/bin/perl -w

#
# Proj_Xlink_List.pm
#  Author: Amanda Orin
#  Date: 2012-06-08
#
#  Rev. 11/11/2015
#  Fix whitespace and remove dead code.
#  - Eric Dattore
#
#   Fix hidden field marking - 10/2020
#   J. Scannell
#
#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# new()
#   * Creates and instance of the Proj_Xlink_List Tool
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
#   * Expected Parameters:
#      -project - a single, or list of projects
#      -fields (optional) - an array of fields to display along with the
#       file name and dir_path, the default is begin_date and end_date
#      -begin_date (optional) - display records greater than this date
#      -end_date (optional) - display records less than this date
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Proj_Xlink_List;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "href", "title", "purpose" );

# The field titles to display at the top of the table
my %field_titles = ("xlink_id" => "ID",
    "href" => "Href",
    "title" => "Title",
    "purpose" => "Purpose",
    "hide" => "Hidden" );

# The field tags to use in the body of the table
my %field_tags = ( "xlink_id" => "<td align=center nowrap>",
    "href" => "<td align=center>",
    "title" => "<td align=center>",
    "purpose" => "<td align=center nowrap>",
    "hide" => "<td align=center>" );

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "proj_xlink_list";
    $self->{group} = "project";

    return $self;
}

sub shortDesc {
    return "External Link List";
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
    return "External Link List";
}

sub queryForm {
    my $self = shift;

    my @projects = getAllProjects();

    my $title = "Project External Link List: Query";

    my $form = cr(
        "<form action=supervisor method=POST onSubmit=\"return checkSelect( this.project, \'Please Select a Project.\' )\">", # Updated the checkSelect string
        "<table border=0 cellpadding=3 cellspacing=1 bgcolor=white width=75%>",
        "<tr><td align=center><b>Select one or more projects<br>",
        "(Ctrl/Shift + click for highlighting multiple projects)</b></td></tr>",
        "<tr><td align=center>",
        "<select size=8 name=project multiple>");#,

    foreach my $proj (@projects) {
        $form = $form . cr( "<option value=\"$proj\">$proj</option>" );
    }
    $form = $form . cr(
        "</select>",
        "</tr></td>",
        "<tr bgcolor=white>",
        "<td>",
        "<b>Select which fields to display:</b>",
        "</td>",
        "</tr>" ,
        "<tr bgcolor=white>",
        "<td align=center><font size=-1><b>",
        "<input type=checkbox name=fields value=xlink_id>ID" . &nbsp(3),
        "<input type=checkbox checked name=fields value=href>Href" . &nbsp(3),
        "<input type=checkbox checked name=fields value=title>Title." . &nbsp(3),
        "<input type=checkbox checked name=fields value=purpose>Purpose" . &nbsp(3),
        "<input type=checkbox name=fields value=hide>Hidden" . &nbsp(3),
        "</td>",
        "</tr>",
        "<tr><td align=right>",
        "<input type=hidden name=results value=$self->{identifier}>",
        "<input type=submit value=\"Submit Query\" name=pgi_submit>",
        "</tr></td>",
        "<tr><td>&nbsp;</td></tr>",
        "</table></form>" );

    return( $title, $form );
}

sub queryResult {
    my $self = shift;

    # Get the parameters
    my $params = shift;
    my @projects = $params->get( "project" );
    my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
    my $nfields = @fields+2;

    # Instantiate the query object
    my $query = EQuery->new();

    # Write the table header
    my $result = cr(
        "<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
        "<tr bgcolor=white>");#,

    # Write column name for each field
    foreach my $f (@fields)
    {
        if( defined( $field_titles{$f} ) )
        {
            $result = $result . cr ( "<td align=center bgcolor=#e9e9e9><b>$field_titles{$f}</b></td>" );
        }
    }
    $result = $result . cr(	"</tr>" );

    # Query CODIAC for each dataset_id
    foreach my $id (@projects)
    {
        my $sql = "SELECT xlink.id as xlink_id, xlink.* FROM xlink,project_xlink,project WHERE project_xlink.xlink_id=xlink.id AND project_xlink.project_id=project.id AND project.name = " .$query->quote( $id ) . " ";

        $sql = $sql . "ORDER BY xlink.href";

        $result = $result . "<tr bgcolor=white><td colspan=$nfields><b>$id:</b></td></tr>";
        $query->query( $sql );
        my $prev_dir = "";

        # Loop through each row
        while( (my %row = $query->getRow() ) )
        {
            # Show the record, only the fields user specified
            $result = $result . cr(
                "<tr bgcolor=white>");#,
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

        # Check to see if this was an empty query
        if( $query->getRowCount() != 0 )
        {
            if ( $query->getRowCount() == 1 ) {
                $result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".$query->getRowCount()." External Link</b></td></tr>";
            } else {
                $result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".$query->getRowCount()." External Links</b></td></tr>";
            }
        }
        else
        {
            $result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".&nbsp(3)."No External Links to Display</b></td></tr>";
        }
        $result = $result . "<tr bgcolor=white><td colspan=$nfields>&nbsp;</td></tr>";
    }

    $result = $result . "</table>";
    my $title = "Project: External Links";

    # Return title and query result
    return( $title, $result );
}

sub getAllProjects {
    my $eq = EQuery->new();
    $eq->query("SELECT DISTINCT name as project_id FROM project WHERE name NOT LIKE \'COMET_CASE%\' ORDER BY project_id" );
    my @projs;

    while( (my %row = $eq->getRow()) ) {
        push(@projs,$row{project_id});
    }

    return @projs;
}

1;

