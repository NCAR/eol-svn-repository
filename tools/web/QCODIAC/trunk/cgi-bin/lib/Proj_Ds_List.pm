#! /usr/bin/perl -w

#
# Proj_Ds_List.pm
#  Author: Dan Sullivan
#  Date: 2003-07-08
#
#  Rev. 11/11/2015
#  Fix whitespace and remove dead code.
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
#   * Expected parameters:
#      --project - a single, or list of projects, if 'All' is
#        given all projects are displayed.
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Proj_Ds_List;
use Utils;
use EQuery;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "proj_ds_list";
    $self->{group} = "project";

    return $self;
}

sub shortDesc {
    return "Project Dataset List";
}

sub longDesc {
    return cr(
        "<table border=0 cellpadding=0 cellspacing=0><tr><td>",
        "Displays a full list of datasets associated with the given project and flags which datasets are password protected.",
        "Note: more parameters will be added later.",
        "</td></tr></table>" );
}

sub linkTitle {
    return "Dataset List";
}

sub queryForm {
    my $self = shift;

    my @projects = getAllProjects();

    my $form = cr(
        "<form action=supervisor method=POST onSubmit=\"return checkSelect( this.project, \'Please Select a Project.\' )\">",
        "<table border=0 cellpadding=3 cellspacing=1 bgcolor=white width=75%>",
        "<tr><td align=center><b>Select a project</b></td></tr>",
        "<tr><td align=center>",
        "<select size=8 name=project>"
        );

    foreach my $proj (@projects) {
        $form = $form . cr( "<option value=\"$proj\">$proj</option>" );
    }
    $form = $form . cr(
        "</select>",
        "</tr></td>",
        "<tr><td align=right>",
        "<input type=hidden name=results value=$self->{identifier}>",
        "<input type=submit value=\"Submit Query\" name=pdl_submit>",
        "</tr></td>",
        "<tr><td>&nbsp;</td></tr>",
        "</table></form>" );

    my $title = "Project Dataset List: Query";
    return ( $title, $form );
}

sub queryResult {
    my $self = shift;
    my $params = shift;
    my $project = $params->get( "project" );
    my $ds_cnt = 0;
    my $nfields = 6;

    my $result = "<table border=0 cellpadding=4 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>";
    $result = cr( $result,
        "<tr>",
        "<td align=center bgcolor=#e9e9e9><b>Hidden?</td>",
        "<td align=center bgcolor=#e9e9e9><b>Password?</td>",
        "<td align=center bgcolor=#e9e9e9><b>Creation Date</td>",
        "<td align=center bgcolor=#e9e9e9><b>Last Updated</td>",
        "<td align=center bgcolor=#e9e9e9><b>Dataset Title</td>",
        "<td align=center bgcolor=#e9e9e9><b>Dataset Id</td>",
        "</tr>"
    );

    if( !$project ) {
        $result = &nbsp(3) . "<b>No Project Selected.</b>";
    } else {
        my $dss = getAllDatasets( $project );

        foreach my $ds (@$dss) {
            my @a = split( " ", $ds->{row_revise_time} );
            $ds->{row_revise_time} = $a[0];

            @a = split( " ", $ds->{row_create_time} );
            $ds->{row_create_time} = $a[0];

            my $pswd = "<font color=red>N</font>";
            $pswd = "<font color=blue>Y</font>" if($ds->{auth_reqd});

            my $hidden = "<font color=red>Y</font>";
            $hidden = "<font color=blue>N</font>" if($ds->{visible});

            $result = $result . cr("<tr>",
                "<td bgcolor=white align=center>$hidden</td>",
                "<td bgcolor=white align=center>$pswd</td>",
                "<td bgcolor=white>$ds->{row_create_time}</td>",
                "<td bgcolor=white>$ds->{row_revise_time}</td>",
                "<td bgcolor=white><a href=supervisor?results=ds_description&dataset_id=$ds->{archive_ident}>$ds->{title}</a></td>",
                "<td bgcolor=white><a href=https://data.eol.ucar.edu/codiac/dss/id?$ds->{archive_ident} target=_blank>$ds->{archive_ident}</a></td>",
                "</tr>" );
            $ds_cnt = $ds_cnt + 1;
        }
    }

    # Include the total at the bottom of the list
    # Check to see if this was an empty query
    if( $ds_cnt != 0 )
    {
        $result = $result . "<tr bgcolor='#e9e9e9'><td colspan=$nfields><b>".$ds_cnt." Datasets</b></td></tr>";
    }
    else
    {
        $result = $result . "<tr bgcolor='#e9e9e9'><td colspan=$nfields><b>".&nbsp(3)."No Datasets to Display</b></td></tr>";
    }

    $result = $result . "</table>";
    my $title = "Project Dataset List";
    return ( $title, $result );
}

sub getAllProjects {
    my $eq = EQuery->new();
    $eq->query( "SELECT name as project_id FROM project WHERE name NOT LIKE \'COMET_CASE%\' ORDER BY project_id" );
    my @projs;

    while( (my %row = $eq->getRow()) ) {
        push(@projs,$row{project_id});
    }

    return @projs;
}

sub getAllDatasets {
    my $project = shift;
    my @dss;

    my $eq = EQuery->new();
    my $sql = "SELECT dataset.* FROM dataset JOIN dataset_project ON dataset.id=dataset_project.dataset_id JOIN project ON dataset_project.project_id=project.id WHERE project.name=".$eq->quote( $project )." ORDER BY dataset.archive_ident";

    $eq->query( $sql );

    while( (my %row = $eq->getRow()) ) {
        push(@dss,\%row);
    }

    return \@dss;
}

1;

