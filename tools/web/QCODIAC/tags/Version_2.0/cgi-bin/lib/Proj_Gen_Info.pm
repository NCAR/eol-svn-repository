#! /usr/bin/perl -w

#
# Proj_Gen_Info.pm
#  Author: Dan Sullivan
#  Date: 2003-07-08
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

package Proj_Gen_Info;
use Utils;
use EQuery;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "proj_gen_info";	
    $self->{group} = "project";
    
    return $self;
}

sub shortDesc {
    return "Project General Information";
}

sub longDesc {
     return cr(
"       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
"			Displays general information about one or more projects, output includes",
"               <dd><li>Full Project Name",
"               <dd><li>Area of Interest (AOI)",
"               <dd><li>Time of Interest (TOI)",
"               <dd><li>Storm ID Prefix",
"       </td></tr></table>" );
}

sub linkTitle {
    return "General Info.";
}

sub queryForm {
    my $self = shift;
    
    my @projects = getAllProjects();
    
    my $form = cr( 
		  "<form action=supervisor method=POST onSubmit=\"return checkSelect( this.project, \'Plses Select a Project.\' )\">",
		  "<table border=0 cellpadding=3 cellspacing=1 bgcolor=white width=75%>",
		  "<tr><td align=center><b>Select one or more projects<br>",
		  "(Ctrl/Shift + click for highlighting multiple projects)</b></td></tr>",
		  "<tr><td align=center>",
		  "<select size=8 name=project multiple>",
		  "<option value=\"All\">All Projects</option>" );
    
    foreach my $proj (@projects) {
	$form = $form . cr( "<option value=\"$proj\">$proj</option>" );
    }
    $form = $form . cr(
		       "</select>",
		       "</tr></td>",
		       "<tr><td align=right>",
		       "<input type=hidden name=results value=$self->{identifier}>",
		       "<input type=submit value=\"Submit Query\" name=pgi_submit>",
		       "</tr></td>",
		       "<tr><td>&nbsp;</td></tr>",
		       "</table></form>" );
    
    my $title = "Project General Information: Query";
    return ( $title, $form );
}

sub queryResult {
    my $self = shift;
    my $params = shift;
    my @projects = $params->get( "project" );
    my $result = "<table border=0 cellpadding=4 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>";
    
    if( !@projects || scalar(@projects) == 0 ) {
	$result = &nbsp(3) . "<b>No Project Selected.</b>"; 
    } else {
	@projects = getAllProjects() if( @projects[0] eq "All" );
	
	foreach my $proj (@projects) {
	    my %hash = getProjectHash( $proj );
	    
	    $result = $result . cr(
				   "<tr><td colspan=3>&nbsp;</td></tr>",
				   "<tr><td bgcolor=white colspan=3 id=tool_link><a target=_blank href=http://www.joss.ucar.edu/cgi-bin/codiac/projs?$proj><b>$proj</b></a></td></tr>",
				   "<tr bgcolor=white>",
				   "<td align=right width=8%><b>Name: </b></td>",
				   "<td>" . &nbsp(3) . "$hash{full_name}</td>",
				   "<td><b>Hidden: </b>",
				   $hash{hide} ? "<font color=red>Yes</font>" : "<font color=blue>No</font>",
				   "</td>",
				   "</tr>",
				   "<tr bgcolor=white>",
				   "<td align=justify colspan=3><p><b>Description:</b></p>",
				   $hash{description}."</td></tr>",
				   "<tr bgcolor=white>",
				   "<td align=right width=8%><b>AOI:</b></td>",
				   "<td colspan=2>", &nbsp(3),
				   "<b>Latitude: </b>" . $hash{minlat} . " to " . $hash{maxlat}, &nbsp(3),
				   "<b>Longitude: </b>" . $hash{minlon} . " to " . $hash{maxlon},
				   "</tr>",		
				   "<tr bgcolor=white>",
				   "<td align=right width=8%><b>TOI:</b></td>",
				   "<td colspan=2>", &nbsp(3),
				   "$hash{begin_date} to $hash{end_date}",
				   "</tr>",

				   "<tr bgcolor=white>",
				   "<td align=right nowrap width=8%><b>Parent Project:</b></td>",
				   "<td colspan=2>",&nbsp(3),$hash{parent}."</td></tr>",

				   "<tr bgcolor=white>",
				   "<td align=right nowrap width=8%><b>Internal Contact:</b</td>",
				   "<td colspan=2>",&nbsp(3),$hash{person_name}."</td></tr>"

				   );		
	}
    }	

    $result = $result . "</table>";
    my $title = "Project General Information";                         
    return ( $title, $result );
}

sub getAllProjects {
    my $eq = EQuery->new();
    $eq->query("SELECT DISTINCT project_id FROM project WHERE project_id NOT LIKE \'COMET_CASE%\' ORDER BY project_id" );
    my @projs;
    
    while( (my %row = $eq->getRow()) ) {
	push(@projs,$row{project_id});
    }
    
    return @projs;
}

sub getProjectHash {
    my $proj = shift;
    my %hash;
    
    # Get the needed fields from the project database
    my $eq = EQuery->new();
    $eq->query( "SELECT project.*,parent.project_id as parent,contact.person_name FROM project LEFT JOIN project AS parent ON project.parent_project_id_fk=parent.project_id LEFT JOIN contact ON project.internal_contact_id_fk=contact.contact_id WHERE project.project_id=" . $eq->quote($proj) );
    my %row = $eq->getRow();	
    
    $row{parent} = "None" if (!defined($row{parent}));

    $hash{project_id} = $row{project_id};
    $hash{full_name} = $row{full_name};
    $hash{begin_date} = (split(' ',$row{begin_date}))[0];
    $hash{end_date} = (split(' ',$row{end_date}))[0];
    $hash{minlat} = $row{minlat};
    $hash{maxlat} = $row{maxlat};
    $hash{minlon} = $row{minlon};
    $hash{maxlon} = $row{maxlon};
    
    # Get the CODIAC prefix
#    $eq = EQuery->new();
#    $eq->query( "SELECT * FROM dataset_project WHERE project_id_fk=" . $eq->quote( $proj ) );
#    %row = $eq->getRow();
        
    return %row;

#    return %hash;
}
1;





