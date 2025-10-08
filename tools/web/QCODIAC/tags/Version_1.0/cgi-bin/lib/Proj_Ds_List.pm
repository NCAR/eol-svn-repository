#
# Proj_Ds_List.pm
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

package Proj_Ds_List;
use Utils;
use EQuery;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "proj_ds_list";	
	$self->{group} = "project";

	return $self;
}

sub shortDesc 
{
	return "Project Dataset List";
}

sub longDesc 
{
     return cr(
"       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
"			Displays a full list of datasets associated with the given project and flags which datasets are password protected.",
"			Note: more parameters will be added later.",
"       </td></tr></table>" );
}

sub linkTitle
{
	return "Dataset List";
}

sub queryForm 
{
	my $self = shift;

	my @projects = getAllProjects();
	
	my $form = cr( 
		"<form action=supervisor method=POST onSubmit=\"return checkSelect( this.project, \'Plses Select a Project.\' )\">",
		"<table border=0 cellpadding=3 cellspacing=1 bgcolor=white width=75%>",
				"<tr><td align=center><b>Select a project</b></td></tr>",
				"<tr><td align=center>",
					"<select size=8 name=project>"
						);
					
					foreach my $proj (@projects)
					{
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

sub queryResult 
{
	my $self = shift;
	my $params = shift;
	my $project = $params->get( "project" );

	my $result = "<table border=0 cellpadding=4 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>";
	$result = cr( $result,  
		"<tr>",
			"<td align=center bgcolor=#e9e9e9><b>Hidden?</td>",
			"<td align=center bgcolor=#e9e9e9><b>Password?</td>",
			"<td align=center bgcolor=#e9e9e9><b>Date</td>",
			"<td align=center bgcolor=#e9e9e9><b>Dataset Title</td>",
			"<td align=center bgcolor=#e9e9e9><b>Storm Id</td>",
		"</tr>"
		);

	if( !$project )
	{
		$result = &nbsp(3) . "<b>No Project Selected.</b>"; 
	}
	else
	{
		my $dss = getAllDatasets( $project );
		my $phash = getPasswordDatasets( $project );
	
		foreach my $ds (@$dss)
		{
			my @a = split( " ", $ds->{revision_date} );
			$ds->{revision_date} = $a[0];
			my $pswd = "<font color=red>N</font>";
			$pswd = "<font color=blue>Y</font>" if( exists( $phash->{$ds->{storm_id}} ) );

			my $hidden = "<font color=blue>N</font>";
			$hidden = "<font color=red>Y</font>" if( $ds->{hide_ind} ne "" );

			$result = $result . cr(
				"<tr>",
					"<td bgcolor=white align=center>$hidden</td>",
					"<td bgcolor=white align=center>$pswd</td>",
					"<td bgcolor=white>$ds->{revision_date}</td>",
					"<td bgcolor=white><a href=supervisor?results=ds_description&storm_id=$ds->{storm_id}>$ds->{title}</a></td>",
					"<td bgcolor=white><a href=/cgi-bin/codiac/dss?$ds->{storm_id} target=_blank>$ds->{storm_id}</a></td>",
				"</tr>" );
		}
	}	

	$result = $result . "</table>";
	my $title = "Project Dataset List";                         
	return ( $title, $result );
}

sub getAllProjects
{
	my $eq = EQuery->new( "project_db" );
	$eq->query( "SELECT DISTINCT id FROM project WHERE id NOT LIKE \'COMET_CASE%\' ORDER BY id" );
	my @projs;

	while( (my %row = $eq->getRow()) )
	{
		$projs[scalar(@projs)] = $row{id};
	}

	return @projs;
}

sub getProjectHash
{
	my $proj = shift;
	my %hash;

	# Get the needed fields from the project database
	my $eq = EQuery->new( "project_db" );
	$eq->query( "SELECT * FROM project WHERE id=" . $eq->quote($proj) );
	my %row = $eq->getRow();	

	$hash{id} = $row{id};
	$hash{name} = $row{name};
	$hash{begin_date} = formatDate( $row{begin_date} );
	$hash{end_date} = formatDate( $row{end_date} );
	$hash{minlat} = $row{minlat};
	$hash{maxlat} = $row{maxlat};
	$hash{minlon} = $row{minlon};
	$hash{maxlon} = $row{maxlon};

	# Get the CODIAC prefix
	$eq = EQuery->new( "catalog_db" );
	$eq->query( "SELECT * FROM storm_id_project WHERE project_id=" . $eq->quote( $proj ) );
	%row = $eq->getRow();

	$hash{prefix} = (%row)? $row{storm_id} . ".xxx" : "none";

	return %hash;
}

sub getAllDatasets
{
	my $project = shift;
	my @dss;

	my $eq = EQuery->new( "catalog_db" );
	my $sql = "SELECT dataset.storm_id, dataset.revision_date, dataset.title, dataset.hide_ind FROM dataset, '/storm/codiac/codiac_db/project_db':dataset_project WHERE " .
						"dataset.storm_id=dataset_project.storm_id AND " .
						"dataset_project.project_id=" . $eq->quote( $project ) . 
						" ORDER BY dataset.storm_id";

	$eq->query( $sql );

	while( (my %row = $eq->getRow()) )
	{
		$dss[scalar(@dss)] = \%row;
	}

	return \@dss;
}

sub getPasswordDatasets
{
	my $project = shift;
	my %hash;
	my $eq = EQuery->new( "catalog_db" );

	my $sql = "SELECT ds_password.storm_id FROM ds_password, '/storm/codiac/codiac_db/project_db':dataset_project WHERE " .
						"ds_password.storm_id=dataset_project.storm_id AND dataset_project.project_id=" . $eq->quote( $project );

	$eq->query( $sql );

	while( (my %row = $eq->getRow()) )
	{
		$hash{$row{storm_id}} = 1;
	}
	
	return \%hash;
}

1;

