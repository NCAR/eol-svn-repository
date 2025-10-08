#
# Ds_Description.pm
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
#   * Expected Parameters:
# 			--storm_id - either a single or comma-delimited list
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Ds_Description;
use Utils;
use EQuery;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "ds_description";	
	$self->{group} = "dataset";

	return $self;
}

sub shortDesc 
{
	return "Dataset Description";
}

sub longDesc 
{
     return cr(
"       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
"           Displays the Dataset Description and Project Link Listing for one or more datasets, output includes: ",
"               <dd><li>Description Fields (Storm ID, Title, etc.)",
"               <dd><li>Contact Fields (Primary and Secondary Contact Associations)",
"               <dd><li>Data Availability (Notes, Browsable, etc.)",
"               <dd><li>Listing of Linked Project(s)",
"       </td></tr></table>" );
}

sub linkTitle
{
	return "Description";
}

sub queryResult 
{
	my $self = shift;
	my $params = shift;
	my $list = $params->get( "storm_id" );
	my @list = split( ",", $list );
	my $result;
	my %platforms = getPlatforms();

	foreach my $storm_id (@list)
	{
		my $eq = EQuery->new( "catalog_db" );
		$eq->query( "SELECT * FROM dataset WHERE storm_id=" . $eq->quote( $storm_id ) );
		my %ds = $eq->getRow();

		if( ! %ds )
		{
			$result = $result . cr(
				"<table border=0 width=100% cellpadding=1 cellspacing=1 bgcolor=#e9e9e9>",
				"<tr><td>&nbsp</td></tr>",		
				"<tr><td bgcolor=white><font size=+1><b>$storm_id:</font></td></tr>",
				"<tr><td bgcolor=white><b>", &nbsp(3), "No entry for $storm_id.</td></tr>",
				"<tr><td bgcolor=white>&nbsp;</td></tr>",
				"</table>" );
		}
		else
		{
			setDisplayFields( \%ds );
			my $projs = getProjectField( $storm_id );

			$result = $result . cr(
				"<table border=0 width=100% cellpadding=1 cellspacing=1 bgcolor=#e9e9e9>",
					"<tr><td>&nbsp</td></tr>",		
					"<tr><td bgcolor=white><font size=+1><b>$storm_id:</font></td></tr>",

					"<tr><td bgcolor=white>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td align=left nowrap width=5%><b>Storm Id: </b></td>",
								"<td width=15%>",
									"<a target=_blank href=http://www.joss.ucar.edu/cgi-bin/codiac/dss?$storm_id>$storm_id</a>",
								"</td>",
								"<td width=15% align=left>",
									"<b>Hidden:" . &nbsp(2) . $ds{hide_ind},
								"</td>",
							"</tr>",
							"<tr>",
								"<td align=left nowrap width=5%><b>Title: </b></td>",
								"<td colspan=2 align=left>$ds{title}</td>",
							"</tr>",
						"</table>",
					"</tr></td>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr><td width=100%><b>Summary: </b></td></tr>",
							"<tr><td width=95%>$ds{summary}</td></tr>",
						"</table>",
					"</td></tr>",
			
					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td align=right nowrap width=15%><b>Data Category: </b></td>",
								"<td align=left width=15% nowrap>$ds{category}</td>",
								"<td align=right nowrap width=15%><b>Platform: </b></td>",
								"<td align=left>$platforms{$ds{platform}} ($ds{platform})</td>",
						"</table>",
					"</td></tr>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td align=right nowrap width=15%><b>Observing Frequency: </b></td>",
								"<td width=15% nowrap>$ds{frequency}</td>",
								"<td align=right nowrap width=10%><b>Sort: </b></td>",
								"<td>$ds{sort_type}</td>",
								"<td align=right nowrap width=10%><b>Type: </b></td>",
								"<td>$ds{spatial_type}</td>",
						"</table>",
					"</td></tr>",
					
					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=0>",
							"<tr>",
								"<td align=right nowrap width=15%><b>Date Range:</b></td>",
								"<td align=right width=5%>Begin: </td>",
								"<td width=5% nowrap>$ds{begin_date}</td>",
								"<td align=right width=20%><b>Latitidue: </b></td>",
								"<td width=5% nowrap>$ds{minlat}</td>",
								"<td width=30% nowrap>$ds{maxlat}</td>",
							"</tr>",
							"<tr>",
								"<td align=right width=15%>&nbsp;</td>",
								"<td align=right width=5%>End: </td>",
								"<td width=5% nowrap>$ds{end_date}</td>",
								"<td align=right width=20%><b>Longitude: </b></td>",
								"<td width=5% nowrap>$ds{minlon}</td>",
								"<td width=30% nowrap>$ds{maxlon}</td>",
							"</tr>",
						"</table>",
					"</td></tr>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td align=right width=15%><b>Region Covered: </b></td>",
								"<td width=75%>$ds{region}</td>",
						"</table>",
					"</td></tr>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td align=right width=15%><b>Readme File: </b></td>",
								"<td width=75%>$ds{readme}</td>",
							"</tr>",
						"</table>",
					"</td></tr>",

					"<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td nowrap align=right width=15%><b>Primary Agency: </b></td>",
								"<td width=15%>$ds{prim_archive_agency}</td>",
								"<td nowrap align=right width=15%><b>Primary Contact: </b></td>",
								"<td width=15%>$ds{contact}</td>",
								"<td nowrap align=right width=15%><b>Dataset: </b></td>",
								"<td width=15%>$ds{prim_agency_dataset}</td>",
							"</tr>",
							"<tr>",
								"<td nowrap align=right width=15%><b>Secondary Agency: </b></td>",
								"<td width=15%>$ds{sec_archive_agency}</td>",
								"<td nowrap align=right width=15%><b>Secondary Contact: </b></td>",
								"<td width=15%>$ds{sec_contact}</td>",
								"<td nowrap align=right width=15%><b>Dataset: </b></td>",
								"<td width=15%>$ds{sec_agency_dataset}</td>",
							"</tr>",
							"<tr>",
								"<td nowrap align=right width=15%><b>Data Source Agency: </b></td>",
								"<td>$ds{source_agency}</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
							"</tr>",
							"<tr>",
								"<td nowrap align=right width=15%><b>Remote URL: </b></td>",
								"<td>$ds{remote_url}</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
							"</tr>",
							"<tr>",
								"<td nowrap align=right width=15%><b>Date data last updated: </b></td>",
								"<td>$ds{date_last_update}</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
								"<td>&nbsp;</td>",
							"</tr>",
						"</table>",
					"</td></tr>",
					
					"<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",

					"<tr bgcolor=white><td>",
						"<b>DATA AVAILABILITY:</b>", &nbsp(6),
						"<b>Online: </b>$ds{avail_flag}", &nbsp(2),
						"<b>Order: </b>$ds{order_flag}", &nbsp(2),
						"<b>Browse: </b>$ds{www_browse_flag}", &nbsp(2),
					"</td></tr>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr><td width=100%><b>Description: </b></td></tr>",
							"<tr><td width=95%>$ds{avail_desc}<br><br></td></tr>",
						"</table>",
					"</td></tr>",

					"<tr bgcolor=white><td>",
						"<table width=100% border=0 cellpadding=4 cellspacing=1>",
							"<tr>",
								"<td align=right width=5% nowrap><b>CATALOG ENTRY: </b></td>",
								"<td align=right width=15% nowrap><b>Author: </b></td>",
								"<td>$ds{author}</td>",
							"</tr>",
							"<tr>",
								"<td>&nbsp;</td>",
								"<td align=right width=15% nowrap><b>Date last revised: </b></td>",
								"<td>$ds{revision_date}</td>",
							"</tr>",
						"</table>",
					"</td></tr>",
				
					"<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",
					"<tr bgcolor=white><td>$projs</td></tr>",

				"</table>" );
			} # end of else to check if %ds is undef
	}

	my $title = "Dataset Description";
	return ( $title, $result );
}

sub queryForm 
{
	my $self = shift;

	my $title = "Dataset Description: Query";                         
	my $form = cr(
		"<form action=supervisor method=POST onSubmit=\"return checkText( this.storm_id, \'Please Enter a Storm Id.\')\">",
		"<table border=0 bgcolor=white cellpadding=4 width=80%>",
			"<tr><td align=center>",
				"<br><b>Enter a list of comma-delimited storm_id numbers (e.g. 77.018,77.019,46.006):</b>" ,
				"<input type=text size=75 name=storm_id>",
			"</td></tr>",
			"<tr><td align=right>",
				"<input type=hidden name=results value=$self->{identifier}>",
				"<input type=submit name=ds_submit value=\"Submit Query\">",
			"</td></tr>",
		"</table></form>" );

	return ( $title, $form );
}

sub setDisplayFields
{
	my $ds = shift;

	if( $ds->{hide_ind} eq "" )
	{
		$ds->{hide_ind} = "<font color=blue>No</font>";
	}
	else
	{
		$ds->{hide_ind} = "<font color=red>Yes</font>";
	}

	if( ! ($ds->{dir_path} eq "" || $ds->{file_name} eq "" ) )
	{
		my $url = "http://www.joss.ucar.edu/" . substr( $ds->{dir_path}, 5 ) . "/" . $ds->{file_name};
		$ds->{readme} = "<a href=$url target=_blank>$ds->{dir_path}/$ds->{file_name}</a>";
	}
	else
	{
		$ds->{readme} = "&nbsp;";
	}

	$ds->{begin_date} = formatDate( $ds->{begin_date} );
	$ds->{end_date} = formatDate( $ds->{end_date} );
	$ds->{date_last_update} = formatDate( $ds->{date_last_update} );

	if( $ds->{remote_url} ne "" )
	{
			$ds->{remote_url} = "<a href=$ds->{remote_url} target=_top>$ds->{remote_url}>";
	}
}

sub getPlatforms
{
	my $eq = EQuery->new( "data_dict_db" );
	$eq->query( "SELECT * FROM platforms" );
	my %plats;

	while( (%row = $eq->getRow()) )
	{
		$plats{$row{id}} = $row{name};		
	}

	return %plats;
}

sub getProjectField
{
	my $storm_id = shift;
	my $eq = EQuery->new( "project_db" );
	$eq->query( "SELECT project_id FROM dataset_project where storm_id=" . $eq->quote( $storm_id ) );
	my @projs;
	my $ret;
	while( (my %row = $eq->getRow()) )
	{
		$projs[scalar(@projs)] = $row{project_id};
	}

	$ret = cr( 
			"<table width=100% border=0 cellpadding=4 cellspacing=1>",
				"<tr>",
				"<td width=8% nowrap><b>LINKED TO PROJECT(S): </b></td>" );
				if( scalar(@projs) != 0 )
				{
					$ret = $ret . cr( "<td>", $projs[0], "</td>" );
				}
				else
				{
					$ret = $ret . cr( "<td>", "<b>none</b>", "</td>" );
				}
				for( my $x = 1; $x < scalar(@projs); $x++ )
				{
					$ret = $ret . cr( "<tr>", 
						"<td width=8%>&nbsp;</td>", 
						"<td>$projs[$x]</td></tr>" );
				}
	$ret = $ret . cr ("</table>" );
			
	return $ret;
}
1;

