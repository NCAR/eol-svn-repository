#
# Proj_Stats.pm
#  Author: Phillip Dressen
#  Date: 2003-07-07
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

package Proj_Stats;
use Utils;
use EQuery;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "proj_stat";	
	$self->{group} = "project";

	return $self;
}

sub shortDesc 
{
	return "Project Statistics";
}

sub longDesc 
{
	return cr( 
"		<table border=0 cellpadding=0 cellspacing=0><tr><td>",
"			Displays general statistics about one or more projects, output includes:",
"				<dd><li>Number of Datasets",
"				<dd><li>Number of Files (On-line / Off-line)",
"				<dd><li>Total Size of files in bytes (On-line / Off-Line)",
"		</td></tr></table>" );
}

sub linkTitle
{
	return "Statistics";
}

sub queryForm 
{
	my $self = shift;

	my $query = EQuery->new( "project_db" );
	my $sql = "SELECT DISTINCT id FROM project WHERE id NOT LIKE \'COMET_CASE%\' ORDER BY id";
	$query->query( $sql );

	my $form = cr( 
		"<form action=supervisor method=POST onSubmit=\"return checkSelect( this.project, \'Please Select a Project\')\">",
		"<table border=0 cellpadding=3 cellspacing=1 bgcolor=white width=75%>",
				"<tr><td align=center><b>Select one or more projects<br>",
				"(Ctrl/Shift + click for highlighting multiple projects)</b></td></tr>",
				"<tr><td align=center>",
					"<select size=8 name=project multiple>" );
					
					while( (my %row = $query->getRow()) )
					{
						my $proj = $row{id};
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
				"<tr><td align=center><font size=-1>",
					"Note: gathering these statistics takes time, please be patient when selecting more than five projects.", 
				"</tr></td>",
			"</table></form>" );

	my $title = "CODIAC Project Statistics: Query Form";

	return ( $title, $form );
}

sub queryResult 
{
	my $params = $_[1];

    my $title = "CODIAC Project Statistics: Results";                         

    my $result = "";

	my $sets = EQuery->new( "project_db" );	
	my $on_line = EQuery->new( "phys_dir_db" );
	my $off_line = EQuery->new( "phys_dir_db" );

	$result = $result . cr( 
"	<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
"		<tr>",
"		<td> <b> <font align=center>Project Name</font> </b> </td> \n",
"		<td> <b> <font align=center>Number of Datasets</font> </b> </td> \n",
"		<td> <b> <font align=center>Number of Online Files</font> </b> </td> \n",
"		<td> <b> <font align=center>Total Size of Online Files (Bytes)</font> </b> </td> \n",
"		<td> <b> <font align=center>Number of Offline Files</font> </b> </td> \n",
"		<td> <b> <font align=center>Total Size of Offline Files (Bytes)</font> </b> </td> \n",
"		</tr>", 
	"" );

	my @proj_list = $params->get("project");
	foreach $proj ( @proj_list ) {

		my $sql = "SELECT COUNT\(*\), storm_id FROM dataset_project WHERE project_id=\'$proj\'";

		$sets->query( $sql );

		# Set Storm Id List ($id_list) and get the total number of 
		# datasets ($count)
		my %row = $sets->getRow();
		my $id_list = "( ";
		while( $row{ 'COUNT(*)' } eq "" ) { 
			if( !($row{ 'storm_id' } eq "") ) {
				$id_list = $id_list . $row{ 'storm_id' };
			} 
			%row = $sets->getRow();
			if( $row{ 'COUNT(*)' } eq "" ) {
				$id_list = $id_list . ", ";
			}
		}
		$id_list = $id_list . " )";
		$count = $row{ 'COUNT(*)' };
	
		# Get the total number of on line files and the total num of bytes
		$sql = "SELECT COUNT(*), SUM(data_amt) FROM on_line_phys_dir WHERE l_dds_id IN $id_list";
		$on_line->query( $sql );

		$sql = "SELECT SUM(num_files), SUM(data_amt) FROM off_line_phys_dir WHERE l_dds_id IN $id_list";
		$off_line->query( $sql );




		$result = $result . cr(
"		<tr bgcolor=white>",
"		<td align=left nowrap>&nbsp;&nbsp;" . $proj  . "&nbsp;&nbsp;</td>",
"		<td align=right>" . formatNum( $count * 1 ) . "&nbsp;&nbsp;</td>" );

		%row = $on_line->getRow();
		$result = $result . cr( 
"		<td align=right>" . formatNum( $row{ 'COUNT(*)' } * 1 )       . "&nbsp;&nbsp;</td>",
"		<td align=right>" . formatNum( $row{ 'SUM(data_amt)' } * 1024 )  . "&nbsp;&nbsp;</td>");
	
        %row = $off_line->getRow();
        $result = $result . cr(
"		<td align=right>" . formatNum( $row{ 'SUM(num_files)' } * 1 ) . "&nbsp;&nbsp;</td>",
"		<td align=right>" . formatNum( $row{ 'SUM(data_amt)' } * 1024 ) . "&nbsp;&nbsp;</td>",
"		</tr>",
		"" );
	
	}


	$result = $result . "</table>";

	return ( $title, $result );
}


sub formatNum {
	my $in_num = shift;

	my $nm = "";
	my @digits = split( //, $in_num);

	for( my $y = 0; $y < @digits; $y++ ) {
		if( ((@digits - $y) % 3) == 0 && $y != 0) {
			$nm = $nm . ",";
		}
		$nm = $nm . $digits[$y];
	}

	return $nm;
}


1;

