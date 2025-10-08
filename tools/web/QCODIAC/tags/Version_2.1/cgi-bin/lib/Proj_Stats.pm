#! /usr/bin/perl -w

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

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "proj_stat";	
    $self->{group} = "project";
    
    return $self;
}

sub shortDesc {
    return "Project Statistics";
}

sub longDesc {
    return cr( 
	      "		<table border=0 cellpadding=0 cellspacing=0><tr><td>",
	      "			Displays general statistics about one or more projects, output includes:",
	      "				<dd><li>Number of Datasets",
	      "				<dd><li>Number of Files (On-line / Off-line)",
	      "				<dd><li>Total Size of files in bytes (On-line / Off-Line)",
	      "		</td></tr></table>" );
}

sub linkTitle {
    return "Statistics";
}

sub queryForm {
    my $self = shift;
    
    my $query = EQuery->new();
    my $sql = "SELECT DISTINCT project_id FROM project WHERE project_id NOT LIKE \'COMET_CASE%\' ORDER BY project_id";
    $query->query( $sql );
    
    my $form = cr("<form action=supervisor method=POST onSubmit=\"return checkSelect( this.project, \'Please Select a Project\')\">",
		  "<table border=0 cellpadding=3 cellspacing=1 bgcolor=white width=75%>",
		  "<tr><td align=center><b>Select one or more projects<br>",
		  "(Ctrl/Shift + click for highlighting multiple projects)</b></td></tr>",
		  "<tr><td align=center>",
		  "<select size=8 name=project multiple>" );
    
    while( (my %row = $query->getRow()) ) {
	my $proj = $row{project_id};
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

sub queryResult {
    my $params = $_[1];
    
    my $title = "CODIAC Project Statistics: Results";                         
    
    my $result = "";

    my $sets = EQuery->new();	
    
    my $result = cr("<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
		    "<tr>",
		    "<td><b><font align=center>Project Name</font></b></td>\n",
		    "<td><b><font align=center>Number of Datasets</font></b></td>\n",
		    "<td><b><font align=center>File Type</font></b></td>\n",
		    "<td><b><font align=center>Number of Files</font></b></td>\n",
		    "<td><b><font align=center>Total Size of Files (Bytes)</font></b></td>\n",
		    "</tr>");
    
    my @proj_list = $params->get("project");
    foreach $proj ( @proj_list ) {
	
	my $datasets = 0;
	my %data_hash;

	my $sql = "SELECT COUNT(*) as dataset_count FROM dataset_project WHERE project_id=".$sets->quote($proj);
	while ((my %row = $sets->getRow())) {
	    $datasets += $row{dataset_count};
	}

	$sql = "SELECT COUNT(*) as file_count,SUM(size_kb) as file_size,purpose FROM file WHERE dataset_id IN (SELECT dataset_id FROM dataset_project WHERE project_id=".$sets->quote($proj).") GROUP BY purpose";

	$sets->query( $sql );
	while ((my %row = $sets->getRow())) {
	    $data_hash{$row{purpose}}{file_count} = $row{file_count};
	    $data_hash{$row{purpose}}{file_size} = $row{file_size};
	    $data_hash{file_total}{file_count} += $row{file_count};
	    $data_hash{file_total}{file_size} += $row{file_size};
	    $data_hash{total}{file_count} += $row{file_count};
	    $data_hash{total}{file_size} += $row{file_size};
	}

	$sql = "SELECT SUM(num_files) as file_count,SUM(size_kb) as file_size FROM tape WHERE dataset_id IN (SELECT dataset_id FROM dataset_project WHERE project_id=".$sets->quote($proj).")";

	$sets->query($sql);
	while ((my %row = $sets->getRow())) {
	    $data_hash{tape}{file_count} = $row{file_count};
	    $data_hash{tape}{file_size} = $row{file_size};
	    $data_hash{total}{file_count} += $row{file_count};
	    $data_hash{total}{file_size} += $row{file_size};
	}

	$result .= cr("<tr bgcolor=white>",
		      "<td align=left nowrap>&nbsp;&nbsp;" . $proj  . "&nbsp;&nbsp;</td>",
		      "<td align=right>" . formatNum( $datasets * 1 ) . "&nbsp;&nbsp;</td>" );

	foreach my $key (sort(keys(%data_hash))) {
	    $result .= cr("<td align=center>".$key."</td>",
			  "<td align=right>".formatNum($data_hash{$key}{file_count}*1)."&nbsp;&nbsp;</td>",
			  "<td align=right>".formatNum($data_hash{$key}{file_size}*1024)."&nbsp;&nbsp;</td>");
	}
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

