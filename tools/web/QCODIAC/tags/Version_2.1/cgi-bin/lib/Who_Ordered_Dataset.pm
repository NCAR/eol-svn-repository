#! /usr/bin/perl -w

#
# Who_Ordered_Dataset.pm
#  Author: Phillip Dressen
#  Date: 2003-06-24
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
#      -storm_id - can be either a single storm_id or comma-delimited list
#      -fields (optional) - an array of fields to display along with the
#       file name and dir_path, the default is begin_date and end_date
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Who_Ordered_Dataset;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "dataset_id", "delivery_date", "email" );

# The field titles to display at the top of the table
my %field_titles = ("dataset_id" => "Dataset ID",
		    "delivery_date" => "Date",
		    "email" => "Email Address",
		    "size_kb" => "Num. Kilobytes",   
		    "source_format_id" => "Source Format",
		    "target_format_id" => "Target Format" );

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "who_order";	
    $self->{group} = "dataset";
    
    return $self;
}

sub shortDesc {
    return "Who Ordered Dataset Listing";
}

sub longDesc {
    return cr("       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
	      "           Displays the Who Ordered Dataset Listing for one or more datasets, output includes: ",
	      "               <dd><li>Dataset ID",                   
	      "               <dd><li>Delivery Date",
	      "               <dd><li>Email Addresses",
	      "               <dd><li>Other optional fields",
	      "       </td></tr></table>" );
}

sub linkTitle {
    return "Who Ordered";
}

sub queryForm {
    my $self = shift;
    my $title = "Who Ordered Dataset Listing: Query Form";
    
    # Create the simple form to query dataset_ids
    my $form = cr("		<form action=supervisor method=POST onSubmit=\"return checkText( this.dataset_id, \'Please Enter a Dataset Id.\')\">" , 
		  "		<table border=0 cellpadding=9 cellspacing=0 bgcolor=#e9e9e9 width=$table_width>" ,	
		  "			<tr bgcolor=white>" ,
		  "				<td align=center colspan=2>" ,	
		  "					<br><b>Enter a list of comma-delimited dataset_id numbers (e.g. 77.018,77.019,46.006)</b>",
		  "					<br><input type=text name=dataset_id size=75><br>",
		  "				</td>" , 
		  "			</tr>",
		  "           <tr bgcolor=white>",
		  "               <td colspan=2>", 
		  "                   <b>Select which fields to display:</b>",
		  "               </td>",
		  "           </tr>" ,
		  "           <tr bgcolor=white>",
		  "               <td align=center colspan=2><font size=-1><b>" );
    
    $form = $form . cr("                   <input type=checkbox checked name=fields value=dataset_id>$field_titles{\"dataset_id\"}", &nbsp(3),"" );
    $form = $form . cr("                   <input type=checkbox checked name=fields value=delivery_date>$field_titles{\"delivery_date\"}", &nbsp(3),"" );
    $form = $form . cr("                   <input type=checkbox checked name=fields value=email>$field_titles{\"email\"}", &nbsp(3),"" );
    $form = $form . cr("                   <input type=checkbox unchecked name=fields value=source_format_id>$field_titles{\"source_format_id\"}", &nbsp(3),"" );
    $form = $form . cr("                   <input type=checkbox unchecked name=fields value=target_format_id>$field_titles{\"target_format_id\"}", &nbsp(3), "" );
    $form = $form . cr("                   <input type=checkbox unchecked name=fields value=size_kb>$field_titles{\"size_kb\"}",  &nbsp(3),"               </td>","           </tr>" , "" );

    $form = $form . cr("			<tr bgcolor=white>",
		       "				<td><font size=-1><b>",
		       "					<input type=checkbox unchecked name=email_only value=selected>Select Only Distinct Email Addresses<br>",
		       "					<input type=checkbox unchecked name=isJoss value=selected>Remove DPG JOSS Employees from listing (Email address is a required field)",
		       "				</td>",
		       "				<td align=right>" ,	
		       "					<input type=hidden name=results value=$self->{identifier}>",
		       "					<input type=submit value=\"Submit Query\">",
		       "				</td>",
		       "			</tr>",
		       "		</table></form>" );
    
    return( $title, $form );
}

sub queryResult {
    my $self = shift;
    my $params = shift;
    my @datasets = split(/,/,$params->get("dataset_id"));

    if( $params->exists( "email_only" ) && $params->get( "email_only" ) eq "selected" ) {
	@def_fields = ( "email" );
    } elsif( $params->exists( "fields" ) ) {
	@def_fields = $params->get("fields");
    }

    my $eq = EQuery->new();
    my $dropJoss = $params->exists("isJoss") && $params->get("isJoss") eq "selected" ?
	"AND email NOT LIKE ".$eq->quote("%joss.ucar.edu")." AND email NOT LIKE ".$eq->quote("%ofps.ucar.edu") :
	    "";

    foreach my $dataset_id (@datasets) {
	my $sql = "SELECT target.full_name as target_name,source.full_name as source_name,codiac_web_orders.* from format as target,format as source,codiac_web_orders where source.format_id=source_format_id and target.format_id=target_format_id and dataset_id=".$eq->quote($dataset_id).$dropJoss." ORDER BY ".($params->exists("sort_who_order") ? $params->get("sort_who_order") : "delivery_date");

	if (@def_fields == 1 && $def_fields[0] eq "email") {
	    $sql = "SELECT DISTINCT email from codiac_web_orders where dataset_id=".$eq->quote($dataset_id).$dropJoss." ORDER BY email";
	}

	$eq->query($sql);

	$result = cr("<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
		     "<tr>" );
	
	if ($eq->getRowCount() > 0) {
	    foreach my $item ( @def_fields ) {
		$result .= cr("<td align=center id=\"sort_link\">",
			      "<b><a href=supervisor?results=$self->{identifier}&dataset_id=$dataset_id&sort_$self->{identifier}=$item$field_list");
		
		if( $params->exists( "email_only" ) && $params->get("email_only") eq "selected" ) {
		    $result = $result . "&email_only=selected>$field_titles{$item}</a></b>";
		} else {
		    $result = $result . ">$field_titles{$item}</a></b>"; 
		}
		$result .= cr("</td>");
	    }
	    $result = $result . "\n\t\t</tr>";
	    
	    while ((my %row = $eq->getRow())) {
		$result .= "<tr>";
		foreach my $item (@def_fields) {
		    $result .= cr("<td bgcolor=white>",
				  $row{$item},
				  "</td>");
		}
		$result .= "</tr>\n";
	    }
	} else {
	    $result .= "<tr bgcolor=white><td align=center><b>Query matched no results.</b></td></tr>\n";
	}
    } 

    $result .= cr("</table>");

    my $title = "Who Ordered Dataset Listing: Query Results";
    
    # Return title and query result
    return( $title, $result );
}

1;

