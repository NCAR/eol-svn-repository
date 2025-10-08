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
use Data::Dumper;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
#my @def_fields = ( "dataset_id", "delivery_date", "email" );
my @def_fields = ( "archive_ident", "delivery_date", "email" );

# The field titles to display at the top of the table
my %field_titles = ("archive_ident" => "Dataset ID",
		    "delivery_date" => "Date",
		    "email" => "Email Address",
		    "size_kb" => "Num. Kilobytes");
# Do not include source/target formats. They are not used often
#		    "source_format_id" => "Source Format",
#		    "target_format_id" => "Target Format" );

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
	      "               <dd><li>Dataset ID</li></dd>",                   
	      "               <dd><li>Delivery Date</li></dd>",
	      "               <dd><li>Email Addresses</li></dd>",
	      "               <dd><li>Other optional fields</li></dd>",
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
    
    #$form = $form . cr("                   <input type=hidden checked name=fields value=dataset_id>"$field_titles{\"dataset_id\"}", &nbsp(3),"" );
    $form = $form . cr("                   <input id=archive_ident type=checkbox checked name=fields value=archive_ident><label for=archive_ident>$field_titles{\"archive_ident\"}</label>", &nbsp(3),"" );
    $form = $form . cr("                   <input id=delivery_date type=checkbox checked name=fields value=delivery_date><label for=delivery_date>$field_titles{\"delivery_date\"}</label>", &nbsp(3),"" );
    $form = $form . cr("                   <input id=email type=checkbox checked name=fields value=email><label for=email>$field_titles{\"email\"}</label>", &nbsp(3),"" );
#    $form = $form . cr("                   <input id=source_format_id type=checkbox unchecked name=fields value=source_format_id><label for=source_format_id>$field_titles{\"source_format_id\"}</label>", &nbsp(3),"" );
#    $form = $form . cr("                   <input id=target_format_id type=checkbox unchecked name=fields value=target_format_id><label for=target_format_id>$field_titles{\"target_format_id\"}</label>", &nbsp(3), "" );
    $form = $form . cr("                   <input id=size_kb type=checkbox unchecked name=fields value=size_kb><label for=size_kb>$field_titles{\"size_kb\"}</label>",  &nbsp(3),"               </td>","           </tr>" , "" );

    $form = $form . cr("			<tr bgcolor=white>",
		       "				<td><font size=-1><b>",
		       "					<input id=email_only type=checkbox unchecked name=email_only value=selected><label for=email_only>Select Only Distinct Email Addresses</label><br>",
		       "					<input id=isJoss type=checkbox checked name=isJoss value=selected><label for=isJoss>Remove EOL staff from listing (Email address is a required field)</label>",
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
    my @datasets = split(/,[\s\x20]*/,$params->get("dataset_id"));

    @def_fields = $params->get("fields");
    my $field_list = "";
    foreach $field (@def_fields) {
        $field_list .= "&fields=$field";
    }

    my $eq = EQuery->new();
    my $dropJoss = ( $params->exists("isJoss") && $params->get("isJoss") eq "selected" ) ? " AND email NOT IN (" . Utils::getIgnoredEmailAddresses() . ") " : " ";
    my $sortOrder = ($params->exists("sort_who_order")) ? " ORDER BY " . $params->get("sort_who_order") . " "  : " ";
    #print Dumper $params->get("sort_who_order");

    foreach my $dataset_id (@datasets) {
		my $sql = "SELECT archive_ident, delivery_date, email, size_kb FROM codiac_web_order_log WHERE archive_ident=".$eq->quote($dataset_id).$dropJoss.$sortOrder;
        #print Dumper $sql;

		if ($params->get("email_only") eq "selected") {
			#$sql = "SELECT DISTINCT email from codiac_web_orders where dataset_id=".$eq->quote($dataset_id).$dropJoss." ORDER BY email";
            #$sql = "SELECT DISTINCT email FROM codiac_web_order_log WHERE archive_ident=".$eq->quote($dataset_id).$dropJoss." ORDER BY email";
            $sql = "SELECT distinct email, archive_ident, delivery_date, size_kb FROM codiac_web_order_log WHERE archive_ident=".$eq->quote($dataset_id).$dropJoss." group by email$sortOrder";
            @def_fields = $params->get('fields');
		}

		$eq->query($sql);

		#$result = cr("<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
		$result .= cr("<table id=\"whoOrderedTable\" class=\"tablesorter\" border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
			"<thead>", "<tr>");
		
		if ($eq->getRowCount() > 0) {
		    	foreach my $item ( @def_fields ) {
                		#print Dumper $jossUrlEncode;
#				$result .= cr("<td align=center id=\"sort_link\">",
#				"<b><a href=supervisor?results=$self->{identifier}&dataset_id=$dataset_id&sort_$self->{identifier}=$item$field_list$jossUrlEncode");
#				if( $params->exists( "email_only" ) && $params->get("email_only") eq "selected" ) {
#				    $result = $result . "&email_only=selected>$field_titles{$item}</a></b>";
#				} else {
#				    $result = $result . ">$field_titles{$item}</a></b>"; 
#				}
#				$result .= cr("</td>");
				$result .= cr("<th align=center>",
				"<b>$field_titles{$item}</b>",
				"</th>");
			}
		$result .= cr("</tr>", "</thead>", "<tbody>");
		    
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
		    $result .= "<tr bgcolor=white><td align=center><b>Query matched no results for $dataset_id.</b></td></tr>\n";
		}
    } 

    $result .= cr("</tbody>", "</table>");
    
    # sort the table
    $result .= '<script type="text/javascript">
    			jQuery(document).ready( function() {
				jQuery(".tablesorter").tablesorter();
			});
		</script>';
    #cr("<script type=\"text/javascript\">","jQuery(document).ready(function() {", "jQuery(\".tablesorter\").tablesorter();", "});", "</script>");
    
    my $title = "Who Ordered Dataset Listing: Query Results";
    
    # Return title and query result
    return( $title, $result );
}

1;
