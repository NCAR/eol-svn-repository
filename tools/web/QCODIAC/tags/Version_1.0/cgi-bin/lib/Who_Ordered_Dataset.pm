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
my @def_fields = ( "storm_id", "del_date", "email_addr" );

# The field titles to display at the top of the table
my %field_titles = (  
                                            "storm_id" => "Storm ID",
                                            "del_date" => "Date",
                                            "email_addr" => "Email Address",
                                            "no_kbytes" => "Num. Kilobytes",   
                                            "source_fmt" => "Source Format",
                                            "target_fmt" => "Target Format" );

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "who_order";	
	$self->{group} = "dataset";

	return $self;
}

sub shortDesc 
{
	return "Who Ordered Dataset Listing";
}

sub longDesc 
{
     return cr(
"       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
"           Displays the Who Ordered Dataset Listing for one or more datasets, output includes: ",
"               <dd><li>Storm ID",                   
"               <dd><li>Delivery Date",
"               <dd><li>Email Addresses",
"               <dd><li>Other optional fields",
"       </td></tr></table>" );
}

sub linkTitle
{
	return "Who Ordered";
}

sub queryForm 
{
	my $self = shift;
	my $title = "Who Ordered Dataset Listing: Query Form";

	# Create the simple form to query storm_ids
	my $form = cr(  
"		<form action=supervisor method=POST onSubmit=\"return checkText( this.storm_id, \'Please Enter a Storm Id.\')\">" , 
"		<table border=0 cellpadding=9 cellspacing=0 bgcolor=#e9e9e9 width=$table_width>" ,	
"			<tr bgcolor=white>" ,
"				<td align=center colspan=2>" ,	
"					<br><b>Enter a list of comma-delimited storm_id numbers (e.g. 77.018,77.019,46.006)</b>",
"					<br><input type=text name=storm_id size=75><br>",
"				</td>" , 
"			</tr>",
"           <tr bgcolor=white>",
"               <td colspan=2>", 
"                   <b>Select which fields to display:</b>",
"               </td>",
"           </tr>" ,
"           <tr bgcolor=white>",
"               <td align=center colspan=2><font size=-1><b>" );

            $form = $form . cr(
"                   <input type=checkbox checked name=fields value=storm_id>$field_titles{\"storm_id\"}", &nbsp(3),
            "" );
            $form = $form . cr(
"                   <input type=checkbox checked name=fields value=del_date>$field_titles{\"del_date\"}", &nbsp(3),
            "" );
            $form = $form . cr(
"                   <input type=checkbox checked name=fields value=email_addr>$field_titles{\"email_addr\"}", &nbsp(3),
            "" );
            $form = $form . cr(
"                   <input type=checkbox unchecked name=fields value=source_fmt>$field_titles{\"source_fmt\"}", &nbsp(3),
            "" );
            $form = $form . cr(
"                   <input type=checkbox unchecked name=fields value=target_fmt>$field_titles{\"target_fmt\"}", &nbsp(3),
            "" );
            $form = $form . cr(
"                   <input type=checkbox unchecked name=fields value=no_kbytes>$field_titles{\"no_kbytes\"}",  &nbsp(3),
"               </td>",
"           </tr>" ,
            "" );

			$form = $form . cr(
"			<tr bgcolor=white>",
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

sub queryResult
{
	my $self = shift;
	my $params = shift;

	if( $params->exists( "email_only" ) && $params->get( "email_only" ) eq "selected" ) {
		@def_fields = ( "email_addr" );
	} elsif( $params->exists( "fields" ) ) {
		@def_fields = $params->get("fields");
	}

	my $field_list = "";
	my $defined_email = 0;
	foreach $item (@def_fields) {
		$field_list = $field_list . "&fields=$item";
		if( $item eq "email_addr" ) {
			$defined_email = 1;
		}
	}

	if( $defined_email == 0 ) {
		$field_list = $field_list . "&fields=email_addr";
		$def_fields[ scalar( @def_fields ) ] = "email_addr";
	}


	my $storm_id = $params->get( "storm_id" );
	my @id_list = split( ",", $storm_id );

	my $result = cr( 
"		<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
"			<tr>" );

	foreach $item ( @def_fields ) {
		$result = $result . cr(
"				<td align=center id=\"sort_link\">") .
"					<b><a href=supervisor?results=$self->{identifier}&storm_id=$storm_id&sort_$self->{identifier}=$item$field_list";
		if( $params->exists( "email_only" ) && $params->get("email_only") eq "selected" ) {
			$result = $result . "&email_only=selected>$field_titles{$item}</a></b>";
		} else {
			$result = $result . ">$field_titles{$item}</a></b>"; 
		}
        	$result = $result . cr(
"				</td>",
		"" );
	}

	$result = $result . "\n\t\t</tr>";

	my $sql_list = "COUNT\(*\)";
	foreach $item ( @def_fields ) {
		if( $sql_list eq "" ) {
			$sql_list = $item;
		} else {
			$sql_list = $sql_list . ", " . $item;
		}
	}
	
	if( $params->exists( "email_only" ) && $params->get( "email_only" ) eq "selected" ) {
		$sql_list = "DISTINCT email_addr";
	}

	my $sort_str = " ORDER BY ";
	if( $params->exists("sort_$self->{identifier}")  ) {
		@sort_optns = $params->get("sort_$self->{identifier}");
		foreach $item ( @sort_optns ) {
			$sort_str = $sort_str . $item;
		}
	} else {
		$sort_str = $sort_str . " storm_id ";
	}

	$sql = "SELECT $sql_list FROM www_delivery_stats WHERE ";

	# Create Hashtable of each search parameter
    my %search;

	$cnt = 0;
	foreach $id ( @id_list ) {
		if( $cnt == 0 ) {
			$sql  = $sql . "storm_id MATCH \'$id\' ";
			$search{ $id } = "f";
		} else {
			$sql = $sql . "OR storm_id MATCH \'$id\' ";
			$search{ $id } = "f";
		}

		$cnt++;
	}

	$sql = $sql . "$sort_str";

	#	print $sql;

		my $query = EQuery->new( "master_db" );
    	$query->query( $sql );

		$removed_cnt = 0;
		while( (my %row = $query->getRow() ) ) {
			if( !( $params->exists( "isJoss" ) && $params->get( "isJoss" ) eq "selected" && isJOSS( $row{ "email_addr" } ) == 1) ) {

				# Mark Hash Table that This ID matches a certain search parameter
				foreach $k ( keys %search ) {
					if( $row{ "storm_id" } =~ /$k/ ) {
						$search{ $k } = "t";
					}
				}

				$result = $result . cr( "           <tr>" );
    			if($row{'COUNT(*)'} eq "") {
					foreach $item ( @def_fields ) {

						#take care of date formating
						if( $item eq "del_date" ) {
							$result = $result . cr("            <td bgcolor=white>" . nbsp(2) . formatDate( $row{$item} ) . "</td>" );
						} else {
   	    					$result = $result . cr("			<td bgcolor=white>" . nbsp(2) . $row{$item} . "</td>" );
						}
   					}
					$result = $result . cr( "           </tr>" );
				} else {
					$final_cnt = $row{ 'COUNT(*)' } - $removed_cnt;

					if( $final_cnt > 0 ) {
						$result = $result . cr( 
"						<td colspan=" . @def_fields . " bgcolor=white><b>" . $final_cnt . " Orders</b></td></tr>",
						"" );

					    foreach $k ( keys %search ) {
							if( $search{ $k } eq "f" ) {
        					$result = $result . cr(
"							<tr><td colspan=" . @def_fields . " bgcolor=white><b>" . $k . " Returned No Results</b></td></tr>",
							"" );
							}
    					}

					} else {
						$result = $result . cr(
"                       <td colspan=" . @def_fields . " bgcolor=white><b><center>Query Matched No Results</center></b></td></tr>",
                        "" );
					}

				}
			} else {
				$removed_cnt++;
			}
		}
	

	#my $query = EQuery->new( "master_db" );
	#$query->query( $sql );

	$result = $result . cr(
"		</table>",
"" );

	my $title = "Who Ordered Dataset Listing: Query Results";

	# Return title and query result
	return( $title, $result );
}


# Simply helper function to format the begin_date and end_date with
#  the corresponding begin and end times.
sub getDateTime
{
	my $field = shift;
	my %row = %{ $_[0] };
	my $date = formatDate( $row{$field} );
	my $hour; my $min;	

	# Get the hour and minute	
	if( $field = "begin_date" )
	{
		$hour = $row{b_hour};
		$min = $row{b_min};	
	}
	else
	{
		$hour = $row{e_hour};
		$min = $row{e_min};	
	}

	# Format the time
	my $time = sprintf( "%2.2d:%2.2d", $hour, $min );

	return $date . &nbsp(3) . $time;
}

# Simple helper function to parse the YYYYMMDD dates CODIAC returns
#  to the easier to read YYYY-MM-DD
sub formatDate
{
	my $date = shift;

	$date = substr( $date, 0, 4 ) . "-" . substr( $date, 4, 2 ) . "-" . substr( $date, 6, 2 );

	return $date;
}

#################################################################
## Instructions for adding more email addresses to the list    ##
## of JOSS Employees that should be removed from the Who       ##
## Ordered listing when the appropriate selection is made      ##
## the user.                                                   ##
##  Below is a sub-routine which uses a 'Perl-hack' version    ##
##  of a switch statement. To add an email address to this     ##
##  list, edit the following lines appropriately and copy      ##
##  them into the body of the 'for' statement enclosing the    ##
##  other email addresses...                                   ##
##                                                             ##
##  # Employee Name                                            ##
##  /user_name\@ucar\.edu/ and do { $retval = 1; last; };      ##
##  /user_name\@joss\.ucar\.edu/ and do { $retval = 1; last }; ##
#################################################################
 
####################################################
## JOSS Employee Email Identification Sub-routine ##
####################################################
sub isJOSS {
    local($email_addr) = @_;
    local($retval);
            
    my $retval = 0;

    for ($email_addr) {
    # Phillip Dressen  
    /drphil\@ucar\.edu/ and do { $retval = 1; last; };
    /drphil\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Dan Sulivan
    /suldan\@ucar\.edu/ and do { $retval = 1; last; };
    /suldan\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Linda Cully
    /cully\@ucar\.edu/ and do { $retval = 1; last; };
    /cully\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Scot Loehrer
    /loehrer\@ucar\.edu/ and do { $retval = 1; last; };
    /loehrer\@joss\.ucar\.edu/ and do { $retval = 1; last; };
 
    # Don Stott
    /stott\@ucar\.edu/ and do { $retval = 1; last; };
    /stott\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Darren Gallant
    /gallant\@ucar\.edu/ and do { $retval = 1; last; };
    /gallant\@joss\.ucar\.edu/ and do { $retval = 1; last; };
    
    # Pat Skinner
    /skinnerp\@ucar\.edu/ and do { $retval = 1; last; };
    /skinnerp\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Joel Clawson
    /jclawson\@ucar\.edu/ and do { $retval = 1; last; };
    /jclawson\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Quinn Daily
    /daily\@ucar\.edu/ and do { $retval = 1; last; };
    /daily\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Richard Bateman
    /bateman\@ucar\.edu/ and do { $retval = 1; last; };
    /bateman\@joss\.ucar\.edu/ and do { $retval = 1; last; };

    # Big tall Drew
    /wilkinsd\@ucar\.edu/ and do { $retval = 1; last; };
    /wilkinsd\@joss\.ucar\.edu/ and do { $retval = 1; last; };
    
    ############### Insert New Addresses Before this Line #####################
    }
    
    return ($retval);
}


1;

