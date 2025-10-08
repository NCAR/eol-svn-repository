#
# Off_Line_Phys_Listing.pm
#  Author: Phillip Dressen
#  Date: 2003-06-09
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
#       seq num, the default is begin_date and end_date, arc_id_num
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Off_Line_Phys_Listing;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "begin_date", "end_date", "arc_media_code" );

# The field titles to display at the top of the table
my %field_titles = (	"seq_num" => "Sequence Num.",
											"begin_date" => "Begin Date",
											"end_date" => "End Date",
											"data_desc" => "Data Desc.",
											"logical_fmt" => "Logical Fmt.",
											"data_amt" => "Data Amt.",
											"hide_ind" => "Hidden",
											"archive_date" => "Archive Date",
											"num_files" => "Number of Files",
											"arc_id_num" => "Primary ID #",
											"arc_media_code" => "Primary Media Code",
											"arc_phys_fmt" => "Primary Physical Format",
											"arc_label_ind" => "Primary Label",
											"bu_id_num" => "Backup ID #",
											"bu_media_code" => "Backup Media Code",
											"bu_phys_fmt" => "Backup Physical Format",
											"bu_label_ind" => "Backup  Label",
											"l_dds_id" => "Storm ID");


#num_files
#arc_id_num
#arc_media_code
#arc_phys_fmt
#arc_label_ind
#bu_id_num
#bu_media_code
#bu_phys_fmt
#bu_lable_ind
#
#
# The field tags to use in the body of the table
my %field_tags = ( 		"seq_num" => "<td align=center nowrap>",
											"begin_date" => "<td align=center nowrap>",
											"end_date" => "<td align=center nowrap>",
											"data_desc" => "<td><font size=-1>",
											"logical_fmt" => "<td>",
											"data_amt" => "<td align=right>",
											"hide_ind" => "<td align=center>",
											"archive_date" => "<td align=center>", 
											"arc_id_num" => "<td align=left>",
											"num_files" => "<td align=right>",
											"arc_id_num" => "<td align=left>",
											"arc_media_code" => "<td>",
											"arc_phys_fmt" => "<td>",
											"arc_label_ind" => "<td align=left>",
											"bu_id_num" => "<td align=left>",
											"bu_media_code" => "<td>",
											"bu_phys_fmt" => "<td>",
											"bu_label_ind" => "<td align=left>",
											"l_dds_id" => "<td align=center>");

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "off_ln_pd";	
	$self->{group} = "dataset";

	return $self;
}

sub shortDesc 
{
	return "Off Line Physical Directory Listing";
}

sub longDesc 
{
	 return cr(
"		<table border=0 cellpadding=0 cellspacing=0><tr><td>",
"			Displays the contents of the Off Line Physical Directory for one of more datasets, output includes: ",
"				<dd><li>Primary ID Number",
"				<dd><li>Begin/End Date",
"				<dd><li>Primary Media Code",
"				<dd><li>Other optional fields",
"		</td></tr></table>" );
}

sub linkTitle
{
	return "Off Line Phys Dir";
}

sub queryForm 
{
	my $self = shift;
	my $title = "Off Line Physical Directory Listing: Query";

	# Create the simple form to query storm_ids
	my $form = cr(  
		"<form action=supervisor method=POST onSubmit=\"return checkMultipleText( [this.storm_id, this.offline_id], \'Please Enter a Storm Id or a Primary/Backup ID Number.\')\">" , 
		"<table border=0 cellpadding=9 cellspacing=0 bgcolor=#e9e9e9 width=$table_width>" ,	
			"<tr bgcolor=white>" ,
				"<td align=center colspan=4>" ,	
					"<br><b>Enter a list of comma-delimited storm_id numbers (e.g. 77.018,77.019,46.006):</b>" ,
					"<br><input type=text name=storm_id size=75><br>",
				"</td>" , 
			"</tr>" ,
"			<tr bgcolor=white>",
"				<td colspan=4>",
"					<center>",
"					<font size=+1><b> OR </b></font>",
"					<br><b>Enter a list of comma-delimited Primary or Backup ID Numbers (e.g. 1464,1468,1466,1470):</b>",
"					<br><input type=text name=offline_id size=75><br>",
"					</center>",
"				</td>",
"			</tr>",
			"<tr bgcolor=white>",
				"<td colspan=4>",	
					"<b>Select which fields to display:</b>",
				"</td>",
			"</tr>",
			"<tr bgcolor=white>",
				"<td align=left valign=top><font size=-1><b>",
					&nbsp(2) . "General Fields:<br>",
					&nbsp(4) . "<input type=checkbox checked name=fields value=begin_date>Begin Date<br>" . &nbsp(3),
					"<input type=checkbox checked name=fields value=end_date>End Date<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=seq_num>Sequence Number<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=archive_date>Archive Date<br>" . &nbsp(3),
"					<input type=checkbox name=fields value=hide_ind>Hidden<br>" . &nbsp(3),
"				</td>",
"				<td align=left valign=top><font size=-1><b>",
					&nbsp(2) . "Data Information:<br>",
					&nbsp(4) . "<input type=checkbox name=fields value=data_desc>Data Desc.<br>" . &nbsp(3),
					"<input type=checkbox name=fields value=data_amt>Data Amnt.<br>" . &nbsp(3),
					"<input type=checkbox name=fields value=num_files>Number of Files<br>" . &nbsp(3),
					"<input type=checkbox name=fields value=logical_fmt>Logical Format<br>" . &nbsp(3),
"               </td>",
"               <td align=left valign=top><font size=-1><b>",
					&nbsp(2) . "Primary Archive Information:<br>",
                    &nbsp(4) . "<input type=checkbox checked name=fields value=arc_media_code>Primary Media Code<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=arc_phys_fmt>Primary Physical Format<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=arc_label_ind>Primary Label<br>" . &nbsp(3),
"               </td>",
"               <td align=left valign=top><font size=-1><b>",
					&nbsp(2) . "Backup Archive Information:<br>",
                    &nbsp(4) . "<input type=checkbox name=fields value=bu_id_num>Backup ID Number<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=bu_media_code>Backup Media Code<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=bu_phys_fmt>Backup Physical Format<br>" . &nbsp(3),
                    "<input type=checkbox name=fields value=bu_label_ind>Backup Label<br>" . &nbsp(3),
"               </td>",
			"</tr>",
			"<tr bgcolor=white>" ,
				"<td align=right colspan=4>" ,	
					"<input type=hidden name=results value=$self->{identifier}>",
					"<input type=submit value=\"Submit Query\">",
				"</td>",
			"</tr>",
		"</form></table>" );

	return( $title, $form );
}

sub queryResult
{
	my $self = shift;

	my %log_fmts = getLogicalFormats();
	my %phys_fmts = getPhysicalFormats();
	my %media_codes = getMediaCodes();

	# Get the parameters
	my $params = shift;
	my $storm_id = $params->get( "storm_id" );
	my $offline_ids = $params->get( "offline_id" );
	my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
	my @tmp;
	my $bu_id_num_found = 0;
	my $c = 1;

	if( $offline_ids ) {
		push( @fields, "l_dds_id", "bu_id_num" );
	}

	foreach my $f (@fields)
	{
		if( $f eq "bu_id_num" )
		{ 
			$tmp[0] = $f; 
			$bu_id_num_found = 1;
		}
		else
		{
			$tmp[$c++] = $f;
		}
	}
	@fields = @tmp if( $bu_id_num_found );
	
	my $nfields = @fields+1;	

	# Instantiate the query object
	my $query = EQuery->new( "phys_dir_db" );

	my @list;
	# Get all of the storm_ids
	if( $offline_ids ) {
		@list = split( ",", $offline_ids );
	} else {
		@list = split( ",", $storm_id );
	}

	# Write the table header
	my $result = cr(
		"<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
			"<tr bgcolor=white>",
"				<td align=center bgcolor=#e9e9e9><b>Primary ID #</b></td>" );
	
					# Write column name for each field	
					foreach my $f (@fields)
					{
						if( defined( $field_titles{$f} ) )
						{
							$result = $result . cr ( "<td align=center bgcolor=#e9e9e9><b>$field_titles{$f}</b></td>" );
						}
					}	
		$result = $result . cr(	"</tr>" );

	# Query CODIAC for each storm_id
	foreach my $id (@list)
	{
		
		my $sql = "SELECT * FROM off_line_phys_dir WHERE l_dds_id = " . $query->quote( $id ) .
							" ORDER BY begin_date, b_hour, b_min";
		if( $offline_ids ) {
			$sql = "SELECT * FROM off_line_phys_dir WHERE arc_id_num MATCH " . $query->quote( $id ) .
					" OR bu_id_num MATCH " . $query->quote( $id ) . 
					" ORDER BY begin_date, b_hour, b_min";
		}

		$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>$id:</td></tr>";
		$query->query( $sql );
		my $prev_dir = "";

		# Loop through each row
		while( (my %row = $query->getRow() ) )
		{
			# Show the record, only the fields user specified
			$result = $result . cr(
"				<tr bgcolor=white>",
"					$field_tags{arc_id_num}" . &nbsp(4) . "$row{arc_id_num}</td>" );
					foreach my $f (@fields)
					{
						if( defined( $field_titles{$f} ) )
						{
							if( $f eq "begin_date" || $f eq "end_date" )
							{ $row{$f} = getDateTime( $f, \%row ); }
							elsif( $f eq "archive_date" )
							{ $row{$f} = formatDate( $row{$f} ); }
							elsif( $f eq "logical_fmt" )
							{ $row{$f} = "$log_fmts{$row{$f}} ($row{$f})"; }
							elsif( $f eq "arc_phys_fmt" || $f eq "bu_phys_fmt" )
							{ $row{$f} = "$phys_fmts{$row{$f}} ($row{$f})"; }
							elsif( $f eq "arc_media_code" || $f eq "bu_media_code" )
							{ $row{$f} = "$media_codes{$row{$f}} ($row{$f})"; }

							$result = $result . cr ( "$field_tags{$f}$row{$f}</td>" );
						}
					}	
				
			$result = $result . cr( "</tr>" );
		}

		# Check to see if this was an empty query
		if( $query->getRowCount() != 0 )
		{
			$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".$query->getRowCount()." Offline Resources</b></td></tr>";
		}
		else
		{
			$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".&nbsp(3)."No Offline Resources to Display</b></td></tr>";
		}
		$result = $result . "<tr bgcolor=white><td colspan=$nfields>&nbsp;</td></tr>";
	}

	$result = $result . "</table>";
	my $title = "Off Line Physical Directory Listing";

	# Return title and query result
	return( $title, $result );
}


# Simple helper function to format the begin_date and end_date with
#  the corresponding begin and end times.
sub getDateTime
{
	my $field = shift;
	my %row = %{ $_[0] };
	my $date = formatDate( $row{$field} );
	my $hour; my $min;	

	# Get the hour and minute	
	if( $field eq "begin_date" )
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

sub getLogicalFormats
{
	my %fmts;

	my $eq = EQuery->new( "phys_dir_db" );
	$eq->query( "SELECT * FROM logical_format" );

	while( (my %row = $eq->getRow()) )
	{
		$fmts{$row{logical_fmt}} = $row{fmt_name};
	} 

	return %fmts;
}

sub getPhysicalFormats
{
	my %fmts;

	my $eq = EQuery->new( "data_dict_db" );
	$eq->query( "SELECT * FROM physical_format" );

	while( (my %row = $eq->getRow()) )
	{
		$fmts{$row{phys_fmt}} = $row{format_name};
	}

	return %fmts;
}

sub getMediaCodes
{
	my %codes;

	my $eq = EQuery->new( "data_dict_db" );
	$eq->query( "SELECT * FROM media_code" );

	while( (my %row = $eq->getRow()) )
	{
		$codes{$row{media_code}} = $row{media_name};
	}
	return %codes;
}
1;

