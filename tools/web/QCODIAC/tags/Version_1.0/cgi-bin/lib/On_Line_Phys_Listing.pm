#
# On_Line_Phys_Listing.pm
#  Author: Dan Sullivan
#  Date: 2003-06-23
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
#      -begin_date (optional) - display records greater than this date
#      -end_date (optional) - display records less than this date
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package On_Line_Phys_Listing;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "begin_date", "end_date", "exists" );

# The field titles to display at the top of the table
my %field_titles = (  "begin_date" => "Begin Date",
											"end_date" => "End Date",
											"data_desc" => "Data Desc.",
											"logical_fmt" => "Logical Fmt.",
											"phys_fmt" => "Physical Fmt.",
											"data_amt" => "Data Amt.",
											"hide_ind" => "Hidden",
											"archive_date" => "Archive Date",
											"exists" => "File Exists" );

# The field tags to use in the body of the table
my %field_tags = (  "begin_date" => "<td align=center nowrap>",
											"end_date" => "<td align=center nowrap>",
											"data_desc" => "<td><font size=-1>",
											"logical_fmt" => "<td>",
											"phys_fmt" => "<td align=center>",
											"data_amt" => "<td align=right>",
											"hide_ind" => "<td align=center>",
											"archive_date" => "<td align=center>",
											"exists"=>"<td align=center>" );
sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "on_ln_pd";	
	$self->{group} = "dataset";

	return $self;
}

sub shortDesc 
{
	return "On Line Physical Directory Listing";
}

sub longDesc 
{
	return cr(
"<table border=0 cellpadding=0 cellspacing=0><tr><td>",
"Displays the contents of the On Line Physical Directory for one or more datasets, output includes: ",
"<dd><li>File name and path",
"<dd><li>Begin/End Date",
"<dd><li>File Existence",
"<dd><li>Other optional fields",
"</td></tr></table>" );
}

sub linkTitle
{
	return "On Line Phys Dir";
}

sub queryForm 
{
	my $self = shift;
	my $title = "On Line Physical Directory Listing: Query";

	# Create the simple form to query storm_ids
	my $form = cr(  
		"<form action=supervisor method=POST onSubmit=\"return checkText( this.storm_id, \'Please Enter a Storm Id.\')\">" , 
		"<table border=0 cellpadding=4 cellspacing=0 bgcolor=#e9e9e9 width=$table_width>" ,	
			"<tr bgcolor=white>" ,
				"<td align=center>" ,	
					"<br><b>Enter a list of comma-delimited storm_id numbers (e.g. 77.018,77.019,46.006):</b>" ,
					"<br><input type=text name=storm_id size=75><br>",
				"</td>" , 
			"</tr>" ,
			"<tr bgcolor=white>",
				"<td>",
					"<b>Time resolution (optional):</b>", 
				"</td>",
			"</tr>",
			"<tr bgcolor=white>",
				"<td align=center>",
					"<b>Begin Date <font size=-2></b>(YYYYMMDD)</font><b>: <input type=text size=8 maxlength=8 name=begin_date></b>", &nbsp(3),
					"<b>End Date <font size=-2></b>(YYYYMMDD)</font><b>: <input type=text size=8 maxlength=8 name=end_date>",
				"</td>",
			"</tr>",
			"<tr bgcolor=white>",
				"<td>",	
					"<b>Select which fields to display:</b>",
				"</td>",
			"</tr>" ,
			"<tr bgcolor=white>",
				"<td align=center><font size=-1><b>",
					"<input type=checkbox checked name=fields value=begin_date>Begin Date" . &nbsp(3),
					"<input type=checkbox checked name=fields value=end_date>End Date" . &nbsp(3),
					"<input type=checkbox name=fields value=data_desc>Data Desc." . &nbsp(3),
					"<input type=checkbox name=fields value=logical_fmt>Logical Fmt." . &nbsp(3),
					"<input type=checkbox name=fields value=phys_fmt>Physical Fmt." . &nbsp(3),
					"<input type=checkbox name=fields value=data_amt>Data Amnt." . &nbsp(3),
					"<input type=checkbox name=fields value=hide_ind>Hidden" . &nbsp(3),
					"<input type=checkbox name=fields value=archive_date>Archive Date" . &nbsp(3),
					"<input type=checkbox checked name=fields value=exists>File Exists" . &nbsp(3),
				"</td>",
			"</tr>",
			"<tr bgcolor=white>" ,
				"<td align=right>" ,	
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

	# Get the logical formats
	my %log_fmts = getLogicalFormats();
	my %phys_fmts = getPhysicalFormats();

	# Get the parameters
	my $params = shift;
	my $storm_id = $params->get( "storm_id" );
	my $begin_date = $params->get( "begin_date" );
	my $end_date = $params->get( "end_date" );
	my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
	my $nfields = @fields+1;	

	# Instantiate the query object
	my $query = EQuery->new( "phys_dir_db" );

	# Get all of the storm_ids
	my @list = split( ",", $storm_id );

	# Write the table header
	my $result = cr(
		"<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
			"<tr bgcolor=white>",
				"<td align=center bgcolor=#e9e9e9><b>File Name</td>" );
	
					# Write column name for each field	
					foreach my $f (@fields)
					{
						if( defined( $field_titles{$f} ) )
						{
							$result = $result . cr ( "<td align=center bgcolor=#e9e9e9><b>$field_titles{$f}</td>" );
						}
					}	
		$result = $result . cr(	"</tr>" );

	# Query CODIAC for each storm_id
	foreach my $id (@list)
	{
		my $sql = "SELECT * FROM on_line_phys_dir WHERE l_dds_id = " . $query->quote( $id ) . " ";

		if( defined( $begin_date ) && $begin_date ne '' )
		{ $sql = $sql . "AND begin_date >= " . $query->quote( $begin_date ) . " "; }

		if( defined( $end_date ) && $end_date ne '' )
		{ $sql = $sql . "AND end_date <= " . $query->quote( $end_date ) . " "; }
 
		$sql = $sql . "ORDER BY dir_path, begin_date, b_hour, b_min";

		$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>$id:</td></tr>";
		$query->query( $sql );
		my $prev_dir = "";

		# Loop through each row
		while( (my %row = $query->getRow() ) )
		{
			# Check to see if this is a new dir_path, if so display it	
			if( $row{dir_path} ne $prev_dir )
			{
				$result = $result . cr(
					"<tr bgcolor=white>",
						"<td colspan=$nfields><b>$row{dir_path}</d></td>",
					"</tr>" );
				$prev_dir = $row{dir_path};
			}

			# Show the record, only the fields user specified
			$result = $result . cr(
				"<tr bgcolor=white>",
					"<td nowrap>", &nbsp(4), "$row{file_name}</td>" );
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
							elsif( $f eq "phys_fmt" )
							{ $row{$f} = "$phys_fmts{$row{$f}} ($row{$f})"; }
							elsif( $f eq "exists" )
							{
								my $val = "<font color=blue>Y</font>";
								if( $row{hw_address} eq "mass_store" )
								{ $val = "<font color=red>? MSS ?</font>" }
								else
								{ $val = "<font color=red>N</font>" if( ! -e "$row{dir_path}/$row{file_name}" ); }
								$row{$f} = $val;	
							}
							$result = $result . cr ( "$field_tags{$f}$row{$f}</td>" );
						}
					}	
				
			$result = $result . cr( "</tr>" );
		}

		# Check to see if this was an empty query
		if( $query->getRowCount() != 0 )
		{
			$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".$query->getRowCount()." Files</b></td></tr>";
		}
		else
		{
			$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".&nbsp(3)."No Files to Display</b></td></tr>";
		}
		$result = $result . "<tr bgcolor=white><td colspan=$nfields>&nbsp;</td></tr>";
	}

	$result = $result . "</table>";
	my $title = "On Line Physical Directory Listing";

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
}
1;

