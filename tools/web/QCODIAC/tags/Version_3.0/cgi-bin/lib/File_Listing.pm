#! /usr/bin/perl -w

#
# File_Listing.pm
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

package File_Listing;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "begin_date", "end_date", "purpose", "format", "exists" );

# The field titles to display at the top of the table
my %field_titles = ("begin_date" => "Begin Date",
		    "end_date" => "End Date",
		    "format" => "Data Format",
		    "purpose" => "Purpose",
		    "size_kb" => "Data Amt.",
		    "hide" => "Hidden",
		    "data_archive_date" => "Archive Date",
		    "exists" => "File Exists" );

# The field tags to use in the body of the table
my %field_tags = ("begin_date" => "<td align=center nowrap>",
		  "end_date" => "<td align=center nowrap>",
		  "purpose" => "<td align=center nowrap>",
		  "format" => "<td align=center>",
		  "size_kb" => "<td align=right>",
		  "hide" => "<td align=center>",
		  "data_archive_date" => "<td align=center>",
		  "exists"=>"<td align=center>" );

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "file_list";	
    $self->{group} = "dataset";
    
    return $self;
}

sub shortDesc {
    return "File Listing";
}

sub longDesc {
    return cr("<table border=0 cellpadding=0 cellspacing=0><tr><td>",
	      "Displays the contents of the files table for one or more datasets, output includes: ",
	      "<dd><li>File name and path",
	      "<dd><li>Begin/End Date",
	      "<dd><li>File Existence",
	      "<dd><li>Other optional fields",
	      "</td></tr></table>" );
}

sub linkTitle {
    return "File Listing";
}

sub queryForm {
    my $self = shift;
    my $title = "Files Listing: Query";
    
    # Create the simple form to query storm_ids
    my $form = cr("<form action=supervisor method=POST onSubmit=\"return checkText( this.dataset_id, \'Please Enter a Storm Id.\')\">" , 
		  "<table border=0 cellpadding=4 cellspacing=0 bgcolor=#e9e9e9 width=$table_width>" ,	
		  "<tr bgcolor=white>" ,
		  "<td align=center>" ,	
		  "<br><b>Enter a list of comma-delimited dataset_id numbers (e.g. 77.018,77.019,46.006):</b>" ,
		  "<br><input type=text name=dataset_id size=75><br>",
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
		  "<input type=checkbox checked name=fields value=purpose>Purpose" . &nbsp(3),
		  "<input type=checkbox checked name=fields value=format>Data Format." . &nbsp(3),
		  "<input type=checkbox name=fields value=size_kb>Data Amnt." . &nbsp(3),
		  "<input type=checkbox name=fields value=hide>Hidden" . &nbsp(3),
		  "<input type=checkbox name=fields value=data_archive_date>Archive Date" . &nbsp(3),
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

sub queryResult {
	my $self = shift;

	# Get the parameters
	my $params = shift;
	my $dataset_id = $params->get( "dataset_id" );
	my $begin_date = $params->get( "begin_date" );
	my $end_date = $params->get( "end_date" );
	my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
	my $nfields = @fields+1;	

	# Instantiate the query object
	my $query = EQuery->new();

	# Get all of the storm_ids
	my @list = split( ",", $dataset_id );

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
#		my $sql = "SELECT * FROM file,format WHERE file.format_id=format.format_id AND dataset_id = " . $query->quote( $id ) . " ";
               my $sql = "SELECT file.*,format.* FROM file,format,dataset WHERE file.format_id=format.id AND file.dataset_id=dataset.id AND archive_ident = " . $query->quote( $id ) . " ";

		if( defined( $begin_date ) && $begin_date ne '' )
		{ $sql = $sql . "AND begin_date >= " . $query->quote( $begin_date ) . " "; }

		if( defined( $end_date ) && $end_date ne '' )
		{ $sql = $sql . "AND end_date <= " . $query->quote( $end_date ) . " "; }
 
		$sql = $sql . "ORDER BY directory, begin_date";

		$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>$id:</td></tr>";
		$query->query( $sql );
		my $prev_dir = "";

		# Loop through each row
		while( (my %row = $query->getRow() ) )
		{
			# Check to see if this is a new dir_path, if so display it	
			if( $row{directory} ne $prev_dir )
			{
				$result = $result . cr(
					"<tr bgcolor=white>",
						"<td colspan=$nfields><b>$row{directory}</d></td>",
					"</tr>" );
				$prev_dir = $row{directory};
			}

			# Show the record, only the fields user specified
			$result = $result . cr(
					       "<tr bgcolor=white>",
					       "<td nowrap>", &nbsp(4), "$row{filename}</td>" );
			foreach my $f (@fields)
			{
			    if( defined( $field_titles{$f} ) )
			    {
				if( $f eq "begin_date" || $f eq "end_date" )
				{ $row{$f} = $row{$f}; }
				elsif( $f eq "archive_date" )
				{ $row{$f} = $row{$f}; }
				elsif( $f eq "format" )
				{ $row{$f} = sprintf("%s (%d)",$row{full_name},$row{format_id}); }
				elsif ($f eq "hide" )
				{ $row{$f} = $row{$f} ? "<font color=red>Y</font>" : "<font color=blue>N</font>"; }
				elsif( $f eq "exists" )
				{
				    my $val = "<font color=blue>Y</font>";
				    if( $row{host} eq "mass_store" || $row{host} eq "hpss" ) # Updated to include HPSS
				    { $val = "<font color=red>? HPSS ?</font>" } # Updated to display "? HPSS ?"
				    else
				    { $val = "<font color=red>N</font>" if( ! -e "$row{directory}/$row{filename}" ); }
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
	my $title = "File Listing";

	# Return title and query result
	return( $title, $result );
}

1;

