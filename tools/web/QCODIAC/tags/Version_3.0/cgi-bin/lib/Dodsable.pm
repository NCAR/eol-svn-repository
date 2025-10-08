#
# Dodsable.pm
#  Author: Joel Clawson
#  Date: 2005-04-25
#

#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# new()
#   * Creates and instance of the Dodsable tool
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

package Dodsable;
use lib ".";
use EQuery;
use URLparams;
use Utils;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "dodsable";	
	$self->{group} = "dataset";

	return $self;
}

sub shortDesc 
{
	return "Dods Accessable Listing";
}

sub longDesc 
{
	return cr(
"<table border=0 cellpadding=0 cellspacing=0><tr><td>",
"Displays the contents of the Dodsable table. ",
"</td></tr></table>" );
}

sub linkTitle
{
	return "Dods Accessable";
}

sub queryForm 
{
	my $self = shift;
	my $title = "Dods Accessable Listing: Query";

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

	# Get the parameters
	my $params = shift;
	my $storm_id = $params->get( "storm_id" );

	# Instantiate the query object
	my $query = EQuery->new( "catalog_db" );

	# Get all of the storm_ids
	my @list = split( ",", $storm_id );

	# Write the table header
	my $result = cr(
		"<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
			"<tr bgcolor=white>",
				"<td align=center bgcolor=#e9e9e9><b>Storm Id</td>",
				"<td align=center bgcolor=#e9e9e9><b>Dodsable</td>" );
		$result = $result . cr(	"</tr>" );

	# Query CODIAC for each storm_id
	foreach my $id (@list)
	{
		my $sql = "SELECT * FROM dodsable WHERE storm_id = " . $query->quote( $id ) . " ";

		$sql = $sql . "ORDER BY storm_id";

		$query->query( $sql );


		# Check to see if this was an empty query
		if( $query->getRowCount() == 0 )
		{
			$result = $result . "<tr bgcolor=white><td><b>$id</b></td><td><font color=red><b>No</b></font></td></tr>";
		}
		else
		{
			$result = $result . "<tr bgcolor=white><td><b>$id</b></td><td><font color=blue><b>Yes</b></font></td></tr>";
		}
	}

	$result = $result . "</table>";
	my $title = "Dods Accessable Listing";

	# Return title and query result
	return( $title, $result );
}

1;

