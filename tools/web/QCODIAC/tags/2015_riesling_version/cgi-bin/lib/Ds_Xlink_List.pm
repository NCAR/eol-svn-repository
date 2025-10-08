#! /usr/bin/perl -w

#
# Ds_Xlink_List.pm
#  Author: Amanda Orin
#  Date: 2012-06-08
#

#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# new()
#   * Creates and instance of the Ds_Xlink_List Tool
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

package Ds_Xlink_List;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "href", "title", "purpose" );

# The field titles to display at the top of the table
my %field_titles = ("xlink_id" => "ID",
		    "href" => "Href",
		    "title" => "Title",
		    "purpose" => "Purpose",
		    "hide" => "Hidden" );

# The field tags to use in the body of the table
my %field_tags = ( "xlink_id" => "<td align=center nowrap>", 
		 "href" => "<td align=center>", 
		 "title" => "<td align=center>", 
		 "purpose" => "<td align=center nowrap>", 
		 "hide" => "<td align=center>" );

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "ds_xlink_list";	
    $self->{group} = "dataset";
    
    return $self;
}

sub shortDesc {
    return "External Link List";
}

sub longDesc {
    return cr("<table border=0 cellpadding=0 cellspacing=0><tr><td>",
	      "Displays the contents of the xlinks table for one or more datasets, output includes: ",
	      "<ul><li>Href and Title</li>",
	      "<li>Purpose</li>",
	      "<li>Other optional fields</li>",
	      "</ul></td></tr></table>" );
}

sub linkTitle {
    return "External Link List";
}

sub queryForm {
    my $self = shift;
    my $title = "Dataset External Link List: Query";
    
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
		  "<b>Select which fields to display:</b>",
		  "</td>",
		  "</tr>" ,
		  "<tr bgcolor=white>",
		  "<td align=center><font size=-1><b>",
		  "<input type=checkbox name=fields value=xlink_id>ID" . &nbsp(3),
		  "<input type=checkbox checked name=fields value=href>Href" . &nbsp(3),
		  "<input type=checkbox checked name=fields value=title>Title." . &nbsp(3),
		  "<input type=checkbox checked name=fields value=purpose>Purpose" . &nbsp(3),
		  "<input type=checkbox name=fields value=hide>Hidden" . &nbsp(3),
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
	my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
	my $nfields = @fields+2;	

	# Instantiate the query object
	my $query = EQuery->new();

	# Get all of the storm_ids
	my @list = split( /,[\s\x20]*/, $dataset_id );

	# Write the table header
	my $result = cr(
		"<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
			"<tr bgcolor=white>");#,
#				"<td align=center bgcolor=#e9e9e9><b>External Link Name</td>" );
	
					# Write column name for each field	
					foreach my $f (@fields)
					{
						if( defined( $field_titles{$f} ) )
						{
							$result = $result . cr ( "<td align=center bgcolor=#e9e9e9><b>$field_titles{$f}</b></td>" );
						}
					}	
		$result = $result . cr(	"</tr>" );

	# Query CODIAC for each dataset_id
	foreach my $id (@list)
	{
		my $sql = "SELECT xlink.id as xlink_id, xlink.* FROM xlink,dataset_xlink,dataset WHERE dataset_xlink.xlink_id=xlink.id AND dataset_xlink.dataset_id=dataset.id AND archive_ident = " .$query->quote( $id ) . " ";

		$sql = $sql . "ORDER BY xlink.href";

		$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>$id:</b></td></tr>";
		$query->query( $sql );
		my $prev_dir = "";

		# Loop through each row
		while( (my %row = $query->getRow() ) )
		{
			# Show the record, only the fields user specified
			$result = $result . cr(
					       "<tr bgcolor=white>");#,
					       #"<td nowrap>", &nbsp(4), "$row{id}</td><td nowrap>", &nbsp(4), 
					       #"<a href='$row{href}'>$row{href}</a></td>" );
			foreach my $f (@fields)
			{
			    if( defined( $field_titles{$f} ) )
			    {
			    	if( $f eq "xlink_id" )
			    	{ $row{$f} = "<a href='https://data.eol.ucar.edu/zinc/xlink/show/$row{$f}'>$row{$f}</a>"; }
				    elsif( $f eq "href" )
				    { $row{$f} = "<a href='$row{$f}'>$row{$f}</a>"; }
					elsif( $f eq "title" )
					{ $row{$f} = $row{$f}; }
					elsif( $f eq "purpose" )
					{ $row{$f} = $row{$f}; }
					elsif ($f eq "hide" )
					{ $row{$f} = $row{$f} ? "<font color=red>Y</font>" : "<font color=blue>N</font>"; }
					
					$result = $result . cr ( "$field_tags{$f}$row{$f}</td>" );
			    }
			}	
			
			$result = $result . cr( "</tr>" );
		}

		# Check to see if this was an empty query
		if( $query->getRowCount() != 0 )
		{
			if ( $query->getRowCount() == 1 ) {
				$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".$query->getRowCount()." External Link</b></td></tr>";
			} else {
				$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".$query->getRowCount()." External Links</b></td></tr>";
			}
		}
		else
		{
			$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>".&nbsp(3)."No External Links to Display</b></td></tr>";
		}
		$result = $result . "<tr bgcolor=white><td colspan=$nfields>&nbsp;</td></tr>";
	}

	$result = $result . "</table>";
	my $title = "Dataset: External Links";

	# Return title and query result
	return( $title, $result );
}

1;

