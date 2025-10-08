#! /usr/bin/perl -w

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

package Tape_Listing;
use lib ".";
use EQuery;
use URLparams;
use Utils;

# Default fields to display
my @def_fields = ( "begin_date", "end_date", "primary_media_format" );

# The field titles to display at the top of the table
my %field_titles = ("sequence_number" => "Sequence Num.",
		    "begin_date" => "Begin Date",
		    "end_date" => "End Date",
		    "contents_note" => "Data Note",
		    "size_kb" => "Data Amt.",
		    "hide" => "Hidden",
		    "data_archive_date" => "Archive Date",
		    "num_files" => "Number of Files",
		    "primary label" => "Primary Label",
		    "primary_media_format" => "Primary Media Format",
		    "backup_media_format" => "Backup Media Format",
		    "backup_label" => "Backup Label",
		    "dataset_id" => "Dataset ID");



# The field tags to use in the body of the table
my %field_tags = ("sequence_number" => "<td align=center nowrap>",
		  "begin_date" => "<td align=center nowrap>",
		  "end_date" => "<td align=center nowrap>",
		  "contents_note" => "<td><font size=-1>",
		  "size_kb" => "<td align=right>",
		  "hide" => "<td align=center>",
		  "data_archive_date" => "<td align=center>", 
		  "num_files" => "<td align=right>",
		  "primary_media_format" => "<td>",
		  "primary_label" => "<td align=left>",
		  "backup_media_format" => "<td>",
		  "backup_label" => "<td align=left>",
		  "dataset_id" => "<td align=center>");

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "tape_list";	
    $self->{group} = "dataset";
    
    return $self;
}

sub shortDesc {
    return "Tape Listing";
}

sub longDesc {
    return cr("		<table border=0 cellpadding=0 cellspacing=0><tr><td>",
	      "			Displays the contents of the Off Line Physical Directory for one of more datasets, output includes: ",
	      "				<dd><li>Primary ID Number",
	      "				<dd><li>Begin/End Date",
	      "				<dd><li>Primary Media Code",
	      "				<dd><li>Other optional fields",
	      "		</td></tr></table>" );
}

sub linkTitle {
    return "Tape Listing";
}

sub queryForm {
    my $self = shift;
    my $title = "Tape Listing: Query";
    
    # Create the simple form to query storm_ids
    my $form = cr(  
		  "<form action=supervisor method=POST onSubmit=\"return checkMultipleText( [this.storm_id, this.offline_id], \'Please Enter a Storm Id or a Primary/Backup ID Number.\')\">" , 
		  "<table border=0 cellpadding=9 cellspacing=0 bgcolor=#e9e9e9 width=$table_width>" ,	
		  "<tr bgcolor=white>" ,
		  "<td align=center colspan=4>" ,	
		  "<br><b>Enter a list of comma-delimited storm_id numbers (e.g. 77.018,77.019,46.006):</b>" ,
		  "<br><input type=text name=dataset_id size=75><br>",
		  "</td>" , 
		  "</tr>" ,
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
		  "<input type=checkbox name=fields value=sequence_number>Sequence Number<br>" . &nbsp(3),
		  "<input type=checkbox name=fields value=data_archive_date>Archive Date<br>" . &nbsp(3),
		  "					<input type=checkbox name=fields value=hide>Hidden<br>" . &nbsp(3),
		  "				</td>",
		  "				<td align=left valign=top><font size=-1><b>",
		  &nbsp(2) . "Data Information:<br>",
		  &nbsp(4) . "<input type=checkbox name=fields value=contents_note>Data Desc.<br>" . &nbsp(3),
		  "<input type=checkbox name=fields value=size_kb>Data Amnt.<br>" . &nbsp(3),
		  "<input type=checkbox name=fields value=num_files>Number of Files<br>" . &nbsp(3),
		  "               </td>",
		  "               <td align=left valign=top><font size=-1><b>",
		  &nbsp(2) . "Primary Archive Information:<br>",
		  &nbsp(4) . "<input type=checkbox checked name=fields value=primary_media_format>Primary Media Format<br>" . &nbsp(3),
		  "<input type=checkbox name=fields value=primary_label>Primary Label<br>" . &nbsp(3),
		  "               </td>",
		  "               <td align=left valign=top><font size=-1><b>",
		  &nbsp(2) . "Backup Archive Information:<br>",
		  &nbsp(4) . "<input type=checkbox name=fields value=backup_media_format>Backup Media Format<br>" . &nbsp(3),
		  "<input type=checkbox name=fields value=backup_label>Backup Label<br>" . &nbsp(3),
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

sub queryResult {
    my $self = shift;
    
    # Get the parameters
    my $params = shift;
    my $dataset_id = $params->get( "dataset_id" );
    my @fields = ($params->exists( "fields" ))? $params->get( "fields" ) : @def_fields;
    my @tmp;
    my $bu_id_num_found = 0;
    my $c = 1;
    
    my $nfields = @fields+1;	
    
    # Instantiate the query object
    my $query = EQuery->new();
    
    # Get all of the dataset_ids
    my @list = split( ",", $dataset_id );

    
    # Write the table header
    my $result = cr("<table border=0 cellpading=3 cellspacing=1 bgcolor=#e9e9e9 width=$table_width>",
		    "<tr bgcolor=white>",
		    "				<td align=center bgcolor=#e9e9e9><b>Primary Label</b></td>" );
			
			# Write column name for each field	
			foreach my $f (@fields) {
			    if( defined( $field_titles{$f} ) ) {
				$result = $result . cr ( "<td align=center bgcolor=#e9e9e9><b>$field_titles{$f}</b></td>" );
			    }
			}	
    $result = $result . cr(	"</tr>" );
    
    # Query CODIAC for each dataset_id
    foreach my $id (@list) {
	
#	my $sql = "SELECT * FROM off_line_phys_dir WHERE l_dds_id = " . $query->quote( $id ) .
#	    " ORDER BY begin_date, b_hour, b_min";
	my $sql = "SELECT tape.*,format.full_name,original.medium_name as primary_media_format,backup.medium_name as backup_media_format FROM tape,format,medium as original,medium as backup WHERE format_id=format_id AND original.medium_id=primary_medium_id AND backup.medium_id=backup_medium_id AND dataset_id=".$query->quote($dataset_id);

	
	$result = $result . "<tr bgcolor=white><td colspan=$nfields><b>$id:</td></tr>";
	$query->query( $sql );
	my $prev_dir = "";
	
	# Loop through each row
	while( (my %row = $query->getRow() ) ) {
	    # Show the record, only the fields user specified
	    $result = $result . cr("				<tr bgcolor=white>",
				   "					$field_tags{primary_label}" . &nbsp(4) . "$row{primary_label}</td>" );
	    foreach my $f (@fields) {
		if( defined( $field_titles{$f} ) ) {
		    if ($f eq "format") {
			$row{$f} = sprintf("%s (%d)",$row{"full_name"},$row{"format_id"});
		    } elsif ($f =~ /(.+)_medium_format/) {
			$row{$f} = sprintf("%s (%d)",$row{$1."_medium_format"},$row{$1."_medium_id"});
		    } elsif ($f eq "hide") {
			$row{$f} = $row{$f} ? "<font color=red>Y</font>" : "<font color=blue>N</font>";
		    }
		    
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
    my $title = "Tape Listing";
    
    # Return title and query result
    return( $title, $result );
}

1;

