#
# Contacts.pm
#  Author: Dan Sullivan
#  Date: 2003-06-26
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
#   * Expected params:
#      -user_id - user_id in database (e.g. dSullivan)
#      -affiliation - of user
#      -first_name, last_name - of user
#        -These four are optional, can give one, two, three,
#          all or none.  If non, then all users are displayed.
#      -sort - the field by which to sort by, user_id,
#        name, affiliation, email_addr.
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Contacts;
use Utils;
use EQuery;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "contact";	
	$self->{group} = "general";

	return $self;
}

sub shortDesc 
{
	return "Contacts/Users";
}

sub longDesc 
{
    return cr(
"       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
"           Displays basic information for a contact, searching options and output include:", 
"               <dd><li>User ID",
"               <dd><li>Name (First and Last)",
"               <dd><li>Affiliation",
"       </td></tr></table>" );

	return "Allows you to query CODIAC's user table by user id, first name and last name.";
}

sub linkTitle
{
	return "Contacts/Users";
}

sub queryForm 
{
	my $self = shift;

	my $form = cr(
		"<form method=POST action=supervisor>",
		"<table border=0 cellpadding=8 cellspacing=0 bgcolor=white width=$table_width>",
				"<tr>",
					"<td colspan=3>&nbsp;</td>",
				"<tr>",
					"<td colspan=3><b>Enter one or more of the following fields, can be a regular expression:</td>",
				"</tr>",
				"<tr>",
					"<td width=10% nowrap align=right>",
						"<b>User Id: </b>",
					"</td>",
					"<td colspan=2>",
						"<input type=text size=20 name=user_id>",	
					"</td>",
				"<tr>",
					"<td width=10% nowrap align=right>",
						"<b>First Name: </b>",
					"</td>",	
					"<td colspan=2>",
						"<input type=text size=20 name=first_name>", &nbsp(4),	
						"<b>Last Name: </b><input type=text size=20 name=last_name>",
					"</td>",
				"</tr>",
				"<tr>",
					"<td align=right><b>Affliation: </b>",
					"<td colspan=2><input type=text size=30 name=affiliation></td>",	
				"</tr>",
				"<tr>",
					"<td colspan=2>",
						&nbsp(5), "<font size=-1><b>To view ALL users/contacts leave all fields blank and click Submit Query.",
					"</td>",
					"<td align=right>",
							"<input type=hidden name=results value=$self->{identifier}>",
							"<input type=submit name=query value=\"Submit Query\">",
						"</td>",
				"</tr>",
				"<tr>",
				"</tr>",
			"</form></table>" );
		

	my $title = "Contacts/Users: Query";
	return ( $title, $form );
}

sub queryResult 
{
	my $self = shift;
	my $params = shift;
	my $query = EQuery->new( "master_db" );

	my $user_id = $params->exists( "user_id" ) ? $params->get( "user_id" ) : "";
	my $first_name = $params->exists( "first_name" ) ? $params->get( "first_name" ) : "";
	my $last_name = $params->exists( "last_name" ) ? $params->get( "last_name" ) : "";
	my $affiliation = $params->exists( "affiliation" ) ? $params->get( "affiliation" ) : "";
	my $sort = $params->exists( "sort" )? $params->get( "sort" ) : "user_id";

	my $sql = "SELECT * FROM \'user\' WHERE 1=1 ";
	$sql = $sql . "AND user_id MATCH ". $query->quote( $user_id ) if( $user_id ne "" );
	$sql = $sql . "AND first_name MATCH ". $query->quote( $first_name ) if( $first_name ne "" );
	$sql = $sql . "AND last_name MATCH ". $query->quote( $last_name ) if( $last_name ne "" );
	$sql = $sql . "AND affiliation MATCH ". $query->quote( $affiliation ) if( $affiliation ne "" );

	$sql = $sql . "ORDER BY $sort";
	$query->query( $sql );

	my $sort_url = "supervisor?results=$self->{identifier}";
	$sort_url = $sort_url . "&user_id=". &escape($user_id) if( $user_id ne "" );
	$sort_url = $sort_url . "&first_name=". &escape($first_name) if( $first_name ne "" );
	$sort_url = $sort_url . "&last_name=". &escape($last_name) if( $last_name ne "" );
	$sort_url = $sort_url . "&affiliation=". &escape($affiliation) if( $affiliation ne "" );

	my $result = cr(
		"<table border=0 cellpadding=2 cellspacing=2 bgcolor=#e9e9e9 width=$table_width>",
			"<tr>",
				"<td align=center id=sort_link>",
					"<a href=$sort_url&sort=user_id>",
					"<b>User Id</a></b>",
				"</td>",
				"<td align=center id=sort_link>",
					"<a href=$sort_url&sort=last_name>",
					"<b>Name</b>",
				"</td>",
				"<td align=center id=sort_link>",
					"<a href=$sort_url&sort=affiliation>",
					"<b>Affiliation</b>",
				"</td>",
				"<td align=center id=sort_link>",
					"<a href=$sort_url&sort=email_addr>",
					"<b>E-mail</b>",
				"</td>",
			"</tr>" );

	while( (my %row = $query->getRow() ) )
	{
		$result = $result . cr(
			"<tr bgcolor=white>",
				"<td>$row{user_id}</td>",
				"<td>", &formatName( \%row ), "</td>",
				"<td>$row{affiliation}</td>",
				"<td>$row{email_addr}</td>",
			"</tr>" );
	}

	$result = $result . cr(
			"<tr><td colspan=4><b>Users Found: ", $query->getRowCount(), "</td></tr>",
		"</table>" );	
			
	my $title = "Contacts/Users";
	return ( $title, $result );
}

sub formatName
{
	my %row = %{ $_[0] };

	my $name = "$row{last_name}, $row{first_name} $row{middle_name}";

	return $name;
}

1;

