#! /usr/bin/perl -w

#
# Contacts.pm
#  Author: Dan Sullivan
#  Date: 2003-06-26
#
#  Rev. 11/9/2015
#  Fix whitespace and remove dead code
#  - Eric Dattore
#
#  Clean up code - 10/2020
#  J. Scannell
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

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);

    $self->{identifier} = "contact";
    $self->{group} = "general";

    return $self;
}

sub shortDesc {
    return "Contacts/Users";
}

sub longDesc {
    return cr(
       "<table border=0 cellpadding=0 cellspacing=0><tr><td>",
       "Displays basic information for a contact, searching options and output include:",
       "<dd><li>User ID</li></dd>",
       "<dd><li>Name (First and Last)</li></dd>",
       "<dd><li>Affiliation</li></dd>",
       "</td></tr></table>" );

    return "Allows you to query CODIAC's user table by user id, first name and last name.";
}

sub linkTitle {
    return "Contacts/Users";
}

sub queryForm {
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
        "<input type=text size=20 name=contact_short_name>",
        "</td>",
        "<tr>",
        "<td width=10% nowrap align=right>",
        "<b>Person Name: </b>",
        "</td>",
        "<td colspan=2>",
        "<input type=text size=50 name=person_name>", &nbsp(4),
        "</tr>",
        "<tr>",
        "<td align=right><b>Organization: </b>",
        "<td colspan=2><input type=text size=30 name=org_name></td>",
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

sub queryResult {
    my $self = shift;
    my $params = shift;
    my $query = EQuery->new(  );

    my $contact_short_name = $params->exists( "contact_short_name" ) ? $params->get( "contact_short_name" ) : "";
    my $person_name = $params->exists( "person_name" ) ? $params->get( "person_name" ) : "";
    my $org_name = $params->exists( "org_name" ) ? $params->get( "org_name" ) : "";
    my $sort = $params->exists( "sort" )? $params->get( "sort" ) : "short_name";
    my $primary_name = $params->exists("primary_name") ? $params->get("primary_name") : "";

    my $sql = "SELECT * FROM contact WHERE 1=1 ";
    $sql = $sql . "AND short_name LIKE ".$query->quote("%".$contact_short_name."%") if( $contact_short_name ne "" );
    $sql = $sql . "AND person_name LIKE ".$query->quote("%".$person_name."%") if( $person_name ne "" );
    $sql = $sql . "AND organization_name LIKE ".$query->quote("%".$org_name."%") if( $org_name ne "" );

    $sql = $sql . "ORDER BY $sort";
    $query->query($sql);

    my $sort_url = "supervisor?results=$self->{identifier}";
    $sort_url = $sort_url . "&contact_short_name=". &escape($contact_short_name) if( $contact_short_name ne "" );
    $sort_url = $sort_url . "&person_name=". &escape($person_name) if( $person_name ne "" );
    $sort_url = $sort_url . "&org_name=". &escape($org_name) if( $org_name ne "" );
    $sort_url = $sort_url . "&primary_name=" . &escape($primary_name) if($primary_name ne "");

    my $result = cr("<table border=0 cellpadding=2 cellspacing=2 bgcolor=#e9e9e9 width=$table_width>",
        "<tr>",
        "<td align=center id=sort_link>",
        "<a href=$sort_url&sort=id>",
        "<b>ID</b></a>",
        "</td>",
        "<td align=center id=sort_link>",
        "<a href=$sort_url&sort=short_name>",
        "<b>User Id</a></b>",
        "</td>",
        "<td align=center id=sort_link>",
        "<a href=$sort_url&sort=person_name>",
        "<b>Name</b>",
        "</td>",
        "<td align=center id=sort_link>",
        "<a href=$sort_url&sort=primary_name>",
        "<b>Primary Name</b></a>",
        "</td>",
        "<td align=center id=sort_link>",
        "<a href=$sort_url&sort=organization_name>",
        "<b>Organization Name</b>",
        "</td>",
        "<td align=center id=sort_link>",
        "<a href=$sort_url&sort=email>",
        "<b>E-mail</b>",
        "</td>",
        "</tr>" );

    while( (my %row = $query->getRow() ) ) {
        $result = $result . cr("<tr bgcolor=white>",
            "<td>$row{id}</td>",
            "<td>$row{short_name}</td>",
            "<td>$row{person_name}</td>",
            "<td>$row{primary_name}</td>",
            "<td>$row{organization_name}</td>",
            "<td>$row{email}</td>",
            "</tr>" );
    }

    $result = $result . cr("<tr><td colspan=4><b>Users Found: ", $query->getRowCount(), "</td></tr>",
        "</table>" );

    my $title = "Contacts/Users";
    return ( $title, $result );
}

1;

