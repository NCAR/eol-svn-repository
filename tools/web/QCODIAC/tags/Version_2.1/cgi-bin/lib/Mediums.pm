#! /usr/bin/perl -w

#
#  Media_Code.pm
#  Author: Phillip Dressen
#  Date: 2003-07-25
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
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Mediums;
use Utils;
use EQuery;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "mediums";	
    $self->{group} = "general";
    
    return $self;
}

sub shortDesc {
    return "Mediums";
}

sub longDesc {
    return "Displays all of the valid media codes.";
}

sub linkTitle{
	return "Mediums";
}

sub queryForm {
    my $self = shift;
    my $params = shift;
    return $self->queryResult( $params );
}

sub queryResult {
    my $self = shift;
    my $params = shift;

    my $title = "Medium Types";                         

    my $sort = ( defined( $params ) && $params->exists( "sort" ) )? $params->get( "sort" ) : "medium_id";

	my @media_codes = getMediaCodes( $sort );

	my $result = cr("",
			"	<table border=0 cellpadding=5 cellspacing=2 bgcolor=#e9e9e9 width=75%>",
			"		<tr>",
			"			<td align=center bgcolor=#e9e9e9 id=sort_link>",
			"				<a href=supervisor?qform=$self->{identifier}&sort=medium_id><b>Medium Id</b></a>",
			"			</td>",
			"			<td align=center bgcolor=#e9e9e9 id=sort_link>",
			"				<a href=supervisor?qform=$self->{identifier}&sort=medium_name><b>Medium Name</b></a>",
			"			</td>",
			"" );

    for( my $x = 0; $x < @media_codes; $x += 2) {
	my @l_key = $media_codes[$x];
    	$result = $result . cr("		<tr bgcolor=white>",
			       "			<td>",
			       $media_codes[$x],
			       "			</td>",
			       "			<td>",
			       $media_codes[$x + 1],
			       "			</td>",
			       "		</tr>",
			       "" );
	
    }
    
    $result = $result . cr(
			   "",
			   "	</table>",
			   "");
    
    return ( $title, $result );
}

sub getMediaCodes {
    my $sort = shift;
    
    my @fmts;
                    
    my $eq = EQuery->new();

    $eq->query( "SELECT * FROM medium ORDER BY $sort" );

    while( (my %row = $eq->getRow()) ) {
	my @temp = ( $row{medium_id}, $row{medium_name} );
	push( @fmts, @temp );
    }
    
    return @fmts;
}

1;

