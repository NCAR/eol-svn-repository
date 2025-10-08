#
# Id_Types.pm
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
#   * Expected parameters:
#     -sort (optional) field by which to sort the table
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Id_Types;
use Utils;
use EQuery;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "id_types";	
	$self->{group} = "general";

	return $self;
}

sub shortDesc 
{
	return "Id Types";
}

sub longDesc 
{
	return "Displays all of the valid observing station identifier types."
}

sub linkTitle
{
	return "Id Types";
}

sub queryForm 
{
	my $self = shift;
	my $params = shift;
	return $self->queryResult( $params );
}

sub queryResult 
{
	my $self = shift;
	my $params = shift;

	my $sort = ( defined( $params ) && $params->exists( "sort" ) )? $params->get( "sort" ) : "code";

	my $query = EQuery->new( "station_db" );

	my $sql = "SELECT * FROM id_type ORDER BY ". $query->quote( $sort );

	$query->query( $sql );

	my $result = cr( "<table border=0 cellpadding=3 cellspacing=2 bgcolor=#e9e9e9 width=$table_width>",
			"<tr>",
				"<td align=center id=\"sort_link\"><b>",
					"<a href=supervisor?qform=$self->{identifier}&sort=code>Code/ID</a></b>",
				"</td>",
				"<td align=center id=\"sort_link\"><b>",
					"<a href=supervisor?qform=$self->{identifier}&sort=desc>Description</a></b>",
				"</td>",
				"<td align=center id=\"sort_link\"><b>",
					"<a href=supervisor?qform=$self->{identifier}&sort=length>Length (#chars)</a></b>",
				"</td>",
			"</tr>" );

	while( (my %row = $query->getRow() ) )
	{
		$result = $result . cr(
			"<tr>",
				"<td bgcolor=white>$row{code}</td>",
				"<td bgcolor=white>$row{desc}</td>",
				"<td bgcolor=white>$row{length}</td>",
			"</tr>" );
	}

	$result = $result . cr( "</table>" );

	my $title = "Id Types";                         

	return ( $title, $result );
}



1;

