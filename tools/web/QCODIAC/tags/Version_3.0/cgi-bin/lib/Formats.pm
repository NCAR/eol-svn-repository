#
#  Physical_Fmt.pm
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

package Formats;
use Utils;
use EQuery;

sub new 
{
	my $class = shift;
	my $self = {};
	bless($self, $class);

	$self->{identifier} = "formats";	
	$self->{group} = "general";

	return $self;
}

sub shortDesc 
{
	return "Formats";
}

sub longDesc 
{
	return "Displays all of the valid format types.";
}

sub linkTitle
{
	return "Formats";
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

        my $title = "Physical Formats";                         

	my $sort = ( defined( $params ) && $params->exists( "sort" ) )? $params->get( "sort" ) : "format_id";

	my @physical_fmts = getPhysicalFormats( $sort );

	my $result = cr(
	"",
"	<table border=0 cellpadding=5 cellspacing=2 bgcolor=#e9e9e9 width=75%>",
"		<tr>",
"			<td align=center bgcolor=#e9e9e9 id=sort_link>",
"				<a href=supervisor?qform=$self->{identifier}&sort=format_id><b>ID</b></a>",
"			</td>",
"			<td align=center bgcolor=#e9e9e9 id=sort_link>",
"				<a href=supervisor?qform=$self->{identifier}&sort=short_name><b>Abbreviation</b></a>",
"			</td>",
"			<td align=center bgcolor=#e9e9e9 id=sort_link>",
"				<a href=supervisor?qform=$self->{identifier}&sort=full_name><b>Full Name</b></a>",
"			</td>",
	"" );

	for( my $x = 0; $x < @physical_fmts; $x += 3) {
		my @l_key = $physical_fmts[$x];
    	$result = $result . cr(
"		<tr bgcolor=white>",
"			<td>",
				$physical_fmts[$x],
"			</td>",
"			<td>",
				$physical_fmts[$x + 1],
"			</td>",
"			<td>",
				$physical_fmts[$x + 2],
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

sub getPhysicalFormats
{
	my $sort = shift;

    my @fmts;
                    
    my $eq = EQuery->new();

#	$eq->query( "SELECT * FROM format ORDER BY $sort" );
        $eq->query( "SELECT id as format_id,format.* FROM format ORDER BY $sort" );

    while( (my %row = $eq->getRow()) )
    {
		my @temp = ( $row{format_id}, $row{short_name}, $row{full_name} );
		push( @fmts, @temp );
    }

    return @fmts;
}

1;

