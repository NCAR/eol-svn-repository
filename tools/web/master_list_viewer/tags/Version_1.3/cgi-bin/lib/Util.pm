#!/usr/bin/perl -w

package Util;

use CGI qw(:standard :html3 );
require Exporter;
use vars qw(@ISA @EXPORT $VERSION );


@ISA = qw(Exporter);
@EXPORT = qw( $td $td2 $td3 $tde $tr $tre $th $the $my_url &println &trim &nbsp $unknown_id %mode_col %mode_font %mode_title &replaceSpace %color_scheme &css);

# Some html tags
$td = "<td>";
$td1 = "<td align=left>";
$td2 = "<td align=center>";
$td3 = "<td align=right>";
$tde = "</td>";
$tr = "<tr>";
$tre = "</tr>";
$th = "<th>";
$the = "</th>";


# The url of this script
$my_url = "http://dmg.eol.ucar.edu/cgi-bin/master_list/master_maint";

sub println
{
	print( @_, "\n" );
}

sub trim
{
	local $str = $_[0];
	$str =~ s/^\s+//;
	$str =~ s/\s+$//;
	return $str;
}

sub nbsp
{
	my $cnt = shift;
	my $str = "";
	for( my $x = 0; $x < $cnt; $x++ )
	{
		$str = $str . "&nbsp;";
	}

	return $str;
}

$unknown_id = 1;

%mode_col = ( "add", "#FFFFFF",
							"update", "#FFFFCC",
							"added", "#FFFFFF" );

%mode_font = ( "add", "#000000",
							"update", "#000000",
							"added", "red" );

%mode_title = ( "add", "Add New Dataset",
								"update", "Update Dataset",
								"added", "Dataset Has Been Added" );

sub replaceSpace
{
	my $str = shift;
	@arr = split( / /, $str );
	$str = join( "+", @arr );
	return $str;
}

%color_scheme = ( "title", "black",
									"text", "black",
									"link", "blue",
									"alink", "#333399",
									"vlink", "#990099",
									"headers", "black",
									"title_bg", "white",
									"head_bg", "white",
									"background", "white" ); 

sub css
{
	my $project = shift;
	#my $dir = "http://www.joss.ucar.edu/dpg/arctic_ml/css";
	my $dir = "http://data.eol.ucar.edu/master_list/arctic/css";
	my $file = "$project.css";
	$file = "DEFAULT.css" if( ! -e "/export/web/data/webapps/master_list/arctic/css/$file" );	

	$file = "$dir/$file";

	return "<link rel=\"STYLESHEET\" type=\"text/css\" href=\"$file\">";
}
1;
