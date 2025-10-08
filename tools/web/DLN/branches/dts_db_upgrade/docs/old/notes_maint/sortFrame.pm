#!/bin/perl 

package sortFrame;

use CGI qw(:standard :html3 );
use DBI;

sub createSortFrame
{
	my $dbh = shift;
	my $file = shift;

	open( OUT, ">$file" ) || die( "Unable to open sortFrame.html" );
	#open( OUT, ">sortFrame.html" ) || die( "Unable to open sortFrame.html" );

	my @proj = getProjects( $dbh );

	showFrame( @proj );

	close( OUT );
}

sub getProjects
{
	my $dbh = shift;
	my $sql = "SELECT id, name FROM Projects ORDER BY name asc";
	my $sth = $dbh->prepare( $sql );
	$sth->execute();
	my @arr;
	my @row;
	my $cnt = 0;
	while( (@row) = $sth->fetchrow() )
	{
		$arr[$cnt] = $row[0];
		$arr[$cnt+1] = $row[1];
		$cnt += 2;
	}
	return @arr;
}

sub showFrame
{
	my @projs = @_;
	#println( *OUT, header() );
	println( *OUT, "<html><head><script language=javascript src=data_notes.js></script>" );
	println( *OUT, "<script language=javascript>" );
		println( *OUT, "function displayTable( form )" );
			println( *OUT, "{" );
				print( OUT "var id = new Array( " );
				for( my $x = 0; $x < @projs; $x+=2 )
				{
					if( $x == (@projs - 2 ) )
					{
						println( *OUT, "\"$projs[$x]\" )" );
					}
					else
					{
						print( OUT "\"$projs[$x]\", " );
					}
				}
				print( OUT "var dis = new Array( " );
				for( my $x = 1; $x < @projs; $x+=2 )
				{
					if( $x == (@projs - 1) )
					{
						println( *OUT, "\"$projs[$x]\" )" );
					}
					else
					{
						print( OUT "\"$projs[$x]\", " );
					}
				}
				println( *OUT, "showTable( form, dis, id ) " );
			println( *OUT, "}" );
	println( *OUT, "</script>" );
	println( *OUT, "</head>" );
	println( *OUT, "<body bgcolor=#FFFFFF alink=#333399 vlink=#333399 link=#333399>" );
	println( *OUT, "<center>" );

	println( *OUT, "<FORM name=\"sortForm\">" );
	println( *OUT, "<table border=0 cellpadding=5 cellspacing=5>" );
	println( *OUT, "<tr>" );
		println( *OUT, "<td align=center><font color=black><b>Project: " );
			println( *OUT, "<select name=\"project\"></b></font>" );
				for( my $x = 0; $x < @projs; $x+=2 )
				{
					println( *OUT, "<option value=$projs[$x]>$projs[$x+1]</option>" );
				}
			println( *OUT, "</select>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td align=center><font color=black><b>Table View: " );
			println( *OUT, "<select name=\"tabletype\" onChange=\"displayTable( this.form );\"></b></font>" );
				println( *OUT, "<option value=2>Long View</option>" );
				println( *OUT, "<option value=1>Wide View</option>" );
			println( *OUT, "</select>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td align=center><font color=black><b>Sort By: " );
			println( *OUT, "<select name=\"sort\"></b></font>" );
				println( *OUT, "<option value=date>Submit Date</option>" );
				println( *OUT, "<option value=storm_id>Storm Id</option>" );
				println( *OUT, "<option value=title>Title</option>" );
				println( *OUT, "<option value=loading>Who's Loading</option>" );
				println( *OUT, "<option value=checker>Checked By</option>" );
				println( *OUT, "<option value=int_contact>Internal Contact</option>" );
				println( *OUT, "<option value=ext_contact>External Contact</option>" );
				println( *OUT, "<option value=ingest>Ingest Location</option>" );
				println( *OUT, "<option value=notes>Notes</option>" );
			println( *OUT, "</select>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td align=center><font color=black><b>Direction: " );
			println( *OUT, "<select name=\"sortdir\"></b></font>" );
				println( *OUT, "<option value=desc>Descending</option>" );
				println( *OUT, "<option value=asc>Ascending</option>" );
			println( *OUT, "</select>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td align=center><font color=black><b>Display: " );
			println( *OUT, "<select name=\"displaycount\"></b></font>" );
				println( *OUT, "<option value=All>All</option>" );
				println( *OUT, "<option value=5>5</option>" );
				println( *OUT, "<option value=10>10</option>" );
				println( *OUT, "<option value=20>20</option>" );
				println( *OUT, "<option value=50>50</option>" );
				println( *OUT, "<option value=75>75</option>" );
				println( *OUT, "<option value=100>100</option>" );
				println( *OUT, "<option value=150>150</option>" );
				println( *OUT, "<option value=200>200</option>" );
				println( *OUT, "<option value=250>250</option>" );
				println( *OUT, "<option value=300>300</option>" );
				println( *OUT, "<option value=350>350</option>" );
				println( *OUT, "<option value=400>400</option>" );
			println( *OUT, "</select>" );
		println( *OUT, "</td> " );
		println( *OUT, "</tr> " );
		println( *OUT, "<td>&nbsp;</td>" );
		println( *OUT, "<td>" );
			println( *OUT, "<input type=checkbox name=hidenotes value=hide></input><b>Hide Notes</b>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td>" );
			println( *OUT, "<input type=checkbox name=hideform value=hide></input><b>Hide Entry Form</b>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td>" );
			println( *OUT, "<input type=checkbox name=breakframes value=hide></input><b>Break out of Frames</b>" );
		println( *OUT, "</td>" );
		println( *OUT, "<td align=center>" );
			println( *OUT, "<input type=submit name=\"Action\" value=\"Show Table\" onClick=\"displayTable( this.form ); return false;\"></input>" );
		println( *OUT, "</td>" );
		println( *OUT, "</tr>" );
println( *OUT, "</table>" );
println( *OUT, "</form></body></html>" );
}

sub connectToDB
{
	return DBI->connect( "DBI:mysql:database=suldan;host=thunder",
												"suldan", "hithere", { RaiseError=>1} ) || die( "Unable to connect to database" );
}

sub println
{
	*OUT = shift;
	my $str = shift;
	print( OUT $str, "\n" );
}
1;
