#!/usr/bin/perl  

#--------------------------------------------------------------------------
# This is the final version of the mk_tables2.pl script that creates
# the tables for the Ivotuk CD.  This script can create the full tables
# and the compressed tables.  Some code must be modified to switch between
# the two. This s/w also makes the separate sub-tables for each site, group,
# discipline, etc. Set the names in the Row.pm and Util.pm hashes for these.
#
# Author: Dan Sullivan
# Date:   Summer, 2002
# rev:    aug 27, 2004, joel clawson
#         Names of sites changed for Seward CD; sitegroup column added.
# rev:    apr 05, 2005, ds
#         New sites added to Table.pm and Util pm; debug variable added. 
#         Note: If code crashes while running, it is most likely caused
#               by a site not listed in both Table.pm and Util.pm. Turn 
#               on the DEBUG variable and run on the table type (e.g. site)
#               which crashed, looking at the last few lines of the log 
#               file for the unlisted site.
#--------------------------------------------------------------------------


use lib ".";
use Table;
use Row;
use Util;

$DEBUG = 0;

$full_table = 1;            # for Seward, only do full tables

$in_file = $ARGV[0];
$out_dir = $ARGV[1];
$type = $ARGV[2];

# for the full tables
if ($full_table == 1) {
    $btn = "Click to Combine Duplicates";
    $txt = "All Records Showing";
    $file_ext = "_full";
    $link_ext = "";
} elsif ($full_table == 0) {
    # for the compressed tables
    $btn = "Click to Show All";
    $txt = "Duplicate Records Combined";
    $file_ext = "";
    $link_ext = "_full";
} else {
    die "need to set value for full_table either as 0 or 1\n";
}

#$cookie = "window.document.cookie='table_view=all\'";
#$cookie = "window.document.cookie='table_view=compress\'";

# Sort fields
#$fields = $sort_fields{$type};         # for sub tables only
$fields = $sort_fields_orig{$type};     # for all tables only

%create = ( "disc" => \&createDisciplines,
            "site" => \&createSites,
            "group" => \&createGroups,
            "date" => \&createDates,
	        "sitegroup" => \&createSiteGroups );

$name_tp = $type;
if( $type eq "disc" ) { $name_tp = "discipline"; }
 
$out = $out_dir . "/" . $name_tp . "s" . $file_ext . ".htm";
open( INPUT, "<$in_file" ) || die( "cannot open $in_file" );
open( OUT, ">$out" ) || die( "cannot open $out_file" );

do
{
    $dummy = <INPUT>;
    print( OUT $dummy ) if($DEBUG);
    $dummy = trim( $dummy );
}
while( $dummy ne "</TR>" );

$link = $name_tp . "s" . $link_ext . ".htm";
writeHeader_big( *OUT, $titles_big{$type}, $link );

$first = 1;
my $table = Table->new();

while( !eof( INPUT ) )
{
    my $row = Row->new();


    $dummy = <INPUT>;
    $dummy = trim( $dummy );
    if( $dummy eq "</TABLE>" ) { last; }

    $s2 = "";
    $s1 = <INPUT>;      #group
    $s1 = trim( $s1 );
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );
        if( index($s2,"<TD") != 0 ) { $s1 = $s1 . " " . $s2;    }
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{group}[0] = $s1;

    printf("Group: %s\n", $row->{group}[0]) if($DEBUG);

    $s1 = $s2;          # members
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "<TD" ) != 0 ) { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{members}[0] = $s1;

    $s1 = $s2;          # dataset
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "<TD" ) != 0 )  { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{dataset} = $s1;

    $s1 = $s2;          # date
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "<TD" ) != 0 )  { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{date}[0] = $s1;

    $s1 = $s2;          # site
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "<TD" ) != 0 )  { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{site}[0] = $s1;

    $s1 = $s2;          # site group
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "<TD" ) != 0 )  { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{sitegroup}[0] = $s1;

    $s1 = $s2;          # discipline
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "<TD" ) != 0 )  { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "<TD" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{disc}[0] =  $s1;

    $s1 = $s2;          # doc
    do
    {
        $s2 = <INPUT>;
        $s2 = trim( $s2 );

        if( index( $s2, "</TR>" ) != 0 )    { $s1 = $s1 . " " . $s2; } 
    }
    while( index( $s2, "</TR>" ) != 0 );
    $s1 = rm_td2( trim( $s1 ) );
    $row->{readme} = $s1;

    printf("%s %s %s %s %s %s\n", $row->{group}[0], $row->{contact}[0], $row->{dataset}[0], $row->{date}[0], $row->{site}[0], $row->{sitegroup}[0]) if($DEBUG);

    $table->addRow( $row );

}

$table->sort( $fields );
$table->showTable( *OUT );
println( *OUT, "</table></body></html>" );

close( INPUT );
close( OUT );

$fields = $sort_fields{$type};          # for sub tables only
$create{$type}->( $table );

sub rm_td
{
    local $str = shift;
    $len = length( $str );
    $str = substr( $str, 4, $len - 8 );

    return $str;
}

sub rm_td2
{
    local $str = shift;
    $len = length( $str );
    $str = substr( $str, 17, $len - 21 );

    return $str;
}

sub trim
{
    local $str = $_[0];
    chop( $str );

    $str =~ s/^\s+//;   
    $str =~ s/\s+$//;
    return $str;
}


sub getTitle
{
    local $str = shift;

    local $pos = index( $str, ">" );
    local $pos2 = index( $str, "<", $pos );
    local $len = $pos2 - $pos - 1;

    $str = substr( $str, $pos + 1, $len ); 

    return $str;
}

sub createDisciplines
{
    my $table = shift;

    my %disc = $table->getDisciplines();

    my $title = "no title";
    my $name = "filename";  
    while( my( $key, $val ) = each(%disc) )
    {
        $name = $keys_disc{$key};
        $title = $key;
        $link = "disc_$name" . $link_ext . ".htm";
        $out = $out_dir . "/disc_" . $name . $file_ext . ".htm";

        print( "opening file disc_$name.htm\n" );
        open( OUT, ">$out" ) || die( "unable to open disc_$key" );

        writeHeader( *OUT, $title, $link );
        $val->sort( $fields );
        $val->showTable( *OUT );
        println( *OUT, "</table></body></html>" );
        close( OUT );
    }
}

sub createSites
{
    my $table = shift;

    my %site = $table->getSites();

    my $title = "no title";
    my $name = "filename";  
    while( my( $key, $val ) = each(%site) )
    {
        $name = $keys_site{$key};
        $title = $key;
        $link = "site_$name" . $link_ext . ".htm";
        $out = $out_dir . "/site_" . $name . $file_ext . ".htm";

        print( "opening file site_$name.htm\n" );
        open( OUT, ">$out" ) || die( "unable to open site_$key" );

        writeHeader( *OUT, $title, $link );                     
        $val->sort( $fields );
        $val->showTable( *OUT );
        println( *OUT, "</table></body></html>" );
        close( OUT );
    }
}

sub createSiteGroups
{
    my $table = shift;

    my %sitegroup = $table->getSiteGroups();

    my $title = "no title";
    my $name = "filename";  
    while( my( $key, $val ) = each(%sitegroup) )
    {
        $name = $keys_sitegroup{$key};
        $title = $key;
        $link = "sitegroup_$name" . $link_ext . ".htm";
        $out = $out_dir . "/sitegroup_" . $name . $file_ext . ".htm";

        print( "opening file sitegroup_$name.htm\n" );
        open( OUT, ">$out" ) || die( "unable to open sitegroup_$key" );

        writeHeader( *OUT, $title, $link );
        $val->sort( $fields );
        $val->showTable( *OUT );
        println( *OUT, "</table></body></html>" );
        close( OUT );
    }
}

sub createGroups
{
    my $table = shift;

    my %group = $table->getGroups();

    my $title = "no title";
    my $name = "filename";  
    while( my( $key, $val ) = each(%group) )
    {
        $name = $keys_group{$key};
        $title = $key;
        $link = "group_$name" . $link_ext . ".htm";
        $out = $out_dir . "/group_" . $name . $file_ext . ".htm";

        print( "opening file group_$name.htm\n" );
        open( OUT, ">$out" ) || die( "unable to open group_$key" );

        writeHeader( *OUT, $title, $link );                     
        $val->sort( $fields );
        $val->showTable( *OUT );
        println( *OUT, "</table></body></html>" );
        close( OUT );
    }
}

sub createDates
{
    my $table = shift;

    my %date = $table->getDates();

    my $title = "no title";
    my $name = "filename";  
    while( my( $key, $val ) = each(%date) )
    {
        #$name = $keys_date{$key};
        $title = $key;
        $name = $key;
        $link = "date_$name" . $link_ext . ".htm";
        $out = $out_dir . "/date_" . $name . $file_ext . ".htm";

        print( "opening file date_$name.htm\n" );
        open( OUT, ">$out" ) || die( "unable to open date_$key" );

        writeHeader( *OUT, $title, $link );                     
        $val->sort( $fields );
        $val->showTable( *OUT );
        println( *OUT, "</table></body></html>" );
        close( OUT );
    }
}


sub writeHeader
{
    *OUT = shift;
    my $title = shift;
    my $link = shift;
    $title = $titles{$title};

    println( *OUT, "<html>" );
    println( *OUT, "<head>" );
    println( *OUT, "<title>$title</title>" );
    println( *OUT, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">" );
    println( *OUT, "<link rel=\"stylesheet\" href=\"../assets/style_sheets/ivotuk.css\" type=\"text/css\">" );
#   println( *OUT, "<script language=javascript src=script.js></script>" );
    println( *OUT, "</head>" );

    println( *OUT, "<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=#000099>" );
    println( *OUT, "<table border=0 width=100% vspace=10>" );
        println( *OUT, "<tr>" );
            println( *OUT, "<td align=left><h3 class=\"sidebar\"><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\" color=#006699>$title</font></b></h3>" );
#           println( *OUT, "<td align=center width=30%>" );
#               println( *OUT, "<form><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\" color=#006699>$txt</font></b><br>" );
#               println( *OUT, "<input class=tablebutton bgcolor=#294365 type=button value=\"$btn\" onClick=\"parent.mainFrame.location=\'$link\'; return false;  \">" );
#           println( *OUT, "</form></td>" );
        println( *OUT, "</tr>" );
    println( *OUT, "</table>" );

    println( *OUT, "<table BORDER class=\"table\" align=\"center\">" );
    println( *OUT, "\t<tr align=LEFT bgcolor=\"#FFFFCC\"> " );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Group</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Contact</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Data Set</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Date</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Site</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Site Group</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Discipline</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Disc</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Documentation</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
    println( *OUT, "\t</tr>" );
}

sub writeHeader_big
{
    *OUT = shift;
    my $title = shift;
    my $link = shift;

    println( *OUT, "<html>" );
    println( *OUT, "<head>" );
    println( *OUT, "<title>$title</title>" );
    println( *OUT, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">" );
    println( *OUT, "<link rel=\"stylesheet\" href=\"../assets/style_sheets/ivotuk.css\" type=\"text/css\">" );
    #println( *OUT, "<script language=javascript src=script.js></script>" );
    println( *OUT, "</head>" );


    println( *OUT, "<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=#000099>" );
    println( *OUT, "<table border=0 width=100% vspace=10>" );
        println( *OUT, "<tr>" );
            println( *OUT, "<td align=left><h3 class=\"sidebar\"><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\" color=#006699>$title</font></b></h3>" );
#           println( *OUT, "<td align=center width=30%>" );
#               println( *OUT, "<form><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" size=\"2\" color=#006699>$txt</font></b><br>" );
#               println( *OUT, "<input class=tablebutton bgcolor=#294365 type=button value=\"$btn\" onClick=\"parent.mainFrame.location=\'$link\'; return false; \">" );
#           println( *OUT, "</form></td>" );
        println( *OUT, "</tr>" );
    println( *OUT, "</table>" );



    println( *OUT, "<table BORDER class=\"table\" align=\"center\">" );
    println( *OUT, "\t<tr align=LEFT bgcolor=\"#FFFFCC\"> " );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Group</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Contact</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Data Set</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Date</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Site</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Site Group</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Discipline</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Disc</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
        println( *OUT, "\t\t<td align=CENTER> " );
        println( *OUT, "\t\t\t<h4><b><font face=\"Verdana, Arial, Helvetica, sans-serif\" color=#004499>Documentation</font></b></h4>" );
        println( *OUT, "\t\t</td>" );
    println( *OUT, "\t</tr>" );
}



