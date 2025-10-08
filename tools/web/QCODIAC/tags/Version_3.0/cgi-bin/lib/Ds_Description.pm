#! /usr/bin/perl -w

#
# Ds_Description.pm
#  Author: Dan Sullivan
#  Date: 2003-07-08
#
#  Author: Joel Clawson
#  Date: 2005-05-24 Upgraded for handling the new MySQL database.

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
# 			--dataset_id - either a single or comma-delimited list
#
# identifier
#   * unique id among the tools, and it has no spaces (var)
#
# group
#   * the tool group this tool belongs to (dataset, project, general)
#-----------------------------------------------------------------------------#

package Ds_Description;
use Utils;
use EQuery;

sub new {
    my $class = shift;
    my $self = {};
    bless($self, $class);
    
    $self->{identifier} = "ds_description";	
    $self->{group} = "dataset";
    
    return $self;
}

sub shortDesc {
    return "Dataset Description";
}

sub longDesc {
    return cr("       <table border=0 cellpadding=0 cellspacing=0><tr><td>",
	      "           Displays the Dataset Description and Project Link Listing for one or more datasets, output includes: ",
	      "               <dd><li>Description Fields (Dataset ID, Title, etc.)",
	      "               <dd><li>Contact Fields (Primary and Secondary Contact Associations)",
	      "               <dd><li>Data Availability (Notes, Browsable, etc.)",
	      "               <dd><li>Listing of Linked Project(s)",
	      "       </td></tr></table>" );
}

sub linkTitle {
    return "Description";
}

sub queryResult {
    my $self = shift;
    my $params = shift;
    my $list = $params->get( "dataset_id" );
    my @list = split( ",", $list );
    my $result;
    my %contacts = getContacts();

    foreach my $dataset_id (@list) {
	my $eq = EQuery->new();
#	$eq->query( "SELECT *,dataset.name as title,frequency.name as frequency,dataset.row_revise_time as update_time FROM dataset,frequency WHERE dataset.frequency_id=frequency.frequency_id AND dataset_id=" . $eq->quote( $dataset_id ) );
        $eq->query( "SELECT *,dataset.title as title,frequency.name as frequency,dataset.row_revise_time as update_time,dataset.row_revise_contact_id as update_contact_id FROM dataset,frequency WHERE dataset.frequency_id=frequency.id AND archive_ident=" . $eq->quote( $dataset_id ) );
	my %ds = $eq->getRow();

	if( ! %ds ) {
	    $result = $result . cr("<table border=0 width=100% cellpadding=1 cellspacing=1 bgcolor=#e9e9e9>",
				   "<tr><td>&nbsp</td></tr>",		
				   "<tr><td bgcolor=white><font size=+1><b>$dataset_id:</font></td></tr>",
				   "<tr><td bgcolor=white><b>", &nbsp(3), "No entry for $dataset_id.</td></tr>",
				   "<tr><td bgcolor=white>&nbsp;</td></tr>",
				   "</table>" );
	} else {
	    my $projects = getProjectField( $dataset_id );
	    my $cats = getCategoryField( $dataset_id );
	    my $platforms = getPlatformField( $dataset_id );
	    my $docs = getDocumentField( $dataset_id );
	    my $pis = getPiField( $dataset_id );
	    
	    $result = $result . cr("<table border=0 width=100% cellpadding=1 cellspacing=1 bgcolor=#e9e9e9>",
				   "<tr><td>&nbsp</td></tr>",		
				   "<tr><td bgcolor=white><font size=+1><b>$dataset_id:</font></td></tr>",
				   
				   "<tr><td bgcolor=white>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr>",
				   "<td align=left nowrap width=5%><b>Dataset Id: </b></td>",
				   "<td width=15%>",
				   "<a target=_blank href=http://data.eol.ucar.edu/codiac/dss/id?$dataset_id>$dataset_id</a>",
				   "</td>",
				   "<td width=15% align=left>",
#				   "<b>Hidden:" . &nbsp(2) . ($ds{hide} ? getYes() : getNo()) . "</b>",
				   "<b>Hidden:" . &nbsp(2) . ($ds{visible} ? getNo() : getYes()) . "</b>",
				   "</td>",
				   "</tr>",
				   "<tr>",
				   "<td align=left nowrap width=5%><b>Title: </b></td>",
				   sprintf("<td colspan=2 align=left>%s</td>",$ds{title}),
				   "</tr>",
				   "</table>",
				   "</tr></td>",

				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr><td width=100%><b>Description: </b></td></tr>",
#				   "<tr><td width=95%>$ds{description}</td></tr>",
                                  "<tr><td width=95%>$ds{summary}</td></tr>",
				   "</table>",
				   "</td></tr>",
				   
				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=0>",
				   "<tr>",
				   "<td align=right nowrap width=15%><b>Date Range:</b></td>",
				   "<td align=right width=5%>Begin: </td>",
				   "<td width=5% nowrap>$ds{begin_date}</td>",
				   "<td align=right width=20%><b>Latitude: </b></td>",
#				   "<td width=5% nowrap>$ds{minlat}</td>",
#				   "<td width=30% nowrap>$ds{maxlat}</td>",
                                  "<td width=5% nowrap>$ds{minimum_latitude}</td>",
                                  "<td width=30% nowrap>$ds{maximum_latitude}</td>",
				   "</tr>",
				   "<tr>",
				   "<td align=right width=15%>&nbsp;</td>",
				   "<td align=right width=5%>End: </td>",
				   "<td width=5% nowrap>$ds{end_date}</td>",
				   "<td align=right width=20%><b>Longitude: </b></td>",
#				   "<td width=5% nowrap>$ds{minlon}</td>",
#				   "<td width=30% nowrap>$ds{maxlon}</td>",
                                  "<td width=5% nowrap>$ds{minimum_longitude}</td>",
                                  "<td width=30% nowrap>$ds{maximum_longitude}</td>",
				   "</tr>",
				   "</table>",
				   "</td></tr>",

				   "<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",
				   
				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr>",
				   "<td align=right nowrap width=15%><b>Observing Frequency: </b></td>",
				   sprintf("<td width=15%s nowrap>%s</td>","%",$ds{frequency}),
				   "<td align=right nowrap width=10%><b>Type: </b></td>",
				   "<td>$ds{spatial_type}</td>",
				   "</table>",
				   "</td></tr>",
				   
				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr>",
				   "<td align=left width=15% nowrap style=vertical-align:top>$cats</td>",
				   "<td align=left width=15% nowrap style=vertical-align:top>$platforms</td>",
				   "<td align=left width=15% nowrap style=vertical-align:top>$projects</td>",
				   "</table>",
				   "</td></tr>",
				   
				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   sprintf("<tr><td><b>EULA Required:</b> %s</td></tr>",$ds{eula_reqd} ? getYes() : getNo()),
				   "<tr>",
				   "<td width=75%>$docs</td>",
				   "</tr>",
				   "</table>",
				   "</td></tr>",

				   "<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",

				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr>",
				   "<td align=left width=15% nowrap style=vertical-align:top>$pis</td>",
				   "<td align=left width=15%><table>",
#				   "<tr><td><b>Displayed Contact:</b></td><td>$contacts{$ds{displayed_contact_id}}</td></tr>",
                                  "<tr><td title=\"Displayed Contact\"><b>Point of Contact:</b></td><td>$contacts{$ds{point_of_contact_id}}</td></tr>",
#				   "<tr><td><b>Source Contact:</b></td><td>$contacts{$ds{source_contact_id}}</td></tr>",
                                  "<tr><td title=\"Source Contact\"><b>Grant Contact:</b></td><td>$contacts{$ds{grant_contact_id}}</td></tr>",
				   "<tr><td><b>Internal Contact:</b></td><td>$contacts{$ds{internal_contact_id}}</td></tr>",
				   "</table></td>",
				   "</table>",
				   "</tr>",
				   
				   "<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",
				   
				   "<tr bgcolor=white><td>",
				   "<b>DATA AVAILABILITY:</b>", &nbsp(6),
#				   sprintf("<b>Online Orderable: </b>%s",$ds{onlineorderable} ? getYes() : getNo()), &nbsp(2),
                                  sprintf("<b>Online Orderable: </b>%s",$ds{online_orderable} ? getYes() : getNo()), &nbsp(2),
#				   sprintf("<b>Offline Orderable: </b>%s",$ds{offlineorderable} ? getYes() : getNo()), &nbsp(2),
                                  sprintf("<b>Offline Orderable: </b>%s",$ds{offline_orderable} ? getYes() : getNo()), &nbsp(2),
				   sprintf("<b>Browseable: </b>%s",$ds{browseable} ? getYes() : getNo()), &nbsp(2),
				   "</td></tr>",
				   "<tr bgcolor=white><td>",
				   &nbsp(30),
				   sprintf("<b>Password Protected: </b>%s",$ds{auth_reqd} ? getYes() : getNo()), &nbsp(2),
				   sprintf("<b>Linked to DODS: </b>%s",$ds{dodsable} ? getYes() : getNo()), &nbsp(2),
				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr><td width=100%><b>Remote URL: </b></td>",
				   sprintf("<td>%s</td></tr>",$ds{remote_url} ? sprintf("<a href=%s>%s</a>",$ds{remote_url},$ds{remote_url}) : "&nbsp;"),
				   "</table>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr><td width=100%><b>Map: </b></td>",
				   sprintf("<td>%s</td></tr>",$ds{map} ? sprintf("<a href=%s>%s</a>",$ds{map},$ds{map}) : "&nbsp;"),
				   "</table>",
				   "</td></tr>",
				   
				   "<tr><td bgcolor=#F8F8F8 width=100%>&nbsp;</td></tr>",

				   "<tr bgcolor=white><td>",
				   "<table width=100% border=0 cellpadding=4 cellspacing=1>",
				   "<tr>",
				   "<td align=right width=5% nowrap><b>CATALOG ENTRY: </b></td>",
				   "<td align=right width=15% nowrap><b>Author: </b></td>",
#				   "<td>$contacts{$ds{row_revise_contact_id}}</td>",
				   "<td>$contacts{$ds{update_contact_id}}</td>",
				   "</tr>",
				   "<tr>",
				   "<td>&nbsp;</td>",
				   "<td align=right width=15% nowrap><b>Date last revised: </b></td>",
				   "<td>$ds{update_time}</td>",
				   "</tr>",
				   "</table>",
				   "</td></tr>",
				   "</table>" );
	} # end of else to check if %ds is undef
    }
    
    my $title = "Dataset Description";
    return ( $title, $result );
}

sub queryForm {
    my $self = shift;

    my $title = "Dataset Description: Query";                         
    my $form = cr("<form action=supervisor method=POST onSubmit=\"return checkText( this.dataset_id, \'Please Enter a Dataset Id.\')\">",
		  "<table border=0 bgcolor=white cellpadding=4 width=80%>",
		  "<tr><td align=center>",
		  "<br><b>Enter a list of comma-delimited dataset_id numbers (e.g. 77.018,77.019,46.006):</b>" ,
		  "<input type=text size=75 name=dataset_id>",
		  "</td></tr>",
		  "<tr><td align=right>",
		  "<input type=hidden name=results value=$self->{identifier}>",
		  "<input type=submit name=ds_submit value=\"Submit Query\">",
		  "</td></tr>",
		  "</table></form>" );
    
    return ( $title, $form );
}

sub getContacts {
    my $eq = EQuery->new();
#    $eq->query("SELECT * FROM contact");
    $eq->query("SELECT id as contact_id,contact.* FROM contact");

    my %contacts;
    while ((%row = $eq->getRow())) {
#	$contacts{$row{contact_id}} = sprintf("%s (%d %s)",$row{person_name},$row{contact_id},$row{contact_short_name});
       $contacts{$row{contact_id}} = sprintf("%s (%d %s)",$row{person_name},$row{id},$row{short_name});
    }
    return %contacts;
}

sub getPiField {
    my $dataset_id = shift;
    my $eq = EQuery->new();
#    $eq->query( "SELECT * FROM contact,dataset_pi where contact_id=pi_contact_id and dataset_id=".$eq->quote($dataset_id) );
    $eq->query( "SELECT contact.*,dataset_contact.* FROM contact,dataset_contact,dataset where contact.id=dataset_contact.contact_id and dataset.id=dataset_contact.dataset_id and iso_citation_role='principalInvestigator' and archive_ident=".$eq->quote($dataset_id) );

    my $ret;
    my @pis;
    while( (%row = $eq->getRow()) ) {
#	push(@pis,sprintf("%s (%d %s)",$row{person_name},$row{contact_id},$row{contact_short_name}));
       push(@pis,sprintf("%s (%d %s)",$row{person_name},$row{contact_id},$row{short_name}));
    }

    $ret = cr("<table width=100% border=0 cellpadding=4 cellspacing=1>",
	      "<tr>",
	      "<td width=8% nowrap><b>PI(s): </b></td>" );

    if( scalar(@pis) != 0 ) {
	$ret = $ret . cr( "<td>", $pis[0], "</td>" );
    } else {
	$ret = $ret . cr( "<td>", "<b>none</b>", "</td>" );
    }

    for( my $x = 1; $x < @pis; $x++ ){
	$ret = $ret . cr("<tr>", 
			 "<td width=8%>&nbsp;</td>", 
			 "<td>$pis[$x]</td></tr>" );
    }
    $ret = $ret . cr ("</table>" );
    
    return $ret;
}

sub getPlatformField {
    my $dataset_id = shift;
    my $eq = EQuery->new();
#    $eq->query( "SELECT * FROM platform,dataset_platform where dataset_platform.platform_id=platform.platform_id and dataset_id=".$eq->quote($dataset_id) );
    $eq->query( "SELECT platform.*, dataset_platform.* FROM platform,dataset_platform,dataset where dataset_platform.platform_id=platform.id and dataset_platform.dataset_id=dataset.id and dataset.archive_ident=".$eq->quote($dataset_id) );

    my $ret;
    my @platforms;
    while( (%row = $eq->getRow()) ) {
	push(@platforms,sprintf("%s (%d)",$row{name},$row{platform_id}));
    }

    $ret = cr("<table width=100% border=0 cellpadding=4 cellspacing=1>",
	      "<tr>",
	      "<td width=8% nowrap><b>Platform(s): </b></td>" );

    if( scalar(@platforms) != 0 ) {
	$ret = $ret . cr( "<td>", $platforms[0], "</td>" );
    } else {
	$ret = $ret . cr( "<td>", "<b>none</b>", "</td>" );
    }

    for( my $x = 1; $x < @platforms; $x++ ){
	$ret = $ret . cr("<tr>", 
			 "<td width=8%>&nbsp;</td>", 
			 "<td>$platforms[$x]</td></tr>" );
    }
    $ret = $ret . cr ("</table>" );
    
    return $ret;
}

sub getProjectField {
    my $dataset_id = shift;
    my $eq = EQuery->new();
#    $eq->query( "SELECT project_id FROM dataset_project where dataset_id=" . $eq->quote( $dataset_id ) );
    $eq->query( "SELECT project.name as project_id FROM dataset_project,dataset,project where dataset_project.project_id=project.id and dataset_project.dataset_id=dataset.id and archive_ident=" . $eq->quote( $dataset_id ) );
    my @projs;
    my $ret;
    while( (my %row = $eq->getRow()) ) {
	push(@projs,$row{project_id});
    }
    
    $ret = cr( 
	      "<table width=100% border=0 cellpadding=4 cellspacing=1>",
	      "<tr>",
	      "<td width=8% nowrap><b>Associated Project(s): </b></td>" );
    if( scalar(@projs) != 0 ) {
	$ret = $ret . cr( "<td>", $projs[0], "</td>" );
    } else {
	$ret = $ret . cr( "<td>", "<b>none</b>", "</td>" );
    }
    for( my $x = 1; $x < scalar(@projs); $x++ ) {
	$ret = $ret . cr( "<tr>", 
			 "<td width=8%>&nbsp;</td>", 
			 "<td>$projs[$x]</td></tr>" );
    }
    $ret = $ret . cr ("</table>" );
    
    return $ret;
}

sub getCategoryField {
    my $dataset_id = shift;
    my $eq = EQuery->new();
#    $eq->query( "SELECT * FROM category,dataset_category where dataset_category.category_id=category.category_id AND dataset_id=" . $eq->quote( $dataset_id ) );
    $eq->query( "SELECT category.*,dataset_category.* FROM category,dataset_category,dataset where dataset_category.category_id=category.id AND dataset_category.dataset_id=dataset.id AND archive_ident=" . $eq->quote( $dataset_id ) );

    my @cats;
    my $ret;
    while( (my %row = $eq->getRow()) ) {
	push(@cats,sprintf("%s (%d)",$row{name},$row{category_id}));
    }
    
    $ret = cr("<table width=100% border=0 cellpadding=4 cellspacing=1>",
	      "<tr>",
	      "<td width=8% nowrap><b>Data Categories: </b></td>" );

    if( scalar(@cats) != 0 ) {
	$ret = $ret . cr( "<td>", $cats[0], "</td>" );
    } else {
	$ret = $ret . cr( "<td>", "<b>none</b>", "</td>" );
    }

    for( my $x = 1; $x < scalar(@cats); $x++ ){
	$ret = $ret . cr("<tr>", 
			 "<td width=8%>&nbsp;</td>", 
			 "<td>$cats[$x]</td></tr>" );
    }
    $ret = $ret . cr ("</table>" );
    
    return $ret;
}

sub getDocumentField {
    my $dataset_id = shift;
    my $eq = EQuery->new();
#    $eq->query( "SELECT * FROM file where purpose!='data' AND dataset_id=" . $eq->quote( $dataset_id ) .
#	       "ORDER BY purpose");
    $eq->query( "SELECT file.* FROM file,dataset where file.dataset_id=dataset.id AND purpose!='data' AND archive_ident=" . $eq->quote( $dataset_id ) .
              "ORDER BY purpose");

    my @docs;
    my $ret;
    while( (my %row = $eq->getRow()) ) {
	if ($row{directory} ne "" && $row{filename} ne "") {
	    my $url = sprintf("http://data.eol.ucar.edu/datafile/nph-get/%s/%s",$dataset_id,$row{filename});
	    push(@docs,sprintf("<b>%-6s:</b> <a href=%s target=_blank>%s/%s</a>",$row{purpose},$url,$row{directory},$row{filename}));
	}
    }
    
    $ret = cr("<table width=100% border=0 cellpadding=4 cellspacing=1>",
	      "<tr>",
	      "<td width=8% nowrap><b>Documentation: </b></td>" );

    if( scalar(@docs) != 0 ) {
	$ret = $ret . cr( "<td>", $docs[0], "</td>" );
    } else {
	$ret = $ret . cr( "<td>", "<b>none</b>", "</td>" );
    }

    for( my $x = 1; $x < scalar(@docs); $x++ ){
	$ret = $ret . cr("<tr>", 
			 "<td width=8%>&nbsp;</td>", 
			 "<td>$docs[$x]</td></tr>" );
    }
    $ret = $ret . cr ("</table>" );
    
    return $ret;
}

sub getNo { return "<font color=blue>No</font>"; }
sub getYes { return "<font color=red>Yes</font>"; }

1;

