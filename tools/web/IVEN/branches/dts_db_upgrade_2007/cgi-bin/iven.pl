#!/bin/perl -w

use strict;
use CGI;

use lib "lib";
use ProjectDBA;

my $cgi = CGI->new();
my $pjname = "IHOP_2002";
#my $pjname = "GCIP/LSA-NW 00";

my $project = ProjectDBA::buildIvenPage( $pjname );

print( $cgi->header() ); 
print( "<html><head><title>Iven Test</title></head>" );
print( "<body>" );
print( "<center><h1>Iventory and Status Page: $project->{pjname}</h1></center>" );

foreach my $pd (@{$project->{products}})
{
	print( "<h3><u>$pd->{pdname}</u></h3>" );
	print( "<table border=1 cellpadding=3 width=98%>" );
		print( "<tr>" );
			print( "<th width=25%>Dataset</th>" );
			print( "<th width=10%>Converting</th>" );
			print( "<th width=8%>Status</th>" );
			print( "<th width=57%>Notes</th>" );
		print( "</tr>" );
	foreach my $ds (@{$pd->{datasets}})
	{
		print( "<tr>" );	
			print( "<td width=25%>$ds->{dsname}</td>" );
			print( "<td width=10%>$ds->{user_processing_object}->{fname}</td>" );
			print( "<td width=8%>$ds->{done}</td>" );
			print( "<td width=57%><div style=\"position:relative;height:100px;overflow:auto;\"> $ds->{admin_notes}</div></td>" );
		print( "</tr>" );	
	}

	print( "</table><br>" );
}

print( "</body>" );
print( "</html>" );
 
