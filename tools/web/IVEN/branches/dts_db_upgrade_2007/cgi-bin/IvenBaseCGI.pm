#! /usr/bin/perl -w

package IvenBaseCGI;

use strict;
use lib ".";
use lib "lib";
use CGI;
use Project;
use Utils;

our @ISA = ("CGI");

sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = CGI->new();
    bless($self, $class);

    return $self;
}

sub printHeaderMenu {
    my ($cgi, $project) = @_;

    println("    <div class=\"headerMenu\">");

    println("        <table class=\"infoBar\">");
    println("            <tr>");
    println("                <td class=\"sidePane\">");
    print  ("                    <div><b>TOI:</b> ");
    print  (substr($project->getBeginDate(), 0, 10));
    print  (" through ");
    print  (substr($project->getEndDate(), 0, 10));
    println("</div>");
    print  ("                    <div><b>Charge \#:</b> ");
    print  ($project->getChargeNumber());
    println("</div>");
    println("                </td>");
    println("                <td class=\"centerPane\">");
    print($project->getProjectId());
    println(" Processed Data Set Listing</div>");
    println("                </td>");
    println("                <td class=\"sidePane\">");
    println("                    <div><b>AOI:</b></div>");
    print  ("                    <div class=\"aoi\">");
    printf ("<b>Min Lat:</b> %7.2f ", $project->getMinLatitude());
    printf ("<b>Max Lat:</b> %7.2f ", $project->getMaxLatitude());
    println("</div>");
    print  ("                    <div class=\"aoi\">");
    printf ("<b>Min Lon:</b> %7.2f ", $project->getMinLongitude());
    printf ("<b>Max Lon:</b> %7.2f ", $project->getMaxLongitude());
    println("</div>");    
    println("                </td>");
    println("            </tr>");
    println("        </table>");
    println("        <table class=\"navBar\">");
    println("            <tr>");
    print  ("                <td><a href=\"/dln/?project=");
    print  ($project->getProjectId());
    print  ("\">DLN for ");
    print  ($project->getProjectId());
    println("</a></td>");
    println("                <td class=\"dropdown\">");
    print  ("                    <select name=\"products\" onChange=\"javascript: selectProduct('");
    print  ($project->getProjectId());
    println("', this);\">");
    println("                        <option value=\"-1\">Select a Dataset</option>");
    foreach my $product ($project->getProductList()) {
	print  ("                        <option value=\"");
	print  ($product->getDatasetId());
	print  ("\">");
	print  ($product->getName());
	println("</option>");
    }
    println("                    </select>");
    println("                </td>");
    print  ("                <td><a href=\"project_view?project=");
    print  ($project->getProjectId());
    println("\">Project Page</a></td>");
    println("                <td><a href=\"project_list\">Home</a></td>");
    println("            </tr>");
    println("        </table>");
    println("    </div>");
}



# This file contains a couple of functions that are used by multiple scripts.
#  Probably could be moved to the lib/Utils.pm module

# Returns a string containing the needed html to display the possible
#  status options for products and datasets.  TBD, DONE and N/A.
# Can be given a status value to be selected in the drop down
#  box.
#sub getStatusDropDown
#{
#	my $st = shift;
#	$st = undef() if( $st && $st eq "" );
#	
#	my %shash = ( "tbd"=>"TBD", "done"=>"DONE", "na"=>"N/A", "in_progress"=>"In Progress", "dnd"=>"NoProc" );
#	my @arr = ( "tbd", "in_progress", "done", "dnd", "na" );
#
#	my $select = "<select name=status>";
#
#	foreach my $s (@arr)
#	{
#		my $selected = "";
#		$selected = "selected" if( $st && $s eq $st );
#		$select = $select . "<option value=$s $selected>$shash{$s}</option>";
#	}	
#	$select = $select . "</select>";
#
#	return $select;
#}

# Returns a count of the number of DONE datasets, TBD datasets, NA datasets, 
#  excluded datasets, and a total count.
#sub getDatasetStats
#{
#	my $datasets = shift;
#	my $done = 0;
#	my $tbd = 0;
#	my $na = 0;
#	my $exclude = 0;
#	my $total = 0;
#
#	foreach my $ds (@{$datasets})
#	{
#		$total++;
#		if( $ds->{status} eq "done" )
#		{
#			$done++;
#		}
#		elsif( $ds->{status} eq "tbd" || $ds->{status} eq "in_progress" )
#		{
#			$tbd++;
#		}
#		elsif( $ds->{status} eq "na" || $ds->{status} eq "dnd" )
#		{
#			$na++;
#		}
#
#		if( $ds->{exclude} )
#		{
#			$exclude++;
#		}
#	}
#
#	return ($done, $tbd, $na, $exclude, $total );
#}
1;
