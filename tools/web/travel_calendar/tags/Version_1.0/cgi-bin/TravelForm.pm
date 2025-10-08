#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
# <p>The <code>travel.cgi</code> script is to generate HTML pages for 
# JOSS personel to create, edit and delete dates/times when they will be
# out of the office.  This is to replace the HTML travel page that needs to
# be manually editted.</p>
#
# @author Joel Clawson
# @version 0.01 Original Creation
##Module--------------------------------------------------------------------
package TravelForm;
use strict;
use lib ".";
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version1";
use Conversions;
#use Travel;
use CGI qw(:standard :html3);
our @ISA = ("CGI");

sub generateAddForm {
    my $self = shift;
    my $form = "";

    $form .= "<head><title>Add Travel Information</title>\n";
    $form .= "</head>\n";
    $form .= "<body>\n";
    $form .= "<form method=POST ";
    $form .= "action=/cgi-bin/dpg/travel/travel.cgi?View=";
    $form .= $self->param("Type");
    $form .= " enctype=application/x-www-form-urlencoded>\n";

    $form .= "<h1>Add Travel Information</h1>\n";
    $form .= "<table>\n";
    $form .= "<tr><th>Who:</th><td>".$self->textfield("name")."</td></tr>\n";
    $form .= "<tr><th>From:</th><td>".$self->textfield("start");
    $form .= "YYYY/MM/DD</td></tr>\n";
    $form .= "<tr><th>To:</th><td>".$self->textfield("end");
    $form .= "YYYY/MM/DD</td></tr>\n";
    $form .= "<tr><th>Why:</th><td>".$self->textarea("why")."</td></tr>\n";
    $form .= "<tr><th>Where:</th><td>".$self->textarea("where");
    $form .= "</td></tr>\n<tr><td>";
    $form .= $self->submit("Travel", "Add")."</td><td>";
    $form .= $self->submit("Travel", "Cancel")."</td></tr>";

    $form .= "</table>\n";
    $form .= "</body>\n";

    return $form;
}

sub generateDeleteForm {
    my $self = shift;
    my $form = "";
    $self->param("line", shift);
    $self->param("name", shift);
    $self->param("start", shift);
    $self->param("end", shift);
    $self->param("why", shift);
    $self->param("where", shift);

    $form .=  "<head><title>Edit Travel Information</title></head>\n";
    $form .=  "<body>\n";
    $form .=  "<form method=POST ";
    $form .=  "action=/cgi-bin/dpg/travel/travel.cgi?View=";
    $form .=  $self->param("Type");
    $form .=  " enctype=application/x-www-form-urlencoded>\n";

    $form .= "<h1>Delete Travel Information</h1>\n";
    $form .= "<table>\n";
    $form .= "<tr><th>Who:</th><td>".$self->param("name")."</td></tr>\n";
    $form .= "<tr><th>From:</th><td>".$self->param("start");
    $form .= "</td></tr>\n";
    $form .= "<tr><th>To:</th><td>".$self->param("end");
    $form .= "</td></tr>\n";
    $form .= "<tr><th>Why:</th><td>".$self->param("why")."</td></tr>\n";
    $form .= "<tr><th>Where:</th><td>".$self->param("where");
    $form .= "</td></tr>\n";
    $form .= "<tr><td colspan=2><font color=red size=+1>Are you sure you ";
    $form .= "want to delete this information from the travel page?</font>";
    $form .= "</td></tr>\n<tr><td>";
    $form .= $self->submit("Travel", "Delete")."</td><td>";
    $form .= $self->submit("Travel", "Cancel")."</td></tr>\n";
    $form .= $self->hidden("Date", $self->param("start"));
    $form .= $self->hidden("line");

    return $form;
}

sub generateEditForm {
    my $self = shift;
    my $form = "";
    $self->param("line", shift);
    $self->param("name", shift);
    $self->param("start", shift);
    $self->param("end", shift);
    $self->param("why", shift);
    $self->param("where", shift);
    $self->param("View", "Calendar");

    $form .=  "<head><title>Edit Travel Information</title></head>\n";
    $form .=  "<body>\n";
    $form .=  "<form method=POST ";
    $form .=  "action=/cgi-bin/dpg/travel/travel.cgi?View=";
    $form .=  $self->param("Type");
    $form .=  " enctype=application/x-www-form-urlencoded>\n";

    $form .= "<h1>Edit Travel Information</h1>\n";
    $form .= "<table>\n";
    $form .= "<tr><th>Who:</th><td>".$self->textfield("name")."</td></tr>\n";
    $form .= "<tr><th>From:</th><td>".$self->textfield("start");
    $form .= "(YYYY/MM/DD)</td></tr>\n";
    $form .= "<tr><th>To:</th><td>".$self->textfield("end");
    $form .= "(YYYY/MM/DD)</td></tr>\n";
    $form .= "<tr><th>Why:</th><td>".$self->textarea("why")."</td></tr>\n";
    $form .= "<tr><th>Where:</th><td>".$self->textarea("where");
    $form .= "</td></tr>\n<tr><td>";
    $form .= $self->submit("Travel", "Update")."</td><td>";
    $form .= $self->submit("Travel", "Cancel")."</td></tr>\n";
    $form .= $self->hidden("View", "Calendar")."\n";
    $form .= $self->hidden("Date", substr($self->param("start"), 0, 7));
    $form .= $self->hidden("Type", "Form")."\n";
    $form .= $self->hidden("line")."\n";

    return $form;
}


