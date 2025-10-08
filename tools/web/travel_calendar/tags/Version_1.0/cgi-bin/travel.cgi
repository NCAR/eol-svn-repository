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
package Travel;
use strict;
use lib ".";
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version1";
use Conversions;
use TravelCalendar;
use TravelForm;
use CGI qw(:standard :html3);
our @ISA = ("CGI");

&main();

##-------------------------------------------------------------------------
# @signature void main()
# <p>Create the travel web page.</p>
##-------------------------------------------------------------------------
sub main() {
    my $travel = Travel->new();

    print $travel->header();
    print "<html>\n";

    if (defined($travel->param("View")) && $travel->param("View") eq "Form") {
	$travel->buildForm();
    } elsif (defined($travel->param("View")) && 
		     $travel->param("View") eq "View") {
	$travel->buildView();
    } else {
	$travel->buildCalendar();
    }

    print "</html>\n";
}

sub buildCalendar {
    my $self = shift;

    if (defined($self->param("Travel"))) {
	my $data = TravelData->new();
	if ($self->param("Travel") eq "Add") {
	    $data->addTravel($self->param("name"), $self->param("start"),
			     $self->param("end"), $self->param("why"),
			     $self->param("where"));
	} elsif ($self->param("Travel") eq "Update") {
	    $data->editTravel($self->param("line"), $self->param("name"), 
			      $self->param("start"), $self->param("end"), 
			      $self->param("why"), $self->param("where"));
	} elsif ($self->param("Travel") eq "Delete") {
	    $data->deleteTravel($self->param("line"));
	}
    }

    if (!defined($self->param("Date")) && defined($self->param("start"))) {
	$self->param("Date", substr($self->param("start"), 0, 7));
    }
    my $calendar = TravelCalendar->new();
    $calendar->param("Type", $self->param("Type"));
    $calendar->param("View", "Form");

    print $calendar->generateCalendar($self->param("Date"));
}

sub buildForm {
    my $self = shift;
    
    if ($self->param("action") eq "Add" || $self->param("action") eq "Edit" ||
	$self->param("action") eq "Delete") {
	my $form = TravelForm->new();
	$form->param("Type", $self->param("Type"));
	
	if ($self->param("action") eq "Add") {
	    print $form->generateAddForm();
	} elsif ($self->param("action") eq "Edit") {
	    print $form->generateEditForm($self->param("line"),
					  $self->param("name"),
					  $self->param("start"),
					  $self->param("end"),
					  $self->param("why"),
					  $self->param("where"));
	} elsif ($self->param("action") eq "Delete") {
	    print $form->generateDeleteForm($self->param("line"),
					    $self->param("name"),
					    $self->param("start"),
					    $self->param("end"),
					    $self->param("why"),
					    $self->param("where"));
	}
	print $self->hidden("Type", "Form");
	print "\n";
    } else {
	$self->buildCalendar();
    }
}

sub buildView {
    my $self = shift;
    my $data = TravelData->new();

    my $name = $data->getName($self->param("line"));
    $self->param("name", $data->getName($self->param("line")));
    $self->param("start", $data->getStartDate($self->param("line")));
    $self->param("end", $data->getEndDate($self->param("line")));
    $self->param("why", $data->getReason($self->param("line")));
    $self->param("where", $data->getWhere($self->param("line")));

    $self->param("Type", "View");
    $self->param("View", "Form");


    print "<head><title>$name Travel Information</title></head>\n";
    print "<body>\n";
    print "<form method=POST ";
    print "action=/cgi-bin/dpg/travel/travel.cgi";
    print " enctype=application/x-www-form-urlencoded>\n";

    print "<h1>$name Travel Information</h1>\n";
    print "<p>Gone from: ".$self->param("start");
    print " to: ".$self->param("end");
    print "</p>\n";

    print "<p><b>Reason: </b>".$self->param("why")."</p>\n";
    print "<p><b>Where: </b>".$self->param("where")."</p>\n";

    print $self->hidden("name", $self->param("name"))."\n";
    print $self->hidden("start", $self->param("start"))."\n";
    print $self->hidden("end", $self->param("end"))."\n";
    print $self->hidden("why", $self->param("why"))."\n";
    print $self->hidden("where", $self->param("where"))."\n";
    print $self->hidden("line", $self->param("line"))."\n";

    print $self->submit("action", "Edit")."\n";
    print $self->submit("action", "Delete")."\n";
    print $self->submit("action", "OK")."\n";

    print $self->hidden("View")."\n";
    print $self->hidden("Type")."\n";
    print "</body>\n";
}
