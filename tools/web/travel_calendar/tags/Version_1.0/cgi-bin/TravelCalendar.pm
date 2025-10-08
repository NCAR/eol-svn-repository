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
package TravelCalendar;
use strict;
use lib ".";
use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version1";
use Conversions;
use TravelData;
use CGI qw(:standard :html3);
our @ISA = ("CGI");

##-------------------------------------------------------------------------
# @signature String buildDay(String date, String week)
# <p>Build the text for the specified day.</p>
#
# @input $date The day to create the text box for.
# @input $week The week the date is in.
# @output $table The table data that is the text box for the date.
##-------------------------------------------------------------------------
sub buildDay {
    my $self = shift;
    my $date = shift;
    my $week = shift;
    my $table = "";
    my $data = TravelData->new();

    my @people;
    my @daysInWeek = split(' ', $week);
    @people = $data->getPeopleGone(sprintf("%04d/%02d/%02d", 
					   substr($date, 0, 4),
					   substr($date, 5, 2), 
					   $daysInWeek[0]),
				   sprintf("%04d/%02d/%02d",
					   substr($date, 0, 4),
					   substr($date, 5, 2),
					   $daysInWeek[scalar(@daysInWeek) 
						       - 1]));
    while (scalar(@people) < 3) { push(@people, -1); }

    foreach my $person (@people) {
	if ($person != -1 &&
	  Conversions::compareDates($data->getStartDate($person), $date) <= 0
	    && Conversions::compareDates($data->getEndDate($person), $date) >= 0) {
	    my $name = $data->getName($person);
	    my $class = $name;
	    $class =~ s/[\s]//g;
	    my $link = "./travel.cgi?View=View;line=".$person;
	    
	    if (Conversions::compareDates($data->getEndDate($person), $date) == 0) {
		$name = "-$name";
		$name .= "-|";
	    } elsif (Conversions::compareDates($data->getStartDate($person), $date) != 0) {
		$name = "-$name";
		while (length($name) < 20) { $name .= "-"; }
	    } else {
		while (length($name) < 20) { $name .= "-"; }
	    }


	    $table .= "<tr><td><tt><a href=$link class=$class>$name</a></tt></td></tr>\n";
	} else {
	    $table .= "<tr><td><tt class=none>-------------</tt></td></tr>\n";
	}
    }
    return $table;
}

##-------------------------------------------------------------------------
# @signature String buildDays(String calendar, int year, int month)
# <p>Build a String containing each day of the month to be displayed on the
# travel page.</p>
#
# @input $calendar The calendar to be displayed.
# @input $year The year of the calendar.
# @input $month The month of the calendar.
# @output $table The HTML table data for the days in the calendar.
##-------------------------------------------------------------------------
sub buildDays {
    my $self = shift;
    my $calendar = shift;
    my $year = shift;
    my $month = shift;
    my @weeks = split('\n', $calendar);
    my $table = "";
    $weeks[0] = "";
    $weeks[1] = "";

    foreach my $week (@weeks) {
	if ($week ne "") {
	    my @days = split(' ', $week);
	    if ($days[0] == 1) {
		my @old_days = @days;
		@days = ();
		while (scalar(@days) < 7 - scalar(@old_days)) { 
		    push(@days, "&nbsp;"); 
		}
		push(@days, @old_days);
	    } else {
		while (scalar(@days) < 7) { push(@days, "&nbsp;"); }
	    }
	    $table .= "<tr>\n";
	    foreach my $day (@days) {
		(my $today_year, my $today_month, my $today) =
		    $self->parseDate();
		$table .= "<td>";
		if ($today_year == $year && $today_month == $month &&
		    $today == $day) {
		    my $title = $self->getMonth($calendar);
		    $title =~ s/[\s]+//;
		    $table .= "<table align=center valign=top width=100% class=today";
		    $table .= $title.">\n";
		} else {
		    $table .= "<table align=center valign=top width=100%>\n";
		}
		$table .= "<tr><th align=right valign=top>";
		$table .= $day."</th></tr>\n";
		if ($day =~ /[0-9]+/) {
		    my $date = sprintf("%04d/%02d/%02d", $year, $month, $day);
		    $table .= $self->buildDay($date, $week);
		}
		$table .= "</table></td>\n";
	    }
	    $table .= "</tr>\n";
	}
    }

    return $table;
}

sub generateCalendar {
    my $self = shift;
    (my $year, my $month, my $day) = $self->parseDate(shift);
    my $calendar = $self->getCalendar($year, $month);

    my $form = "<head><title>JOSS Travel Page</title>\n";
    $form .= "<link rel=\"stylesheet\" type=\"text/css\" href=\"/dpg/TOOLS/travel/travel.css\">\n";
    $form .= "</head>\n<body>\n";

    $form .= "<form method=POST ";
    $form .= "action=/cgi-bin/dpg/travel/travel.cgi";
    $form .= " enctype=application/x-www-form-urlencoded>\n";
    
    $form .= "<table align=center><tr><td><a href=http://www.joss.ucar.edu><img border=0 src=\"/joss_icons/JOSS_small_white.gif\"></a></td>\n";
    $form .= "<td><h1 align=center>JOSS Travel Page</h1></td></tr></table>\n";

    $form .= "<br><table width=100% class=year align=center border=1><tr>\n";
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>%04d<<</a></td>\n", $year - 1, 12, $year - 1);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>January</a></td>\n", $year, 1);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>February</a></td>\n", $year, 2);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>March</a></td>\n", $year, 3);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>April</a></td>\n", $year, 4);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>May</a></td>\n", $year, 5);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>June</a></td>\n", $year, 6);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>July</a></td>\n", $year, 7);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>August</a></td>\n", $year, 8);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>September</a></td>\n", $year, 9);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>October</a></td>\n", $year, 10);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>November</a></td>\n", $year, 11);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>December</a></td>\n", $year, 12);
    $form .= sprintf("<td><a class=month href=./travel.cgi?Date=%04d/%02d>>>%04d</a></td>\n", $year + 1, 1, $year + 1);

    $form .= "</tr></table><br>\n";

    $form .= "<table align=center><tr><td><b>View Month (<i>YYYY/MM</i>):</b>";
    $form .= $self->textfield("Date", "2003/08")."\n";
    $form .= $self->submit("Submit")."</td>\n<td>&nbsp;&nbsp;</td><td>";
    $form .= $self->submit("action", "Add");
    $form .= "<b>New Travel Info</b>";
    $form .= "</td></table><br>\n";

    my $title = $self->getMonth($calendar);
    $title =~ s/[\s]+//;
    $form .= "<table align=center width=100% border=2>\n";
    $form .= "<tr><th class=".$title." colspan=7><font size=32>";
    $form .= "$title</font></th></tr>\n<tr>";
    $form .= "<th class=dayofweek$title width=14%>Sunday</th>\n";
    $form .= "<th class=dayofweek$title width=14%>Monday</th>\n";
    $form .= "<th class=dayofweek$title width=14%>Tuesday</th>\n";
    $form .= "<th class=dayofweek$title width=14%>Wednesday</th>\n";
    $form .= "<th class=dayofweek$title width=14%>Thursday</th>\n";
    $form .= "<th class=dayofweek$title width=14%>Friday</th>\n";
    $form .= "<th class=dayofweek$title width=14%>Saturday</th></tr>\n";
    $form .= $self->buildDays($calendar, $year, $month);
    $form .= "</table>\n";

    $form .= $self->hidden("Type", "Calendar")."\n";
    $form .= $self->hidden("View", "Form")."\n";

    $form .= "<hr><p align=center><a class=month HREF=\"/joss_admin/copyright.html\">&#169 Copyright Notice</a></p>\n";

    $form .= "</body>\n";

    return $form;
}

##-------------------------------------------------------------------------
# @signature String getCalendar(int year, int month)
# <p>The unix calendar for a given year and month.</p>
#
# @input $year The year to get the calendar.
# @input $month The month to get the calendar.
# @output $calendar The calendar for the specified year and month.
##-------------------------------------------------------------------------
sub getCalendar {
    my $self = shift;
    my $year = shift;
    my $month = shift;
    return `cal $month $year`;
}

##-------------------------------------------------------------------------
# @signature String getMonth(String calendar)
# <p>Get the month of the calendar to be displayed in the travel page.</p>
#
# @input $calendar The calendar to parse for the month.
# @output $month The month to be displayed in the calendar.
##-------------------------------------------------------------------------
sub getMonth {
    my $self = shift;
    my $calendar = shift;
    return ((split('\n', $calendar))[0]);
}

##-------------------------------------------------------------------------
# @signature (int year, int month, int day) parseDate()
# <p>Parse the current date to be used to a year, month and day.</p>
#
# @output $year The year of the current date.
# @output $month The month of the current date.
# @output $day The day of the current date.
##-------------------------------------------------------------------------
##-------------------------------------------------------------------------
# @signature (int year, int month, int day) parseDate(String date)
# <p>Parse the specified date into a year, month and day.</p>
# 
# @input $date The date to be parsed in YYYY/MM format.
# @output $year The year of the current date.
# @output $month The month of the current date.
# @output $day The day of the current date.
##-------------------------------------------------------------------------
sub parseDate {
    my $self = shift;
    my $date = shift;
    if (defined($date)) {
	my $year = substr($date, 0, 4);
	my $month = substr($date, 5, 2);
	return ($year, $month, undef());
    } else {
	my @data = localtime();
	return ($data[5] + 1900, $data[4] + 1, $data[3]);
    }
}
