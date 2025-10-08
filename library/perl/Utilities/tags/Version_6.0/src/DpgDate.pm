#! /usr/bin/perl -w

##Module---------------------------------------------------------------------
# <p>The DpgDate.pm modules is a collection of functions that format and 
# manipulate dates.  It is called DpgDate to avoid potential conflicts with
# the perl libraries that probably has Date.pm module of its own.</p>
#
# <p>In previous versions, this was a part of the Conversions.pm
# module.  It has been seperated from that module to keep all of the date functions
# together and to make the module smaller.  It also makes a fix for the name
# since these functions are not really conversions, but date manipulation.</p>
#
# <p>There exists a testing suite for the DpgDate.pm functions.  They are located
# in the TestDpgDate.pl script.  It should be rerun after changes to the module
# to ensure that the functions still work correctly.  New tests should also
# be added when new functionality is added to the module.</p>
#
# @use   use lib "/work/DPG_HTML/BEST_SW/conversion_modules/Version3";
#        use DpgDate;
# @use   $return_value = DpgDate::functionName(param_list)
#
# @author Joel Clawson
# @version 3.0 <p>Formats were added to most of the functions.  This is to allow
# greater flexibility in the use of the functions.  In the previous version, it
# was assumed that the dates and times would be formatted like the QCF format.
# This assumption is not valid for other formats.  The user is now allowed to 
# specify the format they are using and in the formatting functions, which format
# they wish as their result.</p>
# <p>The times were also changed to handle seconds.  The previous versions completely
# ignored seconds, since the formatters did not recognize "S" or "s" as valid format
# codes for a time.  The adjustDateTime function now allows the user to adjust the date
# and time by seconds as well as the other time pieces.</p>
# <p>Checks on the parameter list size were also added.  This is to try to help find
# errors quicker by preventing undefined values being passed to a function.  It still
# does not check the values, only ensures that a correct number of parameters are
# passed to the function.</p>
#
# @author Joel Clawson
# @version 2.0 No changes documented
#
# @author Joel Clawson
# @version 0.01 Original Version
##Module---------------------------------------------------------------------
package DpgDate;
use strict;
use lib ".";
use POSIX;
use Exporter;
our @ISA = ("Exporter");
our @EXPORT = qw(adjustDateTime compareDates convertJulian daysInFeb formatDate formatTime getDateFromSeconds validDate validTime);
$| = 1; # Turn on auto flush.

##---------------------------------------------------------------------------
# @signature (String, String) adjustDateTime(String date, String date_fmt, String time, String time_fmt, int day_offset, int hour_offset, int min_offset, int sec_offset)
# <p>Shift the specified date and time by the amount specified in either 
# positive or negative values.  A combination of negative and positive value
# will subtract and add the values given and does not imply all one or the
# other.
# 
# @input $date The date to be adjusted.
# @input $date_fmt The format of the date using "YMDJ" values along with seperators.
# @input $time The time to be adjusted.
# @input $time_fmt The format of the time using "HMS" values along with seperators.
# @input $day_offset The number of days to increase/decrease the date by.
# @input $hour_offset The number of hours to increase/decrease the hour by.
# @input $min_offset The number of minutes to increase/decrease the minutes by.
# @input $sec_offset The number of seconds to increase/decrease the seconds by.
# @output $date The adjusted date in the same format as the input date.
# @output $time The adjusted time in the same format as the input time.
##---------------------------------------------------------------------------
sub adjustDateTime {
    if (scalar(@_) != 8) {
	die("Invalid parameters to adjustDateTime.\n");
    }
    my ($date,$date_fmt,$time,$time_fmt,$day_off,$hour_off,$min_off,$sec_off) = @_;

    # Change to format to a value that will be used by the adjuster
    my ($year,$day) = split('/',formatDate($date,$date_fmt,"YYYY/JJJ"));
    my ($hour,$min,$sec) = split(':',formatTime($time,$time_fmt,"HH:MM:SS"));

    # Change the seconds to the appropriate minutes and seconds
    $sec += $sec_off;
    while ($sec > 59) { $min_off++; $sec -= 60; }
    while ($sec < 00) { $min_off--; $sec += 60; }
    
    # Change the mintues to the appropriate hours and minutes
    $min += $min_off; 
    while ($min > 59) { $hour_off++; $min -= 60; }
    while ($min < 00) { $hour_off--; $min += 60; }
    
    # Change the hours to the appropriate days and hours
    $hour += $hour_off;
    while ($hour > 23) { $day_off++; $hour -= 24; }
    while ($hour < 00) { $day_off--; $hour += 24; }
    
    # Change the days to the appropriate days and years
    $day += $day_off;
    my $days_in_year = (daysInFeb($year) == 28) ? 365 : 366;
    while ($day > $days_in_year) { 
	$year++;
	$day -= $days_in_year;
	$days_in_year = (daysInFeb($year) == 28) ? 365 : 366;
    }
    while ($day < 1) {
	$year--;
	$day += (daysInFeb($year - 1) == 28) ? 365 : 366;
    }

    # Return the date and time to the original formats.
    return (formatDate(sprintf("%04d%03d",$year,$day),"YYYYJJJ",$date_fmt),
	    formatTime(sprintf("%02d:%02d:%02d",$hour,$min,$sec),"HH:MM:SS",$time_fmt));
}

##--------------------------------------------------------------------------
# @signature int compareDates(String first, String first_fmt, String second, String second_fmt)
# <p>Compare two dates to determine if the first date is before, the same, or
# after the second.</p>
#
# @input $first The first date to compare.
# @input $first_fmt The format of the first date using "YMDJ" values.
# @input $second The second date to compare.
# @input $second_fmt The format of the second date using "YMDJ" values.
# @output $comp A negative number if the first date is greater than the second date, zero
# if they are equal, or a positive number if the first date is less than the second.
##--------------------------------------------------------------------------
sub compareDates {
    if (scalar(@_) != 4) {
	die("Invalid parameters to compareDates\n");
    }
    my ($first,$first_fmt,$second,$second_fmt) = @_;

    my $date1 = formatDate($first,$first_fmt,"YYYYMMDD");
    my $date2 = formatDate($second,$second_fmt,"YYYYMMDD");

    return ($date2 - $date1);
}

##--------------------------------------------------------------------------
# @signature (int, int) convertJulian(int year, int julian)
# <p>Convert a julian date into a month and day.</p>
# @input $year The year of the julian date.
# @input $julian The julian date.
# @output $month The month of the julian date.
# @output $day The day in the month of the julian date.
##--------------------------------------------------------------------------
##--------------------------------------------------------------------------
# @signature int convertJulian(int year, int days, int month)
# <p>Convert a month and day into a julian date.</p>
# @input $year The year of the date.
# @input $days The day of the month of the date.
# @input $month The month of the date.
# @output $julian The julian date from the month and day for the year.
##--------------------------------------------------------------------------
sub convertJulian {
    if (scalar(@_) < 2 || scalar(@_) > 3) {
	die("Invalid parameters to convertJulian\n");
    }

    my $days = $_[1] + 0;
    my $month = $_[2];
    
    my @daysInMonth = (31,daysInFeb($_[0]),31,30,31,30,31,31,30,31,30,31);
    
    if (defined($month)) {
	while ($month > 1) {
	    $days += $daysInMonth[$month-2] if defined($daysInMonth[$month-2]);
	    $month--;
	}
	return $days;
    } else {
	$month = 1;
	while ($days > $daysInMonth[$month - 1]) {
	    $days = $days - $daysInMonth[$month - 1];
	    $month++;
	}
	return ($month, $days);
    }
}

##--------------------------------------------------------------------------
# @signature int daysInFeb(int year)
# <p>Get the number of days in February for the specified year.</p>
#
# @input $year The year to find the number of days in February.
# @output $days The number of days in February for the specified year.
##--------------------------------------------------------------------------
sub daysInFeb {
    if (scalar(@_) != 1) {
	die("Invalid parameters to daysInFeb\n");
    }

    if ((($_[0] % 4 == 0) && ($_[0] % 100 != 0)) || ($_[0] % 400 == 0)) {
	return 29;
    } else {
	return 28;
    }
}

##--------------------------------------------------------------------------
# @signature String formatDate(String date, String in_fmt, String out_fmt)
# <p>Convert the specified date from one format to another.  It recognizes:
# <ul><li>Y - for year</li><li>M - for month</li><li>D - for day</li>
# <li>J - for julian day</li></ul>The formats are case insensitive.</p>
#
# @input $date The date to be reformatted.
# @input $in_fmt The format of the date.
# @input $out_fmt The new format for the date.
# @output $date The date formatted to the output format.
##--------------------------------------------------------------------------
sub formatDate {
    
    #Ensuure valid parameters.
    if (scalar(@_) != 3) {
	die("Invalid parameters to formatDate\n");
    }
    my ($date,$in_fmt,$out_fmt) = @_;

    if (length($date) != length($in_fmt)) {
	die(sprintf("Length of date (%s) is not the same as the length of the format (%s) in formating the date.\n", $date, $in_fmt));
    }

    # Get the output format order for hours,mins,secs
    my %fmt_parts;
    for (my $index = 0; $index < length($out_fmt); $index++) {
	my $char = substr($out_fmt,$index,1);
	
	if ($char =~ /y/i && !defined($fmt_parts{"year"})) { 
	    $fmt_parts{"year"} = scalar(keys(%fmt_parts));
	} elsif ($char =~ /m/i && !defined($fmt_parts{"month"})) {
	    $fmt_parts{"month"} = scalar(keys(%fmt_parts));
	} elsif ($char =~ /d/i && !defined($fmt_parts{"day"})) {
	    $fmt_parts{"day"} = scalar(keys(%fmt_parts));
	} elsif ($char =~ /j/i && !defined($fmt_parts{"julian"})) {
	    $fmt_parts{"julian"} = scalar(keys(%fmt_parts));
	}
    }

    # Define the parts if they are not in the output format.
    $fmt_parts{"year"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"year"}));
    $fmt_parts{"month"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"month"}));
    $fmt_parts{"day"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"day"}));
    $fmt_parts{"julian"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"julian"}));
    
    # Read in the parts of the time into their correct output order
    my @date_parts = (0,0,0,0);
    for (my $index = 0; $index < length($in_fmt); $index++) {
	my $char = substr($in_fmt,$index,1);
	
	if    ($char =~ /y/i) { 
	    $date_parts[$fmt_parts{"year"}] .= substr($date,$index,1); }
	elsif ($char =~ /m/i) { 
	    $date_parts[$fmt_parts{"month"}]  .= substr($date,$index,1); }
	elsif ($char =~ /d/i) { 
	    $date_parts[$fmt_parts{"day"}]  .= substr($date,$index,1); }
	elsif ($char =~ /j/i) {
	    $date_parts[$fmt_parts{"julian"}] .= substr($date,$index,1); }
    }

    # Convert the date to julian or the other way depending on what came in.
    if ($date_parts[$fmt_parts{"julian"}] == 0) {
	$date_parts[$fmt_parts{"julian"}] = 
	    convertJulian($date_parts[$fmt_parts{"year"}],
			  $date_parts[$fmt_parts{"day"}],
			  $date_parts[$fmt_parts{"month"}]);
    } else {
	($date_parts[$fmt_parts{"month"}],$date_parts[$fmt_parts{"day"}]) = 
	    convertJulian($date_parts[$fmt_parts{"year"}],
			  $date_parts[$fmt_parts{"julian"}]);
    }

    # Define the lengths of each field from the output format.
    my @parts = (undef(),undef(),undef(),undef());
    $parts[$fmt_parts{"year"}] = length($1) if ($out_fmt =~ /(y+)/i);
    $parts[$fmt_parts{"month"}] = length($1) if ($out_fmt =~ /(m+)/i);
    $parts[$fmt_parts{"day"}] = length($1) if ($out_fmt =~ /(d+)/i);
    $parts[$fmt_parts{"julian"}] = length($1) if ($out_fmt =~ /(j+)/i);
    
    
    # Define the sprintf format that is equivalent to the output format.
    my $format = $out_fmt;

    # Day needs to be first to prevent from replacing the %d with the format in later
    # format replacements
    if (defined($parts[$fmt_parts{"day"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"day"}]);
	$format =~ s/d+/$length/i;
    } if (defined($parts[$fmt_parts{"month"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"month"}]);
	$format =~ s/m+/$length/i;
    } if (defined($parts[$fmt_parts{"year"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"year"}]);
	$format =~ s/y+/$length/i;
    } if (defined($parts[$fmt_parts{"julian"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"julian"}]);
	$format =~ s/j+/$length/i;
    }

    # Create the time in the output format.
    return sprintf($format,@date_parts);
}

##--------------------------------------------------------------------------
# @signature String formatTime(String time, String in_fmt, String out_fmt)
# <p>Format a time from on format into another format.  It recoginizes:
# <ul><li>H - for hours</li><li>M - for minutes</li><li>S - for seconds</li>
# </ul>The formats are case insensitive.
# 
# @input $time The time to be formatted.
# @input $in_fmt The format of the time.
# @input $out_fmt The output format for the time.
# @output $time The time formatted in the output format.
##--------------------------------------------------------------------------
sub formatTime {
    # Ensure valid parameters
    if (scalar(@_) != 3) {
	die("Invalid parameters to formatTime.\n");
    }
    my ($time,$in_fmt,$out_fmt) = @_;

    # Can only format if the time is the same length as the input format.
    if (length($time) != length($in_fmt)) {
	die(sprintf("Length of time (%s) is not the same as the length of the format (%s) in formating the time.\n", $time, $in_fmt));
    }
    
    # Get the output format order for hours,mins,secs
    my %fmt_parts;
    for (my $index = 0; $index < length($out_fmt); $index++) {
	my $char = substr($out_fmt,$index,1);
	
	if ($char =~ /h/i && !defined($fmt_parts{"hour"})) { 
	    $fmt_parts{"hour"} = scalar(keys(%fmt_parts));
	} elsif ($char =~ /m/i && !defined($fmt_parts{"min"})) {
	    $fmt_parts{"min"} = scalar(keys(%fmt_parts));
	} elsif ($char =~ /s/i && !defined($fmt_parts{"sec"})) {
	    $fmt_parts{"sec"} = scalar(keys(%fmt_parts));
	}
    }

    # Define the parts if they are not in the output format.
    $fmt_parts{"hour"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"hour"}));
    $fmt_parts{"min"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"min"}));
    $fmt_parts{"sec"} = scalar(keys(%fmt_parts)) if (!defined($fmt_parts{"sec"}));
    
    # Read in the parts of the time into their correct output order
    my @time_parts = (0,0,0);
    for (my $index = 0; $index < length($in_fmt); $index++) {
	my $char = substr($in_fmt,$index,1);
	
	if    ($char =~ /h/i) { 
	    $time_parts[$fmt_parts{"hour"}] .= substr($time,$index,1); }
	elsif ($char =~ /m/i) { 
	    $time_parts[$fmt_parts{"min"}]  .= substr($time,$index,1); }
	elsif ($char =~ /s/i) { 
	    $time_parts[$fmt_parts{"sec"}]  .= substr($time,$index,1); 
	}
    }

    # Define the lengths of each field from the output format.
    my @parts = ("","","");
    $out_fmt =~ /(h+)/i;
    $parts[$fmt_parts{"hour"}] = length($1) if (defined($1));
    $out_fmt =~ /(m+)/i;
    $parts[$fmt_parts{"min"}] = length($1) if (defined($1));
    $out_fmt =~ /(s+)/i;
    $parts[$fmt_parts{"sec"}] = length($1) if (defined($1));
    
    
    # Define the sprintf format that is equivalent to the output format.
    my $format = $out_fmt;
    if (defined($parts[$fmt_parts{"hour"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"hour"}]);
	$format =~ s/h+/$length/i;
    } if (defined($parts[$fmt_parts{"min"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"min"}]);
	$format =~ s/m+/$length/i;
    } if (defined($parts[$fmt_parts{"sec"}])) {
	my $length = sprintf("%s0%dd","%",$parts[$fmt_parts{"sec"}]);
	$format =~ s/s+/$length/i;
    }

    # Create the time in the output format.
    return sprintf($format,@time_parts);
}

##--------------------------------------------------------------------------
# @signature (String date, String time) getDateFromSeconds(int seconds)
# <p>Convert a Unix time in seconds past Jan 1 1970 to a date and time.</p>
#
# @input seconds The Unix time in seconds past Jan 1 1970.
# @output date The UTC date of the Unix time in YYYY/MM/DD format.
# @output time The UTC time of the Unix time in HH:MM:SS format.
##--------------------------------------------------------------------------
sub getDateFromSeconds {

    if (scalar(@_) != 1) {
	die("Invalid parameters to getDateFromSeconds.\n");
    }

    # Parse out the time pieces from the provided seconds.
    my ($day_of_week, $month, $day, $hour, $min, $sec, $year) =
	split(' ', strftime("%a %b %e %H %M %S %Y", gmtime($_[0])));

    # Get the number of the month
    if ($month =~ /Jan/) { $month = 1; }
    elsif ($month =~ /Feb/) { $month = 2; }
    elsif ($month =~ /Mar/) { $month = 3; }
    elsif ($month =~ /Apr/) { $month = 4; }
    elsif ($month =~ /May/) { $month = 5; }
    elsif ($month =~ /Jun/) { $month = 6; }
    elsif ($month =~ /Jul/) { $month = 7; }
    elsif ($month =~ /Aug/) { $month = 8; }
    elsif ($month =~ /Sep/) { $month = 9; }
    elsif ($month =~ /Oct/) { $month = 10; }
    elsif ($month =~ /Nov/) { $month = 11; }
    elsif ($month =~ /Dec/) { $month = 12; }
    
    return (sprintf("%04d/%02d/%02d", $year, $month, $day),
	    sprintf("%02d:%02d:%02d", $hour, $min, $sec));
}

##--------------------------------------------------------------------------
# @signature int validDate(String date, String format)
# <p>Determine if a date is a real date.</p>
#
# @input $date The date to check.
# @input $format The format of the date using "YMDJ" codes.
# @output $valid 1 if the specified date is a valid date, 0 otherwise.
# @limitation The year must be 4 digits.
##--------------------------------------------------------------------------
sub validDate {
    if (scalar(@_) != 2) {
	die("Invalid parameters to validDate.\n");
    }
    my ($date,$format) = @_;

    # Parse the date with the format.
    my ($year,$month,$day) = ("","","");
    for (my $index = 0; $index < length($format); $index++) {
	my $char = substr($format,$index,1);

	if ($char =~ /y/i) { $year .= substr($date,$index,1); }
	elsif ($char =~ /m/i) { $month .= substr($date,$index,1); }
	elsif ($char =~ /d/i) { $day .= substr($date,$index,1); }
    }

    # Make sure the date is 4 digits
    if (length($year) < 4) {
	die("Valid Date:  Length of year is not 4.\n");
    }

    # Check if valid day for the given month
    my @daysInMonth = (31,daysInFeb($year),31,30,31,30,31,31,30,31,30,31);
    
    if (0 < $year && $year < 9999 &&
	$month ne "" && 1 <= $month && $month <= 12 &&
	$day ne "" && 1 <= $day && $day <= $daysInMonth[$month - 1]) {
	return 1;
    }

    return 0;
}

##--------------------------------------------------------------------------
# @signature int validTime(String time, String format)
# <p>Determine if a time is a real time.</p>
#
# @input $time The time to check.
# @input $format The format of the time using "HMS" codes.
# @output $valid 1 if the specified time is a valid time, 0 otherwise.
##--------------------------------------------------------------------------
sub validTime {
    
    if (scalar(@_) != 2) {
	die("Invalid parameters to validTime.\n");
    }
    my ($time,$format) = @_;

    my ($hour,$min,$sec) = (0,0,0);
    for (my $index = 0; $index < length($format); $index++) {
	my $char = substr($format,$index,1);

	if ($char =~ /h/i) { $hour .= substr($time,$index,1); }
	elsif ($char =~ /m/i) { $min .= substr($time,$index,1); }
	elsif ($char =~ /s/i) { $sec .= substr($time,$index,1); }
    }

    if (defined($hour) && defined($min) && defined($sec) &&
	$hour =~ /^\d+$/ && 0 <= $hour && $hour <= 23 && 
	$min  =~ /^\d+$/ && 0 <= $min  && $min  <= 59 &&
	$sec  =~ /^\d+$/ && 0 <= $sec  && $sec  <= 59) {
	return 1;
    } else {
	return 0;
    }
}

1; # Needed to return from a script call
