#! /usr/bin/perl -w


package TestDpgDate;
use strict;
use lib ".";
use TestModule;
use DpgDate qw(:DEFAULT);
our @ISA = ("TestModule");

&main();


sub main {
    my $tester = TestDpgDate->new();

    $tester->testAdjustDateTime();
    $tester->testCompDates();
    $tester->testConvJulian();
    $tester->testDaysFeb();


    $tester->testFormatDate();
    $tester->testFormatTime();

    $tester->testValidDate();
    $tester->testValidTime();
}

sub testAdjustDateTime {
    my $self = shift;

    my ($date,$time) = adjustDateTime("2004/05/20","YYYY/MM/DD","15:20","HH:MM",1,3,13,0);
    $self->assertString("2004/05/21", $date, "Good date");
    $self->assertString("18:33", $time, "Good time");

    ($date, $time) = adjustDateTime("20041231","YYYYMMDD","235959","HHMMSS",0,0,0,1);
    $self->assertString("20050101", $date, "Next Year");
    $self->assertString("000000", $time, "Next Minute and Hour");
    
    ($date, $time) = adjustDateTime("2004/01/01","YYYY/MM/DD","00:00","HH:MM",
					  0,0,-1,0);
    $self->assertString("2003/12/31", $date, "Previous Year");
    $self->assertString("23:59", $time, "Previous Minute and Hour");
    
    ($date, $time) = adjustDateTime("2004/05/20","YYYY/MM/DD","19:32","HH:MM",
					  0,1,-60,0);
    $self->assertString("2004/05/20", $date, "Matching offset date");
    $self->assertString("19:32", $time, "Matching offset time");
}

sub testCompDates {
    my $self = shift;

    $self->assertValue(1,compareDates("20040503","YYYYMMDD","2004/05/04","YYYY/MM/DD") > 0,"Compare Dates: Start < End");
    $self->assertValue(1,compareDates("2005/03/12","YYYY/DD/MM","20030303","YYYYMMDD") < 0,"Compare Dates: Start > End");
    $self->assertValue(0,compareDates("20050210","YYYYMMDD","02-10-2005","MM-DD-YYYY"),"Compare Dates: Start == End");
}

sub testConvJulian {
    my $self = shift;
    my ($month, $day);

    ($month, $day) = convertJulian(2003, 1);
    $self->assertValue(1, $month, "2003 001 -> 2003/01/01 month");
    $self->assertValue(1, $day, "2003 001 -> 2003/01/01 day");
    ($month, $day) = convertJulian(2003, 365);
    $self->assertValue(12, $month, "2003 365 -> 2003/12/31 month");
    $self->assertValue(31, $day, "2003 365 -> 2003/12/31 day");
    ($month, $day) = convertJulian(2004, 1);
    $self->assertValue(1, $month, "2004 001 -> 2004/01/01 month");
    $self->assertValue(1, $day, "2004 001 -> 2004/01/01 day");
    ($month, $day) = convertJulian(2004, 366);
    $self->assertValue(12, $month, "2004 365 -> 2004/12/31 month");
    $self->assertValue(31, $day, "2004 365 -> 2004/12/31 day");

    $self->assertValue(1, convertJulian(2003,1,1),
		       "2003/01/01 -> 001");
    $self->assertValue(365, convertJulian(2003,31,12),
		       "2003/12/31 -> 365");
    $self->assertValue(1, convertJulian(2004,1,1),
		       "2004/01/01 -> 001");
    $self->assertValue(366, convertJulian(2004,31,12),
		       "2004/12/31 -> 366");
}

sub testDaysFeb {
    my $self = shift;

    $self->assertValue(28, daysInFeb(2003), "2003 Feb");
    $self->assertValue(29, daysInFeb(2004), "2004 Feb");
    $self->assertValue(29, daysInFeb(2000), "2000 Feb");
    $self->assertValue(28, daysInFeb(1900), "1900 Feb");
}

sub testFormatDate {
    my $self = shift;

    $self->assertString("2005/02/10",formatDate("02102005","MMDDYYYY","YYYY/MM/DD"),
			"Format Date: YYYY/MM/DD");
    $self->assertString("20050210",formatDate("10022005","DDMMYYYY","YYYYMMDD"),
			"Format Date: YYYYMMDD");
    $self->assertString("10-2005-02",formatDate("2005/02/10","YYYY/MM/DD",
						      "DD-YYYY-MM"),
			"Format Date: DD-YYYY-MM");
    $self->assertString("2005-031",formatDate("2005/01/31","YYYY/MM/DD","YYYY-JJJ"),
			"Format Date: YYYY-JJJ");
    $self->assertString("20050131",formatDate("2005031","YYYYJJJ","YYYYMMDD"),
			"Format Date: From julian");
}

sub testFormatTime {
    my $self = shift;
    
    $self->assertString("12,23,44",formatTime("23:44:12","HH:MM:SS","SS,HH,MM"),
			"Format Time: reorder output");
    $self->assertString("12:44",formatTime("124408","HHMMSS","HH:MM"),
			"Format Time: dropping seconds");
    $self->assertString("00:00",formatTime("0:0:0","H:M:S","HH:MM"),
			"Format Time: formating zeros");
    $self->assertString("12:00:00",formatTime("12","HH","HH:MM:SS"),
			"Format Time: adding minutes and seconds");
}

sub testValidDate {
    my $self = shift;

    $self->assertValue(1,validDate("2005/01/01","YYYY/MM/DD"),
		       "Valid Date: standard date format");
    $self->assertValue(1,validDate("2004/02/29","YYYY/MM/DD"),
		       "Valid Date: leap year");
    $self->assertValue(1,validDate("20051231","YYYYMMDD"),
		       "Valid Date: YYYYMMDD");
    $self->assertValue(1,validDate("04041847","DDMMYYYY"),
		       "Valid Date: DDMMYYYY");
    $self->assertValue(1,validDate("20000229","YYYYMMDD"),"Valid Date: 400 year");

    $self->assertValue(0,validDate("20040001","YYYYMMDD"),"Valid Date: 0 month");
    $self->assertValue(0,validDate("20041301","YYYYMMDD"),"Valid Date: 13 month");
    $self->assertValue(0,validDate("20040600","YYYYMMDD"),"Valid Date: 0 day");
    $self->assertValue(0,validDate("20040631","YYYYMMDD"),"Valid Date: 6/31");
    $self->assertValue(0,validDate("20030229","YYYYMMDD"),
		       "Valid Date: bad leap year");
    $self->assertValue(0,validDate("19000229","YYYYMMDD"),"Valid Date: 100 year");
}

sub testValidTime {
    my $self = shift;

    # Valid times
    $self->assertValue(1,validTime("00:00:00","HH:MM:SS"),"Valid Time: all zeros");
    $self->assertValue(1,validTime("00:00","MM:HH"),
		       "Valid Time: all zeros (no secs)");
    $self->assertValue(1,validTime("23:59:59","HH:MM:SS"),"Valid Time: 23:59:59");
    $self->assertValue(1,validTime("23:59","HH:MM"),"Valid Time: 23:59");
    $self->assertValue(1,validTime("13,32,34","HH,MM,SS"),
		       "Valid Time: comma delimited");
    $self->assertValue(1,validTime("212112","HHMMSS"),"Valid Time: no delimiters");
    $self->assertValue(1,validTime("1331","HHMM"),"Valid Time: no delim, no secs");

    # Bad Value check
    $self->assertValue(0,validTime("-10000","HHMMSS"),"Valid Time: negative hour");
    $self->assertValue(0,validTime("240000","HHMMSS"),"Valid Time: 24 hour");
    $self->assertValue(0,validTime("00-100","HHMMSS"),"Valid Time: negative minute");
    $self->assertValue(0,validTime("00:60:00","HH:MM:SS"),"Valid Time: 60 minute");
    $self->assertValue(0,validTime("000-1","HMMSS"),"Valid Time: negative second");
    $self->assertValue(0,validTime("00060","HHMSS"),"Valid Time: 60 second");
}



