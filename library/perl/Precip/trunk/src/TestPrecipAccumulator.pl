#! /usr/bin/perl -w

package TestPrecipAccumulator;
use strict;
use lib "/net/work/lib/perl/Utilities";
use DpgDate qw(:DEFAULT);
use PrecipAccumulator;
use TestModule;
our @ISA = ("TestModule");

my $START_DATE = "20050430";
my $START_TIME = "0000";
my $END_DATE = "20050502";
my $END_TIME = "0000";
my $MISSING = "-999.99";

&main();

sub main {
    my $tester = TestPrecipAccumulator->new();

    $tester->test_good_1_minute();
    $tester->test_bad_1_minute();

    $tester->test_good_5_minute();
    $tester->test_bad_5_minute();

    $tester->test_good_10_minute();
    $tester->test_bad_10_minute();

    $tester->test_good_15_minute();
    $tester->test_bad_15_minute();

    $tester->test_good_20_minute();
    $tester->test_bad_20_minute();

    $tester->test_good_30_minute();
    $tester->test_bad_30_minute();

    $tester->test_good_60_minute();
    $tester->test_bad_60_minute();
}

sub assign_full_data {
    my ($self,$accumulator,$freq) = @_;

    my ($date,$time) = ($START_DATE,$START_TIME);
    while (sprintf("%08d%04d",$date,$time) <= sprintf("%08d%04d",$END_DATE,$END_TIME)) {
	$accumulator->addPrecip($date,"YYYYMMDD",$time,"HHMM",$freq);
	($date,$time) = adjustDateTime($date,"YYYYMMDD",$time,"HHMM",0,0,$freq,0);
    }
}

sub assign_partial_data {
    my ($self,$accumulator,$freq) = @_;
    my ($date,$time) = ($START_DATE,$START_TIME);
    while (sprintf("%08d%04d",$date,$time) <= sprintf("%08d%04d",$END_DATE,$END_TIME)) {
	$accumulator->addPrecip($date,"YYYYMMDD",$time,"HHMM",$freq) 
	    unless ($time == $freq || ($freq == 60 && $time == 0));
	($date,$time) = adjustDateTime($date,"YYYYMMDD",$time,"HHMM",0,0,$freq,0);
    }
}

sub create_full_accumulator {
    my ($self,$in,$out) = @_;

    my $accumulator = PrecipAccumulator->new(*STDOUT,$in,$out,$MISSING);
    $self->assign_full_data($accumulator,$in);
    return $accumulator;
}

sub create_partial_accumulator {
    my ($self,$in,$out) = @_;

    my $accumulator = PrecipAccumulator->new(*STDOUT,$in,$out,$MISSING);
    $self->assign_partial_data($accumulator,$in);
    return $accumulator;
}

sub test_no_missing_accumulation {
    my ($self,$accumulator,$freq) = @_;

    my ($date,$time) = ($START_DATE,sprintf("%04d",$START_TIME));

    # The first value should be missing.
    $self->assertValue($MISSING,$accumulator->getAccumulation($date,"YYYYMMDD",$time,"HHMM"),
		       "No Missing $freq: $date $time");
    ($date,$time) = adjustDateTime($date,"YYYYMMDD",$time,"HHMM",0,0,$freq,0);
    
    # The rest of the data should equal the frequency
    while (sprintf("%08d%04d",$date,$time) <= sprintf("%08d%04d",$END_DATE,$END_TIME)) {
	$self->assertValue($freq,$accumulator->getAccumulation($date,"YYYYMMDD",$time,"HHMM"),
			   "No Missing $freq: $date $time");
	($date,$time) = adjustDateTime($date,"YYYYMMDD",$time,"HHMM",0,0,$freq,0);
    }
}

sub test_some_missing_accumulation {
    my ($self,$accumulator,$freq) = @_;
    my ($date,$time) = ($START_DATE,sprintf("%04d",$START_TIME));

    # The first value should be missing.
    $self->assertValue($MISSING,$accumulator->getAccumulation($date,"YYYYMMDD",$time,"HHMM"),
		       "Some Missing $freq: $date $time");
    ($date,$time) = adjustDateTime($date,"YYYYMMDD",$time,"HHMM",0,0,$freq,0);
    
    # The rest should equal the frequency except when the time == freq
    while (sprintf("%08d%04d",$date,$time) <= sprintf("%08d%04d",$END_DATE,$END_TIME)) {
	$self->assertValue($time == $freq || $freq == 1440 || ($freq == 60 && $time == 100) ? 
			   $MISSING : $freq,
			   $accumulator->getAccumulation($date,"YYYYMMDD",$time,"HHMM"),
			   "Some Missing $freq: $date $time");
	($date,$time) = adjustDateTime($date,"YYYYMMDD",$time,"HHMM",0,0,$freq,0);
    }
}

sub test_bad_1_minute {
    my ($self) = @_;
    my $accum;

    printf("Testing partial missing 1 to 5 minute\n");
    $accum = $self->create_partial_accumulator(1,5);
    $self->test_some_missing_accumulation($accum,5);

    printf("Testing partial missing 1 to 10 minute\n");
    $accum = $self->create_partial_accumulator(1,10);
    $self->test_some_missing_accumulation($accum,10);

    printf("Testing partial missing 1 to 15 minute\n");
    $accum = $self->create_partial_accumulator(1,15);
    $self->test_some_missing_accumulation($accum,15);

    printf("Testing partial missing 1 to 20 minute\n");
    $accum = $self->create_partial_accumulator(1,20);
    $self->test_some_missing_accumulation($accum,20);

    printf("Testing partial missing 1 to 30 minute\n");
    $accum = $self->create_partial_accumulator(1,30);
    $self->test_some_missing_accumulation($accum,30);

    printf("Testing partial missing 1 minute to hourly\n");
    $accum = $self->create_partial_accumulator(1,60);
    $self->test_some_missing_accumulation($accum,60);

    printf("Testing partial missing 1 minute to daily\n");
    $accum = $self->create_partial_accumulator(1,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_bad_5_minute {
    my ($self) = @_;
    my $accum = undef();

    printf("Testing partial missing 5 to 10 minute\n");
    $accum = $self->create_partial_accumulator(5,10);
    $self->test_some_missing_accumulation($accum,10);

    printf("Testing partial missing 5 to 15 minute\n");
    $accum = $self->create_partial_accumulator(5,15);
    $self->test_some_missing_accumulation($accum,15);

    printf("Testing partial missing 5 to 20 minute\n");
    $accum = $self->create_partial_accumulator(5,20);
    $self->test_some_missing_accumulation($accum,20);

    printf("Testing partial missing 5 to 30 minute\n");
    $accum = $self->create_partial_accumulator(5,30);
    $self->test_some_missing_accumulation($accum,30);

    printf("Testing partial missing 5 minute to hourly\n");
    $accum = $self->create_partial_accumulator(5,60);
    $self->test_some_missing_accumulation($accum,60);

    printf("Testing partial missing 5 minute to daily\n");
    $accum = $self->create_partial_accumulator(5,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_bad_10_minute {
    my ($self) = @_;
    my $accum = undef();

    printf("Testing partial missing 10 to 20 minute\n");
    $accum = $self->create_partial_accumulator(10,20);
    $self->test_some_missing_accumulation($accum,20);

    printf("Testing partial missing 10 to 30 minute\n");
    $accum = $self->create_partial_accumulator(10,30);
    $self->test_some_missing_accumulation($accum,30);

    printf("Testing partial missing 10 minute to hourly\n");
    $accum = $self->create_partial_accumulator(10,60);
    $self->test_some_missing_accumulation($accum,60);

    printf("Testing partial missing 10 minute to daily\n");
    $accum = $self->create_partial_accumulator(10,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_bad_15_minute {
    my ($self) = @_;
    my $accum = undef();

    printf("Testing partial missing 15 to 30 minute\n");
    $accum = $self->create_partial_accumulator(15,30);
    $self->test_some_missing_accumulation($accum,30);

    printf("Testing partial missing 15 minute to hourly\n");
    $accum = $self->create_partial_accumulator(15,60);
    $self->test_some_missing_accumulation($accum,60);

    printf("Testing partial missing 15 minute to daily\n");
    $accum = $self->create_partial_accumulator(15,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_bad_20_minute {
    my ($self) = @_;
    my $accum = undef();

    printf("Testing partial missing 20 minute to hourly\n");
    $accum = $self->create_partial_accumulator(20,60);
    $self->test_some_missing_accumulation($accum,60);

    printf("Testing partial missing 20 minute to daily\n");
    $accum = $self->create_partial_accumulator(20,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_bad_30_minute {
    my ($self) = @_;
    my $accum = undef();

    printf("Testing partial missing 30 minute to hourly\n");
    $accum = $self->create_partial_accumulator(30,60);
    $self->test_some_missing_accumulation($accum,60);

    printf("Testing partial missing 30 minute to daily\n");
    $accum = $self->create_partial_accumulator(30,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_bad_60_minute {
    my ($self) = @_;
    my $accum = undef();

    printf("Testing partial missing hourly to daily\n");
    $accum = $self->create_partial_accumulator(60,1440);
    $self->test_some_missing_accumulation($accum,1440);
}

sub test_good_1_minute {
    my $self = shift;

    printf("Testing 1 to 5 minute\n");
    my $accum = $self->create_full_accumulator(1,5);
    $self->test_no_missing_accumulation($accum,5);

    printf("Testing 1 to 10 minute\n");
    $accum = $self->create_full_accumulator(1,10);
    $self->test_no_missing_accumulation($accum,10);

    printf("Testing 1 to 15 minute\n");
    $accum = $self->create_full_accumulator(1,15);
    $self->test_no_missing_accumulation($accum,15);
    
    printf("Testing 1 to 20 minute\n");
    $accum = $self->create_full_accumulator(1,20);
    $self->test_no_missing_accumulation($accum,20);
    
    printf("Testing 1 to 30 minute\n");
    $accum = $self->create_full_accumulator(1,30);
    $self->test_no_missing_accumulation($accum,30);

    printf("Testing 1 minute to hourly\n");
    $accum = $self->create_full_accumulator(1,60);
    $self->test_no_missing_accumulation($accum,60);

    printf("Testing 1 minute to daily\n");
    $accum = $self->create_full_accumulator(1,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

sub test_good_5_minute {
    my $self = shift;
    my $accum = undef();

    printf("Testing 5 to 10 minute\n");
    $accum = $self->create_full_accumulator(5,10);
    $self->test_no_missing_accumulation($accum,10);

    printf("Testing 5 to 15 minute\n");
    $accum = $self->create_full_accumulator(5,15);
    $self->test_no_missing_accumulation($accum,15);
    
    printf("Testing 5 to 20 minute\n");
    $accum = $self->create_full_accumulator(5,20);
    $self->test_no_missing_accumulation($accum,20);
    
    printf("Testing 5 to 30 minute\n");
    $accum = $self->create_full_accumulator(5,30);
    $self->test_no_missing_accumulation($accum,30);

    printf("Testing 5 minute to hourly\n");
    $accum = $self->create_full_accumulator(5,60);
    $self->test_no_missing_accumulation($accum,60);

    printf("Testing 5 minute to daily\n");
    $accum = $self->create_full_accumulator(5,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

sub test_good_10_minute {
    my $self = shift;
    my $accum = undef();

    printf("Testing 10 to 20 minute\n");
    $accum = $self->create_full_accumulator(10,20);
    $self->test_no_missing_accumulation($accum,20);
    
    printf("Testing 10 to 30 minute\n");
    $accum = $self->create_full_accumulator(10,30);
    $self->test_no_missing_accumulation($accum,30);

    printf("Testing 10 minute to hourly\n");
    $accum = $self->create_full_accumulator(10,60);
    $self->test_no_missing_accumulation($accum,60);

    printf("Testing 10 minute to daily\n");
    $accum = $self->create_full_accumulator(10,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

sub test_good_15_minute {
    my $self = shift;
    my $accum = undef();

    printf("Testing 15 to 30 minute\n");
    $accum = $self->create_full_accumulator(15,30);
    $self->test_no_missing_accumulation($accum,30);

    printf("Testing 15 minute to hourly\n");
    $accum = $self->create_full_accumulator(15,60);
    $self->test_no_missing_accumulation($accum,60);

    printf("Testing 15 minute to daily\n");
    $accum = $self->create_full_accumulator(15,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

sub test_good_20_minute {
    my $self = shift;
    my $accum = undef();

    printf("Testing 20 minute to hourly\n");
    $accum = $self->create_full_accumulator(20,60);
    $self->test_no_missing_accumulation($accum,60);

    printf("Testing 20 minute to daily\n");
    $accum = $self->create_full_accumulator(20,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

sub test_good_30_minute {
    my $self = shift;
    my $accum = undef();

    printf("Testing 30 minute to hourly\n");
    $accum = $self->create_full_accumulator(30,60);
    $self->test_no_missing_accumulation($accum,60);

    printf("Testing 30 minute to daily\n");
    $accum = $self->create_full_accumulator(30,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

sub test_good_60_minute {
    my $self = shift;
    my $accum = undef();

    printf("Testing hourly to daily\n");
    $accum = $self->create_full_accumulator(60,1440);
    $self->test_no_missing_accumulation($accum,1440);
}

1;
