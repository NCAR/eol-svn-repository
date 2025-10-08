package LTime;

# --------------------------------------------------------------
# LTime.pm ("Log Time"):
#  A nice lid module used during testing to keep track of how
#  long scripts execute.  Should not be in use upon release.
# --------------------------------------------------------------

use Time::HiRes qw(gettimeofday);

my $start = gettimeofday();

sub restart
{
	$start = gettimeofday();
}

sub log
{
	my $str = shift;

	open( TLOG, ">>time_log" );

	my $end = gettimeofday();
	my $duration = $end - $start;
	print( TLOG "$str: start: $start end: $end duration: $duration\n" );

	close( TLOG );
}
1;
