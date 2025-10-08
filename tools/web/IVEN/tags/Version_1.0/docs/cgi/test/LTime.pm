package LTime;

# --------------------------------------------------------------
# LTime.pm ("Log Time"):
#  A nice lil module used during testing to keep track of how
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

	#open( TLOG, ">>time_log" );

	my $end = gettimeofday();
	my $duration = $end - $start;
	#print( TLOG "$str: start: $start end: $end duration: $duration\n" );
	#printf( TLOG "%-20s-> start: %16.5f end: %16.5f duration: %8.5f\n", $str, $start, $end, $duration ); 
	printf( "%-20s-> start: %16.5f end: %16.5f duration: %8.5f\n", $str, $start, $end, $duration ); 
	#close( TLOG );

	$start = gettimeofday();
}
1;
