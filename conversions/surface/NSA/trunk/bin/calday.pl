#!/usr/bin/perl

#----------------------------------------------------------------
# julian day routines taken from Astro::Time Perl module
#   by Chris Phillips  phillips@jive.nl
#----------------------------------------------------------------

my $Quiet = 0;
my @days = (31,28,31,30,31,30,31,31,30,31,30,31);

#----------------------------------------------------------------
# $dayno = cal2dayno($day, $month, $year);
#
# Returns the day number corresponding to $day of $month in $year.
#----------------------------------------------------------------

sub cal2dayno ($$$) {

  my ($day, $month, $year) = @_;
  return undef if (!monthOK($month));
  return undef if (!dayOK($day, $month, $year));

  $month--; # For array indexing

  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  my $mon;
  my $dayno = $day;
  for ($mon=0; $mon<$month; $mon++) {
    $dayno += $days[$mon];
  }

  return($dayno);
}


#----------------------------------------------------------------
# ($day, $month) = dayno2cal($dayno, $year);
#
# Return the $day and $month corresponding to $dayno of $year.
#----------------------------------------------------------------

sub dayno2cal ($$) {

  my($dayno, $year) = @_;
  return undef if (!daynoOK($dayno, $year));

  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  my $month = 0;
  my $end = $days[$month];
  while ($dayno>$end) {
    $month++;
    $end+= $days[$month];
  }
  $end -= $days[$month];
  my $day = $dayno - $end;
  $month++;

  return($day, $month);
}

#----------------------------------------------------------------
# Is the Julian Date valid?
#----------------------------------------------------------------
sub daynoOK($$)
{
	my $dayno = shift;
	my $year = shift;

	return ( $dayno > 1 && ( (leap($year) && $dayno <= 366) || (!leap($year) && $dayno <= 365) ) );
}

#----------------------------------------------------------------
# $isleapyear = leap($year);
#
# Returns true if $year is a leap year.
#   input: year in full
#----------------------------------------------------------------

sub leap ($) {
  my $year = shift;
  return (((!($year%4))&&($year%100))||(!($year%400)));
}


#----------------------------------------------------------------
# Is the month valid?
#----------------------------------------------------------------

sub monthOK ($) {
  my $month = shift;

  if ($month > 12 || $month < 1) {
    warn "$month out of range" if (!$Quiet);
    return 0;
  } else {
    return 1
  }
}


#----------------------------------------------------------------
# Is the day of month OK?
#----------------------------------------------------------------

sub dayOK ($$$) {
  my ($day, $month, $year) = @_;
	

  $month--;             # For array indexing
  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  if ($day < 1 || $day > $days[$month]) {
    warn "$day out of range" if (!$Quiet);
    return 0;
  } else {
    return 1;
  }
}

1;
