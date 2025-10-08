#!/usr/bin/perl

use Getopt::Std;
use Time::gmtime;
#use Time::localtime;
use strict;

#************************
# script to list the times from
# a .dat file (used to detect
# errors)
# SJS 08/2009
#************************

if ( $#ARGV < 0 )  {
  print "list_times.pl\n";
  print "\t-i: input file (.dat)\n";
  print "\t-o: output file\n";
  print "\t-l: [true or false] limit output (print only begin & end time)\n";
  exit();
}

our($opt_i, $opt_o, $opt_l);

&getopt('iol');
my $in_fname = $opt_i;
my $out_fname = $opt_o;
my $limit = $opt_l;

die "Sorry, $in_fname doesn't exist\n" if ( !-e $in_fname );

open(IN, $in_fname) || die "cannot open $in_fname";
my ($base_time,$timestamp, @epoch_seconds, $fname);
my $count = 0;
while ( <IN> ) {
  chop;
  # get the name of this file from
  # the file just in case the filename
  # doesn't match the real filename
  if ( /^netcdf/ ) {
    $fname = $_;
    $fname =~ s/netcdf //g;
  } # endif
  s/\s+//g;	# get rid of any spaces
  s/;//g;	# get rid of the trailing ';'
  my ($id, $value) = split(/:/, $_);
  $base_time = $value if ( $id eq 'base_time' );
  if ( $id eq  'time_offset' ) {
    my @values = split(/,/, $value);
    foreach $timestamp (@values) {
      #print "base_time = $base_time and timestamp = $timestamp\n";
      $epoch_seconds[$count] = $base_time + $timestamp;
      $count++;
    } # end foreach
  } # endif
} # end while
close(IN);

my ($i, $year, $month, $day, $hour, $min, $sec, $tm);
print "$fname: ";
my $num_records = $#epoch_seconds + 1;
if ( $limit eq 'true' ) {
  print "num records: $num_records   time limits: ";
  $tm = gmtime($epoch_seconds[0]);
  $year = $tm->year+1900;
  $month = $tm->mon+1;
  $day = $tm->mday;
  $hour = $tm->hour;
  $min = $tm->min;
  $sec = $tm->sec;
  printf ("%02d/%02d/%4d %02d:%02d:%02d %s",
  $month,$day,$year,$hour,$min,$sec, " - " );
  $tm = gmtime($epoch_seconds[$#epoch_seconds]);
  $year = $tm->year+1900;
  $month = $tm->mon+1;
  $day = $tm->mday;
  $hour = $tm->hour;
  $min = $tm->min;
  $sec = $tm->sec;
  printf ("%02d/%02d/%4d %02d:%02d:%02d\n",
  $month,$day,$year,$hour,$min,$sec);
} else {
  print "\n";
  foreach $i (@epoch_seconds) {
    $tm = gmtime($i);
    $year = $tm->year+1900;
    $month = $tm->mon+1;
    $day = $tm->mday;
    $hour = $tm->hour;
    $min = $tm->min;
    $sec = $tm->sec;
    printf ("\t%02d/%02d/%4d %02d:%02d:%02d\n",
    $month,$day,$year,$hour,$min,$sec);
  } # end foreach
} # endif
