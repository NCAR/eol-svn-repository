#! /usr/bin/perl -w

use strict;

my $INPUT_DIR = "../final";
my $SINGLE_ID = "../qc_versions/single_id";
my $SINGLE_STATION = "../qc_versions/single_station";

my $DEFAULT_ID = "MISS";
my $DEFAULT_LATITUDE = 36.72167;
my $DEFAULT_LONGITUDE = -118.14167;
my $DEFAULT_ELEVATION = 1146.00;

my $MAPPING = {};
$MAPPING->{"B"}->{"code"} = 1;
$MAPPING->{"D"}->{"code"} = 2;
$MAPPING->{"F2"}->{"code"} = 3;
$MAPPING->{"F6"}->{"code"} = 4;
$MAPPING->{"OV2"}->{"code"} = 5;

&main();

sub main {
  mkdir($SINGLE_STATION) unless (-e $SINGLE_STATION);
  mkdir($SINGLE_ID) unless (-e $SINGLE_ID);

  opendir(my $INPUT,$INPUT_DIR) or die("Can't read $INPUT_DIR\n");
  my @files = grep(/\.0qc$/,readdir($INPUT));
  closedir($INPUT);

  foreach my $file (@files) {
    open(my $FILE, sprintf("%s/%s",$INPUT_DIR,$file)) or die("Can't read $file\n");
    open(my $OUT_ID, sprintf(">%s/%s",$SINGLE_ID,$file)) or die("Can't write $SINGLE_ID/$file\n");
    open(my $OUT_STN, sprintf(">%s/%s",$SINGLE_STATION,$file)) or die("Can't write $SINGLE_STATION/$file\n");
    foreach my $line (<$FILE>) {
      printf($OUT_ID "%s %-15s %s %02d %s",substr($line,0,44),$DEFAULT_ID,substr($line,61,193),
	     $MAPPING->{trim(substr($line,44,15))}->{"code"},substr($line,258));
      printf($OUT_STN "%s %-15s %10.5f %11.5f   0 %7.2f %s %02d %s",substr($line,0,44),$DEFAULT_ID,
	     $DEFAULT_LATITUDE, $DEFAULT_LONGITUDE,$DEFAULT_ELEVATION,
	     substr($line,96,158),$MAPPING->{trim(substr($line,44,15))}->{"code"},substr($line,258));
    }
    close($FILE);
    close($OUT_ID);
    close($OUT_STN);
  }
}

sub trim {
  my ($line) = @_;
  $line =~ s/^\s+//;
  $line =~ s/\s+$//;
  return $line;
}
