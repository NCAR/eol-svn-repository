#!/usr/bin/perl -I /net/work/CEOP/version2/data_processing/other/NSA/susans_playground

use Getopt::Std;
require "conversion.constants.twr.pl";

# print the specified column and flag to check against
# the DQR values

if ( $#ARGV < 0 ) {
  print "USAGE: print_final_twr_column.pl\n";
  print "\t-i: twr input file name (.twr)\n";
  print "\t-n: column name\n";
  print "\t-h: height\n";
  exit();
} else {
   getopt(inh);
} # endif

my $params = &params();

# the column number in the data file
# note: the keys must match the categories
# as described in conversion.constants.pl
my %index;
$index{'lat'} = 7;
$index{'lon'} = 8;
$index{'alt'} = 9;
$index{'height'} = 10;
$index{'stn_pres'} = 11;
$index{'temp_air'} = 13;
$index{'dew_pt'} = 15;
$index{'rel_hum'} = 17;
$index{'spec_hum'} = 19;
$index{'wind_spd'} = 21;
$index{'wind_dir'} = 23;
$index{'u_wind'} = 25;
$index{'v_wind'} = 27;

# the names of the columns
my @column_name = keys(%index);

my $twr_file = $opt_i;
#my $dat_dir = $opt_d;
my $name_to_find = lc($opt_n);
my $height_to_find = $opt_h;

if ( !-e $twr_file ) {
  print "ERROR: input file: $twr_file does not exist!!\n";
  exit();
}

#if ( !-e $dat_dir ) {
#  print "ERROR: data directory: $dat_dir does not exist!!\n";
#  exit();
#}

# make sure that the name_to_find exists!
my $column;
my $found = grep(/\b$name_to_find\b/i, @column_name);
if ( !$found ) {
  print "Sorry, $can't find $name_to_find\n";
  print "Choices are:\n";
  foreach $column (@column_name) {
    print "\t$column\n";
  } # end foreach
  exit();
} # endif

# make sure the user entered a height
if ( !$height_to_find ) {
  print "ERROR: need to specify a height..\n";
  exit();
} # endif

open(IN, $twr_file) || die "cannot open $twr_file";

my @tmp;
my $index_to_find = $index{$name_to_find};
my $flag_index_to_find = $index_to_find+1;
my ($value, $flag, $height, $epoch);
while ( <IN> ) {
  chop;
  @tmp = split(/\s+/, $_);
  $height = $tmp[$index{'height'}];
  $value = $tmp[$index_to_find];
  $flag = $tmp[$index_to_find+1];
  #$epoch = convert_to_epoch($tmp[0], $tmp[1]);
  if ( $height == $height_to_find ) {
    printf ("%10s %5s %10s %5s %10s %7.2f %7.2f %s\n", $tmp[0], $tmp[1], $tmp[2], $tmp[3], $tmp[6], $height, $value, $flag);
  }
} # end while
close(IN);
#-------------------------------------
sub convert_to_epoch {

  my $date = shift;
  my $time = shift;
  print "converting $date $time\n";exit();

}
