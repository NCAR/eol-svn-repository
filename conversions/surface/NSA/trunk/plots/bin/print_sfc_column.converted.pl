#!/usr/bin/perl -I /net/work/CEOP/version2/data_processing/other/NSA/susans_playground

use Getopt::Std;
#require "conversion.constants.twr.pl";

# print the specified column and flag to check against
# the DQR values

if ( $#ARGV < 0 ) {
  print "USAGE: print_final_sfc_column.pl\n";
  print "\t-i: sfc input file name (.sfc)\n";
  print "\t-n: column name\n";
  exit();
} else {
   getopt(in);
} # endif

#my $category_ref = &list_of_categories();
#my @column_name = @$category_ref;
my $column;
#my $params = &params();

# the column number in the data file
# note: the keys must match the categories
# as described in conversion.constants.pl
my %index;
$index{'lat'} = 7;
$index{'lon'} = 8;
$index{'alt'} = 9;
$index{'stn_pres'} = 10;
$index{'temp_air'} = 12;
$index{'dew_pt'} = 14;
$index{'rel_hum'} = 16;
$index{'spec_hum'} = 18;
$index{'wind_spd'} = 20;
$index{'wind_dir'} = 22;
$index{'u_wind'} = 24;
$index{'v_wind'} = 26;
$index{'precip'} = 28;
$index{'snow'} = 30;
$index{'short_in'} = 32;
$index{'short_out'} = 34;
$index{'long_in'} = 36;
$index{'long_out'} = 38;
$index{'net_rad'} = 40;
$index{'skintemp'} = 42;
$index{'par_in'} = 44;
$index{'par_out'} = 46;

my $sfc_file = $opt_i;
#my $dat_dir = $opt_d;
my $name_to_find = $opt_n;

if ( !-e $sfc_file ) {
  print "ERROR: input file: $sfc_file does not exist!!\n";
  exit();
}

#if ( !-e $dat_dir ) {
#  print "ERROR: data directory: $dat_dir does not exist!!\n";
#  exit();
#}

# make sure that the name_to_find exists!
#my $column;
#my $found = grep(/\b$name_to_find\b/, @column_name);
#if ( !$found ) {
#  print "Sorry, $can't find $name_to_find\n";
#  print "Choices are:\n";
#  foreach $column (@column_name) {
#    print "\t$column\n";
#  } # end foreach
#  exit();
#} # endif

open(IN, $sfc_file) || die "cannot open $sfc_file";

my @tmp;
my $index_to_find = $index{$name_to_find};
my $flag_index_to_find = $index_to_find+1;
my ($value, $flag, $epoch);
while ( <IN> ) {
  chop;
  @tmp = split(/\s+/, $_);
  $value = $tmp[$index_to_find];
  $flag = $tmp[$index_to_find+1];
  printf ("%10s %5s %10s %5s %10s %7.2f %s\n", $tmp[0], $tmp[1], $tmp[2], $tmp[3], $tmp[6], $value, $flag);
} # end while
close(IN);
#-------------------------------------
sub convert_to_epoch {

  my $date = shift;
  my $time = shift;
  print "converting $date $time\n";exit();

}
