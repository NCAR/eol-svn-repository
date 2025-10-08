#!/usr/bin/perl

use Getopt::Std;

# print the specified column and flag to check against
# the DQR values

if ( $#ARGV < 0 ) {
  print "USAGE: print_column.pl\n";
  print "\t-i: input file name (.sfc)\n";
  print "\t-n: column name\n";
  exit();
} else {
   getopt(in);
} # endif

my @column_name;
push(@column_name,'lat');
push(@column_name,'lon');
push(@column_name,'alt');
push(@column_name,'stn_press');
push(@column_name,'temp_air');
push(@column_name,'dew_pt');
push(@column_name,'rel_hum');
push(@column_name,'spec_hum');
push(@column_name,'wind_spd');
push(@column_name,'wind_dir');
push(@column_name,'u_wind');
push(@column_name,'v_wind');
push(@column_name,'precip');
push(@column_name,'snow');
push(@column_name,'short_in');
push(@column_name,'short_out');
push(@column_name,'long_in');
push(@column_name,'long_out');
push(@column_name,'net_rad');
push(@column_name,'skin_temp');
push(@column_name,'par_in');
push(@column_name,'par_out');

my %index;
$index{'lat'} = 7;
$index{'lon'} = 8;
$index{'alt'} = 9;
$index{'stn_press'} = 10;
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
$index{'skin_temp'} = 42;
$index{'par_in'} = 44;
$index{'par_out'} = 46;

my $sfc_file = $opt_i;
my $name_to_find = $opt_n;

if ( !-e $sfc_file ) {
  print "ERROR: $sfc_file does not exist!!\n";
  exit();
}

# make sure that the name_to_find exists!
my $column;
my $name = grep(/$name_to_find/, @column_name);
if ( !$name ) {
  print "Sorry, $can't find $name_to_find\n";
  print "Choices are:\n";
  foreach $column (@column_name) {
    print "\t$column\n";
  } # end foreach
  exit();
} # endif

open(IN, $sfc_file) || die "cannot open $sfc_file";

my @tmp;
my $index_to_find = $index{$name_to_find};
my $flag_index_to_find = $index_to_find+1;
my ($value, $flag);
while ( <IN> ) {
  chop;
  @tmp = split(/\s+/, $_);
  $value = $tmp[$index_to_find];
  $flag = $tmp[$index_to_find+1];
  printf ("%10s %5s %10s %5s %10s %7.2f %s\n", $tmp[0], $tmp[1], $tmp[2], $tmp[3], $tmp[6], $value, $flag);
} # end while
close(IN);
