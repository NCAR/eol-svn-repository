#!/usr/bin/perl

use Getopt::Std;
use File::Basename;

#******************************
# script to list all the variables (along with the
# description and units) from a netcdf file
# input parameters:
# -i = name of dat directory (where the .dat files reside)
# -o = name of output file
# SJS 06/2008
#******************************

if ( $#ARGV < 0 ) {
   # no arguments specified so show the options 
   print "list_all_vars.pl\n";
   print "\t-i = name of dat directory (where the .dat files reside)\n";
   print "\t-o: output filename\n";
   exit();
}

# fetch the options
getopt(io);

# make sure that the user specified the input file
if ( !$opt_i) {
   print "Error: input option (-i) not set..\n";
   exit();
} # endif

if ( !$opt_o ) {
   print "Error: output option (-i) not set..\n";
   exit();
}
my $dat_dir = $opt_i;
my $out_fname = $opt_o;

my $file;
opendir(DIR, $dat_dir) || die "cannot open $dat_dir";
my @list_of_files = grep(/.dat$/, readdir(DIR));
foreach $file(@list_of_files) {
  read_dat("$dat_dir/$file");

}
closedir(DIR);

sub read_dat {

  my $full_path = shift;

}
