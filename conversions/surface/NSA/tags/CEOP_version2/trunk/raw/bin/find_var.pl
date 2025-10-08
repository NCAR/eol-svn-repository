#!/usr/bin/perl -I../../../lib

use CDF::ncmanipulate;
use Getopt::Std;

use strict;

#**************************************
# script to make sure that a given variable
# exists for each netcdf file in a given
# directory...this is to make sure that
# all the variables exist in all the netcdf
# files before processing the CEOP data
# options:
#  	-d: base directory (where the cdf files reside)
#	-n: name of parameter
#	-o: name of output file
# SJS 01/2009
#**************************************

if ($#ARGV < 0 ) {
  print "Usage: find_var.pl\n";
  print "\t-d: name of directory where netcdf files reside\n";
  print "\t-n: name of the parameter to find\n";
  print "\t-o: name of the output file\n";
  exit();
}

my %option;
getopt("dno:", \%option);

die "$option{'d'} does not exist" if ( !-e $option{'d'} );
die "$option{'n'} must be specified" if ( !$option{'n'} );
my $base_dir = $option{'d'};
my $var_to_find = $option{'n'};
my $out_fname;
if ( !$option{'o'} ) {
  $out_fname = "$var_to_find.".time().".out";
} else {
  $out_fname = $option{'o'};
} # endif

# open the output file
open(OUTPUT, ">>$out_fname") || die "cannot open $out_fname";
# open the input directory
opendir(DIR, $base_dir) || die "cannot open $base_dir";
# now, get a list of the cdf files
my @list_of_files = grep(/.cdf$/, readdir(DIR));
closedir(DIR);

my ($file, $nc, $full_path);
# look for the variable in each file
foreach $file (@list_of_files) {
  # assemble the full path name for
  # the netcdf file
  $full_path = "$base_dir/$file";

  # first, read in the netcdf file
  $nc = nc_read($full_path);
  
  #print "looking for $var_to_find in $full_path\n";

  # look for the variable in the the netcdf file
  if ( !nc_find_var($nc,$var_to_find) ) { 
    # can't find the variable so print out a warning
    print OUTPUT "can't find $var_to_find in $file\n";
  }# endif

  nc_close($nc);

} # end foreach
close(OUT);
