#!/usr/bin/perl

use Getopt::Std;

#******************************
# script to list all the variables (along with the
# description and units) from a netcdf file
# input parameters:
# -i = name of netcdf file
# -o = name of output file
# SJS 06/2008
#******************************

if ( $#ARGV < 0 ) {
   # no arguments specified so show the options 
   print "list_all_vars.pl\n";
   print "\t-i: input netcdf filename\n";
   print "\t-o: output filename\n";
   exit();
}

# fetch the options
getopt(io);

# make sure that the user specified the input file
if ( !$opt_i) {
   print "Error: input file option (-i) not set..\n";
   exit();
} # endif

# construct the default output filename
if ( !$opt_o) {
  # change the extension to out
  @tmp = split(/\./, $opt_i);
  $tmp[$#tmp] = "out";
  $opt_o = join(".", @tmp);
}

my $in_fname = $opt_i;
my $out_fname = $opt_o;

my %var, %units, %description;
if ( -e $in_fname ) {

  # put the contents of the header into
  # a temporary file
  $tmp_fname = "header.tmp";
  $cmd = "ncdump -h $opt_i > $tmp_fname";
  system($cmd);

  # now, open the temporary file with the header information
  # and parse out the variable name, description and units
  open(HEADER, "$tmp_fname") || die "cannot open $tmp_fname";
  while ( <HEADER> ) {
    chop;
    # get rid of any leading spacing or tabs
    s/^\s+//;
    s/^\t+//;
    if ( /^\w+\:\w+\s\=\s/ ) {
       my ($key, $value) = split(/\s*\=\s*/);
       my ($name, $id) = split(/\:/, $key);
       if ( $id =~ /units/ ) { $units{$name} = $value; }
       if ( $id =~ /long_name/ ) { $description{$name} = $value; }
    } # endif
  } # end while
  close(HEADER);

  # finally, print them out to the output file
  open(OUT, ">>$out_fname") || die "cannot open $out_fname";
  @tmp = split(/\//, $in_fname);
  print OUT "***************\n";
  print OUT "$tmp[$#tmp]\n";
  print OUT "***************\n";
  foreach $key(sort keys(%description)) {
    print OUT "name: $key\n";
    print OUT "units: $units{$key}\n";
    print OUT "description: $description{$key}\n";
    print OUT "***************\n";
  }  # end foreach
  close(OUT);

  # delete the temporary header file
  unlink($tmp_fname);

} else {

   print "$in_fname doesn't exist!!\n";
   exit();

}
