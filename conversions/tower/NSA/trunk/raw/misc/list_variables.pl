#!/usr/bin/perl

use Getopt::Std;

#***************************************
# Script to read in the list of variables
# from an input file (-i option) and creates 
# a list of variable description and units
# Input parameters:
#    -i: input file 
#    -o: name of the output file that contains
#        the variable descriptions and units         
#    -t: title (ie: gndrad, skyrad, mettwr...etc)
# SJS 05/21/2008 #***************************************

my $in_fname, $out_fname;

# fetch the arguments
if ( $#ARGV < 0 ) {
  # show the argument list to the user
  print "USAGE: list_variables.pl\n";
  print "\t-i: input file\n";
  print "\t-o: output file\n";
  print "\t-t: title (network name)\n";
  exit();
} else {

  # get the command line arguments
  getopt(iot);

  if ( !$opt_i ) {
    print "Error: input file option (-i) not set..\n";
    exit();
  } # endif

  if ( !$opt_o ) {
    $opt_o = "all_variables.txt";
  } # endif

  if ( !$opt_t ) {
    $opt_t = "unknown";
  }

  # the name of the input file which contains a list
  # of variables to process
  $in_fname = $opt_i;
  $out_fname = $opt_o;
  $title = $opt_t;

} # endif

open(IN, "$in_fname") || die "cannot open $in_fname";
my $cdf_fname, @var, %long_name, %unit;
while ( <IN> ) {
  chop;
  if ( !/^#/ ) {  # ignore the comments
    # fetch the name value pairs from the input file
    ($name, $value) = split(/=/, $_);
    # get rid of the spaces
    $value =~ s/\s+//g;
    $name =~ s/\s+//g if $name =~ /var/;
    if ( $name =~ /netcdf filename/ ) {
      # the name of the netcdf file
      $cdf_fname = $value;
      # make sure the net cdf file exists
      if ( !-e $cdf_fname ) {
        print "ERROR: $cdf_fname does not exist..\n";
        exit();
      } # endif
    } elsif ($name =~ /\bvar\b/) {
      # add the variable name to the list
      push(@var, $value);
    } # endif
  }
} # end while
close(IN);

# now, parse out the description and units from the netcdf file
# (not the most elegant way....)

# dump out the header to a file
my $header_fname = "header.tmp";
my $cmd = "ncdump -h $cdf_fname > $header_fname";
$status = system($cmd);
# does the netcdf file exist??
if ( -e $cdf_fname ) {
  my $cmd = "ncdump -h $cdf_fname > $header_fname";
  `$cmd`;
} else {
  die "Error: netcdf file does not exist..\n";
} # endif

open(HEADER, "$header_fname") || die "cannot open $header_fname";
while ( <HEADER> ) {
  chop;
  s/^\s+//g;
  s/^\t+//g;
  foreach $name (@var) {
    if (/\b$name:long_name/) {
      ($id, $description) = split(/=/, $_);
      $description =~ s/\s+\"//g;
      $description =~ s/"\s+//g;
      $description =~ s/;$//g;
      $long_name{$name} = $description;
    } # endif
    if (/\b$name:unit/) {
      ($id, $units) = split(/=/, $_);
      $units =~ s/\s+\"//g;
      $units =~ s/"\s+//g;
      $units =~ s/;$//g;
      $unit{$name} = $units;
    } # endif
  } # end foreach

}
close(HEADER);

open(OUT, ">$out_fname") || die "cannot open $out_fname";
print OUT "-------------------------------------------------------------\n\n";
print OUT "$title\n\n";
foreach $name (@var) {
  $long_name{$name} = "\"$long_name{$name}\"";
  #$str = sprintf("%-30s%-70s%-10s", $name, $long_name{$name}, $unit{$name});
  $str = sprintf("%-70s%-30s%-10s", $long_name{$name}, $name, $title);
  print OUT "$str\n";
}
close(OUT);
