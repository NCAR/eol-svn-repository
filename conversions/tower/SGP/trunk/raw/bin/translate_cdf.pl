#!/usr/local/bin/perl

# Script to translate the raw cdf files to the
# format that the conversion software uses.
# This script needs to be run on a solaris machine
# SJS 08/2009

#*********************************
# the user supplied constants

# the name of the translator executable (nesob_dump)
my $EXE_FNAME = "/net/work/CEOP/version2/data_processing/other/SGP/raw/nesob_dump";
die "$EXE_FNAME doesn't exist" if ( !-e $EXE_FNAME);

# where the netcdf files reside that have been copied from the original 
# directory
my $CDF_DIR = "/net/work/CEOP/version2/data_processing/other/SGP/raw/cdf";
die "$CDF_DIR doesn't exist" if ( !-e $CDF_DIR);

# the base directory (raw)
my $RAW_DIR = "/net/work/CEOP/version2/data_processing/other/SGP/raw";
die "$RAW_DIR doesn't exist" if ( !-e $RAW_DIR);

# the list of variables names to pull out of the netcdf file
my @LIST_OF_VARS;
push(@LIST_OF_VARS, "base_time");
push(@LIST_OF_VARS, "time_offset");
push(@LIST_OF_VARS, "temp_60m");
push(@LIST_OF_VARS, "qc_temp_60m");
push(@LIST_OF_VARS, "temp_25m");
push(@LIST_OF_VARS, "qc_temp_25m");
push(@LIST_OF_VARS, "rh_60m");
push(@LIST_OF_VARS, "qc_rh_60m");
push(@LIST_OF_VARS, "rh_25m");
push(@LIST_OF_VARS, "qc_rh_25m");
push(@LIST_OF_VARS, "lat");
push(@LIST_OF_VARS, "lon");
push(@LIST_OF_VARS, "alt");
my $var_str = join(",", @LIST_OF_VARS);

#*********************************
my ($file, $cmd);

opendir(DIR, $CDF_DIR) || die "can't open $CDF_DIR";
my @list_of_files = grep {/cdf/} readdir(DIR);
closedir(DIR);
foreach $file (@list_of_files) {
  $file =~ /(\w{3})(\w+)([CE]\d{1,3}).b1.(\d{4})(\d{2})(\d{2})\.(\d{2})(\d{2})/;
  my $type = $2;
  my $year = $4;
  next if ( $year ne '2009'); 
  my $translate_dir = "$RAW_DIR/$type";
  # create the directory where the .dat files will reside
  # separate by years so that we can run 1 year at a time
  # for testing purposes
  if (!-e $translate_dir) {
    mkdir($translate_dir) || die "Can't create $translate_dir";
  }
  $translate_dir = "$RAW_DIR/$type/$year";
  if (!-e $translate_dir) {
    mkdir($translate_dir) || die "Can't create $translate_dir";
  }
  # now, translate the netcdf file
  my $dat_fname = $file;
  $dat_fname =~ s/cdf/dat/g;
  # finally, run the translator
  $cmd = "$EXE_FNAME -v $var_str $CDF_DIR/$file > $translate_dir/$dat_fname";
  print "processing $file\n";
  system($cmd);
}
