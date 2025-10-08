#!/usr/bin/perl


use Getopt::Std;
use strict;

#********************************
# Script to copy the raw netcdf files
# and uncompress them
# SJS 08/2009
#********************************

#********************************
# THE CONSTANTS
# where the original netcdf files reside...Scot
# will notify when these files are available for
# processing
my @CDF_DIR;
push (@CDF_DIR, "/ingest/CEOP/v2/ARM/SGP/TWR/2005");
push (@CDF_DIR, "/ingest/CEOP/v2/ARM/SGP/TWR/2006");
push (@CDF_DIR, "/ingest/CEOP/v2/ARM/SGP/TWR/2007");
push (@CDF_DIR, "/ingest/CEOP/v2/ARM/SGP/TWR/2008");
my $original_dir;
foreach $original_dir (@CDF_DIR) {
  die "$original_dir doesn't exist" if ( !-e $original_dir);
} # end foreach

# the name of the DQR file with the flagging information 
my $DQR_FNAME = "/ingest/CEOP/v2/ARM/SGP/TWR/SGP_TWR_flagging_2005_2008.txt";
die "$DQR_FNAME doesn't exist" if ( !-e $DQR_FNAME );

# base directory where the translated files (.dat) will reside
my $BASE_DAT_DIR = "/net/work/CEOP/version2/data_processing/other/SGP/raw";
die "$BASE_DAT_DIR doesn't exist" if ( !-e $BASE_DAT_DIR);

# where the netcdf files reside that have been copied from the original directory
my $BASE_CDF_DIR = "/net/work/CEOP/version2/data_processing/other/SGP/raw/cdf";
die "$BASE_CDF_DIR doesn't exist" if ( !-e $BASE_CDF_DIR);
#********************************

my ($dir, $file, $cmd);
foreach $dir (@CDF_DIR) {
  opendir(DIR, $dir) || die "cannot open $dir"; 
  my @list_of_files = grep {/cdf/} readdir(DIR);
  # examine the first file and pull out the data type
  closedir(DIR);
  # now, process the netcdf files
  foreach $file (@list_of_files) {
    $cmd = "cp $dir/$file $BASE_CDF_DIR";
    system($cmd);
    #system($cmd);
    $cmd = "gunzip $BASE_CDF_DIR/$file";
    system($cmd);
    $file =~ s/.gz//g;
  } # end foreach $dir
} # end foreach $file
