#!/usr/bin/perl -w

##Module-----------------------------------------------------------------------
### <p>This convert-qcf-to-netcdf.pl script runs the jar file to convert all the
##  QCF data files for a project to netcdf data files.  Run this script from the
#$  directory where the QCF data files are located for the dataset that will be
##  converted to netcdf format.  The netcdf data files will be written in the same 
##  directory as the original QCF files.
##
##  Usage: convert-qcf-to-netcdf.pl dataset-id database-user
##
### @author Janet Scannell May 2024
##
####Module-----------------------------------------------------------------------
##

use strict;
use lib "/net/work/lib/perl/mysql";
use DateTime;
use File::Basename;
use MySqlDatabase;
use MySqlDataset;
use MySqlFile;

my $datasetid = $ARGV[0];
my $user = $ARGV[1];
my $pass = "";
my $first = 1;

# Check for correct number of parameters
die "Usage: convert-qcf-to-netcdf.pl dataset-id database-user\n" if ($#ARGV !=1);

# Ask for database password, so the password doesn't show up in .history file
while ($pass eq "") {
   print "Please enter the password for database user $user\n";
   $pass = <STDIN>;
}
chop($pass);

# Open the current directory and look for .qcf files 
opendir(PDIR, ".");
my @datafiles = grep(/^.*.qcf$/, readdir(PDIR));
closedir(PDIR);

# Loop through all the data files and create a netcdf file for each data file.
foreach my $file (@datafiles) {
   if ($datasetid eq "19.014" or $datasetid eq "40.010") {
      system("java -cp /net/work/software/qcf2cdf/QCFiosp/dist/QCFiosp.jar qcf2cdf/QCFiospWind -i $file -o . -u $user -p $pass -d $datasetid");
   } elsif ($datasetid eq "1.38") {
      system("java -cp /net/work/software/qcf2cdf/QCFiosp/dist/QCFiosp.jar qcf2cdf/QCFiospShortStnNoNominal -i $file -o . -u $user -p $pass -d $datasetid");
   } elsif ($datasetid eq "1.33" or $datasetid eq "1.65") {
      system("java -cp /net/work/software/qcf2cdf/QCFiosp/dist/QCFiosp.jar qcf2cdf/QCFiospShortStn -i $file -o . -u $user -p $pass -d $datasetid");
   } else {
      system("java -cp /net/work/software/qcf2cdf/QCFiosp/dist/QCFiosp.jar qcf2cdf/QCFiosp -i $file -o . -u $user -p $pass -d $datasetid");
   }
}
