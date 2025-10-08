#!/usr/bin/perl -w

##Module-----------------------------------------------------------------------
### <p>The create-das.pl script creates the .das files needed for the ASCII 
##  QCF (.qcf and .0qc) and PQCF (.pqcf) data files to be displayed in Opendap.
##  Basically, a sample.das file is created for the dataset.  Then this
##  program is run to create all of the .das files for the dataset.  The
##  program reads all the particular data files in the directory and determines
##  the name for the .das file.  Then the program changes the start and end 
##  dates, in the global attributes section, to be the correct dates for that
##  particular data file.  This program assumes that the current directory is
##  the working directory for the .das files.
##
##  Usage: create-das.pl file-type sample-das-file
##
### @author Janet Scannell May 2021
##
##  Modification: Allow user to select type of naming for the QCF data files.
##       jns - June 2021
## 
##       Allow user to select 0qc, dqc, or dqcf file type for the QCF data files.
##       jns - July 2021
##
##       Add formats for daily PQCF datasets.  Add another format type for a
##       different naming convention.
##       jns - September 2021
##
##       If filetype is 0qc.gz (for example), name the das file .0qc.das for
##       opendap to correctly read the .das file.
##       jns - January 2022
##      
##       Add a format for the monthly PQCF dataset.  Add code to allow for one
##       year from the start date to the end date.
##       jns - January 2022
###
####Module-----------------------------------------------------------------------
##

use DateTime;
use File::Basename;

# Check for correct number of parameters
die "Usage: create-das.pl file-type sample-das-file\n" if ($#ARGV !=1);

my $filetype = $ARGV[0];
my $sampledas = $ARGV[1];
my $num_selections;
my $user_opt = 0;
my $first = 1;
my $in_year = 0;
my ($year, $month, $day, $format_date, $startdate, $enddate, $base) = "";
my @selection = ("YYYYMMDD.*", "YYMMDD.*", "*_YYMMDD.*", "*_YYYYMMDD.*", "*_MDD.*", "*_YYMMDD_*.*", "YYYYMM.*", "YYMM.*", "*_YYMM.*", "*_YYYYMM.*", "*_YYYYMM_*.*", "*_YYMM_*.*", "*_YYYY.*");
my @expression = ("(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)", "(\\d\\d)(\\d\\d)(\\d\\d)", ".*_(\\d\\d)(\\d\\d)(\\d\\d)", ".*_(\\d\\d\\d\\d)(\\d\\d)(\\d\\d)", ".*_()(\\d)(\\d\\d)",
	         ".*_(\\d\\d)(\\d\\d)(\\d\\d)_.*",
                 "(\\d\\d\\d\\d)(\\d\\d)", "(\\d\\d)(\\d\\d)", ".*_(\\d\\d)(\\d\\d)", ".*_(\\d\\d\\d\\d)(\\d\\d)", ".*_(\\d\\d\\d\\d)(\\d\\d)_.*", ".*_(\\d\\d)(\\d\\d)_.*", ".*(\\d\\d\\d\\d)");

# Print different file naming conventions.
# Ask user to select the correct one.
$num_selections = $#selection + 1;
while ($user_opt < 1 or $user_opt > $num_selections) {
   if ($first == 0) {
      print "Incorrect selection.  Please try again.\n\n\n";
   } else {
      $first = 0;
   }
   print "Carefully select the correct option for the naming of the data files.\n";
   my $num = 1;
   foreach my $option (@selection) {
      print "$num - $option\n";
      $num++;
   }
   $user_opt = <STDIN>;
   chomp($user_opt);
}
$first = 1;
# If option selected is #5, ask user for the 4 digit year of the data.
if ($user_opt == 5) {
   while ($in_year < 1900 or $in_year > 2999) {
      if ($first == 0) {
         print "Incorrect year entered.  Please try again.\n\n\n";
      } else {
         $first = 0;
      }
      print "Enter the four digit year associated with this data.\n";
      $in_year = <STDIN>;
      chomp($in_year);
   }
}

# Open the current directory and look for files of the type specified on
# the command line.
opendir(PDIR, ".");
my @datafiles = grep(/^.*.$filetype$/, readdir(PDIR));
closedir(PDIR);

# Open sample das file.
if (! open(INFILE, "<$sampledas")) {
   die "ERROR: Could not open $sampledas file for reading.  Terminating...\n";
}
my @lines = <INFILE>;
close(INFILE);

# Loop through all the data files and create a das file for each data file.
foreach my $file (@datafiles) {
# Set name for das file.
   $base = basename($file, ".$filetype");
   if ($filetype =~ /(.*)\.gz$/) {
      $outfile = $base . '.' . $1 . '.das';
   } else {
      $outfile = $base . '.das';
   }
# Use the regular expression for the specific file name to pull out
# year, month, and day.  Options 6-9 are only for daily PQCF files
# which contain one month of data. Option 10 is only for monthly
# PQCF files which contain one year of data.
   $base =~ /^$expression[$user_opt-1]$/;
   $year = $1;
# Set the year for option 5 as entered by the user.
   if ($user_opt == 5) {
      $year = $in_year;
   }
   if ($year < 100) {
      $year = $year + 1900;
   }
   if ($user_opt < 13) {
      $month = sprintf("%02d", $2);
   }
   if ($user_opt < 7) {
      $day = $3;
      $formatdate = "$year-$month-$day";
      $startdate = "\"$formatdate 00:00:00\"\;";
      $enddate = "\"$formatdate 23:59:59\"\;";
   } elsif ($user_opt < 13) {
# daily PQCF files, no day in file name.
# Determine last day of the month for the data file.
      my $dt = DateTime->last_day_of_month(
         year      => $year,
         month     => $month,
         time_zone => 'UTC',
      );
      $day = sprintf("%02d", $dt->day);
      $startdate = "\"$year-$month-01 00:00:00\"\;";
      $enddate = "\"$year-$month-$day 23:59:59\"\;";
   } else {
# monthly PQCF files, no day or month in file name.
      $startdate = "\"$year-01-01 00:00:00\"\;";
      $enddate = "\"$year-12-31 23:59:59\"\;";
   }
# Edit the lines with start and end date_time to be correct for the data file.
   foreach my $line (@lines) {
      $line =~ s/Start_Date_Time.*/Start_Date_Time $startdate/g;
      $line =~ s/End_Date_Time.*/End_Date_Time $enddate/g;
   }
# Create das file.
   if (! open(OUTFILE, ">$outfile")) {
      die "ERROR: Could not open $outfile file for writing.  Terminating...\n";
   }
   foreach $line (@lines) {
      print OUTFILE "$line";
   }
   close(OUTFILE);
}

