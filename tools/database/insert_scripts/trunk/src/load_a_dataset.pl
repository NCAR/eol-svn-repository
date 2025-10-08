#!/usr/bin/perl -w
##------------------------------------------------------------------------------
# Wrapper script to create a dataset in codiac, load the files, and create an 
# entry in the DTS for this dataset, and let the user know what they need to do
# to complete the dataset loading.
# 
# Janine Aquino 2/12/2015
# 
# Updated 2021 for changes to archive permissions and preferred workflow. In 
# specific:
# - Separated insert_proj_dataset into insert_project and insert_dataset and 
#   deprecated insert_project at JJA request. New project must now be created
#   via the web interface.
# - Removed insert_multiple_files from this script. With move to campaign 
#   storage, must be eoldata user to run insert_multiple_files. So for now
#   just inform user of this script to become eoldata and run
#   insert_multiple_files after this script successfully completes.
##------------------------------------------------------------------------------
use strict;
use File::Basename;

my $deployed_dir = dirname($0);

# Usage
sub usage {
    my @ARGV = shift;
    print "Usage: $0 <dataset>.yml [-c]\n";
    print "\tRuns\t\"insert_dataset add_dataset=<dataset>.yml\"\n";
    print "\t\t\"insert_dts add_dts=<dataset>.yml\"\n";
    print "\tIf -c option is used, will not insert to DTS, just codiac\n";
    exit(1);
}

if ($#ARGV == -1) #no options, only program name
{
    usage(@ARGV);
}

my $DTS = "";

if ($#ARGV == 1) #YAML file and -c
{
    $DTS = $ARGV[1];
}

my @args;
# Insert a new dataset to an existing project in codiac
@args = ("$deployed_dir/insert_dataset", "add_dataset=$ARGV[0]") ;
system(@args) == 0
    or die "system @args failed: $?";

if ($DTS ne "-c") 
{
# Insert a new dataset to an existing project in the DTS
@args = ("$deployed_dir/insert_dts","add_dts=$ARGV[0]");
system(@args) == 0
    or die "system @args failed: $?";
}

# Insert files into the new codiac dataset.

print("insert_multiple_files now works with campaign storage, not HPSS\n");
print("In order to run insert_multiple_files, you must become user eoldata.\n");
print("So when this script exits, please manually run:\n");
print("\t>sudo su --login eoldata\n");
print("\t>/net/work/bin/scripts/insert/insert_multiple_files\n");
print("\t\t -u <your username> $ARGV[0]\n");
print("\t>exit\n");
print("\t>whoami  <- to confirm you are no longer eoldata\n\n");
#@args = ("$deployed_dir/insert_multiple_files","$ARGV[0]");
#system(@args) == 0
#    or die "system @args failed: $?";

print "Other things you must still do manually (which could be automated):\n";
print "\tRun /net/work/bin/emdac/lsdsfiles -lv <archive_ident>\n";
print "\tCheck and test order dataset, make visible in zinc,then add to ML\n";
print "\tAdd codiac user for restricted datasets, EULA files, & assign DOIs\n";
print "\tAdd xlinks and/or companions\n";
print "\tIf loading browseable files, add plot type and browse_extract\n";
print "\t\tprogram (sti).\n";
