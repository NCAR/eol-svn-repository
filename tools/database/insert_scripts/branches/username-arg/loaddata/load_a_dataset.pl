#!/usr/bin/perl -w
##------------------------------------------------------------------------------
# Wrapper script to create a dataset in codiac, load the files, and create an 
# entry in the DTS for this dataset, and let the user know what they need to do
# to complete the dataset loading.
# 
# None of these scripts need proj.yml, but insert_proj_dataset add_project does.
#
# Janine Aquino 2/12/2015
##------------------------------------------------------------------------------
use strict;

my $deployed_dir = "/net/work/bin/scripts/insert";

# Usage
sub usage {
    my @ARGV = shift;
    print "Usage: $0 <dataset>.yml [-c]\n";
    print "\tRuns\t\"insert_proj_dataset add_dataset=<dataset>.yml\"\n";
    print "\t\t\"insert_dts add_dts=<dataset>.yml\"\n";
    print "\t\t\"insert_multiple_files <dataset>.yml\"\n";
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
@args = ("$deployed_dir/insert_proj_dataset", "add_dataset=$ARGV[0]") ;
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
@args = ("$deployed_dir/insert_multiple_files","$ARGV[0]");
system(@args) == 0
    or die "system @args failed: $?";

print "Run /net/work/bin/emdac/lsdsfiles -lv <archive_ident>\n";
print "Still check and test order by hand, make visible in zinc,".
      "then add to ML\!\!\!\n";
print "Adding codiac user for restricted datasets, EULA files, and ".
      "assigning DOIs must be done by hand\n";
print "Doesn't add xlinks or companions, so add those manually too.\n";
print "If loading browseable files, hand add plot type and browse_extract".
      " program (sti).\n";
