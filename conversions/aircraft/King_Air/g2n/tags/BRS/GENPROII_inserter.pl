#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
use MySqlFile;

my $dataset_id = "182.002";
my $dirNC = "/EOL/1985/brs/aircraft/kingair_n312d/LRT/NetCDF";

print "Starting\n";
my @files;
my @fltnos;
open FH, "files.txt";
while (<FH>) {
    	chomp $_;
	my @split = split(',', $_);
	if ($#split == 1) {
		push @files, $split[0];
		push @fltnos, $split[1];
	}
	else
	{
		if ($split[0] =~ /([A-Za-z](F|f)\d\d\w?)/) {
		    push @files, $split[0];
		    push @fltnos, $1;
		}
		else
		{
		    print "Unrecognized line $_";
		    exit(1);
		}
	}
}
close FH;

my $database = MySqlDatabase->new("zediupdate", "change-456");
#$database->setHost("localhost");
$database->connect();

my $msg;
for (my $i = 0; $i <= $#files; $i++) {
	$files[$i] =~ /(.*)\/([^\/]*)$/ or die "$files[$i]\n";
    	my $dirGP = $1;
	my $file = $2;
	my $fltno = $fltnos[$i];

	print "inserting $file into $dataset_id\n";	
	my @ncFiles = grep(/$fltno\..*\.nc$/,split('\n',`msls -l $dirNC`));

	if (($#ncFiles == 0) && ($ncFiles[0] =~ /(\d\d\d\d)(\d\d)(\d\d).(\d\d)(\d\d)(\d\d).(\d\d)(\d\d)(\d\d)/)) {

		my $mysqlFile = MySqlFile->new();
		$mysqlFile->setHost("mass_store");
		$mysqlFile->setDatasetId($dataset_id);
		$mysqlFile->setFile($dirGP, $file);
		$mysqlFile->setFormatId(20);
		$mysqlFile->setBeginDate($1, $2, $3, $4, $5, $6);
		$mysqlFile->setEndDate($1, $2, $3, $7, $8, $9);
		$mysqlFile->setEvent($fltno);
		
		$msg = $mysqlFile->insert($database);
		unless ($msg eq "") { last; }
	}
	elsif ($#ncFiles > 0)
	{
		$msg =  "Error: To many matching NetCDF files found.\n";
		last;
	}
	elsif ($#ncFiles < 0)
	{
		$msg = "Error: No matching NetCDF file found.\n";
		last;
	    }
	else
	{
		$msg = "Error: NetCDF file not renamed.\n";
		last;
	}
}
if ($msg eq "") {$msg = "Successfully inserted GENPROII files"; $msg .= $database->commit(); }
else {$msg .= "Database rolled back.\n" . $database->rollback(); }
$database->disconnect();
print "$msg\n";
