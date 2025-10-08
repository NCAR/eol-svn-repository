#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
use MySqlFile;

my $dataset_id = "130.002";
my $dirGP = "/RAF/1991/739/LRT";
my $dirNC = $dirGP . "/NetCDF";

print "Starting\n";
my @files = grep(/^\-.*(\wF\d\d\w?)$/,split('\n',`msls -l $dirGP`));

for my $file_data (@files) {
	
	my $file = (split(' ', $file_data))[8];
	print "inserting $file into $dataset_id\n";
	if ($file =~ /(\wF\d\d\w?)$/) {
	    	
		my @ncFiles = grep(/$1.(\d{8}).(\d{6}).(\d{6}).*\.nc$/,split('\n',`msls -l $dirNC`));

		if (($#ncFiles == 0) && ($ncFiles[0] =~ /(\d\d\d\d)(\d\d)(\d\d).(\d\d)(\d\d)(\d\d).(\d\d)(\d\d)(\d\d)/)) {
			my $database = MySqlDatabase->new("ingest", "gob-ble");
			#$database->setHost("localhost");

			my $mysqlFile = MySqlFile->new();
			$mysqlFile->setHost("mass_store");
			$mysqlFile->setDatasetId($dataset_id);
			$mysqlFile->setFile($dirGP, $file);
			$mysqlFile->setFormatId(20);
			$mysqlFile->setBeginDate($1, $2, $3, $4, $5, $6);
			$mysqlFile->setEndDate($1, $2, $3, $7, $8, $9);
			$mysqlFile->setEvent($file);
			
			$database->connect();
			my $msg = $mysqlFile->insert($database);
			if ($msg eq "") { $msg .= $database->commit(); }
			else {$msg .= "Database rolled back.\n" . $database->rollback(); }

			$database->disconnect();
			print "$msg\n";
		}
	}
}
