#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
use MySqlFile;

my $dataset_id = "6.17";
my $directory = "/RAF/1992/269/HRT";
print "Starting\n";
my @files = grep(/^\-.*(\wF\d\d)$/,split('\n',`msls -l $directory`));

for my $file_data (@files) {
	
	my $file = (split(' ', $file_data))[8];
	print "inserting $file into $dataset_id\n";
	if ($file =~ /(\wF\d\d)$/) {
	    	
		my @ncFiles = grep(/$1.(\d{8}).(\d{6}).(\d{6})\.PNI\.nc$/,split('\n',`msls -l $directory/NetCDF`));

		if (($#ncFiles == 0) && ($ncFiles[0] =~ /(\d\d\d\d)(\d\d)(\d\d).(\d\d)(\d\d)(\d\d).(\d\d)(\d\d)(\d\d)\.PNI\.nc/)) {
			my $database = MySqlDatabase->new("zediupdate", "change-456");
			#$database->setHost("localhost");

			my $mysqlFile = MySqlFile->new();
			$mysqlFile->setHost("mass_store");
			$mysqlFile->setDatasetId($dataset_id);
			$mysqlFile->setFile($directory, $file);
			$mysqlFile->setFormatId(20);
			$mysqlFile->setBeginDate($1, $2, $3, $4, $5, $6);
			$mysqlFile->setEndDate($1, $2, $3, $7, $8, $9);

			$database->connect();
			my $msg = $mysqlFile->insert($database);
			if ($msg eq "") { $msg .= $database->commit(); }
			else {$msg .= "Database rolled back.\n" . $database->rollback(); }

			$database->disconnect();
			print "$msg\n";
		}
	}
}
