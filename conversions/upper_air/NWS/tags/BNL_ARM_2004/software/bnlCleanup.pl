#! /usr/bin/perl -w
#Author: Ben Golden
#This program goes through the directories of the BNL_ARM_NWS/NWS/ folder and gzips the files found in each folder.
#The files are then moved to a respective directory found in BNL_ARM_NWS/processed/. These operations are performed
#one directory at a time. The script itself should be placed in the software directory inside NWS. 

#Source and Destination Directories for the move.
my $srcDir = "../";
my $dstDir = "../../processed/";

#The individual subfolders (found in both Source and Destination directories) that are gziped and moved. 
my @copyStructure = qw"raw_data output final rrs_raw_data rrs_output rrs_final logs final_merged";

foreach(@copyStructure){							#for each subfolder.
	my $toOpen = "$srcDir" . "$_" . "/";
	print "Running gzip and moving: " . "$toOpen\n";			#print a notification.
	system("gzip $toOpen*.*") or warn "No files to gzip in: " . "$toOpen";	#call gzip on that directory.
	opendir DH, "$toOpen" or die "Cannot open $srcDir$_: $!";		#open the directory.
	foreach $file (readdir DH){						#read through each file.
		rename "$toOpen$file" , "$dstDir" . "$_" . "/" . "$file";	#move to destination.
	}
}
