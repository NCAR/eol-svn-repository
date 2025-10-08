#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
my $msg = "";

my $database = MySqlDatabase->new("zediupdate","change-456");
#$database->setHost("merlot.eol.ucar.edu");
$database->connect();

if ($#ARGV != 2) {
	print<<EOM;
usage:
	msrelocate taplog project

	taplog: Filename of a taplog in /net/www/raf/Catalog
	project: Acronym to use in new /EOL path (Forced to lower-case)

Output: Commands are listed to the screen and logged in msmv-project.log before they are run
	Successful commands (confirmed w/ hsi ls) are logged in moves.log for use updating CODIAC
EOM
exit (1);

}

my $taplog = $ARGV[0];
my $projName = $ARGV[1];
my $datasetPrefix = $ARGV[2];

#Avoid User Error (Project Name should be lowercase)
$projName = lc $projName;

my $projPlat = "";
my $projYear = "";
my $projNum = "";
my $projDataType = "";

my %aircraft = (N312D => 'kingair_n312d',
    		N304D => 'queenair_n304d',
		N306D => 'queenair_n306d',
		N307D => 'sabreliner_n307d',
		N308D => 'electra_n308d',
	        N2UW => 'kingair_n2uw');


#load files from CODIAC
print "#Loading files from CODIAC where dataset_id LIKE \"$datasetPrefix%\"\n";

($msg, my %data) = $database->selectFull("file","*","dataset_id LIKE \"$datasetPrefix%\"");
if ($msg ne "") { print "\033[1;4;31m$msg\nDatabase Rolled back!\n\033[0m"; $database->rollback(); $database->disconnect(); exit(1); }
unless ($database->getRows() >= 1) { print "No files found in CODIAC!\n";
    $database->rollback(); $database->disconnect(); exit(1); }

#figure out how the data is layed out
my $filenameIndex = -1;
my $directoryIndex = -1;
my $file_idIndex = -1;
for (my $i = 0; $i <= $#{$data{"name"}}; $i++) {

	if (@{$data{"name"}}[$i] eq "filename") { $filenameIndex = $i; }
	if (@{$data{"name"}}[$i] eq "directory") { $directoryIndex = $i; }
	if (@{$data{"name"}}[$i] eq "file_id") { $file_idIndex = $i; }
}
die "Could not parse CODIAC filename\n" unless $filenameIndex != -1;
die "Could not parse CODIAC directory\n" unless $directoryIndex != -1;
die "Could not parse CODIAC directory\n" unless $file_idIndex != -1;

#load taplog
unless (-e "/net/www/raf/Catalog/$taplog") {print "Taplog not found!\n"; $database->rollback(); $database->desconnect(); exit(1); }
open FILE, "/net/www/raf/Catalog/$taplog";

#compare taplog to CODIAC
print "#Parsing taplog\n";
my @updates;
my $oldDir;
my $firstFile = 1;
while (<FILE>)
{
    	if ($_ =~ /Aircraft:\s*(\S{4,5})/) 	{
	    	if (defined $aircraft{$1}) { $projPlat = $aircraft{$1}; }
		else { die "Unkown Platform $1\n"; }
	}
	elsif ($_ =~ /Project no\.:\s*(\d\d\d\d)-(\d\d\d)/)
	{
		chomp ($projYear = $1);
		chomp ($projNum = $2);
	}
	elsif ($_ =~ /Data type:\s*(\S+)/){
		chomp ($projDataType = $1);
	}
	elsif ($_ =~ /MSS path name:\s*(\S+)/) {
		chomp ($oldDir = $1);
		$oldDir =~ s/\/fltno$//;
		$oldDir =~ s/\/Gx+$//;
		$oldDir =~ s/\/RMGRx+$//;
	}
	elsif ($_ =~ /\<[Aa]\S*\>\s*(\S{3,7})\s+(\S+)?\s+(\S+)?\s+\d+\/\d+\/(\d+).*\</)
	{
	    	if ($4 < $projYear && $firstFile) { $projYear = $4; $firstFile = 0; }
		else { $firstFile = 0; }

	    	die "Unable to parse old directory path\n" unless (defined $oldDir);
	    	die "Unable to parse project year\n" unless (defined $projYear);
	    	die "Unable to parse project number\n" unless (defined $projNum);

	    	my $TLdir = "";

	    	#Parse Filename
		chomp(my $fltno = $1);

		my $filename = "ERROR1";
		if (defined($2)) { $filename = $2;}
		else { $filename = $1; }

		my $temp = "ERROR2";
		if (defined($3)) {chomp($temp = $3); }

		if ($filename =~ /^TL/)
		{
		    	$TLdir = $filename;
			$filename = $temp;
		}
		if ($filename eq "-") { $filename = $fltno; }
		$filename =~ s/\*$//;

		#fix fltno
		$fltno =~ s/\*$//;

		#Fixup path name
		my $oldDirFix = $oldDir;
		$oldDirFix =~ s/fltno/$fltno/;

		#Generate Target Dir
		my $newDir = "/EOL/$projYear/$projName/aircraft/$projPlat/$projDataType/GENPROII-COS";
		#Multiple TL dirs used
		if ($TLdir ne "")
		{
			$oldDirFix =~ s/TL..../$TLdir/;
			$newDir .= "/$fltno";
		}


		push @updates, ["$oldDirFix/$filename", "$newDir/$filename"];
	}
	elsif ($_ =~ /\<[Aa]\>/)
	{
		print "Unrecognized Line:\n";
		print "\t$_\n";
	}
}
close FILE;

open FILE, "changes.csv";
while (<FILE>) {
        chomp $_;
    	my @split = split(/,/, $_);
	if ($#split != 1) { next; }
	for (my $i = 0; $i <= $#updates; $i++)
	{
	    $updates[$i][0] =~ s/$split[0]/$split[1]/;
	}
}
close FILE;

print "#Attempting CODIAC updates\n";
#print $#{$data{"row"}} . "\n";
for (my $i = 0; $i <= $#updates; $i++)
{
    my $chk = 0;
    foreach (@{$data{"row"}})
    {
    	my $oldpath = @{$_}[$directoryIndex] . "/" . @{$_}[$filenameIndex];
	#print "oldpath: $oldpath\n";
	my $file_id = @{$_}[$file_idIndex];

	if ($oldpath eq $updates[$i][0])
	{
	    my @split = split(/\//, $updates[$i][1]);
	    my $newfilename = $split[$#split];
	    my $newdirectory = $updates[$i][1];
	    $newdirectory =~ s/\/$newfilename$//;
	    #print "$newdirectory   /   $newfilename      $file_id\n";
	    #print "#Updating!\n";
	    $msg = $database->update("file", "directory=\'$newdirectory\', filename=\'$newfilename\'", "file_id=$file_id");
	    if ($msg ne "") { print "\033[1;4;31m$msg\nDatabase Rolled back!\n\033[0m"; $database->rollback(); $database->disconnect(); exit(1); }
	    
	    $chk++;;
	}
    }
    print "\t\033[1;4;31mWARNING: Could not find CODIAC match for $updates[$i][0]\033[0m\n" unless $chk;
    print "\tWARNING: Multiple matches found for $updates[$i][0]\n" if $chk > 1;
}

if (`hsi ls /EOL/$projYear/$projName/aircraft/$projPlat 2>&1` =~ /No such file or directory/) {
    print "\t\033[1;4mNotice: /EOL/$projYear/$projName/aircraft/$projPlat Does not exist\033[0m\n";
}

#Create list of final commands
print "#Creating command list and verifying existing files\n";
#open LOG, ">msmv-$projName.log";
my @commands;
for (my $i = 0; $i <= $#updates; $i++)
{
	if (`hsi ls $updates[$i][0] 2>&1` !~ /No such file or directory/) {
	    push @commands, "hsi mv $updates[$i][0] $updates[$i][1]";
	    print "$commands[$#commands]\n";
#	    print LOG "$commands[$#commands]\n";
	}
	else
	{
		print "\t\033[1;4;31mWARNING: $updates[$i][0]  does not exist!\033[0m\n"
	}

	#Add V* files if they exist
	if ($updates[$i][0] !~ /\/G\d+/) { next; };
	$updates[$i][0] =~ s/\/G(\d+)/\/V$1/;
	$updates[$i][1] =~ s/\/G(\d+)/\/V$1/;
	if (`hsi ls $updates[$i][0] 2>&1` !~ /No such file or directory/) {
	    push @commands, "hsi mv $updates[$i][0] $updates[$i][1]";
	    print "$commands[$#commands]\n";
#	    print LOG "$commands[$#commands]\n";
	}
}
#close LOG;

open LOG1, ">>moves.log";
print "# " . ($#commands+1) . " moves are ready\n\n#Execute the commands as listed? yes == enter, no == anything else: ";
chomp (my $responce = <STDIN>);
if ($responce eq "")
{
	print "#MOVING\n";
	foreach (@commands) {
	    #print "$_\n";
	    system($_); ##LIVE
	    my @split = split(/ /, $_);
	    if (`hsi ls $split[$#split] 2>&1` =~ /No such file or directory/) {
		print "\t\033[1;4;31mERROR: DESTINATION FILES DOES NOT EXIST!\n\t$split[$#split]\033[0m\n"
	    }
	    else {
		print "#Successfully moved: $split[3] => $split[$#split]\n";
		print LOG1 "$_\n";
	    }
	}
	print $database->commit();
}
else
{
	print "#Job canceled.\n";
	print $database->rollback();
}
close LOG1;

print $database->disconnect();
print "\n\n";
exit(0);
