#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
my $msg = "";

my $database = MySqlDatabase->new("zediupdate","change-456");
#$database->setHost("merlot.eol.ucar.edu");
$database->connect();

my $oldname = $ARGV[0];
my $newname = $ARGV[1];

($msg, my %data) = $database->selectFULL("project","*","project_id=\"$oldname\"");
if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }

if ($database->getRows() == 1) {
	my $full_name = "";

	#Data cleanup
	for (my $i = 0; $i <= $#{$data{"type"}}; $i++) {
		if (defined(@{@{$data{"row"}}[0]}[$i]) && @{@{$data{"row"}}[0]}[$i] eq $oldname) { @{@{$data{"row"}}[0]}[$i] = $newname; }
		#Get project full name (so it can be easily modified later
		if (@{$data{"name"}}[$i] eq "full_name") { $full_name = @{@{$data{"row"}}[0]}[$i];}
		#update the gcmd_name is needed
		if (@{$data{"name"}}[$i] eq "gcmd_name" && defined(@{@{$data{"row"}}[0]}[$i])) { @{@{$data{"row"}}[0]}[$i] =~ s/$oldname/$newname/;}
		#Replace empty values with NULL
		if (!defined(@{@{$data{"row"}}[0]}[$i]) || @{@{$data{"row"}}[0]}[$i] eq "") { @{@{$data{"row"}}[0]}[$i] = "NULL"; @{$data{"type"}}[$i] = 0; }
		#Get rid of row_revise_time
		if (@{$data{"name"}}[$i] eq "row_revise_time") { splice @{$data{"name"}}, $i, 1; splice @{@{$data{"row"}}[0]},$i,1; splice @{$data{"type"}},$i,1; $i--; next; }
		#Add quotation marks as needed
		if ((@{$data{"type"}}[$i] == 12 || @{$data{"type"}}[$i] == 11 || @{$data{"type"}}[$i] == -4) && defined(@{@{$data{"row"}}[0]}[$i])) {
			@{@{$data{"row"}}[0]}[$i] = "\"" . @{@{$data{"row"}}[0]}[$i] . "\"";
		}
	}
	#bah! full name is unique...
	$msg .= $database->update("project", "full_name=\"$full_name TEMP\"" ,"project_id=\"$oldname\"");
	if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }

	#Insert renamed project
        $msg .= $database->insert("project",join(',', @{$data{"name"}}),join(',', @{@{$data{"row"}}[0]}));
	if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }

	#Move rows which depend on old project to new project
	$msg .= $database->update("dataset_prefix_project", "project_id=\"$newname\"" ,"project_id=\"$oldname\"");
	if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }
	
	$database->setMaxAffectedRows(10);
	$msg .= $database->update("project_xlinks", "project_id=\"$newname\"" ,"project_id=\"$oldname\"");
	if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }
	
	$msg .= $database->update("dataset_project", "project_id=\"$newname\"" ,"project_id=\"$oldname\"");
	if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }

	#Delete old project
        $msg .= $database->delete("project","project_id=\"$oldname\"");
	if ($msg ne "") { print "$msg\nDatabase Rolled back!\n"; $database->rollback(); $database->disconnect(); exit(1); }
	
}
else
{
	print "No project with project_id==\"$oldname\" was found\n";
	$database->disconnect();
	exit(0);
}
#for (my $i = 0; $i <= $#{$data{"type"}}; $i++) {
#	print @{$data{"name"}}[$i] . "," . @{@{$data{"row"}}[0]}[$i] . "\n";
#}
if ($msg eq "") {print "Project was successfully renamed!\n"; $database->commit();}
else {print "$msg\n";}

$database->disconnect();
exit(0);

