#! /usr/bin/perl -w

use strict;
use DTSUtil;
$! = 1;

my $UTIL = DTSUtil->new();

&main();

sub main {
	renameML();
}

sub renameML {
    my $conn = $UTIL->getConnectionToML();
    my @tables = @{$conn->selectall_arrayref("SHOW TABLES LIKE 'ds%'")};
    my $err = 0;

    foreach my $table (@tables) {
	$table = $table->[0];
	if ($table !~ /dscatlink/) {
	    $table =~ /^ds(.+)$/;
	    my $project = $1;
	    #my $mlRef = $conn->selectall_hashref("SELECT id,data_set FROM $table WHERE data_set LIKE '%[JOSS]%' OR data_set LIKE '%(JOSS)%'","id");
	    my $mlRef = $conn->selectall_hashref("SELECT id,data_set FROM $table","id");

	    foreach my $id (keys(%{$mlRef})) {
		$mlRef->{$id}->{"data_set"} =~ s/[\[\(]NCAR\/ATD[\]\)]/\[NCAR\/EOL\]/;
		printf("%s\n",$mlRef->{$id}->{"data_set"});
		
		$conn->prepare("UPDATE $table SET data_set=? WHERE id=?")->
		    execute($mlRef->{$id}->{"data_set"},$id);
	    }
	}
    }
}
