#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
my $msg = "";

unless ($#ARGV == 3) {
print <<EOM;
usage: ./copyColumns MATCH COPY SRC DEST
	MATCH = Comma delimited list of column names that must match
	COPY = Comma delimited list of columns to copy
	SRC = dataset_id containing source files
	DEST = dataset_id containing destination files

example:
	./copyColumns.pl event begin_date,end_date 120.002 120.001

Will copy begin_date and end_date of file in 120.002 to files in 120.001 where the event string matches

EOM
exit(1);
}

my $src_dataset_id = $ARGV[2];
my $dest_dataset_id = $ARGV[3];
my @copy_column_name = split(/,/,$ARGV[1]);
my @copy_column;
my @match_column_name = split(/,/,$ARGV[0]);
my @match_column;

my $file_id_index = -1;

my $database = MySqlDatabase->new("zediupdate","change-456");
#$database->setHost("merlot.eol.ucar.edu");
$database->connect();

#Auto cleanup on die
local $SIG{'__DIE__'} = sub { print "Rolling back database!\n"; print $database->rollback(); print $database->disconnect(); };

#Select files from src dataset
($msg, my %src_Data) = $database->selectFull("file", "*", "dataset_id='$src_dataset_id'");
die "MySql error\n" unless $msg eq "";
die "No files found for dataset: $src_dataset_id\n" unless $database->getRows() > 0;
%src_Data = FormatData(%src_Data);

#Select files from dest dataset
($msg, my %dest_Data) = $database->selectFull("file", "*", "dataset_id='$dest_dataset_id'");
die "MySql error\n" unless $msg eq "";
die "No files found for dataset: $dest_dataset_id\n" unless $database->getRows() > 0;
%dest_Data = FormatData(%dest_Data);

#Parse column names to get indexes
for (my $i = 0; $i <= $#{$src_Data{"name"}}; $i++) {
	if ("file_id" eq $src_Data{"name"}[$i])
	{
		$file_id_index = $i;
		last;
	}
}
die "Unable to find file_id column!\n" unless $file_id_index > -1;

foreach my $column_name (@copy_column_name) {
	my $chk = 0;
	for (my $i = 0; $i <= $#{$src_Data{"name"}}; $i++) {
		if ($column_name eq $src_Data{"name"}[$i])
		{
			push @copy_column, [$column_name, $i];
			$chk++;
		}
	}
	die "No column matched: $column_name\n" if $chk == 0;
	die "Multiple columns matched: $column_name\n" if $chk > 1;
}

foreach my $column_name (@match_column_name) {
	my $chk = 0;
	for (my $i = 0; $i <= $#{$src_Data{"name"}}; $i++) {
		if ($column_name eq $src_Data{"name"}[$i])
		{
			push @match_column, [$column_name, $i];
			$chk++;
		}
	}
	die "No column matched: $column_name\n" if $chk == 0;
	die "Multiple columns matched: $column_name\n" if $chk > 1;
}

my @updated_row;
#foreach (@{$dest_Data{"row"}}) {
#	my @dest_row = @{$_};
for (my $a = 0; $a <= $#{$dest_Data{"row"}}; $a++) {
	my @dest_row = @{$dest_Data{"row"}[$a]};

	for (my $i = 0; $i <= $#{$src_Data{"row"}}; $i++) {
		my @src_row = @{$src_Data{"row"}[$i]};

		my $matches = 0;
		foreach (@match_column) {
			my $index = @{$_}[1];
			if (lc $src_row[$index] eq lc $dest_row[$index]) {
				$matches++;
			}
		}
		
		if ($matches == ($#match_column+1)) {
			push @updated_row, [$i, $a];
		}
	}
}

foreach (@updated_row) {
	my @src_row = @{@{$src_Data{"row"}}[@{$_}[0]]};
	my @dest_row = @{@{$dest_Data{"row"}}[@{$_}[1]]};

	my $set_line = "";
	my $display_line = "$dest_row[3]/$dest_row[4]:\n";
	$display_line =~ s/\"//g;
	foreach (@copy_column)
	{
		$set_line .= @{$_}[0] . "=" . $src_row[@{$_}[1]] . ",";
		$display_line .= "\t" . @{$_}[0] . "=" . $src_row[@{$_}[1]] . "\n";
	}
	$set_line =~ s/,$//;
	print "$display_line";
	$msg = $database->update("file", $set_line, "file_id=$dest_row[$file_id_index]");
	die "Mysql Error\n" unless $msg eq "";
}


print "\n\n#" . ($#updated_row+1) . " files are ready to be updated\n\n";
print "#To update the files, press Enter.\n";
print "#To cancel the update, enter any value and press Enter.\n\n";
print "#>>";
my $result = <STDIN>;
chomp $result;
if ($result eq "")
{
	print $database->commit();
}
else
{
	print $database->rollback();
}

print $database->disconnect();
exit(0);

sub FormatData
{
    my (%Data) = @_;
    for (my $i = 0; $i <= $#{$Data{"type"}}; $i++) {
	    if ((@{$Data{"type"}}[$i] == 12 || @{$Data{"type"}}[$i] == 11 || @{$Data{"type"}}[$i] == -4 || @{$Data{"type"}}[$i] == 1)) {
		    for (my $ii = 0; $ii <= $#{$Data{"row"}}; $ii++)
		    {
			    if (defined @{@{$Data{"row"}}[$ii]}[$i]) {
				    @{@{$Data{"row"}}[$ii]}[$i] = "\"" . @{@{$Data{"row"}}[$ii]}[$i] . "\"";
			    }
			    else
			    {
				    @{@{$Data{"row"}}[$ii]}[$i] = "NULL";
			    }
		    }
	    }
    }
    return %Data
}
