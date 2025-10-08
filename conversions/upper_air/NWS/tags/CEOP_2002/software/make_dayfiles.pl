#! /usr/bin/perl -w

use strict;

&main();

sub get_output_directory { return "../codiac"; }
sub get_source_directory { return "../final"; }

sub main {
    opendir(my $SRC,get_source_directory()) or die("Can't open source directory\n");
    my @files = grep(/\.cls$/,readdir($SRC));
    closedir($SRC);

    create_day_files(@files);
}

sub create_day_files {
    my @files = @_;

    my %days = determine_days(@files);

    mkdir(get_output_directory()) unless(-e get_output_directory());

    foreach my $stn (keys(%days)) {
	foreach my $day (keys(%{ $days{$stn}})) {
	    printf("%s: %s\n",$stn,$day);
	    system(sprintf("cat %s > %s/%s_%s.cls",join(" ",sort(@{ $days{$stn}{$day}})),
			   get_output_directory(),$stn,$day));
	}
    }
}

sub determine_days {
    my @files = @_;

    my %days; 

    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",get_source_directory(),$file)) or
	    die("Can't open $file\n");
	my @lines = <$FILE>;
	close($FILE);

	$lines[11] =~ /:(\d+),\s*(\d+),\s*(\d+),\s*\d+:\d+:\d+/;
	my $date = sprintf("%04d%02d%02d",$1,$2,$3);
	$file =~ /^([^_]+)_\d+.*\.cls$/;

	push(@{ $days{$1}{$date}},sprintf("%s/%s",get_source_directory(),$file));
    }

    return %days;
}
