#! /usr/bin/perl -w
##Module------------------------------------------------------------------------------------
# <p>The make_highres_dayfiles.pl script is used for merging individual CLASS formatted
# sounding files into day files.  These files are the invidual soundings stacked next to
# each other by station by time.</p>
# <p>This script also counts the number of soundings that are included for each network
# along with the total sounding count used to create the day files.</p>
#
# @author Joel Clawson
##Module------------------------------------------------------------------------------------
use strict;

&main();

# Data location Constants
sub get_output_directory { return "../codiac"; }
sub get_source_directory { return "../data"; }

# Constants to be changed with each project.
sub get_project { return "RICO"; }
sub get_begin_date { return "20041124"; }
sub get_end_date { return "20050131"; }

##------------------------------------------------------------------------------------------
# @signature void main()
# <p>Create day files from the data for only the soundings that fall within the project
# time of interest and output the number of soundings that were included.</p>
##------------------------------------------------------------------------------------------
sub main {
    my @files = ();

    opendir(my $SRC,get_source_directory()) or die("Can't open source directory\n");
    foreach my $dir (grep(/^[^\.]/,readdir($SRC))) {
	opendir(my $DIR,sprintf("%s/%s",get_source_directory(),$dir)) or die("Can't open $dir\n");
	foreach my $file (grep(/\.cls$/,readdir($DIR))) {
	    push(@files,sprintf("%s/%s",$dir,$file));
	}
	closedir($DIR);

    }
    closedir($SRC);

    create_day_files(@files);
}

##------------------------------------------------------------------------------------------
# @signature void create_day_files(String[] files)
# <p>Create day files using the data in the specified file list.</p>
#
# @input files[] The list of files to be combined into day files.
##------------------------------------------------------------------------------------------
sub create_day_files {
    my @files = @_;

    # Get the list of dates to use to make day files.
    my $days = determine_days(@files);

    mkdir(get_output_directory()) unless(-e get_output_directory());

    # Create the day files and count the number of files used in the creation of the day files.
    my $count = 0;
    my $net_counts;
    foreach my $day (sort(keys(% {$days}))) {
	printf("Creating day file for day: %s\n",$day);
	$count += @{ $days->{$day}};
	system(sprintf("cat %s > %s/%s_HighRes_%s.cls",join(" ",sort(@{ $days->{$day}})),
		       get_output_directory(),get_project(),$day));

	foreach my $file (@ {$days->{$day}}) {
	    $file =~ /([^\/]+)\/[^\/]+$/;
	    if (defined($net_counts->{$1})) {
		$net_counts->{$1}++;
	    } else {
		$net_counts->{$1} = 1;
	    }
	}
    }

    printf("%d Soundings were included between %s and %s\n",$count,get_begin_date(),get_end_date());
    foreach my $net (sort(keys(%{$net_counts}))) {
	printf("\t%d in network %s\n",$net_counts->{$net},$net);
    }
}

##------------------------------------------------------------------------------------------
# @signature Hash* determine_days(String[] files)
# <p>Determine the nominal dates for the list of files.</p>
#
# @input files[] The list of files to use to determine the nominal dates.
# @output $days The hash reference telling what files are to be put with which day.
##------------------------------------------------------------------------------------------
sub determine_days {
    my @files = @_;

    my $days;

    foreach my $file (@files) {
	open(my $FILE,sprintf("%s/%s",get_source_directory(),$file)) or
	    die("Can't open $file\n");
	my @lines = <$FILE>;
	close($FILE);

	# Parse out the nominal date from the file.
	$lines[11] =~ /:\s*(\d+),\s*(\d+),\s*(\d+),?\s*\d+:\d+:\d+/;
	my $date = sprintf("%04d%02d%02d",$1,$2,$3);

	# Only handle files that have the nominal date in the project time of interest.
	if (in_time_of_interest($date)) {
	    push(@{ $days->{$date}},sprintf("%s/%s",get_source_directory(),$file));
	}
    }

    return $days;
}

##------------------------------------------------------------------------------------------
# @signature boolean in_time_of_interest(String date)
# <p>Determine if the specified date is in the time of interest for the project.</p>
#
# @input $date The date to be checked.
# @output $check <code>true</code> if the date is in the time of interest, otherwise <code>
#    false</code>.
##------------------------------------------------------------------------------------------
sub in_time_of_interest {
    my ($date) = @_;
    return (get_begin_date() <= $date && $date <= get_end_date());
}
