#! /usr/bin/perl -w

##Module--------------------------------------------------------------------------------
# <p>The script check_class_header.pl is a script that is used for checking CLASS
# formatted header files.  It makes sure that the nominal date is actually nominal for
# the actual date.  It also will read in all of the ascension numbers for the files in
# the processing directory to search for missing files.  (This will also find the most
# recent processed file for each station to get the most recent acension number to make
# sure that there is not a sounding missing between processings.)</p>
#
# @author Joel Clawson
#
# @author L. Cully
# <p> Updated library section.</p>
#
##Module--------------------------------------------------------------------------------
use strict;
####use lib "/work/software/conversion_modules/Version4";


use lib "../lib";

if (-e "/net/work") {
    use lib "/net/work/lib/perl/Utilities";
    use lib "/net/work/lib/perl/UpperAir";
    use lib "/net/work/lib/perl/Station";
} else {
    use lib "/work/lib/perl/Utilities";
    use lib "/work/lib/perl/UpperAir";
    use lib "/work/lib/perl/Station";
}

use DpgDate;

my $CLASS_DIR = "../output";
my $PREV_DIR = "../processed/final";

my $data = {};

&main();

##---------------------------------------------------------------------------------------
# @signature void main()
# <p>Run the program to check for missing ascension numbers and to see if the actual date
# lines up with the nominal date.</p>
##---------------------------------------------------------------------------------------
sub main {
    opendir(my $DIR,$CLASS_DIR) or die("Cannot open $CLASS_DIR\n");
    my @files = grep(/\.cls$/,readdir($DIR));
    closedir($DIR);

    foreach my $file (sort(@files)) {

	open(my $FILE,sprintf("%s/%s",$CLASS_DIR,$file)) or die("Can't read $file\n");
	my @lines = <$FILE>;
	close($FILE);

	$lines[2] =~ /Release Site Type\/Site ID:\s+(.+)/;
	my $station = $1;

	$data->{$station}->{"id"} = substr($file,0,3);

	$lines[4] =~ /(\d{4}, \d{2}, \d{2}, \d{2}:\d{2}:\d{2})/;
	my ($actdate) = $1;

	$lines[5] =~ /Ascension No:\s+\d(\d+)/;
	push(@{ $data->{$station}->{"ascnum"}},$1);

	$lines[11] =~ /(\d{4}, \d{2}, \d{2}, \d{2}:\d{2}:\d{2})/;
	my ($nomdate) = $1;

	# Compare the actual and nominal datetimes.
	my $min = substr($actdate,17,2);
	my ($date,$time) = adjustDateTime(substr($actdate,0,12),"YYYY, MM, DD",
					  substr($actdate,14,8),"HH:MM:SS",0,1,
					  -1 * $min,0);
	if (sprintf("%s, %s",$date,$time) ne $nomdate) {
	    printf("%s: %s, %s (%s) != %s\n",$file,$date,$time,$actdate,$nomdate);
	}
    }

    # Get the last sounding ascension number that was processed previously.
    foreach my $station (keys(%{ $data})) {
	my $id = $data->{$station}->{"id"};

	opendir(my $DIR,$PREV_DIR) or die("Can't open directory $PREV_DIR\n");
	my $last_file = (reverse(sort(grep(/^$id.*\.cls(\.gz)$/,readdir($DIR)))))[0];
	closedir($DIR);

	if ($last_file =~ /\.gz/) {
	    system("gunzip $PREV_DIR/$last_file");
	}

	my $file = $last_file;
	$file =~ s/\.gz//;
	my $FILE;
	open($FILE,sprintf("%s/%s",$PREV_DIR,$file)) or die("Can't read file $file\n");
	my @lines = <$FILE>;
	close($FILE);

	if ($last_file =~ /\.gz/) {
	    system("gzip $PREV_DIR/$file");
	}

	$lines[5] =~ /Ascension No:\s+\d(\d+)/;
	push(@{ $data->{$station}->{"ascnum"}},$1);
    }

    # Check for missining ascension numbers for each station.
    foreach my $station (keys(%{ $data})) {
	my $num;
	foreach my $asc (sort(@{ $data->{$station}->{"ascnum"}})) {
	    if (!defined($num) || ($asc == $num + 1)) { $num = $asc; }
	    else {
		printf("Missing an ascension number before: %s - %d\n",$station,$asc);
		$num = $asc;
	    }
	}
    }
}
