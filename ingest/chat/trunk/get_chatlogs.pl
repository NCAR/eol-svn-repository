#!/usr/bin/perl
use strict;
use lib "/net/work/lib/perl/mail";
use lib "/h/eol/dmg/HPSS_cronjobs/ingest/chat/trunk";
use MAIL;
use Net::FTP;
use FindBin qw($Script $Bin);
require 'chatrooms.pl';
###############################################################################
# For documentation see
# https://internal.eol.ucar.edu/content/chatlog-inventory
###############################################################################
my @monitors = ("eol-cds-ingest\@ucar.edu");
my $logdir = "/net/work/Projects/chatlog_recovery/logs";

my $header = "I am $Bin/$Script running on ".`/bin/hostname`."\n";

# Figure out what day yesterday was
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
                                                    localtime(time-(86400));
# Convert date to string in chatlog filenames (yyyy.mm.dd)
my $day=sprintf("%04d.%02d.%02d",$year+1900,$mon+1,$mday);

my $body="";
&download_chat($body);
&del_chatlogs($body);
my @svn_errors = `./rm_server_msgs 2>&1`;
# sed has a bug where when editing inline it throws an error:
# "sed: cannot rename /net/work/Projects/chatlog_recovery/logs/<file>: Device
# or resource busy" but it still works. So if this is the only error, ignore
# it.
foreach my $line (@svn_errors) { 
    if ($line !~ /sed: cannot rename .*: Device or resource busy/) {
	$body .= $line;
    }
}
&sort_chat($body);

if ($body =~ /ACTION REQUIRED/) {
    &sendMailAndDie("$header $body");
}
#-------------------------------------------------------------------------------
# Subroutine to download new chatLogs daily.
#-------------------------------------------------------------------------------
sub download_chat 
{
 
    $body .= "Downloading data for $day\n";
    
    # Get a listing of all chatlogs on the ftp server
    my $chatdir = '/net/cds/chat';
    opendir (DIR,$chatdir) or sendMailAndDie("Can't open dir $chatdir:$!\n");
    my @files = grep { !/^\./ } readdir(DIR);
    closedir (DIR);
    if (scalar @files == 0) 
        { sendMailAndDie("Cannot list directory $chatdir"); }

    # Download just those files that match the date string
    foreach my $file (@files) {
        if ($file =~ /$day/) {
            system(`cp $chatdir/$file $logdir/$file`)
                or sendMailAndDie("cp failed on $file");
        }
    }
    return($body);
}

#-------------------------------------------------------------------------------
# Subroutine to delete chatLogs that only contain messages from EOL's server
# or output of replay commands.
#-------------------------------------------------------------------------------
sub del_chatlogs 
{
    $body .= "\n Deleting empty chatlogs\n";

    opendir (DIR,$logdir) or sendMailAndDie("Can't open dir $logdir:$!\n");
    my @files = grep { !/^\./ } readdir(DIR);
    closedir (DIR);
    
    my @patterns=(
	"] @",
	"] \<groundbot\>",
	"] \<gvbot(gvbot\@localhost)\>",
	"Monitoring channel",
        );
    foreach my $file (@files) {
	next unless (-f "$logdir/$file");
    
	#$body .= "\t$logdir/$file\n";
    
        open(FP,"$logdir/$file") or sendMailAndDie("Can't open $logdir/$file:$!\n");
        my @chatlines = ();
        while (<FP>) {
            my $found=0;
            foreach my $pattern (@patterns) {
    	    if ($_ =~ $pattern) {
                    $found = 1;
    	    }
            }
            if (!$found) { 
    	    push @chatlines,$_;
            }
        }
        my $numlines = scalar(@chatlines)."\n";
	chomp ($numlines);
	#$body .= "\t$numlines lines in file";
    
        if ($numlines ==0 ) {
	    system ("rm $logdir/$file");
	    #$body .= "- deleted\n";
	}
    
    }
}


#-------------------------------------------------------------------------------
# Subroutine to sort chatLogs into subdirs by project.
# Mapping of chatrooms to projects is in chatrooms.pl
#-------------------------------------------------------------------------------
sub sort_chat
{
    $body .= "\n Sorting chatlogs in $logdir to project dirs\n";

    opendir(DIR,$logdir) or sendMailAndDie("Can't open dir $logdir: $!\n");
    my @files = grep { !/^\./ } readdir(DIR);
    close(DIR);
    
    foreach my $file (@files) {
	next unless (-f "$logdir/$file");
	#$body .= "\t$logdir/$file\n";
	my ($room,$year,$mon,$day) = split('\.',"$file");
	$room =~ s/#//;
	my $chatmap = $ChatMap::chatmap{$room};

	my $begindate = $ChatMap::project_dates{$chatmap}[0];
	$begindate =~ s/\///g;
	my $enddate = $ChatMap::project_dates{$chatmap}[1];
	$enddate =~ s/\///g;
	my $today = "$year$mon$day";

        if (! $chatmap) {
	    # If don't find a mapping for this chatroom, notify the monitors.
	    $body .= "ACTION REQUIRED: Did not find project for room $room. Update to ChatMap needed.\n";
	} elsif (($today gt $enddate) || ($today lt $begindate)) {
	    # If out of date for project, notify monitors.
	    $body .= "ACTION REQUIRED: Chatroom $room is not within dates for project $chatmap.\n";
	} else {
	    if ($chatmap =~ /doNotSave/) {
                $body .= "\n Deleting chatlog $file\n";
		system("rm $logdir/$file");
	    } elsif ($chatmap =~ /operational/) {
		if (! -d "$logdir/$chatmap") {
		    system("mkdir $logdir/$chatmap");
		}
		if (! -d "$logdir/$chatmap/$year") {
		    system("mkdir $logdir/$chatmap/$year");
		}
		system("mv $logdir/$file $logdir/$chatmap/$year/$file\n");
	    } else {
		if (! -d "$logdir/Projects/$chatmap") {
		    system("mkdir $logdir/Projects/$chatmap");
		}
	        system("mv $logdir/$file $logdir/Projects/$chatmap/$file\n");
	    }
	}
    }
}

#-------------------------------------------------------------------------------
# Subroutine to send output of script to email address
#-------------------------------------------------------------------------------
sub sendMailAndDie {
    my ($body) = @_;
    MAIL::send_mail("get_chatlogs log",$0."\n\n".$body, @monitors);
    exit(1);
}
