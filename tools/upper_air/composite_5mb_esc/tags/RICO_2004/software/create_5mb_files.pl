#! /usr/bin/perl -w

use strict;
use Cwd;
use File::Copy;

*STDERR = *STDOUT;

my $PROGRAM = "../../extracter/bin/extract5mb";

my $INPUTDIR = "../data";
my $FINALDIR = "../5mb";
my $LOGDIR = "../logs";

&main();

sub main {
    
    mkdir($FINALDIR) unless (-e $FINALDIR);
    mkdir($LOGDIR) unless (-e $LOGDIR);

    opendir(my $DIR,$INPUTDIR) or die("Can't open directory: $INPUTDIR\n");
    my @networks = grep(/^[^\.]+$/,readdir($DIR));
    closedir($DIR);

    foreach my $network (sort(@networks)) {

	mkdir(sprintf("%s/%s",$FINALDIR,$network)) unless (-e sprintf("%s/%s",$FINALDIR,$network));
	mkdir(sprintf("%s/%s",$LOGDIR,$network)) unless (-e sprintf("%s/%s",$LOGDIR,$network));

	opendir(my $DIR,sprintf("%s/%s",$INPUTDIR,$network)) or die("Can't open directory: $network\n");
	my @files = grep(/\.cls$/,readdir($DIR));
	closedir($DIR);

	chdir(sprintf("%s/%s",$INPUTDIR,$network)) or die("Can't change to $INPUTDIR/$network\n");
	printf("Working in: %s\n",cwd());

	foreach my $file (@files) {

	    printf("\t%s %s\n",$PROGRAM,$file);

	    system(sprintf("%s %s",$PROGRAM,$file));

	    $file =~ s/cls$/05mb/;
	    move($file,sprintf("../%s/%s/%s",$FINALDIR,$network,$file)) or die("Can't copy to final directory\n");

	    $file =~ s/05mb$/log/;
	    move($file,sprintf("../%s/%s/%s",$LOGDIR,$network,$file)) or die("Can't copy to log direcotry\n");
	}

	chdir("../../software");

    }
}
