#! /usr/bin/perl -w

use strict;
use DBI;
$| = 1;

my $TRUE = 1;
my $FALSE = 0;

my $NOTE_URL = "/work/DPG_HTML/EPIC/epic_notes.html";
my $PROJECT = "EPIC";

my $USER_MAP = {"lec"=>"lCully",
		"jc"=>"jClawson",
		"jg"=>"jGoldstein",
		"jm"=>"jmeitin",
		"dw"=>"dWilkinson",
		"rb"=>"rBateman",
		"ps"=>"pSkinner",
		"xx"=>"unassigned",
		"pd"=>"pDressen",
		"dg"=>"dGallant",
		"ds"=>"dSullivan",
		"ab"=>"aBhatia"};

my $CONN;

my @datasets = ();

&main();

sub main {

    $CONN = DBI->connect("DBI:mysql:database=dln;host=hurricane","dln","codiac",
			 {AutoCommit => 0, RaiseError => 0})
	or die("Unable to establis a connection to DLN!\n");
    
    my $err = 0;

    $err = parseHTML();

    printf("%d\n",scalar(@datasets));

    createProject();
    insertDatasets();
    $CONN->commit();
}

sub cleanHTML {
    my ($line) = @_;

    $line =~ s/\<\/?td([^\>]+)?\>//g;

    $line =~ s/\<\/?font[^\>]*\>//g;

    $line =~ s/\&nbsp;//g;

    $line =~ s/^\s+//;
    $line =~ s/\s+$//;

    return $line;
}

sub createProject {
    $CONN->prepare("INSERT INTO project(pname,storm_id_prefix,active) VALUES(?,?,?)")->
	execute($PROJECT,72,1) or die($DBI::errstr);
}

sub insertDatasets {
    my $err = $FALSE;
    
    foreach my $dataset (@datasets) {
	$CONN->prepare("INSERT INTO dataset(storm_id,title,readme,master,checked,loaded,date,notes,project,loader,checker,remote_url,int_contact,ext_contact,ext_email,ingest,archive) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)")->execute($dataset->{"storm_id"},$dataset->{"title"},$dataset->{"readme"},$dataset->{"master"},$dataset->{"checked"},$dataset->{"loaded"},$dataset->{"date"},$dataset->{"notes"},$dataset->{"project"},$dataset->{"loader"},"unassigned",$dataset->{"remote_url"},"unassigned","","","","") || die($DBI::errstr);
    }

    return $err;
}

sub isSpacerRow {
    my @entries = @_;

    my $allMissing = $TRUE;
    foreach my $entry (@entries) {
	$entry = cleanHTML($entry);
	$allMissing = $allMissing && ($entry =~ /^\s*$/); 
    }
    return $allMissing;
}

sub parseHTML {
    my $err = 0;
    my $missingCounter = 0;

    open(my $HTML,$NOTE_URL) or die("Cannot open $NOTE_URL\n");
    
    my $inTable = $FALSE;
    while (defined(my $line = <$HTML>) && !$inTable) { 
	$inTable = ($line =~ /\<table/i) ? $TRUE : $FALSE;
	if ($inTable) {
	    my $readHeader = $FALSE;
	    while (defined($line = <$HTML>) && !$readHeader) {
		$readHeader = ($line =~ /\<\/tr\>/i) ? $TRUE : $FALSE;
	    }
	}
    }

    my $count = 0;
    while (defined(my $line = <$HTML>)) {
	next if ($line !~ /^\s*\<tr\>\s*$/);

	my @colList = ();
	$line = <$HTML>;
	while ($line !~ /^\<\/tr\>\s*$/) { chomp($line),push(@colList,$line); $line = <$HTML>; }

	$count++;
	next if (isSpacerRow(@colList));

	cleanHTML($colList[0]) =~ /\<a href=\"([^\"]+)\".*\>(.*)\<\/a\>/;

#	printf("%d: %s %s (%s)\n",$count,$1,$2,cleanHTML($colList[0]));

	my $dataset = {};
	my ($url,$id) = ($1,$2);
	$dataset->{"project"} = $PROJECT;
	$dataset->{"storm_id"} = defined($id) && ($id =~ /link/i || $id =~ /\d+\.\d+/) ? trim($id) : "99.999";
	$dataset->{"remote_url"} = defined($url) && $url =~ /cgi\-bin\/codiac/ ? "" : $url;


	$colList[1] = cleanHTML($colList[1]);
	if ($colList[1] =~ /can remove/i) {
	    $dataset->{"notes"} = "<p>Can Remove?</p>";
	    $colList[1] = substr(trim($colList[1]),0,3);
	} else {
	    $dataset->{"notes"} = "";
	}
	
	my @loaderList = split("/",$colList[1]);
	@loaderList = ("xx") if (join(", ",@loaderList) eq "N, A");
	@loaderList = ("xx") if (@loaderList == 0);

	$dataset->{"loader"} = $USER_MAP->{lc(trim($loaderList[scalar(@loaderList)-1]))};

#	printf("%s\n",$colList[1]);
#	printf("\t%s: %s\n",join(",",@loaderList),$dataset->{"loader"});

	$dataset->{"title"} = cleanHTML($colList[2]);
	$dataset->{"readme"} = cleanHTML($colList[3]) =~ /Y/i ? $TRUE : $FALSE;
	$dataset->{"checked"} = cleanHTML($colList[4]) =~ /Y/i ? $TRUE : $FALSE;
	$dataset->{"loaded"} = cleanHTML($colList[4]) =~ /Y/i ? $TRUE : $FALSE;
	$dataset->{"master"} = cleanHTML($colList[5]) =~ /Y/i ? $TRUE : $FALSE;
	$dataset->{"date"} = "1900-01-01";
	$dataset->{"date"} = sprintf("%04d-%02d-%02d",split('/',cleanHTML($colList[6])))
	    if (cleanHTML($colList[6]) =~ /^\d+\/\d+\/\d+$/);
	$dataset->{"notes"} .= $colList[7];

	$dataset->{"title"} =~ s/^EPIC:?\s*//;
	$dataset->{"notes"} =~ s/\<\/?td([^\>]+)?\>//g;
	$dataset->{"notes"} =~ s/^\s+//;
	$dataset->{"notes"} =~ s/\s+$//;


#	printf("%s: %s\n",$dataset->{"storm_id"},$dataset->{"date"});

	push(@datasets,$dataset);
    }

#    printf("%s\n",$count);

    close($HTML);
    return $err;
}

sub trim {
    my $line = shift;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
