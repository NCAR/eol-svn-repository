#! /usr/bin/perl -w

use strict;
use DBI;

my $TRUE = 1;
my $FALSE = 0;

my $HEAD_MAP = {};
my $CAT_MAP = {};

my $PROJECT = "EPIC";
my $ML_FILE = "/web/docs/epic/dm/data_list.html";

my $CONN;

&main();

sub main {
    $CONN = DBI->connect("DBI:mysql:database=ml;host=hurricane","mlist","world-data",
			 {AutoCommit => 0, RaiseError => 0})
	or die("Unable to establis a connection to ML!\n");
    

    setupCategories();
    parseHTML();

    createDatabaseTables();
    insertCats();
    insertDatasets();

    $CONN->disconnect();
}

sub cleanHTML {
    my ($line) = @_;

    $line =~ s/\<\/?td([^\>]+)?\>//gi;

    $line =~ s/\<\/?font[^\>]*\>//gi;
    $line =~ s/\<\/?b\>//gi;
    $line =~ s/\<br\>//gi;

    $line =~ s/\&nbsp;//g;

    $line =~ s/target\=[\"\']?_top[\"\']?//gi;

    $line =~ s/\<!\-\-.+\-\-\>//gi;

    $line =~ s/^\s+//;
    $line =~ s/\s+$//;

    return $line;
}

sub createDatabaseTables {
    my $sqlHEAD = "CREATE TABLE headEPIC (   id int(11) NOT NULL auto_increment,   name varchar(50) default NULL,   abrv varchar(20) default NULL,   is_category int(11) default '0',   proj_id int(11) default NULL,   PRIMARY KEY  (id))";
    my $sqlCAT = "CREATE TABLE catEPIC (   id int(11) NOT NULL auto_increment,   name varchar(50) default NULL,   abrv varchar(20) default NULL,   heading_id int(11) default NULL,   PRIMARY KEY  (id))";
    my $sqlDS = "CREATE TABLE dsEPIC (   id int(11) NOT NULL auto_increment,   data_set varchar(200) default NULL,   link varchar(200) default NULL,   date date default NULL,   document varchar(200) default NULL,   category int(11) NOT NULL default '0',   in_progress int(11) default '0',   map_url varchar(200) default NULL,   table_url varchar(200) default NULL,   instrument_url varchar(200) default NULL,   expected_date date default NULL,   PRIMARY KEY  (id))";

    $CONN->prepare($sqlHEAD)->execute() or die($DBI::errstr);
    $CONN->prepare($sqlCAT)->execute() or die($DBI::errstr);
    $CONN->prepare($sqlDS)->execute() or die($DBI::errstr);
}

sub formatDate {
    my ($date) = @_;
    $date =~ s/\<img[^\>]+\>//;
    my ($day,$month,$year) = split(' ',$date);

    if ($month =~ /jan/i) { $month = 1; }
    elsif ($month =~ /feb/i) { $month = 2; }
    elsif ($month =~ /mar/i) { $month = 3; }
    elsif ($month =~ /apr/i) { $month = 4; }
    elsif ($month =~ /may/i) { $month = 5; }
    elsif ($month =~ /june?/i) { $month = 6; }
    elsif ($month =~ /july?/i) { $month = 7; }
    elsif ($month =~ /aug/i) { $month = 8; }
    elsif ($month =~ /sep/i) { $month = 9; }
    elsif ($month =~ /oct/i) { $month = 10; }
    elsif ($month =~ /nov/i) { $month = 11; }
    elsif ($month =~ /dec/i) { $month = 12; }
    else { die("Unknown month: $month\n"); }

    return sprintf("%04d-%02d-%02d",$year,$month,$day);
}

sub insertDatasets {
    foreach my $cat (keys(%{$CAT_MAP})) {
	foreach my $dataset (@{ $CAT_MAP->{$cat}->{"datasets"}}) {
	    my $sql = sprintf("INSERT INTO ds%s(data_set,link,date,document,category) values(?,?,?,?,?)",$PROJECT);
	    $CONN->prepare($sql)->execute($dataset->{"data_set"},$dataset->{"link"},
					  defined($dataset->{"date"}) ? $dataset->{"date"} : "0000-00-00",
					  $dataset->{"document"},$CAT_MAP->{$cat}->{"id"});
	}
	foreach my $links (@{ $CAT_MAP->{$cat}->{"links"}}) {
	    my $sql = sprintf("INSERT INTO ds%s(data_set,link,category,date) values(?,?,?,?)",$PROJECT);
	    $CONN->prepare($sql)->execute(split("<-->",$links),$CAT_MAP->{$cat}->{"id"},"0000-00-00");
	}
    }
}

sub insertCats {
    foreach my $head (keys(%{$HEAD_MAP})) {
	my $sql = sprintf("INSERT INTO head%s(name,abrv,is_category) values(?,?,?)",$PROJECT);
	$CONN->prepare($sql)->execute($head,$HEAD_MAP->{$head}->{"abrev"},
				      $HEAD_MAP->{$head}->{"isCat"}) or die($DBI::errstr);
	$HEAD_MAP->{$head}->{"id"} = $CONN->last_insert_id(undef(),undef(),"head$PROJECT","id");
    }

    foreach my $cat (keys(%{$CAT_MAP})) {
	my $sql = sprintf("INSERT INTO cat%s(name,abrv,heading_id) values(?,?,?)",$PROJECT);
	$CONN->prepare($sql)->execute($CAT_MAP->{$cat}->{"title"},$cat,
				      $CAT_MAP->{$cat}->{"head"}->{"id"});
	$CAT_MAP->{$cat}->{"id"} = $CONN->last_insert_id(undef(),undef(),"cat$PROJECT","id");
    }
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
    open(my $HTML,$ML_FILE) or die("Cannot open $ML_FILE\n");
    
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
    my $currentCategory;
    while (defined(my $line = <$HTML>)) {
	die($line) if (defined($currentCategory) && $line =~ /\<td\>/i);
	next if ($line !~ /^\s*\<tr\>\s*$/i);

	my @colList = ();
    	$line = <$HTML>;
	while ($line !~ /^\s*\<\/tr\>\s*$/i) { chomp($line),push(@colList,$line); $line = <$HTML>; }

	next if (isSpacerRow(@colList));

	if ($colList[0] =~ /type\=\"cat\"/) {
	    $colList[0] = cleanHTML($colList[0]);

	    $colList[0] =~ /^\<a.+name=\"([^\"]+)\"\>\<\/a\>(.*)/;
	    $currentCategory = $1;
	    $colList[0] = trim($2);

	    while ($colList[0] =~ /\<a/) {
		$colList[0] =~ s/\<a\s+href\=\"([^\"]+)\"\s*\>([^\<]+)\<\/a\>//;
		$count++;
		push(@{$CAT_MAP->{$currentCategory}->{"links"}},
		     sprintf("%s<-->%s",trim($2),trim($1)));
	    }

	    next;
	} elsif (!defined($currentCategory)) {
	    die("No category defined for: $colList[0]");
	} else {
	    $count++;
	    my $dataset = {};
	    $colList[0] = cleanHTML($colList[0]);

	    if ($colList[0] =~ /\<a\s+href\s*\=\"([^\"]+)\"\s*\>([^\<]+)\<\/a>/i) {
		$dataset->{"data_set"} = trim($2);
		$dataset->{"link"} = trim($1);
	    } else {
		$dataset->{"data_set"} = trim($colList[0]);
		$dataset->{"link"} = "";
	    }

	    $dataset->{"date"} = cleanHTML($colList[1]) eq "" ? "0000-00-00" :
		formatDate(cleanHTML($colList[1]));

	    $colList[2] = cleanHTML($colList[2]);
	    if ($colList[2] =~ /\<a\s+href\s*\=\"([^\"]+)\"\s*\>documentation/i) {
		$dataset->{"document"} = trim($1);
	    } else {
		$dataset->{"document"} = "";
	    }
	    
	    push(@{$CAT_MAP->{$currentCategory}->{"datasets"}},$dataset);
	}

    }

    printf("%s\n",$count);
}

sub setupCategories {
    $HEAD_MAP->{"Aircraft Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Aircraft Data"}->{"abrev"} = "Aircraft";
    $CAT_MAP->{"C-130"}->{"head"} = $HEAD_MAP->{"Aircraft Data"};
    $CAT_MAP->{"C-130"}->{"abrev"} = "C-130";
    $CAT_MAP->{"C-130"}->{"title"} = "NSF C-130";
    $CAT_MAP->{"P-3"}->{"head"} = $HEAD_MAP->{"Aircraft Data"};
    $CAT_MAP->{"P-3"}->{"abrev"} = "P-3";
    $CAT_MAP->{"P-3"}->{"title"} = "NOAA P-3";

    $HEAD_MAP->{"Buoy Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"Buoy Data"}->{"abrev"} = "buoy";
    $CAT_MAP->{"buoy"}->{"head"} = $HEAD_MAP->{"Buoy Data"};
    $CAT_MAP->{"buoy"}->{"abrev"} = "buoy";
    $CAT_MAP->{"buoy"}->{"title"} = "Buoy Data";

    $HEAD_MAP->{"GTS Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"GTS Data"}->{"abrev"} = "gts";
    $CAT_MAP->{"GTS"}->{"head"} = $HEAD_MAP->{"GTS Data"};
    $CAT_MAP->{"GTS"}->{"abrev"} = "gts";
    $CAT_MAP->{"GTS"}->{"title"} = "GTS Data";

    $HEAD_MAP->{"Model Output"}->{"isCat"} = 1;
    $HEAD_MAP->{"Model Output"}->{"abrev"} = "model";
    $CAT_MAP->{"Model"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"Model"}->{"abrev"} = "model";
    $CAT_MAP->{"Model"}->{"title"} = "Model Output";

    $HEAD_MAP->{"Satellite Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Satellite Data"}->{"abrev"} = "Satellite";
    $CAT_MAP->{"Satellite"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"Satellite"}->{"title"} = "Satellite Data";
    $CAT_MAP->{"GOES"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"GOES"}->{"title"} = "GOES";
    $CAT_MAP->{"POES"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"POES"}->{"title"} = "POES";
    $CAT_MAP->{"DMSP"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"DMSP"}->{"title"} = "DMSP";
    $CAT_MAP->{"QuikSCAT"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"QuikSCAT"}->{"title"} = "QuikSCAT";
    $CAT_MAP->{"GlobalIR"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"GlobalIR"}->{"title"} = "Global IR";
    $CAT_MAP->{"SeaWIFS"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"SeaWIFS"}->{"title"} = "SeaWIFS";
    $CAT_MAP->{"TERRA"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"TERRA"}->{"title"} = "TERRA";
    $CAT_MAP->{"TRMM"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"TRMM"}->{"title"} = "TRMM";

    $HEAD_MAP->{"Ship Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Ship Data"}->{"abrev"} = "Ship";
    $CAT_MAP->{"Ship_New_Horizon"}->{"head"} = $HEAD_MAP->{"Ship Data"};
    $CAT_MAP->{"Ship_New_Horizon"}->{"abrev"} = "horizon";
    $CAT_MAP->{"Ship_New_Horizon"}->{"title"} = "New Horizon";
    $CAT_MAP->{"Ship_Ron_Brown"}->{"head"} = $HEAD_MAP->{"Ship Data"};
    $CAT_MAP->{"Ship_Ron_Brown"}->{"abrev"} = "brown";
    $CAT_MAP->{"Ship_Ron_Brown"}->{"title"} = "Ron Brown";
    $CAT_MAP->{"Ship_Roger_Revelle"}->{"head"} = $HEAD_MAP->{"Ship Data"};
    $CAT_MAP->{"Ship_Roger_Revelle"}->{"abrev"} = "revelle";
    $CAT_MAP->{"Ship_Roger_Revelle"}->{"title"} = "Roger Revelle";

    $HEAD_MAP->{"Surface Based Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"Surface Based Data"}->{"abrev"} = "surface";
    $CAT_MAP->{"Surface"}->{"head"} = $HEAD_MAP->{"Surface Based Data"};
    $CAT_MAP->{"Surface"}->{"abrev"} = "surface";
    $CAT_MAP->{"Surface"}->{"title"} = "Surface Based Data";


    $HEAD_MAP->{"Upper Air Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"Upper Air Data"}->{"abrev"} = "upper_air";
    $CAT_MAP->{"upper_air"}->{"head"} = $HEAD_MAP->{"Upper Air Data"};
    $CAT_MAP->{"upper_air"}->{"abrev"} = "upper_air";
    $CAT_MAP->{"upper_air"}->{"title"} = "Upper Air Data";
}

sub trim {
    my $line = shift;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
