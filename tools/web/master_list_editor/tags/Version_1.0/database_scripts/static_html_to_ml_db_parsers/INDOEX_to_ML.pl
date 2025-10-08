#! /usr/bin/perl -w

use strict;
use DBI;

my $TRUE = 1;
my $FALSE = 0;

my $HEAD_MAP = {};
my $CAT_MAP = {};

my $PROJECT = "INDOEX";
my $ML_FILE = "/web/docs/indoex/dm/data_list.html";

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

    $line =~ s/\<img[^\>]+\>//gi;

    $line =~ s/\&nbsp;//g;

    $line =~ s/target\=[\"\']?_top[\"\']?//gi;

    $line =~ s/\<!\-\-.+\-\-\>//gi;

    $line =~ s/^\s+//;
    $line =~ s/\s+$//;

    return $line;
}

sub createDatabaseTables {
    my $sqlHEAD = "CREATE TABLE head$PROJECT (   id int(11) NOT NULL auto_increment,   name varchar(50) default NULL,   abrv varchar(20) default NULL,   is_category int(11) default '0',   proj_id int(11) default NULL,   PRIMARY KEY  (id))";
    my $sqlCAT = "CREATE TABLE cat$PROJECT (   id int(11) NOT NULL auto_increment,   name varchar(50) default NULL,   abrv varchar(20) default NULL,   heading_id int(11) default NULL,   PRIMARY KEY  (id))";
    my $sqlDS = "CREATE TABLE ds$PROJECT (   id int(11) NOT NULL auto_increment,   data_set varchar(200) default NULL,   link varchar(200) default NULL,   date date default NULL,   document varchar(200) default NULL,   category int(11) NOT NULL default '0',   in_progress int(11) default '0',   map_url varchar(200) default NULL,   table_url varchar(200) default NULL,   instrument_url varchar(200) default NULL,   expected_date date default NULL,   PRIMARY KEY  (id))";

    $CONN->prepare($sqlHEAD)->execute() or die($DBI::errstr);
    $CONN->prepare($sqlCAT)->execute() or die($DBI::errstr);
    $CONN->prepare($sqlDS)->execute() or die($DBI::errstr);
}

sub formatDate {
    my ($date) = @_;
    $date =~ s/\<img[^\>]+\>//;
    $date =~ s/added//i;
    $date =~ s/updated//i;
    my ($day,$month,$year) = split(' ',trim($date));

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

	    $CONN->prepare($sql)->execute($dataset->{"title"},$dataset->{"link"},
					  defined($dataset->{"date"}) ? $dataset->{"date"} : "0000-00-00",
					  defined($dataset->{"document"}) ? $dataset->{"document"} : "",$CAT_MAP->{$cat}->{"id"});
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
	$inTable = ($line =~ /\<dl\>/i) ? $TRUE : $FALSE;
    }

    my $count = 0;
    my $currentCategory;
    my $line;
    while (defined($line = <$HTML>) && $line !~ /^\s*\<\/dl\>\s*$/ ) {
	next if ($line =~ /^\s*$/ || $line =~ /^\s*\<p\>\s*$/ || $line =~ /^\s*\<!\-\-/);

	if ($line !~ /\<a name/ && $line !~ /\<dd\>/) {
	    die("Unknown line: $line\n");
	}
	
	if (trim($line) =~ /\<a name=\"(.+)\"\>\<\/a\>/) {
	    $currentCategory = $1;
	} elsif (trim($line) =~ /\<dd\>/) {
	    $line = cleanHTML(substr($line,4));
	    my $dataset = {};

	    if ($line =~ /^\<a href=\"([^\"]+)\"\s*\>(.+)\<\/a\>([^\<]+)?\<a href=\"([^\"]+)\"\s*\>([^\<]+)\<\/a\>.*$/) {
#		printf("%s\n",$line);
#		printf("\t%s -> %s -> %s\n",$1,trim($2),defined($3) ? trim($3) : "");
#		printf("\t\t%s -> %s\n",$4,$5);

		my $date = $3;
		$dataset->{"title"} = trim($2);
		$dataset->{"link"} = trim($1);
		$dataset->{"document"} = trim($4);
		$dataset->{"date"} = formatDate($date) if (defined($date) && $date !~ /^\s*$/);
		

#		die();
	    } elsif ($line =~ /^\<a href=\"([^\"]+)\"\s*\>(.+)\<\/a\>([^\<]+)?.*$/) {
#		printf("%s\n",$line);
#		printf("\t%s -> %s -> %s\n",$1,trim($2),defined($3) ? trim($3) : "");
		
		$dataset->{"title"} = trim($2);
		$dataset->{"link"} = trim($1);
		my $date = $3;
		$dataset->{"date"} = formatDate($date) if (defined($date) && $date !~ /^\s*$/);
	    } else {
#		printf("%s\n",$line);
		
		$dataset->{"title"} = $line;
	    }

	    $dataset->{"title"} =~ s/^INDOEX:\s*//;
	    
	    $dataset->{"title"} =~ s/^Aircraft\s+//;
	    $dataset->{"title"} =~ s/^C\-130/NSF C\-130/;
	    $dataset->{"title"} =~ s/^Model\s+//;
	    $dataset->{"title"} =~ s/^Satellite\s+//;
	    $dataset->{"title"} =~ s/^Ship\s+//;
	    $dataset->{"title"} =~ s/^Upper Air\s+//;

	    $dataset->{"title"} =~ s/\(([^\(\)]+)\)$/\[$1\]/;

#	    printf("%s\n",$dataset->{"title"});

	    push(@{$CAT_MAP->{$currentCategory}->{"datasets"}},$dataset);

	    $count++;
	}
	
    }

    printf("%s\n",$count);
}

sub setupCategories {
    $HEAD_MAP->{"Aircraft Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Aircraft Data"}->{"abrev"} = "Aircraft";
    $CAT_MAP->{"C130"}->{"head"} = $HEAD_MAP->{"Aircraft Data"};
    $CAT_MAP->{"C130"}->{"title"} = "NSF C-130";
    $CAT_MAP->{"citation"}->{"head"} = $HEAD_MAP->{"Aircraft Data"};
    $CAT_MAP->{"citation"}->{"title"} = "Citation";
    $CAT_MAP->{"mystere"}->{"head"} = $HEAD_MAP->{"Aircraft Data"};
    $CAT_MAP->{"mystere"}->{"title"} = "Mystere";

    $HEAD_MAP->{"Diego Garcia Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"Diego Garcia Data"}->{"abrev"} = "Diego";
    $CAT_MAP->{"diego"}->{"head"} = $HEAD_MAP->{"Diego Garcia Data"};
    $CAT_MAP->{"diego"}->{"title"} = "Diego Garcia Data";

    $HEAD_MAP->{"GTS Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"GTS Data"}->{"abrev"} = "GTS";
    $CAT_MAP->{"upperair"}->{"head"} = $HEAD_MAP->{"GTS Data"};
    $CAT_MAP->{"upperair"}->{"title"} = "Upper Air Data";
    $CAT_MAP->{"surface"}->{"head"} = $HEAD_MAP->{"GTS Data"};
    $CAT_MAP->{"surface"}->{"title"} = "Surface Data";

    $HEAD_MAP->{"India Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"India Data"}->{"abrev"} = "India";
    $CAT_MAP->{"bhubneshwar"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"bhubneshwar"}->{"title"} = "Bhubneshwar Data";
    $CAT_MAP->{"delhi"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"delhi"}->{"title"} = "Delhi Data";
    $CAT_MAP->{"dharward"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"dharward"}->{"title"} = "Dharward Data";
    $CAT_MAP->{"goa"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"goa"}->{"title"} = "Goa Data";
    $CAT_MAP->{"jodhpur"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"jodhpur"}->{"title"} = "Jodhpur Data";
    $CAT_MAP->{"kodaikanal"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"kodaikanal"}->{"title"} = "Kodaikanal Data";
    $CAT_MAP->{"minicoy"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"minicoy"}->{"title"} = "Minicoy Data";
    $CAT_MAP->{"mtabu"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"mtabu"}->{"title"} = "Mtabu Data";
    $CAT_MAP->{"mumbai"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"mumbai"}->{"title"} = "Mumbai Data";
    $CAT_MAP->{"mysore"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"mysore"}->{"title"} = "Mysore Data";
    $CAT_MAP->{"nagpur"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"nagpur"}->{"title"} = "Nagpur Data";
    $CAT_MAP->{"pune"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"pune"}->{"title"} = "Pune Data";
    $CAT_MAP->{"srinagar"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"srinagar"}->{"title"} = "Srinagar Data";
    $CAT_MAP->{"trivandrum"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"trivandrum"}->{"title"} = "Trivandrum Data";
    $CAT_MAP->{"varanasi"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"varanasi"}->{"title"} = "Varanasi Data";
    $CAT_MAP->{"vizag"}->{"head"} = $HEAD_MAP->{"India Data"};
    $CAT_MAP->{"vizag"}->{"title"} = "Vizag Data";

    $HEAD_MAP->{"Maldives Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Maldives Data"}->{"abrev"} = "Maldives";
    $CAT_MAP->{"hulule"}->{"head"} = $HEAD_MAP->{"Maldives Data"};
    $CAT_MAP->{"hulule"}->{"title"} = "Hulule Data";
    $CAT_MAP->{"kco"}->{"head"} = $HEAD_MAP->{"Maldives Data"};
    $CAT_MAP->{"kco"}->{"title"} = "KCO Data";

    $HEAD_MAP->{"Mauritius Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"Mauritius Data"}->{"abrev"} = "Mauritius";
    $CAT_MAP->{"mauritius"}->{"head"} = $HEAD_MAP->{"Mauritius Data"};
    $CAT_MAP->{"mauritius"}->{"title"} = "Mauritius Data";

    $HEAD_MAP->{"Model Output"}->{"isCat"} = 0;
    $HEAD_MAP->{"Model Output"}->{"abrev"} = "Model";
    $CAT_MAP->{"avn"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"avn"}->{"title"} = "AVN";
    $CAT_MAP->{"ecmwf"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"ecmwf"}->{"title"} = "ECMWF";
    $CAT_MAP->{"fsu"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"fsu"}->{"title"} = "FSU";
    $CAT_MAP->{"match"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"match"}->{"title"} = "MATCH";
    $CAT_MAP->{"nogaps"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"nogaps"}->{"title"} = "NOGAPS";
    $CAT_MAP->{"plan"}->{"head"} = $HEAD_MAP->{"Model Output"};
    $CAT_MAP->{"plan"}->{"title"} = "Planning Mtg Forecast Charts";


    $HEAD_MAP->{"Satellite Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Satellite Data"}->{"abrev"} = "Satellite";
    $CAT_MAP->{"avhrr"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"avhrr"}->{"title"} = "AVHRR";
    $CAT_MAP->{"dmsp"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"dmsp"}->{"title"} = "DMSP";
    $CAT_MAP->{"ers"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"ers"}->{"title"} = "ERS";
    $CAT_MAP->{"insat"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"insat"}->{"title"} = "INSAT";
    $CAT_MAP->{"meteosat5"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"meteosat5"}->{"title"} = "Meteosat-5";
    $CAT_MAP->{"seawifs"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"seawifs"}->{"title"} = "SeaWIFS";
    $CAT_MAP->{"trmm"}->{"head"} = $HEAD_MAP->{"Satellite Data"};
    $CAT_MAP->{"trmm"}->{"title"} = "TRMM";

    $HEAD_MAP->{"Ship Data"}->{"isCat"} = 0;
    $HEAD_MAP->{"Ship Data"}->{"abrev"} = "Ship";
    $CAT_MAP->{"ronbrown"}->{"head"} = $HEAD_MAP->{"Ship Data"};
    $CAT_MAP->{"ronbrown"}->{"title"} = "Ron Brown";
    $CAT_MAP->{"sagar"}->{"head"} = $HEAD_MAP->{"Ship Data"};
    $CAT_MAP->{"sagar"}->{"title"} = "Sagar Kanya";

    $HEAD_MAP->{"Constant Level Balloon Data"}->{"isCat"} = 1;
    $HEAD_MAP->{"Constant Level Balloon Data"}->{"abrev"} = "constant";
    $CAT_MAP->{"constant"}->{"head"} = $HEAD_MAP->{"Constant Level Balloon Data"};
    $CAT_MAP->{"constant"}->{"title"} = "Constant Level Balloon Data";
}

sub trim {
    my $line = shift;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}
