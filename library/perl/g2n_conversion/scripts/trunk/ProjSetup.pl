#! /usr/bin/perl -w

use strict;
use lib "/h/eol/stroble/scripts/MySQL/lib";
use MySqlDatabase;
my $msg = "";

#my $database = MySqlDatabase->new("stroble","tempass");
my $database = MySqlDatabase->new("zediupdate","change-456");
#database->setHost("localhost");
$database->connect();

if ($#ARGV != 0)
{
	print "Only one argument is accepted\n";
	exit(1);
}
open FH, $ARGV[0];
while (<FH>) {
	if ($_ =~ /^#/) { next; }	
       if ($_ =~ /add_project=(.*)/) {
	   	unless (-e $1) { die "$1 does not exist!\n"; }
		$msg = addproject($database, $1);
		if ($msg ne "") {
			print "Message: $msg\n";
			$database->rollback();
			$database->disconnect();
			exit(1);
		}
	}
	elsif ($_ =~ /add_dataset=(.*)/) {
	   	unless (-e $1) { die "$1 does not exist!\n"; }
		$msg = adddataset($database, $1);
		if ($msg ne "") {
			print "Message: $msg\n";
			$database->rollback();
			$database->disconnect();
			exit(1);
		}
	}
}
close FH;

if ($msg eq "") { 
    printf("\n\nTo commit changes, press Enter.\n");
    printf("To cancel changes, enter any value and press Enter.\n\n");
    printf(">> ");
    my $result = <STDIN>;
    
    if ($result =~ /^\s*$/) {
	$database->commit() 
    }
    else { 
	print "User Selected to Cancel!\n";
	$database->rollback();
    }
}
else {print "Message: $msg\n"; $database->rollback() }
$database->disconnect();

sub GetAttr {
	my $attr = shift;
	my $filename = shift;
	my $temp = "";
	
	#print "$filename\n";
	open TFH, $filename;
	#print "$filename\n";
	while (<TFH>) {
		if ($_ =~ /(.*)=(.*)/) {
			if ($1 eq $attr) { 
			    $temp = $2;
			    close TFH;
			    if ($temp =~ /->(.*)/)
			    {
				my @split1 = split(/:/, $1);
				my $subfilename = $split1[0];
				my $subattr = $attr;
				if ($#split1 == 1)
				{
				    $subattr = $split1[1];
				}
				return GetAttr($subattr, $subfilename);
				last;
			    }
			    else {
				return $temp;
			        last;	
			    }
			}
		}	
	}
	close TFH;
	return "";
}

sub ProcAttr
{
    my $attr = shift;
    my $value = shift;

    my @split = split(/;/, $value);
    my $return = "";

    foreach my $command (@split)
    {
	if ($command =~ /^->(.*)$/)
	{
		my @arg = split(/:/, $1);
		if ($#arg == 1)
		{
		    $return .= GetAttr($arg[1], $arg[0]);
		}
		else
		{
		    $return .= GetAttr($attr, $arg[0]);
		}
	}
	elsif ($command =~ /^\/\.(.+)->(.+)$/)
	{
	    #Putting $1 and $2 in $s and $r is neccessary for reasons beyond my understanding 
	    # s/$1/$2/g gets a uninitialized error on $2..
	    my $s = $1;
 	    my $r = $2;
	    $return =~ s/$s/$r/g;
	}
	elsif ($command =~ /^\+(.*)$/)
	{
		$return += $1;
	}
	else
	{
		$return .= $command;
	}
    }

   return $return;
}

sub addproject {
	my $msg = "";
	my $db = shift;
	my $filename = shift;
	my @ATTR;
	my @VAL;

	open PFH, $filename;
	while (<PFH>) {
		my $line = $_;
		if ($line =~ /^#/) { next; }
		if ($line =~ /(.*)=(.*)/) {
			push @ATTR, $1;
			push @VAL, ProcAttr($1, $2);
		}
	}
	close PFH;

	#Insert project
	my $table = "project";
	my @attr = ();
	push @attr, "project_id";
	push @attr, "full_name";
	push @attr, "description";
	push @attr, "begin_date";
	push @attr, "end_date";
	push @attr, "minlat";
	push @attr, "minlon";
	push @attr, "maxlat";
	push @attr, "maxlon";
	push @attr, "internal_contact_id";
	push @attr, "parent_project_id";
	push @attr, "hide";
	my @attribute = ();
	my @value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	print "\nNew Project: $value[1] ($value[0])\n Description: $value[2]\n Begin: $value[3] End: $value[4]\n Lat: $value[5]  $value[7]\n Lon: $value[6]  $value[8]\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }
	
	#Insert project_prefix
	$table = "dataset_prefix_project";
	@attr = ();
	push @attr, "dataset_id_prefix";
	push @attr, "project_id";
	@attribute = ();
	@value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	print " Prefix: $value[0]\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }

	#TODO:I should be able to add/insert fully specified xlinks.

	#Add xlinks to project where
	$table = "project_xlinks";
	@attr = ();
	push @attr, "project_id";
	my @attributetemp = ();
	my @valuetemp = ();
	my $where;
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attributetemp, $ATTR[$i]; push @valuetemp, $VAL[$i]; }
			if ($ATTR[$i] eq "add_xlink_where")
			{
				$where = $VAL[$i];
			}
		}
	}

	#print "Adding XLINKS WHERE $where\n";
	if (defined $where && $where ne "") {
	    ($msg, my @data) = $database->selectAll("xlink","xlink_id",$where);
	    if ($msg ne "") { return $msg; }
	    foreach (@data) {
		    my @attribute = @attributetemp;
		    my @value = @valuetemp;
		    push @attribute, "xlink_id";
		    push @value, @{$_}[0];
		    print " xlink: @{$_}[0]\n";
		    #print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
		    $msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	    }
	}
	
	return $msg;
}

sub adddataset {
	my $db = shift;
	my $filename = shift;
	
	my @ATTR;
	my @VAL;
	open DFH, $filename;
	while (<DFH>) {
		my $line = $_;
		if ($line =~ /^#/) { next; }
		if ($line =~ /(.*)=(.*)/) {
			push @ATTR, $1;
			push @VAL, ProcAttr($1, $2);
		}
	}
	close DFH;

	#TODO

	#Add dataset
	#Insert dataset
	my $table = "dataset";
	my @attr = ();
	push @attr, "dataset_id";
	push @attr, "name";
	push @attr, "description";
	push @attr, "begin_date";
	push @attr, "end_date";
	push @attr, "minlat";
	push @attr, "minlon";
	push @attr, "maxlat";
	push @attr, "maxlon";
	push @attr, "frequency_id";
	push @attr, "spatial_type";
	push @attr, "displayed_contact_id";
	push @attr, "source_contact_id";
	push @attr, "internal_contact_id";
	push @attr, "auth_reqd";
	push @attr, "eula_reqd";
	push @attr, "onlineorderable";
	push @attr, "offlineorderable";
	push @attr, "browseable";
	push @attr, "dodsable";
	push @attr, "is_eol_data";
	push @attr, "hide";
	push @attr, "row_revise_contact_id";
	my @attribute = ();
	my @value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	print "\nNew Dataset: $value[0]\n Name:$value[1]\n Description:$value[2]\n Begin:$value[3] End:$value[4]\n Lat:$value[5]  $value[7]\n Lon:$value[6]  $value[8]\n";
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }

	#Add Project Ref
	$table = "dataset_project";
	@attr = ();
	push @attr, "dataset_id";
	push @attr, "project_id";
	@attribute = ();
	@value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	print " Project Ref:$value[1]\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }

	#Add Dataset Ref (If Specified)
	$table = "dataset_references";
	@attr = ();
	push @attr, "dataset_id";
	push @attr, "referenced_dataset_id";
	push @attr, "reference_type";
	@attribute = ();
	@value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	if (defined($value[1]) && $value[1] ne "")
	{
	    print " $value[2] $value[1])\n";
	    #print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	    $msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	    if ($msg ne "") {print "Rolling Back Database!\n"; $database->rollback(); return $msg; }
    	}
	
	#Add Categorys
	#print "Setting Dataset Category\n";
	$table = "dataset_category";
	@attr = ();
	push @attr, "dataset_id";
	push @attr, "category_id";
	@attribute = ();
	@value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }

	#Add Category WHERE

	#Add Platforms
	#print "Setting Dataset Platform\n";
	$table = "dataset_platform";
	@attr = ();
	push @attr, "dataset_id";
	push @attr, "platform_id";
	@attribute = ();
	@value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	print " Platform ID:$value[1]\n";
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }
	
	#Add Platform WHERE

	#Add Docs

	#Add Xlinks

	#Add Xlinks WHERE

	#Options (Subset/Browse)
	#print "Setting Dataset Options\n";
	$table = "codiac_dataset_options";
	@attr = ();
	push @attr, "dataset_id";
	push @attr, "x_subset";
	push @attr, "y_subset";
	push @attr, "z_subset";
	push @attr, "t_subset";
	push @attr, "p_subset";
	push @attr, "stnid_subset";
	push @attr, "event_subset";
	push @attr, "file_subset";
	push @attr, "order_allow_compress";
	push @attr, "order_max_size_gb";
	push @attr, "order_directory_levels";
	push @attr, "order_select_prog";
	push @attr, "order_merge_prog";
	push @attr, "order_parm_list_prog";
	push @attr, "order_stnid_list_prog";
	push @attr, "order_fgr_prog";
	push @attr, "browse_stn_scan_prog";
	push @attr, "browse_extract_prog";
	push @attr, "browse_param_prog";
	@attribute = ();
	@value = ();
	foreach (@attr) {
		$a = $_;
		for (my $i = 0; $i <= $#ATTR; $i++) {
			if ($a eq $ATTR[$i]) { push @attribute, $ATTR[$i]; push @value, $VAL[$i]; }
		}
	}
	#print "INSERT INTO $table(" . join(',', @attribute) . ") VALUES(" . join(',', @value) . ")\n";
	$msg .= $database->insert($table,join(',', @attribute),join(',', @value));
	if ($msg ne "") { return $msg; }

	#Set Format

	#Set Plot
	
	return "";
}
