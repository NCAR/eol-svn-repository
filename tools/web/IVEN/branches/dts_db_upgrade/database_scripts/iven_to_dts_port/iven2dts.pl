#! /usr/bin/perl -w


package Iven2DTS;
use strict;
use DBI;

&main;

sub main {
    my $self = Iven2DTS->new();
    $self->setup();

#    $self->project() or $self->teardown();
    $self->project();
    $self->users() or $self->teardown();
#    $self->products() or $self->teardown();
    $self->products();
#    $self->datasets() or $self->teardown();
    $self->datasets();

    $self->{"DTS"}->commit();
    $self->teardown();
}

sub create_status_map {
    my ($self) = @_;
    my $map = {};
    
    my $sth = $self->{"DTS"}->prepare("INSERT INTO dts.status(status_name,status_style,is_resolved) VALUES(?,?,?)");
    my $data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='TBD'");
    if (@$data == 0) {
	$sth->execute("TBD","tbd",0);
	$map->{"tbd"} = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$map->{"tbd"} = $data->[0]->[0];
    }
    $data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='In Progress'");
    if (@$data == 0) {
	$sth->execute("In Progress","inprogress",0);
	$map->{"in_progress"} = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$map->{"in_progress"} = $data->[0]->[0];
    }
    $data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='Done'");
    if (@$data == 0) {
	$sth->execute("Done","done",1);
	$map->{"done"} = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$map->{"done"} = $data->[0]->[0];
    }
    $data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='Skip'");
    if (@$data == 0) {
	$sth->execute("Skip","skip",1);
	$map->{"dnd"} = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$map->{"dnd"} = $data->[0]->[0];
    }
    $data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='N/A'");
    if (@$data == 0) {
	$sth->execute("N/A","na",1);
	$map->{"na"} = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$map->{"na"} = $data->[0]->[0];
    }
    
    return $map;
}

sub current_date {
    my @today = localtime();
    return sprintf("%04d-%02d-%02d %02d-%02d-%02d",$today[5]+1900,$today[4]+1,
		   $today[3],$today[2],$today[1],$today[0]);
}

sub dataset_dts_dataset {
    my ($self,$dataset_id) = @_;
    my $success_flag = 1;

    my $data = $self->{"DTS"}->selectall_arrayref("SELECT dataset_id FROM dts.dataset WHERE dataset_id='$dataset_id'");
    if (@$data == 0) {
	my $stmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset(dataset_id,dataset_type,entered_contact_id,entered_date,row_revise_contact_id) VALUES(?,?,?,?,?)");
	$stmt->execute($dataset_id,"Processed",1,current_date(),1);
    } else {
	my $stmt = $self->{"DTS"}->prepare("UPDATE dts.dataset SET dataset_type=?,row_revise_contact_id=1 WHERE dataset_id=?");
	$stmt->execute("Processed",$dataset_id);
    }

    return $success_flag;
}

sub dataset_dts_process {
    my ($self,$data) = @_;
    my $success_flag = 1;

    my $status_map = $self->create_status_map();

#    my $composite_id = $self->{"composites"}->{$data->[0]}->{$data->[1]};

    my $test = $self->{"DTS"}->selectall_arrayref("SELECT dataset_id FROM dts.dataset_process WHERE dataset_id='".$data->[3]."'");
    if (@$test == 0) {
	my $stmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset_process(dataset_id,process_contact_id,process_status_id,process_work_dir,process_final_dir,process_repos_dir,process_howto_link,process_stn_list_link,process_plot_dir,process_on_hold,row_revise_contact_id) VALUES(?,?,?,?,?,?,?,?,?,?,?)");
	$stmt->execute($data->[3],$self->{"users"}->{lc($data->[5])},$status_map->{$data->[13]},
		       "",defined($data->[7]) ? $data->[7] : "",defined($data->[9]) ? $data->[9]."**" : "",defined($data->[16]) ? $data->[16] : "",
		       defined($data->[8]) ? $data->[8] : "",defined($data->[10]) ? $data->[10] : "",
		       defined($data->[15]) ? $data->[15] : 0,1);
    } else {
	my $stmt = $self->{"DTS"}->prepare("UPDATE dts.dataset_process SET process_contact_id=?,process_status_id=?,process_work_dir=?,process_final_dir=?,process_repos_dir=?,process_howto_link=?,process_stn_list_link=?,process_plot_dir=?,process_on_hold=?,row_revise_contact_id=? WHERE dataset_id=?");
	$stmt->execute($self->{"users"}->{lc($data->[5])},$status_map->{$data->[13]},
		       "",defined($data->[7]) ? $data->[7] : "","",defined($data->[16]) ? $data->[16] : "",
		       defined($data->[8]) ? $data->[8] : "",defined($data->[10]) ? $data->[10] : "",
		       defined($data->[15]) ? $data->[15] : 0,1,$data->[3]);
    }

    my $note = sprintf("%s %s",defined($data->[12]) ? $data->[12] : "",defined($data->[11]) ? $data->[11] : "");
    if (defined($data->[9])) { 
	$note = "<p>** The value for the Repos Dir field comes from the software field in the original IVEN database.</p>\n\n".$note; 
    }
    $note = trim($note);

    my $dataset_id = $data->[3];
    if ($note ne "") {
	my $noteStmt = $self->{"DTS"}->prepare("SELECT note_id FROM dts.dataset_note WHERE dataset_id=?");
	$noteStmt->execute($dataset_id);

	if ($noteStmt->rows() != 0) {
	    my $note_id = ($noteStmt->fetchrow_array())[0];
	    my $dts_note = $self->{"DTS"}->selectall_arrayref("SELECT note_text FROM dts.note WHERE note_id=$note_id")->[0]->[0];
     	    $dts_note = trim($dts_note."\n\n".$note);
	    my $pstmt = $self->{"DTS"}->prepare("UPDATE dts.note SET note_text=?,row_revise_contact_id=1 WHERE note_id=?");
	    $pstmt->execute($dts_note,$note_id);
	} else {
	    my $pstmt = $self->{"DTS"}->prepare("INSERT INTO dts.note(author_id,note_text,entered_date,row_revise_contact_id) VALUES(?,?,?,?)");
	    $pstmt->execute(1,$note,current_date(),1);
	    my $note_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.note","note_id");
	    $pstmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset_note(dataset_id,note_id) VALUE(?,?)");
	    $pstmt->execute($dataset_id,$note_id);
	}
    }

    if (defined($self->{"composites"}->{$data->[0]}->{$data->[1]})) {
	my $pstmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset_source_dataset(dataset_id,source_dataset_id,excluded_flag) VALUES(?,?,?)");
	$pstmt->execute($self->{"composites"}->{$data->[0]}->{$data->[1]},$dataset_id,defined($data->[14]) ? $data->[14] : 0);
    } elsif (defined($self->{"groups"}->{$data->[0]}->{$data->[1]})) {
	my $gstmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset_in_group(group_id,dataset_id,excluded_flag) VALUES(?,?,?)");
	$gstmt->execute($self->{"groups"}->{$data->[0]}->{$data->[1]},$dataset_id,defined($data->[14]) ? $data->[14] : 0);
    } else {
	printf("Cannot find parent composite/group to assign %s as source.\n",$dataset_id);
	$success_flag = 0;
    }

    return $success_flag;
}

sub dataset_dts_source {
    my ($self,$dataset_id,$title) = @_;
    my $success_flag = 1;

    my $srcs = $self->{"IVEN"}->selectall_arrayref("SELECT source_id FROM dataset_source WHERE dataset_id='$dataset_id'");
    if (@$srcs == 0) {
	printf("Dataset %s %s does not have any source datasets defined!\n",$dataset_id,$title);
	$success_flag = 0;
    } else {
	foreach my $row (@$srcs) {
	    if ($dataset_id ne $row->[0]) {
		my $stmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset_source_dataset(dataset_id,source_dataset_id) VALUES(?,?)");
		$stmt->execute($dataset_id,$row->[0]);
	    }
	}
    }

    return $success_flag;
}

sub dataset_jedi_test {
    my ($self,$dataset_id,$project,$name,$platform_id) = @_;
    my $success_flag = 1;
    my $data = $self->{"DTS"}->selectall_arrayref("SELECT dataset_id,name FROM jedi7.dataset WHERE dataset_id='$dataset_id'");

    if (@$data == 0) {
	printf("Dataset %s cannot be found in jedi!\n",$dataset_id);
	$success_flag = 0;
    } elsif (@$data > 1) {
	printf("Dataset %s returned multiple rows from the database!\n",$dataset_id);
	$success_flag = 0;
    } else {
	my $row = $data->[0];
	my $test_name = $name;
	$test_name =~ s/\[NCAR\/ATD\]/\[NCAR\/EOL\]/;
	$test_name =~ s/\[JOSS\]/\[NCAR\/EOL\]/;
	$test_name =~ s/\s*<font.+font>//;

	if ($test_name ne $row->[1]) {
	    printf("Dataset %s has different values for the name.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $dataset_id,$test_name,$row->[1]);
	    $success_flag = 0;
	}
	
	my $proj_data = $self->{"DTS"}->selectall_arrayref("SELECT project_id FROM jedi7.dataset_project WHERE dataset_id='$dataset_id' AND project_id='$project'");
	if (@$proj_data == 0) {
	    printf("Dataset %s is not associated with project %s in JEDI!\n",$dataset_id,$project);
	    $success_flag = 0;
	}

	if (defined($platform_id)) {
	    my $plat_data = $self->{"DTS"}->selectall_arrayref("SELECT platform_id FROM jedi7.dataset_platform WHERE dataset_id='$dataset_id' AND platform_id='$platform_id'");
	    if (@$plat_data == 0) {
		printf("Dataset %s is not associated with platform %d in JEDI!\n",$dataset_id,$platform_id);
		$success_flag = 0;
	    }
	}
    }

    return $success_flag;
}

sub datasets {
    my ($self) = @_;
    my $success_flag = 0;
    my $select = "SELECT datasets.pjname,datasets.pdname,datasets.dsname,dataset_id,platform_type,user_processing,raw_data,final_data,station_info,software,plots,status_notes,admin_notes,status,exclude,questions,how_to,readme,best_sw FROM datasets LEFT JOIN dataset_id ON (datasets.dsname=dataset_id.dsname AND datasets.pdname=dataset_id.pdname AND datasets.pjname=dataset_id.pjname) ORDER BY dataset_id";
    my $data = $self->{"IVEN"}->selectall_arrayref($select);

    foreach my $row (@$data) {
	if (!defined(@$row[3]) || @$row[3] eq "") {
	    printf("Dataset %s in product %s for %s does not have a dataset id!\n",
		   @$row[2],@$row[1],@$row[0]);
	    $success_flag = 0;
	} else {
	    $success_flag = $self->dataset_jedi_test(@$row[3],@$row[0],@$row[2],@$row[4]) && $success_flag;
	    $success_flag = $self->dataset_dts_dataset(@$row[3]) && $success_flag;
	    $success_flag = $self->dataset_dts_process($row) && $success_flag;
	    $success_flag = $self->dataset_dts_source(@$row[3],@$row[2]) && $success_flag;
	}
    }

    return $success_flag;
}

sub iven2dts_project_id {
    my ($self,$iven_project) = @_;

    return ($iven_project);
}

##------------------------------------------------------------------------------------
# @signature Iven2DTS new()
# <p>Create a new Iven2DTS object.</p>
#
# @output $self The new object.
##------------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $class = ref($invocant) || $invocant;
    my $self = {};
    bless($self,$class);

    return $self;
}

sub product_dts_test {
    my ($self,$dataset_id,$type,$project,$name,$status) = @_;
    my $success_flag = 1;

    if ($type eq "Composite") {
	my $stmt = $self->{"DTS"}->prepare("SELECT dataset_type FROM dts.dataset WHERE dataset_id=?");
	$stmt->execute($dataset_id);

	if ($stmt->rows() == 0) {
	    my $istmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset(dataset_id,dataset_type,entered_contact_id,entered_date,row_revise_contact_id) VALUES(?,?,1,CURRENT_DATE(),1)");
	    $istmt->execute($dataset_id,$type);
	} else {
	    my @row = $stmt->fetchrow_array();
	    if ($row[0] ne $type) {
		printf("Dataset %s has different value for type.\n\tIVEN: %s\n\tDTS:  %s\n",
		       $dataset_id,$type,$row[0]);
		$success_flag = 0;
	    }
	}

	$stmt = $self->{"DTS"}->prepare("INSERT INTO dts.dataset_process(dataset_id,process_status_id,row_revise_contact_id) VALUES(?,?,1)");
	$stmt->execute($dataset_id,$self->create_status_map()->{$status});

	$self->{"composites"}->{$project}->{$name} = $dataset_id;
    } elsif ($type eq "Group") {
	my $stmt = $self->{"DTS"}->prepare("INSERT INTO dts.project_dataset_group(project_id,group_name,group_status_id) VALUES(?,?,?)");
	$stmt->execute($project,$name,$self->create_status_map()->{$status});
	my $group_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.project_dataset_group","group_id");
	$self->{"groups"}->{$project}->{$name} = $group_id;
    } else {
	printf("Unknown product type: %s\n",$type);
	$success_flag = 0;
    }

    return $success_flag;
}

sub product_jedi_test {
    my ($self,$dataset_id,$project,$product) = @_;
    my $success_flag = 1;
    my $test = $self->{"DTS"}->prepare("SELECT name,project_id FROM jedi7.dataset LEFT JOIN jedi7.dataset_project ON jedi7.dataset.dataset_id=jedi7.dataset_project.dataset_id WHERE jedi7.dataset.dataset_id=? AND project_id=?");
    $test->execute($dataset_id,$project);

    if ($test->rows() == 0) {
	printf("Dataset %s - %s cannot be found in jedi7.dataset\n",$dataset_id,$product);
	$success_flag = 0;
    } else {
	my @data = $test->fetchrow_array();
	
	$product =~ s/\[JOSS\]/\[NCAR\/EOL\]/;

	if ($data[0] ne $product) {
	    printf("Dataset %s has different values for the name.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $dataset_id,$product,$data[0]);
	    $success_flag = 0;
	}
	if (!defined($data[1])) {
	    printf("Dataset %s is not associated with project %s in JEDI.\n",$dataset_id,$project);
	    $success_flag = 0;
	}
    }

    return $success_flag;
}

sub products {
    my ($self) = @_;
    my $success_flag = 1;
    my $select = "SELECT products.pjname,products.pdname,dataset_id,type,status,note FROM products LEFT JOIN product_id ON products.pjname=product_id.pjname AND products.pdname=product_id.pdname ORDER BY pjname,pdname";
    my $data = $self->{"IVEN"}->selectall_arrayref($select);

    foreach my $row (@$data) {
	if (!defined($row->[2]) || ($row->[2] eq "" && $row->[3] eq "Composite")) {
	    printf("Product %s for %s does not have a dataset id!\n",$row->[1],$row->[0]);
	    $success_flag = 0;
	} else {
	    if ($row->[3] eq "Composite") {
		$success_flag = $self->product_jedi_test($row->[2],$row->[0],$row->[1]) && $success_flag;
	    }
	    $success_flag = $self->product_dts_test($row->[2],$row->[3],$row->[0],$row->[1],$row->[4]) && $success_flag;
	}
    }

    return $success_flag;
}

##------------------------------------------------------------------------------------
# @signature int project()
# <p>Read in the Iven project data, compare it with the data in jedi and in the dts,
# and update the dts database with the new data.</p>
#
# @output $success_flag A boolean integer if all of the project data was successfully
# merged into the dts database.
##------------------------------------------------------------------------------------
sub project {
    my ($self) = @_;
    my $success_flag = 1;
    my $select = "SELECT pjname,full_name,storm_id_prefix,begin_date,end_date,minlat,maxlat,minlon,maxlon,admin_notes,link_source,link_target,charge_num FROM iven.projects";

    my $data = $self->{"IVEN"}->selectall_hashref($select,"pjname");
    foreach my $key (keys(%{ $data})) {
	foreach my $project_id ($self->iven2dts_project_id($key)) {
	    $success_flag = $self->project_jedi_test($data->{$key},$project_id) && $success_flag;
	    $success_flag = $self->project_dts_test($data->{$key},$project_id) && $success_flag;
	    $self->update_dts_project($project_id,$data->{$key}->{"admin_notes"},$data->{$key}->{"charge_num"});
	}
    }

    return $success_flag;
}

sub project_dts_test {
    my ($self,$data,$project) = @_;
    my $success_flag = 1;
    my $select = "SELECT charge_number FROM dts.project WHERE project_id=?";
    my $stmt = $self->{"DTS"}->prepare($select);
    $stmt->execute($project);

    $data->{"charge_num"} = 0 unless(defined($data->{"charge_num"}));

    if ($stmt->rows() != 0) {
	my @row = $stmt->fetchrow_array();

	if ($data->{"charge_num"} != $row[0]) {
	    printf("Project %s has different values for the charge number.\n\tIVEN: %s\n\tDTS:  %s\n",
		   $project,$data->{"charge_num"},$row[0]);
	    $success_flag = 0;
	}
    }

    return $success_flag;
}

sub project_jedi_test {
    my ($self,$data,$project) = @_;
    my $success_flag = 1;
    my $select = "SELECT full_name,begin_date,end_date,minlat,minlon,maxlat,maxlon FROM jedi7.project WHERE project_id=?";

    my $stmt = $self->{"DTS"}->prepare($select);
    $stmt->execute($project);
    
    if ($stmt->rows() == 0) {
	printf("Project %s cannot be found in the jedi database.\n",$project);
	$success_flag = 0;
    } else {
	my @row = $stmt->fetchrow_array();

	if ($data->{"full_name"} ne $row[0]) {
	    printf("Project %s has different values for the full name.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"full_name"},$row[0]);
	    $success_flag = 0;
	}
	if ($data->{"begin_date"} ne (split(' ',$row[1]))[0]) {
	    printf("Project %s has different values for the begin date.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"begin_date"},$row[1]);
	    $success_flag = 0;
	}
	if ($data->{"end_date"} ne (split(' ',$row[2]))[0]) {
	    printf("Project %s has different values for the end date.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"end_date"},$row[2]);
	    $success_flag = 0;
	}
	if ($data->{"minlat"} != $row[3]) {
	    printf("Project %s has different values for the min lat.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"minlat"},$row[3]);
	    $success_flag = 0;
	}
	if ($data->{"minlon"} != $row[4]) {
	    printf("Project %s has different values for the min lon.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"minlon"},$row[4]);
	    $success_flag = 0;
	}
	if ($data->{"maxlat"} != $row[5]) {
	    printf("Project %s has different values for the max lat.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"maxlat"},$row[5]);
	    $success_flag = 0;
	}
	if ($data->{"maxlon"} != $row[6]) {
	    printf("Project %s has different values for the max lon.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $project,$data->{"maxlon"},$row[6]);
	    $success_flag = 0;
	}
    }

    return $success_flag;
}

##------------------------------------------------------------------------------------
# @signature void setup()
# <p>Set up the connection to the database and prepare all the necessary variables to
# be used in the porting.</p>
#
# @input $self The Iven2DTS object that called the function.
##------------------------------------------------------------------------------------
sub setup {
    my ($self) = @_;
    
    $self->{"IVEN"} = DBI->connect("DBI:mysql:database=iven;host=hurricane.joss.ucar.edu",
				   "dpg","joel123",{RaiseError => 1});
    $self->{"DTS"} = DBI->connect("DBI:mysql:database=dts;host=127.0.0.1",
				  "dts","manage-all",{AutoCommit => 0, RaiseError => 1});
}

##------------------------------------------------------------------------------------
# @signature void teardown()
# <p>Cleanly close down the database connections and exit the script.</p>
#
# @input $self The Iven2DTS object that called the function.
##------------------------------------------------------------------------------------
sub teardown {
    my ($self) = @_;

    $self->{"DTS"}->rollback();
    
    $self->{"IVEN"}->disconnect();
    $self->{"DTS"}->disconnect();

    exit(0);
}

sub trim {
    my ($line) = @_;
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}

sub update_dts_project {
    my ($self,$project,$notes,$charge_number) = @_;
    my $success_flag = 1;
    my $test = $self->{"DTS"}->prepare("SELECT project_id FROM dts.project WHERE project_id=?");
    $test->execute($project);

    if ($test->rows() == 0) {
	my $stmt = $self->{"DTS"}->prepare("INSERT INTO dts.project(project_id,active,charge_number) VALUES(?,?,?)");
	$stmt->execute($project,1,$charge_number);
    } else {
	my $stmt = $self->{"DTS"}->prepare("UPDATE dts.project SET active=1,charge_number=? WHERE project_id=?");
	$stmt->execute($charge_number,$project);
    }

    if (defined($notes)) {
	my $stmt = $self->{"DTS"}->prepare("INSERT INTO dts.note(note_text,entered_date,author_id,row_revise_contact_id) VALUES(?,?,1,1)");
	$stmt->execute($notes,current_date());
	my $note_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.note","note_id");
	$stmt = $self->{"DTS"}->prepare("INSERT INTO project_note(project_id,note_id) VALUES(?,?)");
	$stmt->execute($project,$note_id);
    }

    return $success_flag;
}

sub user_jedi_test {
    my ($self,$short,$name,$email,$active) = @_;
    my $success_flag = 1;
    my $stmt = $self->{"DTS"}->prepare("SELECT contact_id,person_name,email,active_editor FROM jedi7.contact WHERE contact_short_name=?");
    $stmt->execute($short);

    if ($stmt->rows() == 1) {
	my @row = $stmt->fetchrow_array();
	
	if ($row[1] !~ /$name/) {
	    $name =~ s/\.\+/ /;
	    printf("User %s has different values for the name.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $short,$name,$row[1]);
	    $success_flag = 0;
	}
	if ($row[2] ne $email) {
	    printf("User %s has different values for the email.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $short,$email,$row[2]);
	    $success_flag = 0;
	}
	if ($row[3] != $active) {
	    printf("User %s has different values for the active flag.\n\tIVEN: %s\n\tJEDI: %s\n",
		   $short,$active,$row[3]);
	    $success_flag = 0;
	}
	$self->{"users"}->{lc($short)} = $row[0];
    } elsif ($stmt->rows() > 1) {
	printf("Multiple entried in JEDI for contact_short_name=%s\n",$short);
	$success_flag = 0;
    } elsif ($short eq "unassigned") {
	$self->{"users"}->{lc($short)} = undef();
    } elsif ($short eq "other") {
	$self->{"users"}->{lc($short)} = 1;
    } else {
	printf("JEDI does not contain a user with contact_short_name=%s\n",$short);
    }
    

    return $success_flag;
}

sub users {
    my ($self) = @_;
    my $success_flag = 1;
    my $select = "SELECT LOWER(uid) AS uid,fname,lname,email,active FROM users";
    my $data = $self->{"IVEN"}->selectall_hashref($select,"uid");

    foreach my $id (keys(%{$data})) {
	$success_flag = $self->user_jedi_test($id,
					      join("",$data->{$id}->{"fname"},".+",$data->{$id}->{"lname"}),
					      $data->{$id}->{"email"},$data->{$id}->{"active"}) &&
						  $success_flag;
						   
    }

    return $success_flag;
}
