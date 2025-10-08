#! /usr/bin/perl -w

package IvenDB;
use strict;
use lib ".";
use DBI;
use Composite;
use Dataset;
use Group;
use Project;
use Status;
use User;

sub addDatasetAssociation {
    my ($self,$dataset_id,$source_id) = @_;
    my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.dataset_source_dataset(dataset_id,source_dataset_id) VALUES(?,?)");
    $stmt->execute($dataset_id,$source_id);
    $self->{"DB"}->commit();
}

sub addGroupAssociation {
    my ($self,$group_id,$dataset_id) = @_;
    my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.dataset_in_group(group_id,dataset_id) VALUES(?,?)");
    $stmt->execute($group_id,$dataset_id);
    $self->{"DB"}->commit();
}

sub delete {
    my ($self,$entry) = @_;
    $entry->delete($self);
    $self->{"DB"}->commit();
}

sub deleteGroup {
    my ($self,$group) = @_;
    my $stmt = $self->{"DB"}->prepare("DELETE FROM dts.project_dataset_group WHERE group_id=?");
    $stmt->execute($group->getId());
}

sub excludeFromDataset {
    my ($self,$dataset_id,$source_id,$flag) = @_;
    my $stmt = $self->{"DB"}->prepare("UPDATE dts.dataset_source_dataset SET excluded_flag=? WHERE dataset_id=? AND source_dataset_id=?");
    $stmt->execute($flag,$dataset_id,$source_id);
    $self->{"DB"}->commit();
}

sub excludeFromGroup {
    my ($self,$group_id,$dataset_id,$flag) = @_;
    my $stmt = $self->{"DB"}->prepare("UPDATE dts.dataset_in_group SET excluded_flag=? WHERE group_id=? AND dataset_id=?");
    $stmt->execute($flag,$group_id,$dataset_id);
    $self->{"DB"}->commit();
}

sub disconnect {
    my ($self) = @_;
    $self->{"DB"}->disconnect();
}

sub getCompositeDatasets {
    my ($self,$comp) = @_;
    my @datasets = ();
    
    my $stmt = $self->{"DB"}->prepare("SELECT dts.dataset_source_dataset.source_dataset_id,name,excluded_flag,process_status_id,".
				      "process_on_hold,process_contact_id,dnote.note_id,dnote.note_text,dataset_type, ".
				      "source_utc_offset,source_dst_flag,source_time_note_id,stime.note_text,collection_time_note_id,ctime.note_text ".
				      "FROM dts.dataset_source_dataset ".
				      "JOIN jedi7.dataset ON dts.dataset_source_dataset.source_dataset_id=jedi7.dataset.dataset_id ".
				      "LEFT JOIN dts.dataset ON dts.dataset_source_dataset.source_dataset_id=dts.dataset.dataset_id ".
				      "LEFT JOIN dts.dataset_process ON dts.dataset_source_dataset.source_dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.status ON dts.dataset_process.process_status_id=dts.status.status_id ".
				      "LEFT JOIN dts.dataset_note ON dts.dataset_source_dataset.source_dataset_id=dts.dataset_note.dataset_id ".
				      "LEFT JOIN dts.note AS dnote ON dts.dataset_note.note_id=dnote.note_id ".
				      "LEFT JOIN dts.dataset_source_info ON dts.dataset_source_dataset.source_dataset_id=dts.dataset_source_info.dataset_id ".
				      "LEFT JOIN dts.note AS stime ON dts.dataset_source_info.source_time_note_id=stime.note_id ".
				      "LEFT JOIN dts.note AS ctime ON dts.dataset_source_info.collection_time_note_id=ctime.note_id ".
				      "WHERE dts.dataset_source_dataset.dataset_id=? ".
				      "ORDER BY excluded_flag,is_resolved,status_name,name");
    $stmt->execute($comp->getId());
    while (my @row = $stmt->fetchrow_array()) {
	my $dataset = Dataset->new();
	$dataset->setId($row[0]);
	$dataset->setName($row[1]);
	$dataset->setExcludedFlag($row[2]);
	$dataset->setStatusId($row[3]);
	$dataset->setQuestionFlag($row[4]);
	$dataset->setProcessContactId($row[5]);
	$dataset->setNoteId($row[6]);
	$dataset->setNote($row[7]);
	$dataset->setDatasetType($row[8]);
	$dataset->setUTCOffset($row[9]);
	$dataset->setDSTFlag($row[10]);
	$dataset->setSourceTimeNoteId($row[11]);
	$dataset->setSourceTimeNote($row[12]);
	$dataset->setCollectionTimeNoteId($row[13]);
	$dataset->setCollectionTimeNote($row[14]);

	my $readmeStmt = $self->{"DB"}->prepare("SELECT directory,filename FROM jedi7.file WHERE purpose='doc' AND dataset_id=?");
	$readmeStmt->execute($dataset->getId());
	my @readmes = ();
	while (my @read = $readmeStmt->fetchrow_array()) {
	    my $doc = sprintf("%s/%s",$read[0],$read[1]);
	    $doc =~ s/\/web/www\.joss\.ucar\.edu/;
	    $doc =~ s/\/\//\//g;
	    push(@readmes,"http://".$doc);
	}
	$dataset->addReadmes(@readmes);

	push(@datasets,$dataset);
    }

    return @datasets;
}

sub getCompositeStats {
    my ($self,$comp) = @_;
    my $stmt = $self->{"DB"}->prepare("SELECT COUNT(*),COUNT(IF(is_resolved=1,1,NULL)),COUNT(IF(excluded_flag=1,1,NULL)),".
				      "COUNT(IF(excluded_flag=1 AND is_resolved=1,1,NULL)) ".
				      "FROM dts.dataset_source_dataset JOIN dts.dataset_process ON ".
				      "dts.dataset_source_dataset.source_dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.status ON dts.dataset_process.process_status_id=dts.status.status_id ".
				      "WHERE dts.dataset_source_dataset.dataset_id=?");
    $stmt->execute($comp->getId());
    return $stmt->fetchrow_array();
}

sub getDataset {
    my ($self,$id) = @_;
    my $dataset = Dataset->new();

    my $stmt = $self->{"DB"}->prepare("SELECT jedi7.dataset.dataset_id,name,dataset_type,process_contact_id,process_status_id,".
				      "process_work_dir,process_final_dir,process_repos_dir,process_howto_link,process_stn_list_link,".
				      "process_plot_dir,process_on_hold,dnote.note_id,dnote.note_text, ".
				      "source_utc_offset,source_dst_flag,source_time_note_id,snote.note_text,collection_time_note_id,cnote.note_text ".
				      "FROM jedi7.dataset LEFT JOIN dts.dataset ON jedi7.dataset.dataset_id=dts.dataset.dataset_id ".
				      "LEFT JOIN dts.dataset_process ON jedi7.dataset.dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.dataset_note ON jedi7.dataset.dataset_id=dts.dataset_note.dataset_id ".
				      "LEFT JOIN dts.note AS dnote ON dts.dataset_note.note_id=dnote.note_id ".
				      "LEFT JOIN dts.dataset_source_info ON jedi7.dataset.dataset_id=dts.dataset_source_info.dataset_id ".
				      "LEFT JOIN dts.note AS snote ON source_time_note_id=snote.note_id ".
				      "LEFT JOIN dts.note AS cnote ON collection_time_note_id=cnote.note_id ".
				      "WHERE jedi7.dataset.dataset_id=?");
    $stmt->execute($id);

    my @row = $stmt->fetchrow_array();
    $dataset->setId($row[0]);
    $dataset->setName($row[1]);
    $dataset->setDatasetType($row[2]);
    $dataset->setProcessContactId($row[3]);
    $dataset->setStatusId($row[4]);
    $dataset->setHomeDir($row[5]);
    $dataset->setFinalDir($row[6]);
    $dataset->setReposDir($row[7]);
    $dataset->setHowTo($row[8]);
    $dataset->setStationList($row[9]);
    $dataset->setPlotDir($row[10]);
    $dataset->setQuestionFlag($row[11]);
    $dataset->setNoteId($row[12]);
    $dataset->setNote($row[13]);
    $dataset->setUTCOffset($row[14]);
    $dataset->setDSTFlag($row[15]);
    $dataset->setSourceTimeNoteId($row[16]);
    $dataset->setSourceTimeNote($row[17]);
    $dataset->setCollectionTimeNoteId($row[18]);
    $dataset->setCollectionTimeNote($row[19]);

    $dataset->addDatasets($self->getCompositeDatasets($dataset));
    $dataset->addProducts($self->getProducts($dataset));

    my $readmeStmt = $self->{"DB"}->prepare("SELECT directory,filename FROM jedi7.file WHERE purpose='doc' AND dataset_id=?");
    $readmeStmt->execute($id);
    my @readmes = ();
    while (my @read = $readmeStmt->fetchrow_array()) {
	my $doc = sprintf("%s/%s",$read[0],$read[1]);
	$doc =~ s/\/web/www\.joss\.ucar\.edu/;
	$doc =~ s/\/\//\//g;
	push(@readmes,"http://".$doc);
    }
    $dataset->addReadmes(@readmes);

    return $dataset;
}

sub getGroup {
    my ($self,$id) = @_;
    my $group = Group->new();
    my $stmt = $self->{"DB"}->prepare("SELECT dts.project_dataset_group.group_id,group_name,group_status_id,dts.note.note_id,note_text ".
				      "FROM dts.project_dataset_group ".
				      "LEFT JOIN dts.project_dataset_group_note ON dts.project_dataset_group.group_id=".
				      "dts.project_dataset_group_note.group_id ".
				      "LEFT JOIN dts.note ON dts.project_dataset_group_note.note_id=dts.note.note_id ".
				      "WHERE dts.project_dataset_group.group_id=?");
    $stmt->execute($id);
    my @row = $stmt->fetchrow_array();
    $group->setId($row[0]);
    $group->setName($row[1]);
    $group->setStatusId($row[2]);
    $group->setNoteId($row[3]);
    $group->setNote($row[4]);
    $group->addDatasets($self->getGroupDatasets($group));

    return $group;
}

sub getGroupDatasets {
    my ($self,$group) = @_;
    my @datasets = ();
    my $stmt = $self->{"DB"}->prepare("SELECT dts.dataset_in_group.dataset_id,name,excluded_flag,process_status_id,".
				      "process_on_hold,process_contact_id,dts.note.note_id,note_text ".
				      "FROM dts.dataset_in_group ".
				      "JOIN jedi7.dataset ON dts.dataset_in_group.dataset_id=jedi7.dataset.dataset_id ".
				      "LEFT JOIN dts.dataset_process ON dts.dataset_in_group.dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.status ON dts.dataset_process.process_status_id=dts.status.status_id ".
				      "LEFT JOIN dts.dataset_note ON dts.dataset_in_group.dataset_id=dts.dataset_note.dataset_id ".
				      "LEFT JOIN dts.note ON dts.dataset_note.note_id=dts.note.note_id ".
				      "WHERE dts.dataset_in_group.group_id=? ".
				      "ORDER BY excluded_flag,is_resolved,status_name,name");
    $stmt->execute($group->getId());
    while (my @row = $stmt->fetchrow_array()) {
	my $dataset = Dataset->new();
	$dataset->setId($row[0]);
	$dataset->setName($row[1]);
	$dataset->setExcludedFlag($row[2]);
	$dataset->setStatusId($row[3]);
	$dataset->setQuestionFlag($row[4]);
	$dataset->setProcessContactId($row[5]);
	$dataset->setNoteId($row[6]);
	$dataset->setNote($row[7]);
	push(@datasets,$dataset);
    }
    return @datasets;
}

sub getGroupStats {
    my ($self,$group) = @_;
    my $stmt = $self->{"DB"}->prepare("SELECT COUNT(*),COUNT(IF(is_resolved=1,1,NULL)),COUNT(IF(excluded_flag=1,1,NULL)),".
				      "COUNT(IF(excluded_flag=1 AND is_resolved=1,1,NULL)) ".
				      "FROM dts.dataset_in_group JOIN dts.dataset_process ON ".
				      "dts.dataset_in_group.dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.status ON dts.dataset_process.process_status_id=dts.status.status_id ".
				      "WHERE dts.dataset_in_group.group_id=?");
    $stmt->execute($group->getId());
    return $stmt->fetchrow_array();
}

sub getNote {
    my ($self,$id) = @_;
    my $stmt = $self->{"DB"}->prepare("SELECT note_text FROM dts.note WHERE note_id=?");
    $stmt->execute($id);
    return $stmt->fetchrow_array();
}

sub getProductList {
    my ($self,$project,$sort) = @_;
    if ($sort eq "") { $sort = "name"; }
    elsif ($sort eq "status") { $sort = "is_resolved,status_name,name"; }

    my @products = ();
    my $stmt = $self->{"DB"}->prepare("(SELECT jedi7.dataset.dataset_id,name,0,process_status_id,status_name,is_resolved ".
				      "FROM jedi7.dataset JOIN dts.dataset ON jedi7.dataset.dataset_id=dts.dataset.dataset_id ".
				      "JOIN jedi7.dataset_project ON jedi7.dataset.dataset_id=jedi7.dataset_project.dataset_id ".
				      "LEFT JOIN dts.dataset_process ON dts.dataset.dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.status ON dts.dataset_process.process_status_id=dts.status.status_id ".
				      "WHERE project_id=? AND dataset_type='Composite') ".
				      "UNION ".
				      "(SELECT group_id,group_name AS name,1,group_status_id,status_name,is_resolved ".
				      "FROM dts.project_dataset_group ".
				      "LEFT JOIN dts.status ON dts.project_dataset_group.group_status_id=dts.status.status_id ".
				      "WHERE project_id=?) ".
				      "ORDER BY $sort");
    $stmt->execute($project->getId(),$project->getId());
    while (my @row = $stmt->fetchrow_array()) {
	my $product = $row[2] ? Group->new() : Composite->new();
	$product->setId($row[0]);
	$product->setName($row[1]);
	$product->setStatusId($row[3]);
	$product->addDatasets($product->loadDatasets($self));
	push(@products,$product);
    }

    return @products;
}

sub getProducts {
    my ($self,$dataset) = @_;
    my @products = ();
    my $stmt = $self->{"DB"}->prepare("(SELECT jedi7.dataset.dataset_id,name,0,process_status_id ".
				      "FROM jedi7.dataset JOIN dts.dataset ON jedi7.dataset.dataset_id=dts.dataset.dataset_id ".
				      "JOIN jedi7.dataset_project ON jedi7.dataset.dataset_id=jedi7.dataset_project.dataset_id ".
				      "LEFT JOIN dts.dataset_process ON dts.dataset.dataset_id=dts.dataset_process.dataset_id ".
				      "LEFT JOIN dts.dataset_source_dataset ON jedi7.dataset.dataset_id=dts.dataset_source_dataset.dataset_id ".
				      "WHERE source_dataset_id=? AND dataset_type='Composite') ".
				      "UNION ".
				      "(SELECT dts.project_dataset_group.group_id,group_name AS name,1,group_status_id ".
				      "FROM dts.project_dataset_group ".
				      "JOIN dts.dataset_in_group ON dts.project_dataset_group.group_id=dts.dataset_in_group.group_id ".
				      "WHERE dataset_id=?) ORDER BY name");
    $stmt->execute($dataset->getId(),$dataset->getId());
    while (my @row = $stmt->fetchrow_array()) {
	my $product = $row[2] ? Group->new() : Composite->new();
	$product->setId($row[0]);
	$product->setName($row[1]);
	$product->setStatusId($row[3]);
	push(@products,$product);
    }
    return @products;
}

sub getProductStats {
    my ($self,$product) = @_;
    return $product->getStats($self);
}

sub getProject {
    my ($self,$project_id) = @_;
    my $stmt = $self->{"DB"}->prepare("SELECT full_name,begin_date,end_date,minlat,maxlat,minlon,maxlon, ".
				      "charge_number,dataset_id_prefix,dts.note.note_id,note_text FROM jedi7.project ".
				      "JOIN jedi7.dataset_prefix_project ON jedi7.project.project_id=".
				      "jedi7.dataset_prefix_project.project_id ".
				      "JOIN dts.project ON jedi7.project.project_id=dts.project.project_id ".
				      "LEFT JOIN dts.project_note ON jedi7.project.project_id=dts.project_note.project_id ".
				      "LEFT JOIN dts.note ON dts.project_note.note_id=dts.note.note_id ".
				      "WHERE jedi7.project.project_id=?");
    $stmt->execute($project_id);

    my @row = $stmt->fetchrow_array();
    my $project = Project->new();
    $project->setId($project_id);
    $project->setName($row[0]);
    $project->setBeginDate($row[1]);
    $project->setEndDate($row[2]);
    $project->setMinLatitude($row[3]);
    $project->setMaxLatitude($row[4]);
    $project->setMinLongitude($row[5]);
    $project->setMaxLongitude($row[6]);
    $project->setChargeNumber($row[7]);
    $project->setPrefix($row[8]);
    $project->setNoteId($row[9]);
    $project->setNote($row[10]);

    return $project;
}

sub getProjectList {
    my ($self) = @_;
    my @projects = ();

    my $stmt = $self->{"DB"}->prepare("SELECT dts.project.project_id,full_name FROM dts.project ".
				      "JOIN jedi7.project ON dts.project.project_id=jedi7.project.project_id ".
				      "WHERE dts.project.active=1");
    $stmt->execute();

    while (my @row = $stmt->fetchrow_array()) {
	my $project = Project->new();
	$project->setId($row[0]);
	$project->setName($row[1]);
	push(@projects,$project);
    }

    return @projects;
}

sub getStatusList {
    my ($self) = @_;
    my @statuses = ();

    my $stmt = $self->{"DB"}->prepare("SELECT status_id,status_name,status_style,is_resolved FROM dts.status ORDER BY status_name");
    $stmt->execute();

    while (my @row = $stmt->fetchrow_array()) {
	my $status = Status->new();
	$status->setId($row[0]);
	$status->setName($row[1]);
	$status->setStyle($row[2]);
	$status->setIsDone($row[3]);
	push(@statuses,$status);
    }

    return @statuses;
}

sub getUserList {
    my ($self) = @_;
    my @users = ();

    my $stmt = $self->{"DB"}->prepare("SELECT contact_id,person_name,email,active_editor ".
				      "FROM jedi7.contact WHERE active_editor=1 ORDER BY person_name");
    $stmt->execute();

    while (my @row = $stmt->fetchrow_array()) {
	my $user = User->new();
	$user->setId($row[0]);
	$user->setName($row[1]);
	$user->setEmail($row[2]);
	$user->setActive($row[3]);
	push(@users,$user);
    }

    return @users;
}

sub insert {
    my ($self,$entry) = @_;
    $entry->insert($self);
    $self->{"DB"}->commit();
}

sub insertDataset {
    my ($self,$dataset) = @_;

    $self->processJediDataset($dataset);
    $self->processDtsDataset($dataset);
    $self->processDtsDatasetProcess($dataset);
    $self->processDtsDatasetSource($dataset);
    $self->processDtsDatasetNote($dataset);
}

sub insertGroup {
    my ($self,$group) = @_;
    my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.project_dataset_group(project_id,group_name,group_status_id) ".
				      "VALUES(?,?,?)");
    $stmt->execute($group->getProject()->getId(),$group->getName(),$group->getStatusId());
    if ($group->getNote() !~ /^\s*$/) {
	$stmt = $self->{"DB"}->prepare("INSERT INTO dts.project_dataset_group_note(group_id,note_id) VALUES(?,?)");
	$stmt->execute($self->{"DB"}->last_insert_id(undef(),undef(),"dts.project_dataset_group","group_id"),
		       $self->insertNote($group->getNote()));
    }
}

sub insertNote {
    my ($self,$note) = @_;
    my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.note(note_text,author_id,entered_date,row_revise_contact_id) ".
				      "VALUES(?,1,CURRENT_DATE(),1)");
    $stmt->execute($note);
    return $self->{"DB"}->last_insert_id(undef(),undef(),"dts.note","note_id");
}

sub new {
    my $invocant = shift;
    my $class = $invocant || ref($invocant);
    my $self = {};
    bless($self,$class);

    $self->{"DB"} = DBI->connect("DBI:mysql:database=dts;host=127.0.0.1","dts","manage-all",
				 { AutoCommit=>0,RaiseError=>1}) or 
				     die("Unable to connect to the DTS database!\n");

    return $self;
}

sub processDtsDataset {
    my ($self,$dataset) = @_;

    my $test = $self->{"DB"}->prepare("SELECT dataset_id FROM dts.dataset WHERE dataset_id=?");
    $test->execute($dataset->getId());

    if (scalar(@{ $test->fetchall_arrayref()})) {
	my $stmt = $self->{"DB"}->prepare("UPDATE dts.dataset SET dataset_type=? WHERE dataset_id=?");
	$stmt->execute($dataset->getDatasetType(),$dataset->getId());
    } else {
	my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.dataset(dataset_id,dataset_type,entered_contact_id,entered_date,".
					  "row_revise_contact_id) VALUES(?,?,1,CURRENT_DATE(),1)");
	$stmt->execute($dataset->getId(),$dataset->getDatasetType());
    }
}

sub processDtsDatasetNote {
    my ($self,$dataset) = @_;

    if ($dataset->getNoteId() == -1 && $dataset->getNote() !~ /^\s*$/) {
	my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.dataset_note(dataset_id,note_id) VALUES(?,?)");
	$stmt->execute($dataset->getId(),$self->insertNote($dataset->getNote()));
    } elsif ($dataset->getNoteId() != -1) {
	$self->updateNote($dataset->getNoteId(),$dataset->getNote());
    }
}

sub processDtsDatasetProcess {
    my ($self,$dataset) = @_;

    my $test = $self->{"DB"}->prepare("SELECT dataset_id FROM dts.dataset_process WHERE dataset_id=?");
    $test->execute($dataset->getId());

    if (scalar(@{ $test->fetchall_arrayref()})) {
	my $stmt = $self->{"DB"}->prepare("UPDATE dts.dataset_process SET process_status_id=?,process_contact_id=?,process_work_dir=?,process_final_dir=?,".
					  "process_repos_dir=?,process_howto_link=?,process_stn_list_link=?,process_plot_dir=?,process_on_hold=?,".
					  "row_revise_contact_id=? WHERE dataset_id=?");
	$stmt->execute($dataset->getStatusId() == -1 ? undef() : $dataset->getStatusId(),
		       $dataset->getProcessContactId() == -1 ? undef() : $dataset->getProcessContactId(),
		       $dataset->getHomeDir(),$dataset->getFinalDir(),$dataset->getReposDir(),$dataset->getHowTo(),$dataset->getStationList(),
		       $dataset->getPlotDir(),$dataset->hasQuestions(),1,$dataset->getId());
    } else {
	my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.dataset_process(dataset_id,process_status_id,process_contact_id,process_work_dir,".
					  "process_final_dir,process_repos_dir,process_howto_link,process_stn_list_link,process_plot_dir,".
					  "process_on_hold,row_revise_contact_id) ".
					  "VALUES(?,?,?,?,?,?,?,?,?,?,1)");
	$stmt->execute($dataset->getId(),$dataset->getStatusId() == -1 ? undef() : $dataset->getStatusId(),
		       $dataset->getProcessContactId() == -1 ? undef() : $dataset->getProcessContactId(),
		       $dataset->getHomeDir(),$dataset->getFinalDir(),$dataset->getReposDir(),$dataset->getHowTo(),$dataset->getStationList(),
		       $dataset->getPlotDir(),$dataset->hasQuestions());
    }
}

sub processDtsDatasetSource {
    my ($self,$dataset) = @_;
    my $test = $self->{"DB"}->prepare("SELECT dataset_id FROM dts.dataset_source_info WHERE dataset_id=?");
    $test->execute($dataset->getId());

    if (scalar(@{ $test->fetchall_arrayref()})) {
	my $snote_id = undef();
	if ($dataset->getSourceTimeNoteId() == -1 && $dataset->getSourceTimeNote() !~ /^\s*$/) {
	    $snote_id = $self->insertNote($dataset->getSourceTimeNote());
	} elsif ($dataset->getSourceTimeNoteId() != -1) {
	    $self->updateNote($dataset->getSourceTimeNoteId(),$dataset->getSourceTimeNote());
	    $snote_id = $dataset->getSourceTimeNoteId();
	}
	my $cnote_id = undef();
	if ($dataset->getCollectionTimeNoteId() == -1 && $dataset->getCollectionTimeNote() !~ /^\s*$/) {
	    $cnote_id = $self->insertNote($dataset->getCollectionTimeNote());
	} elsif ($dataset->getCollectionTimeNoteId() != -1) {
	    $self->updateNote($dataset->getCollectionTimeNoteId(),$dataset->getCollectionTimeNote());
	    $cnote_id = $dataset->getCollectionTimeNoteId();
	}

	my $stmt = $self->{"DB"}->prepare("UPDATE dts.dataset_source_info SET source_utc_offset=?,source_dst_flag=?,source_time_note_id=?,".
					  "collection_time_note_id=?,row_revise_contact_id=1 WHERE dataset_id=?");
	$stmt->execute($dataset->getUTCOffset(),$dataset->getDSTFlag(),$snote_id,$cnote_id,$dataset->getId());
    } else {
	my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.dataset_source_info(dataset_id,source_utc_offset,source_dst_flag,source_time_note_id,".
					  "collection_time_note_id,row_revise_contact_id) VALUES(?,?,?,?,?,1)");
	$stmt->execute($dataset->getId(),$dataset->getUTCOffset(),$dataset->getDSTFlag(),
		       $dataset->getSourceTimeNote() =~ /^\s*$/ ? undef() : $self->insertNote($dataset->getSourceTimeNote()),
		       $dataset->getCollectionTimeNote() =~ /^\s*$/ ? undef() : $self->insertNote($dataset->getCollectionTimeNote()));
    }
}

sub processJediDataset {
    my ($self,$dataset) = @_;

    my $test = $self->{"DB"}->prepare("SELECT dataset_id FROM jedi7.dataset WHERE dataset_id=?");
    $test->execute($dataset->getId());

    if (scalar(@{ $test->fetchall_arrayref()})) {
	my $stmt = $self->{"DB"}->prepare("UPDATE jedi7.dataset SET name=? WHERE dataset_id=?");
	$stmt->execute($dataset->getName(),$dataset->getId());
    } else {
	my $stmt = $self->{"DB"}->prepare("INSERT INTO jedi7.dataset(dataset_id,name,begin_date,end_date,".
					  "minlat,minlon,maxlat,maxlon,internal_contact_id,displayed_contact_id,hide) ".
					  "VALUES(?,?,?,?,?,?,?,?,1,1,1)");
	my $project = $dataset->getProject();
	$stmt->execute($dataset->getId(),$dataset->getName(),$project->getBeginDate,$project->getEndDate(),
		       $project->getMinLatitude(),$project->getMinLongitude(),
		       $project->getMaxLatitude(),$project->getMaxLongitude());

	$stmt = $self->{"DB"}->prepare("INSERT INTO jedi7.dataset_project(project_id,dataset_id) VALUES(?,?)");
	$stmt->execute($project->getId(),$dataset->getId());
    }
}

sub removeDatasetAssociation {
    my ($self,$dataset_id,$source_id) = @_;
    my $stmt = $self->{"DB"}->prepare("DELETE FROM dts.dataset_source_dataset WHERE dataset_id=? AND source_dataset_id=?");
    $stmt->execute($dataset_id,$source_id);
    $self->{"DB"}->commit();
}

sub removeGroupAssociation {
    my ($self,$group_id,$source_id) = @_;
    my $stmt = $self->{"DB"}->prepare("DELETE FROM dts.dataset_in_group WHERE group_id=? AND dataset_id=?");
    $stmt->execute($group_id,$source_id);
    $self->{"DB"}->commit();
}

sub update {
    my ($self,$entry) = @_;
    $entry->update($self);
    $self->{"DB"}->commit();
}

sub updateDataset {
    my ($self,$dataset) = @_;

    $self->processJediDataset($dataset);
    $self->processDtsDataset($dataset);
    $self->processDtsDatasetProcess($dataset);
    $self->processDtsDatasetSource($dataset);
    $self->processDtsDatasetNote($dataset);
}

sub updateGroup {
    my ($self,$group) = @_;
    my $stmt = $self->{"DB"}->prepare("UPDATE dts.project_dataset_group SET group_name=?,group_status_id=? WHERE group_id=?");
    $stmt->execute($group->getName(),$group->getStatusId() == -1 ? undef() : $group->getStatusId(),$group->getId());
    if ($group->getNoteId() == -1 && $group->getNote() !~ /^\s*$/) {
	$stmt = $self->{"DB"}->prepare("INSERT INTO dts.project_dataset_group_note(group_id,note_id) VALUES(?,?)");
	$stmt->execute($group->getId(),$self->insertNote($group->getNote()));
    } elsif ($group->getNoteId() != -1) {
	$self->updateNote($group->getNoteId(),$group->getNote());
    }
}

sub updateNote {
    my ($self,$id,$text) = @_;
    my $stmt = $self->{"DB"}->prepare("UPDATE dts.note SET note_text=?,author_id=1,row_revise_contact_id=1 ".
				      "WHERE note_id=?");
    $stmt->execute($text,$id);
}

sub updateProject {
    my ($self,$project) = @_;
    if ($project->getNoteId() == -1 && $project->getNote() !~ /^\s*$/) {
	my $stmt = $self->{"DB"}->prepare("INSERT INTO dts.project_note(project_id,note_id) VALUES(?,?)");
	$stmt->execute($project->getId(),$self->insertNote($project->getNote()));
    } elsif ($project->getNoteId() != -1) {
	$self->updateNote($project->getNoteId(),$project->getNote());
    }
}

1;
