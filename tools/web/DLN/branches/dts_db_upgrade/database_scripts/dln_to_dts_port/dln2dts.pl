#! /usr/bin/perl -w

package DLN2DTS;
use strict;
use DBI;

&main();

sub main {
    my $self = DLN2DTS->new();
    $self->setup();

    $self->project() or $self->teardown();
    $self->user() or $self->teardown();
#    $self->dataset() or $self->teardown();
    $self->dataset();
    $self->insert_datasets_into_jedi() or $self->teardown();
    $self->insert_datasets_into_dts() or $self->teardown();

    $self->{"DTS"}->commit();
    $self->teardown();
}

##----------------------------------------------------------------------------------
# @signature String clean_arctic_title(String title)
# <p>Clean up the dataset title for an ITEX/ATLAS arctic project.  It removes the
# years, and PIs from the title that are in other parts of the metadata.
#
# @input $self The DLN2DTS object beining worked on.
# @input $title The title to be cleaned.
# @output $title The cleaned up title.
##----------------------------------------------------------------------------------
sub clean_arctic_title {
    my ($self,$title) = @_;

    $title =~ s/^\d{4}\-\d{4}[,]?\s*//;
    $title =~ s/^(\d{4}[,]?\s*)+\s*//;

    $title =~ s/ Avg / Average /g;
    $title =~ s/ Temps / Temperatures /g;

    $title =~ s/\s+\(Ahlquist\)$//;
    $title =~ s/\s+\(Alatalo\)$//;
    $title =~ s/\s+\(Beringer\)$//;
    $title =~ s/\s+\(Bret\-Harte\)$//;
    $title =~ s/\s+\(Calef\)$//;
    $title =~ s/\s+\(Epstein\)$//;
    $title =~ s/\s+\(Hollister\)$//;
    $title =~ s/\s+\(Jia.*\)$//;
    $title =~ s/\s+\(Jonsdottir\)$//;
    $title =~ s/\s+\(Magnusson\)$//;
    $title =~ s/\s+\(Molau\)$//;
    $title =~ s/\s+\(Oberbauer\)$//;
    $title =~ s/\s+\(Robinson.*\)$//;
    $title =~ s/\s*\(Romanovsky\)$//;
    $title =~ s/\s+\(Suding\)$//;
    $title =~ s/\s+\(Totland\)$//;
    $title =~ s/\s+\(M?Walker\)$//;
    $title =~ s/\s+\(Webber\)$//;
    $title =~ s/\s+\(Welker.*\)$//;

    return $title;
}

##----------------------------------------------------------------------------------
# @signature String clean_categories_from_title(String dataset_id, String title)
# <p>Clean up a dataset title by removing the leading category information and store
# it within the object for later use.</p>
#
# @param $self The DLN2DTS object being worked on.
# @param $dataset_id The id for the dataset title being cleaned.
# @param $title The title to be cleaned.
# @output $title The cleaned up title.
##----------------------------------------------------------------------------------
sub clean_categories_from_title {
    my ($self,$dataset_id,$title) = @_;
    
    if ($title =~ /^Aircraft:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Aircraft"} = 1;
    } elsif ($title =~ /^Aircraft Radar:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Aircraft"} = 1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Radar"} = 1;
    } elsif ($title =~ /^Ancillary:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Ancillary"} = 1;
    } elsif ($title =~ /^GPS:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"GPS"} = 1;
    } elsif ($title =~ /^GTS:\s+(.+)$/i) {
        $title = $1;
    } elsif ($title =~ /^Land:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Land"} = 1;
    } elsif ($title =~ /^Lidar:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Lidar"} = 1;
    } elsif ($title =~ /^Mesonet:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Mesonet"} = 1;
    } elsif ($title =~ /^Model:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Model"} = 1;
    } elsif ($title =~ /^Oceanographic:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Oceanography"} = 1;
    } elsif ($title =~ /^Oceanographic Surface:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Oceanography"} = 1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Surface"} = 1;
    } elsif ($title =~ /^Profiler\/Sodar:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Profiler/Sodar"} = 1;
    } elsif ($title =~ /^Radar:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Radar"} = 1;
    } elsif ($title =~ /^Radiation:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Radiation"} = 1;
    } elsif ($title =~ /^Satellite:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Satellite"} = 1;
    } elsif ($title =~ /^Ship:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Ship"} = 1;
    } elsif ($title =~ /^Soundings?:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Sounding"} = 1;
    } elsif ($title =~ /^Sub\-?Surface:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Subsurface"} = 1;
    } elsif ($title =~ /^Surface:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Surface"} = 1;
    } elsif ($title =~ /^Surface Based:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Surface Based"} = 1;
    } elsif ($title =~ /^Upper[\-\s]Air:\s+(.+)$/i) {
        $title = $1;
	$self->{"datasets"}->{$dataset_id}->{"cats"}->{"Upper Air"} = 1;
    }

    return $title;
}

##----------------------------------------------------------------------------------
# @signature String clean_dataset_title(String dataset_id, String title)
# <p>Clean up the dataset title be removing all of the information in it that can
# be found in the other metadata.  This includes project names, HTML tags, and 
# categories.</p>
#
# @param $self The DLN2DTS object being worked on.
# @param $dataset_id The id for the dataset title being cleaned.
# @param $title The title to be cleaned.
# @output $title The cleaned up title.
##----------------------------------------------------------------------------------
sub clean_dataset_title {
    my ($self,$dataset_id,$title) = @_;

    $title =~ s/^CEOP\/EOP\-?\d(\/ \d)?\s+//;
    $title =~ s/^CEOP\s+//;
    $title =~ s/\s+/ /g;
    $title =~ s/^\s+//;
    $title =~ s/(\s+\-)?\s+<font.+<\/font>\s*$//;
    $title =~ s/<b>//;
    $title =~ s/<\/b>//;
    $title =~ s/\(EXCEL\)/\(Excel\)/;

    $title =~ s/\[JOSS\]\s*$/\[NCAR\/EOL\]/;
    $title =~ s/\[NCAR\/ATD\]\s*$/\[NCAR\/EOL\]/;
    $title =~ s/\[ATD\]\s*$/\[NCAR\/EOL\]/;

    $title =~ s/^ATLAS:\s*//;
    $title =~ s/^ITEX:\s*//;
    $title =~ s/^SBI:\s*//;
    $title =~ s/^SHEBA:\s*//;

    $title = $self->clean_categories_from_title($dataset_id,$title);

    return trim($title);
}

##----------------------------------------------------------------------------------
# @signature int dataset()
# <p>Translate the information in the dataset table of the dln database into this
# object.  It will test information in the DLN with the data in JEDI.</p>
#
# @param $self The DLN2DTS object calling the function.
# @return $success_flag A 1 value if all of the dataset values were read correctly
# and passed the tests with other databases or a 0 value if any issue was discovered.
# @warning This function will quit on any error in the commands sent to the database.
##----------------------------------------------------------------------------------
sub dataset {
    my ($self) = @_;
    my $SELECT = "SELECT dsid,storm_id as dataset_id,title as name, readme as readme_flag, master as ml_flag, checked as checked_flag, loaded as loaded_flag, date as entered_date, notes as note_text, project as project_id, loader, checker, int_contact, ext_contact, ext_email, ingest as ingest_dir, archive as archive_dir, remote_url FROM dataset";

    # Read in the dataset data from the DLN.
    my $data = $self->{"DLN"}->selectall_hashref($SELECT,"dsid");
    my $success_flag = 1;

    # Loop through all of the entries in the database.
    foreach my $key (sort(keys(%{$data}))) {

	# Put all of the DTS specific data into its hash for later use
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"ingest_dir",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"ingest_dir"},
					   $data->{$key}->{"ingest_dir"}) && $success_flag;
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"readme_flag",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"readme_flag"},
					   $data->{$key}->{"readme_flag"}) && $success_flag;

	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"loader_id",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"loader_id"},
					   $self->{"users"}->{lc($data->{$key}->{"loader"})}) && $success_flag;
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"loaded_flag",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"loaded_flag"},
					   $data->{$key}->{"loaded_flag"}) && $success_flag;
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"load_data_dir",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"load_data_dir"},
					   $data->{$key}->{"ingest_dir"}) && $success_flag;
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"load_archive_dir",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"load_archive_dir"},
					   $data->{$key}->{"archive_dir"}) && $success_flag;
	
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"entered_date",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"entered_date"},
					   $data->{$key}->{"entered_date"}) && $success_flag;
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"approver_id",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"approver_id"},
					   $self->{"users"}->{lc($data->{$key}->{"checker"})}) && $success_flag;
	$success_flag = $self->dln_compare($data->{$key}->{"dataset_id"},"approved_flag",
					   \$self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"approved_flag"},
					   $data->{$key}->{"checked_flag"}) && $success_flag;
	
	if (defined($self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"note"})) {
	    $self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"note"} .= $data->{$key}->{"note_text"};
	} else {
	    $self->{"dtsDB"}->{$data->{$key}->{"dataset_id"}}->{"note"} = $data->{$key}->{"note_text"};
	}


	# Check to see if the dataset is already in JEDI.
	if ($self->dataset_in_jedi($data->{$key}->{"dataset_id"})) {
	    $success_flag = $self->dataset_jedi_test($data->{$key}->{"dataset_id"},
						     $data->{$key}->{"project_id"},
						     trim($data->{$key}->{"name"}),
						     trim($data->{$key}->{"int_contact"}),
						     trim($data->{$key}->{"ext_contact"}),
						     trim($data->{$key}->{"ext_email"}),
						     defined($data->{$key}->{"remote_url"}) ?
						     trim($data->{$key}->{"remote_url"}) : "") &&
						     $success_flag;
	} else {
	    # Prepare the DLN data for JEDI
	    my $dataset_id = $data->{$key}->{"dataset_id"};

	    # Don't handle missing dataset ids.  Make the DLN have it preset to make the script easier.
	    if ($dataset_id =~ /^99\.999/ || $dataset_id =~ /link/i) {
		printf("Dataset %s - %s cannot have a missing dataset_id!\n",$dataset_id,$data->{$key}->{"project_id"});
		delete($self->{"dtsDB"}->{$dataset_id});
		$success_flag = 0;
	    } else {
		# Clean the dataset title of all unnecessary information and put it into the hash.
		my $name = $self->clean_dataset_title($dataset_id,trim($data->{$key}->{"name"}));
		$name = $self->clean_arctic_title($dataset_id,$name) if ($data->{$key}->{"project_id"} =~ /ATLAS|ITEX/);
		$success_flag = $self->dln_compare($dataset_id,"name",
						   \$self->{"datasets"}->{$dataset_id}->{"name"},$name) && $success_flag;
		my @projects = $self->dln2dts_project_id($data->{$key}->{"project_id"});
		
		# Determine and set the dataset metadata based on the projects associated with it.
		my @where = ();
		foreach my $project (@projects) { 
		    push(@where,sprintf("project_id='%s'",$project)); 
		    $self->{"datasets"}->{$dataset_id}->{"projects"}->{$project} = 1;
		}
		my $SELECT = sprintf("SELECT MIN(begin_date) AS begin_date,MAX(end_date) AS end_date,MIN(minlat) AS minlat,MIN(minlon) AS minlon,MAX(maxlat) AS maxlat,MAX(maxlon) AS maxlon FROM jedi7.project WHERE %s",join(" OR ",@where));
		my $proj_data = $self->{"DTS"}->selectall_arrayref($SELECT);

		$self->{"datasets"}->{$dataset_id}->{"description"} = sprintf("This dataset contains the %s.",$name);
		$self->{"datasets"}->{$dataset_id}->{"begin_date"} = $proj_data->[0]->[0];
		$self->{"datasets"}->{$dataset_id}->{"end_date"} = $proj_data->[0]->[1];
		$self->{"datasets"}->{$dataset_id}->{"minlat"} = $proj_data->[0]->[2];
		$self->{"datasets"}->{$dataset_id}->{"minlon"} = $proj_data->[0]->[3];
		$self->{"datasets"}->{$dataset_id}->{"maxlat"} = $proj_data->[0]->[4];
		$self->{"datasets"}->{$dataset_id}->{"maxlon"} = $proj_data->[0]->[5];

		# Assign the internal contact information
		if (defined($self->{"users"}->{$data->{$key}->{"int_contact"}})) {
		    $success_flag = $self->dln_compare($dataset_id,"internal_contact_id",
						       \$self->{"datasets"}->{$dataset_id}->{"internal_contact_id"},
						       $self->{"users"}->{$data->{$key}->{"int_contact"}}) && $success_flag;
		}

		# Check the external contact information
		if (defined($data->{$key}->{"ext_contact"}) && trim($data->{$key}->{"ext_contact"}) ne "" &&
		    trim($data->{$key}->{"ext_contact"}) !~ /^none$/i ) {
		    my $SELECT = sprintf("SELECT contact_id,email FROM jedi7.contact WHERE person_name='%s'",
					 $data->{$key}->{"ext_contact"});
		    my $contact_data = $self->{"DTS"}->selectall_arrayref($SELECT);

		    if (@$contact_data) {
			my $email = trim($data->{$key}->{"ext_email"});

			if (defined($email) && $email ne "" && $email ne $contact_data->[0]->[1]) {
			    printf("(Contact) - External email for contact %s from DLN (%s) doesn't match JEDI (%s)!\n",
				   $data->{$key}->{"ext_contact"},$email,$contact_data->[0]->[1]);
			    $success_flag = 0;
			} else {
			    $self->{"datasets"}->{$dataset_id}->{"source_contact_id"} = $contact_data->[0]->[0];
			}

		    } else {
			printf("Can't find contact information for %s (email: %s) in JEDI!\n",
			       $data->{$key}->{"ext_contact"},
			       defined($data->{$key}->{"ext_email"}) ? $data->{$key}->{"ext_email"} : "");
			$success_flag = 0;
		    }
		}

		# Check the remote_url
		if (defined($data->{$key}->{"remote_url"}) && trim($data->{$key}->{"remote_url"}) ne "") {
		    $success_flag = $self->dln_compare($dataset_id,"remote_url",
						       \$self->{"datasets"}->{$dataset_id}->{"remote_url"},
						       trim($data->{$key}->{"remote_url"})) && $success_flag;
		    $success_flag = $self->dln_compare($dataset_id,"onlineorderable",
						       \$self->{"datasets"}->{$dataset_id}->{"onlineorderable"},0) && $success_flag;
		} else {
		    $success_flag = $self->dln_compare($dataset_id,"onlineorderable",
						       \$self->{"datasets"}->{$dataset_id}->{"onlineorderable"},1) && $success_flag;
		}
	    }
	}
    }

    return $success_flag;
}

##----------------------------------------------------------------------------------
# @signature int dataset_in_jedi(String dataset_id)
# <p>Determine if the dataset with the specified dataset id is in JEDI.</p>
#
# @param $self The DLN2DTS object calling the function.
# @param $dataset_id The dataset id to be found in JEDI.
# @return 1 if the dataset was found, 0 if it was not.
# @warning The function will teminate the script if it finds more than one dataset
# with the dataset id.  This should never happen as the dataset_id is the primary
# key for a dataset.
##----------------------------------------------------------------------------------
sub dataset_in_jedi {
    my ($self,$dataset_id) = @_;
    my $SELECT = "SELECT COUNT(dataset_id) FROM jedi7.dataset WHERE dataset_id='$dataset_id'";

    my $data = $self->{"DTS"}->selectall_arrayref($SELECT);

    if ($data->[0]->[0] < 1) { return 0; }
    elsif ($data->[0]->[0] == 1) { return 1; }
    else {
	printf("Something is very very wrong!!!  There are multiple datasets with the same id %s in JEDI!!\n\n",$dataset_id);
	$self->teardown();
    }
}

##----------------------------------------------------------------------------------
# @signature int dataset_jedi_test(String dataset_id, String dln_project_id, String name, int readme_flag, String int_contact, String ext_contact, String ext_email, String remote_url)
# <p>Compare the dataset to the dataset in JEDI to make sure all of the information
# is the same before merging them together.</p>
# 
# @input $self The DLN2DTS object calling this function.
# @input $dataset_id The dataset id of the dataset being tested against JEDI.
# @input $dln_project_id The project id used by DLN for the dataset.
# @input $name The name of the dataset being tested.
# @input $int_contact The id used by the DLN for the internal contact.
# @input $ext_contact The name of the external contact for the dataset.
# @input $ext_email The email address for the external contact.
# @input $remote_url The remote URL for the dataset.
# @output $success_flag 1 if all of the values match between the DLN and JEDI, 0 if
# one or more of the values do not match.
##----------------------------------------------------------------------------------
sub dataset_jedi_test {
    my ($self,$dataset_id,$dln_project_id,$name,$int_contact,$ext_contact,$ext_email,$remote_url) = @_;
    my $SELECT = "SELECT dataset_id,name,internal_contact_id,source_contact_id,source.person_name AS source_name,source.email AS source_email,displayed_contact_id,displayed.person_name AS displayed_name,displayed.email AS displayed_email,urls.has_url FROM jedi7.dataset LEFT JOIN jedi7.contact AS displayed ON displayed_contact_id=displayed.contact_id LEFT JOIN jedi7.contact AS source ON source_contact_id=source.contact_id JOIN (SELECT COUNT(*) > 0 AS has_url FROM jedi7.xlink NATURAL JOIN jedi7.dataset_xlinks WHERE dataset_id='$dataset_id' AND href LIKE '%$remote_url%') AS urls WHERE dataset_id='$dataset_id'";

    my $data = $self->{"DTS"}->selectall_arrayref($SELECT);
    my $success_flag = 1;

    # Remove various parts of the dataset title that are not necessary (i.e project names, categories, etc.)
    $name = $self->clean_dataset_title($dataset_id,$name);

    # Do some specific cleaning of the dataset title for the Arctic Projects
    if ($dln_project_id =~ /ITEX|ATLAS/) {
	$name = $self->clean_arctic_title($name);
    }

    # Replace all references of JOSS to NCAR/EOL
    $data->[0]->[1] =~ s/JOSS/NCAR\/EOL/g;
    $name =~ s/JOSS/NCAR\/EOL/g;

    # Compare the dataset title between DLN and JEDI
    if ($name ne $data->[0]->[1]) {
	printf("Dataset %s - %s has different dataset names between DLN and JEDI!\n",$dataset_id,$dln_project_id);
	printf("\tDLN:  %s\n\tJEDI: %s\n\n",$name,$data->[0]->[1]);
	$success_flag = 0;
    }

    # Make sure the dataset-project associations are in JEDI
    foreach my $project_id ($self->dln2dts_project_id($dln_project_id)) {
	my $SELECT = "SELECT COUNT(*) > 0 AS has_project FROM jedi7.dataset_project WHERE dataset_id='$dataset_id' AND project_id='$project_id'";
	my $proj_data = $self->{"DTS"}->selectall_arrayref($SELECT);
	if (!$proj_data->[0]->[0]) {
	    printf("Dataset %s does not have project %s assigned in JEDI!\n",$dataset_id,$project_id);
	}
    }

    # Compare the internal contact between DLN and JEDI
    if (defined($self->{"users"}->{$int_contact}) && defined($data->[0]->[2]) && 
	$self->{"users"}->{$int_contact} != $data->[0]->[2]) {
	printf("Dataset %s - %s has different internal contacts between DLN (%d) and JEDI (%d)!\n",
	       $dataset_id,$dln_project_id,$self->{"users"}->{$int_contact},$data->[0]->[2]);
    }

    # Compare the external contact information
    if (defined($ext_contact) && $ext_contact ne "" && $ext_contact !~ /^none$/i) {

	if (defined($data->[0]->[4]) && $ext_contact =~ /$data->[0]->[4]/i) {
	    if (defined($data->[0]->[5]) && defined($ext_email) && $ext_email ne "" && $ext_email !~ /$data->[0]->[5]/i) {
		printf("\t(Src) External email for contact %s from DLN (%s) doesn't match JEDI (%s)!\n",
		       $ext_contact,$ext_email,$data->[0]->[5]);
	    }
	    $success_flag = 0;
	} elsif (defined($data->[0]->[9]) && $ext_contact =~ /$data->[0]->[9]/i) {
	    if (defined($data->[0]->[10]) && defined($ext_email) && $ext_email ne "" && $ext_email !~ /$data->[0]->[10]/i) {
		printf("\t(Disp) - External email for contact %s from DLN (%s) doesn't match JEDI (%s)!\n",
		       $ext_contact,$ext_email,$data->[0]->[10]);
	    }
	    $success_flag = 0;
	} else {
	    $self->{"dtsDB"}->{$dataset_id}->{"note"} = sprintf("DTS could not match the external contact %s with the source or display contact in jedi.\n\n",$ext_contact);
	    $self->{"dtsDB"}->{$dataset_id}->{"note"} .= defined($ext_email) && $ext_email ne "" ? "  The email address from DLN is $ext_email\n\n" : "\n\n";
	}
    }

    # Test the remote readme
    if ($remote_url ne "" && !$data->[0]->[11]) {
	printf("Dataset %s - %s does not have the remote_url: % set in JEDI!\n",
	       $dataset_id,$dln_project_id,$remote_url);
    }

    return $success_flag;
}

##----------------------------------------------------------------------------------
# @signature int dln_compare(String dataset_id, String code, Object store, Object value)
# <p>Compare the store value with a new value to make sure they are the same for the
# same dataset in the DLN.  This is done because the DLN has a new dataset entry for
# each project even though they have the same dataset id.</p>
#
# @input $dataset_id The id of the dataset being compared.
# @input $code The String to be displayed for the type of the values.
# @input $store The reference to the stored value to be compared.
# @input $value The value to compare with the stored value.
# @output $success_flag 1 if the comparison passed, 0 if it did not.
##----------------------------------------------------------------------------------
sub dln_compare {
    my ($self,$dataset_id,$code,$store,$value) = @_;

    if (defined($$store)) {
	if ($$store ne $value) {
	    printf("DLN has internal conflicts with stored value (%s) with new value (%s) for %s for dataset %s\n",
		   $$store,$value,$code,$dataset_id);
	    return 0;
	}
    } else {
	$$store = $value;
    }

    return 1;
}

##----------------------------------------------------------------------------------
# @signature String[] dln2dts_project_id(String project_id)
# <p>Covert a DLN project id into a list of project ids that are in JEDI.</p>
#
# @input $project_id The DLN project id to be converted.
# @output $proj_list The list of JEDI project ids for the DLN project id.
##----------------------------------------------------------------------------------
sub dln2dts_project_id {
    my ($self,$project_id) = @_;

    return ("IHOP_2002") if ($project_id eq "IHOP");
    return ("CEOP/EOP-3","CEOP/EOP-4") if ($project_id eq "CEOP/EOP-3/4");

    return ($project_id);
}

##----------------------------------------------------------------------------------
# @signature int insert_datasets_into_dts()
# <p>Insert all of the DTS data that has been prepared into the DTS database.</p>
#
# @input $self The DLN2DTS object that contains all of the data and called the function.
# @output $success_flag 1 if all of the data was successfully inserted into the 
# database or 0 if there was a problem.
# @warning The function will die if there is a problem with the database connection.
##----------------------------------------------------------------------------------
sub insert_datasets_into_dts {
    my ($self) = @_;
    my $success_flag = 1;

    my $done_id = 0;
    my $tbd_id = 0;

    # Make sure that the "Done" and "TBD" statuses are in the status table and get the ids for the
    # two statuses to be used later.
    my $status_sth = $self->{"DTS"}->prepare("INSERT INTO dts.status(status_name,status_style,is_resolved) VALUES(?,?,?)");
    my $status_data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='Done'");
    if (!defined($status_data->[0]->[0])) {
	$status_sth->execute("Done","done",1);
	$done_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$done_id = $status_data->[0]->[0];
    }
    $status_data = $self->{"DTS"}->selectall_arrayref("SELECT status_id FROM dts.status WHERE status_name='TBD'");
    if (!defined($status_data->[0]->[0])) {
	$status_sth->execute("TBD","tbd",0);
	$tbd_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.status","status_id");
    } else {
	$tbd_id = $status_data->[0]->[0];
    }

    foreach my $dataset_id (keys(%{ $self->{"dtsDB"}})) {
	
	# Insert into the dts.dataset table
	$self->{"DTS"}->prepare("INSERT INTO dts.dataset(dataset_id,entered_contact_id,entered_date,approved_contact_id,approved_time,approved_status_id,readme_flag,row_revise_contact_id) VALUES(?,?,?,?,?,?,?,?)")
	    ->execute($dataset_id,
		      1,
		      $self->{"dtsDB"}->{$dataset_id}->{"entered_date"},
		      $self->{"dtsDB"}->{$dataset_id}->{"approver_id"},
		      defined($self->{"dtsDB"}->{$dataset_id}->{"approver_id"}) ? "NOW()" : undef(),
		      defined($self->{"dtsDB"}->{$dataset_id}->{"approved_flag"}) && $self->{"dtsDB"}->{$dataset_id}->{"approved_flag"} ? $done_id : $tbd_id,
		      $self->{"dtsDB"}->{$dataset_id}->{"readme_flag"},
		      1);
	delete($self->{"dtsDB"}->{$dataset_id}->{"entered_date"});
	delete($self->{"dtsDB"}->{$dataset_id}->{"approver_id"});
	delete($self->{"dtsDB"}->{$dataset_id}->{"approved_flag"});
	delete($self->{"dtsDB"}->{$dataset_id}->{"readme_flag"});

	# Insert into the dts.dataset_ingest table
	if (defined($self->{"dtsDB"}->{$dataset_id}->{"ingest_dir"}) && $self->{"dtsDB"}->{$dataset_id}->{"ingest_dir"} ne "") {
	    $self->{"DTS"}->prepare("INSERT INTO dts.dataset_ingest(dataset_id,ingest_dir,row_revise_contact_id) VALUES(?,?,?)")
		->execute($dataset_id,$self->{"dtsDB"}->{$dataset_id}->{"ingest_dir"},1);
	}
	delete($self->{"dtsDB"}->{$dataset_id}->{"ingest_dir"});

	# Insert into the dts.dataset_load table
	$self->{"DTS"}->prepare("INSERT INTO dts.dataset_load(dataset_id,load_contact_id,load_status_id,load_data_dir,load_archive_dir,row_revise_contact_id) VALUES(?,?,?,?,?,?)")
	    ->execute($dataset_id,
		      $self->{"dtsDB"}->{$dataset_id}->{"loader_id"},
		      defined($self->{"dtsDB"}->{$dataset_id}->{"loaded_flag"}) && $self->{"dtsDB"}->{$dataset_id}->{"loaded_flag"} ? $done_id : (defined($self->{"dtsDB"}->{$dataset_id}->{"loader_id"}) && $self->{"dtsDB"}->{$dataset_id}->{"loader_id"} != 1 ? $tbd_id : undef()),
		      defined($self->{"dtsDB"}->{$dataset_id}->{"load_data_dir"}) ? $self->{"dtsDB"}->{$dataset_id}->{"load_data_dir"} : "",
		      defined($self->{"dtsDB"}->{$dataset_id}->{"load_archive_dir"}) ? $self->{"dtsDB"}->{$dataset_id}->{"load_archive_dir"} : "",
		      1);
	delete($self->{"dtsDB"}->{$dataset_id}->{"loader_id"});
	delete($self->{"dtsDB"}->{$dataset_id}->{"loaded_flag"});
	delete($self->{"dtsDB"}->{$dataset_id}->{"load_data_dir"});
	delete($self->{"dtsDB"}->{$dataset_id}->{"load_archive_dir"});

	# Insert the note into the dts.note table and hook it up with the dataset.
	if (defined($self->{"dtsDB"}->{$dataset_id}->{"note"}) && $self->{"dtsDB"}->{$dataset_id}->{"note"} ne "") {
	    $self->{"DTS"}->prepare("INSERT INTO dts.note(author_id,note_text,entered_date,row_revise_contact_id) VALUES(?,?,?,?)")
		->execute(1,$self->{"dtsDB"}->{$dataset_id}->{"note"},"CURRENT_DATE",1);
	    my $note_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"dts.note","note_id");
	    $self->{"DTS"}->prepare("INSERT INTO dts.dataset_note(dataset_id,note_id) VALUES(?,?)")
		->execute($dataset_id,$note_id);
	}
	delete($self->{"dtsDB"}->{$dataset_id}->{"note"});

	# Sanity Check to make sure all of the variables have been put into the database.
	foreach my $key (keys(%{ $self->{"dtsDB"}->{$dataset_id}})) {
	    printf("%s\n",$key);
	    $success_flag = 0;
	}
    }

    return $success_flag;
}

##----------------------------------------------------------------------------------
# @signature int insert_dataset_into_jedi()
# <p>Insert the datasets that have not been inserted into JEDI into the database.</p>
#
# @input $self The DLN2DTS Object that contains the dataset data and called the function.
# @output $success_flag 1 if all of the datasets were successfully inserted into the
# database or 0 if a problem occurred.
# @warning If the connection to the database has a problem, the function will die.
##----------------------------------------------------------------------------------
sub insert_datasets_into_jedi {
    my ($self) = @_;
    my $success_flag = 1;

    foreach my $dataset_id (keys(%{ $self->{"datasets"}})) {
	my $test_in_jedi = $self->{"DTS"}->selectall_arrayref("SELECT COUNT(*) > 0 FROM jedi7.dataset WHERE dataset_id='$dataset_id'");

	# Dataset is not in JEDI, so insert it!
	if (!$test_in_jedi->[0]->[0]) {
	    # Add the dataset to the dataset table
	    my $INSERT = "INSERT INTO jedi7.dataset(dataset_id,name,description,begin_date,end_date,minlat,minlon,maxlat,maxlon,source_contact_id,internal_contact_id,onlineorderable) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)";
	    $self->{"DTS"}->prepare($INSERT)->execute($dataset_id,
						      $self->{"datasets"}->{$dataset_id}->{"name"},
						      $self->{"datasets"}->{$dataset_id}->{"description"},
						      $self->{"datasets"}->{$dataset_id}->{"begin_date"},
						      $self->{"datasets"}->{$dataset_id}->{"end_date"},
						      $self->{"datasets"}->{$dataset_id}->{"minlat"},
						      $self->{"datasets"}->{$dataset_id}->{"minlon"},
						      $self->{"datasets"}->{$dataset_id}->{"maxlat"},
						      $self->{"datasets"}->{$dataset_id}->{"maxlon"},
						      $self->{"datasets"}->{$dataset_id}->{"source_contact_id"},
						      defined($self->{"datasets"}->{$dataset_id}->{"internal_contact_id"}) ? $self->{"datasets"}->{$dataset_id}->{"internal_contact_id"} : 1,
						      $self->{"datasets"}->{$dataset_id}->{"onlineorderable"});

	    # Add the projects for the dataset to the dataset_project table.
	    $INSERT = "INSERT INTO jedi7.dataset_project(dataset_id,project_id) VALUES(?,?)";
	    foreach my $project_id (keys(%{ $self->{"datasets"}->{$dataset_id}->{"projects"}})) {
		if ($self->{"datasets"}->{$dataset_id}->{"projects"}->{$project_id}) {
		    $self->{"DTS"}->prepare($INSERT)->execute($dataset_id,$project_id);
		}
	    }

	    # Delete the data that has already been inserted into the database.
	    # (This is to help with making sure that all of the data has been inserted and to free up the space again.)
	    delete($self->{"datasets"}->{$dataset_id}->{"name"});
	    delete($self->{"datasets"}->{$dataset_id}->{"description"});
	    delete($self->{"datasets"}->{$dataset_id}->{"begin_date"});
	    delete($self->{"datasets"}->{$dataset_id}->{"end_date"});
	    delete($self->{"datasets"}->{$dataset_id}->{"minlat"});
	    delete($self->{"datasets"}->{$dataset_id}->{"minlon"});
	    delete($self->{"datasets"}->{$dataset_id}->{"maxlat"});
	    delete($self->{"datasets"}->{$dataset_id}->{"maxlon"});
	    delete($self->{"datasets"}->{$dataset_id}->{"source_contact_id"});
	    delete($self->{"datasets"}->{$dataset_id}->{"internal_contact_id"});
	    delete($self->{"datasets"}->{$dataset_id}->{"onlineorderable"});
	    delete($self->{"datasets"}->{$dataset_id}->{"projects"});
	}

	# Need to get the dataset name from JEDI to use for part of merging categories.
	my $dataset_name = $self->{"DTS"}->selectall_arrayref("SELECT name from jedi7.dataset WHERE dataset_id='$dataset_id'")->[0]->[0];

	# Handle the categories that were parsed off of the dataset name.
	foreach my $category (keys(%{ $self->{"datasets"}->{$dataset_id}->{"cats"}})) {

	    if ($category eq "Model") { $category = "Models/Analyses"; }
	    elsif ($category eq "Subsurface") { $category = "Sub-Surface"; }
	    elsif ($category eq "Ancillary") { $category = "Ancillary Info"; }
	    elsif ($category eq "GPS" && ($dataset_name =~ /precipitable water/i || $dataset_name eq "Mobile Data [Purdue University]")) { 
		$category = "Upper Air";
	    } elsif ($category eq "Mesonet") { $category = "Surface"; }
	    elsif ($category eq "Profiler/Sodar") { $category = "Upper Air"; }
	    # Assume all radiation categories are already correct since it could vary
	    elsif ($category eq "Radiation") { next; }

	    # Sanity check to make sure the category is to be assigned to the dataset
	    if ($self->{"datasets"}->{$dataset_id}->{"cats"}->{$category}) {
		my $cat_data = $self->{"DTS"}->selectall_arrayref("SELECT category_id,name FROM jedi7.category WHERE name='$category'");
		if (defined($cat_data->[0]->[0])) {
		    my $ds_data = $self->{"DTS"}->selectall_arrayref("SELECT COUNT(*) > 0 FROM jedi7.dataset_category WHERE dataset_id='$dataset_id' AND category_id=".$cat_data->[0]->[0]);
		    if ($ds_data->[0]->[0]) {
			# Dataset is already linked to the category.  Ignore and go on.
			next;
		    } else {
			# Dataset needs to be linked to the category.
			$self->{"DTS"}->prepare("INSERT INTO jedi7.dataset_category(dataset_id,category_id) VALUES(?,?)")
			    ->execute($dataset_id,$cat_data->[0]->[0]);
		    }
		} else {
		    printf("Can't find category (%s) in JEDI database (%s)!\n",$category,$dataset_name);
		    $success_flag = 0;
		}
	    }
	}
	delete($self->{"datasets"}->{$dataset_id}->{"cats"});

	# Insert the remote url as an xlink and hook it up wi the dataset.
	if (defined($self->{"datasets"}->{$dataset_id}->{"remote_url"})) {
	    $self->{"DTS"}->prepare("INSERT INTO jedi7.xlink(href,purpose) VALUES(?,?)")
		->execute($self->{"datasets"}->{$dataset_id}->{"remote_url"},"info");
	    my $link_id = $self->{"DTS"}->last_insert_id(undef(),undef(),"jedi7.xlink","xlink_id");
	    $self->{"DTS"}->prepare("INSERT INTO jedi7.dataset_xlinks(dataset_id,xlink_id) VALUES(?,?)")
		->execute($dataset_id,$link_id);
	    delete($self->{"datasets"}->{$dataset_id}->{"remote_url"});
	}

	# Sanity check to make sure that all of the dataset information was put into JEDI
	if (keys(%{ $self->{"datasets"}->{$dataset_id}}) > 0) {
	    printf("%s\n",$dataset_id);
	    foreach my $key (sort(keys(%{ $self->{"datasets"}->{$dataset_id}}))) {
		printf("\t%s: %s\n",$key,$self->{"datasets"}->{$dataset_id}->{$key});
	    }
	    $success_flag = 0;
	}
    }

    return $success_flag;
}

##----------------------------------------------------------------------------------
# @signature void insert_dts_project(String project_id, int active)
# <p>Insert the specified into the project table of the DTS database.</p>
#
# @input $self The DLN2DTS object that called the function.
# @input $project_id The project to be inserted into the database.
# @input $active The flag for the project to mark it as active.
# @warning The function will die if the project cannot be inserted.
##----------------------------------------------------------------------------------
sub insert_dts_project {
    my ($self,$project_id,$active) = @_;
    my $INSERT = "INSERT INTO dts.project(project_id,active) VALUES(?,?)";

    $self->{"DTS"}->prepare($INSERT)->execute($project_id,$active);
}

##----------------------------------------------------------------------------------
# @signature DLN2DTS new()
# <p>Create a new DLN2DTS Object.</p>
#
# @output $self The new object.
##----------------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $class = ref($invocant) || $invocant;
    my $self = {};
    bless($self,$class);

    return $self;
}

##----------------------------------------------------------------------------------
# @signature int project()
# <p>Port the DLN projects to JEDI and DTS.</p>
#
# @input $self The DLN2DTS object that called the function.
# @output $success_flag 1 if the projects were successfully ported, 0 if not.
##----------------------------------------------------------------------------------
sub project {
    my ($self) = @_;
    my $SELECT = "SELECT pname as project_id, storm_id_prefix as dataset_id_prefix, active FROM project";
    my $success_flag = 1;

    my $data = $self->{"DLN"}->selectall_hashref($SELECT,"project_id");
    foreach my $key (keys(%{$data})) {
	foreach my $project_id ($self->dln2dts_project_id($key)) {
	    $success_flag = $self->project_jedi_test($project_id) && $success_flag;
	    $success_flag = $self->project_jedi_prefix_test($project_id,$data->{$key}->{"dataset_id_prefix"}) 
		&& $success_flag;
	    $self->insert_dts_project($project_id,$data->{$key}->{"active"});
	}
    }

    return $success_flag;
}

##----------------------------------------------------------------------------------
# @signature int project_jedi_test(String project_id)
# <p>Test the specified project with JEDI to make sure that it exists.</p>
#
# @input $self The DLN2DTS object that called the function.
# @input $project_id The id of the project to be tested in JEDI.
# @output $success_flag 1 if the project exists in JEDI, 0 if it does not.
##----------------------------------------------------------------------------------
sub project_jedi_test {
    my ($self,$project_id) = @_;
    my $SELECT = "SELECT COUNT(project_id) FROM jedi7.project WHERE project_id='$project_id'";

    my $data = $self->{"DTS"}->selectall_arrayref($SELECT);
    
    return 1 if ($data->[0]->[0] == 1);

    printf("The project with project_id='%s' does not have a count of 1 in JEDI!  Has count of: %d!\n",
	   $project_id,$data->[0]->[0]);
    return 0;
}

##----------------------------------------------------------------------------------
# @signature int project_jedi_prefix_test(String project_id, int dataset_id_prefix)
# <p>Test the dataset_id_prefix with the value in JEDI for the project.</p>
#
# @input $self The DLN2DTS object that called the function.
# @input $project_id The project id being tested in the database.
# @input $dataset_id_prefix The DLN prefix to be tested in JEDI.
# @output $success_flag 1 if the prefixes match, 0 if they do not.
##----------------------------------------------------------------------------------
sub project_jedi_prefix_test {
    my ($self,$project_id,$dataset_id_prefix) = @_;
    my $SELECT = "SELECT dataset_id_prefix FROM jedi7.dataset_prefix_project WHERE project_id='$project_id'";
    
    my $data = $self->{"DTS"}->selectall_arrayref($SELECT);


    if (@$data) {
	my $success_flag = 0;
	my @prefixes = ();
	foreach my $row (@$data) {
	    $success_flag = $success_flag || $row->[0] == $dataset_id_prefix;
	    push(@prefixes,$row->[0]);
	}
	return $success_flag if ($success_flag);

	printf("Project %s does not have prefix %d in JEDI!  It has prefixes %s.\n",
	       $project_id,$dataset_id_prefix,join(" ",@prefixes));
	return $success_flag;
    } else {
	printf("Project %s does not have a dataset_prefix_id assigned in JEDI!\n",$project_id);
	return 0;
    }
}

##----------------------------------------------------------------------------------
# @signature void setup()
# <p>Initialize the database connections and prepare all necessary variables before
# the porting is to begin.</p>
#
# @input $self The DLN2DTS object that called the function.
##----------------------------------------------------------------------------------
sub setup {
    my ($self) = @_;

    # Defined the database connections used in the porting.
    $self->{"DLN"} = DBI->connect("DBI:mysql:database=dln;host=hurricane.joss.ucar.edu",
				  "dln","",{RaiseError => 1});
    $self->{"DTS"} = DBI->connect("DBI:mysql:host=127.0.0.1",
				  "dts","manage-all",{AutoCommit => 0, RaiseError => 1});
}

##----------------------------------------------------------------------------------
# @signature void teardown()
# <p>Cleanly close down the database connections and exit the script.</p>
#
# @input $self The DLN2DTS object that called the function.
##----------------------------------------------------------------------------------
sub teardown {
    my ($self) = @_;

    $self->{"DTS"}->rollback();

    $self->{"DLN"}->disconnect();
    $self->{"DTS"}->disconnect();

    exit(0);
}

##----------------------------------------------------------------------------------
# @signature String trim(String line)
# <p>Remove all of the leading and trailing white space of a String.</p>
#
# @input $line The String to be trimmed.
# @output $line The trimmed String.
##----------------------------------------------------------------------------------
sub trim {
    my $line = shift;
    return $line if (!defined($line));
    $line =~ s/^\s+//;
    $line =~ s/\s+$//;
    return $line;
}

##----------------------------------------------------------------------------------
# @signature int user()
# <p>Port the user table in the DLN to the DTS.
#
# @input $self The DLN2DTS object that called the function.
# @output $success_flag 1 if all of the users were successfully ported to the DTS
# and JEDI database, 0 if any problem occurred.
##----------------------------------------------------------------------------------
sub user {
    my ($self) = @_;
    my $SELECT = "SELECT LOWER(uid) as contact_short_name, CONCAT(first_name,' ',last_name) as person_name, email, active FROM user";
    my $success_flag = 1;

    my $data = $self->{"DLN"}->selectall_hashref($SELECT,"contact_short_name");

    foreach my $contact_short_name (keys(%{$data})) {
	$success_flag = $self->user_jedi_test($contact_short_name,$data->{$contact_short_name}->{"person_name"},
					      $data->{$contact_short_name}->{"email"},
					      $data->{$contact_short_name}->{"active"}) && $success_flag;
    }

    return $success_flag;
}

##----------------------------------------------------------------------------------
# @signature int user_jedi_test(String contact_short_name, String person_name, String email, int active_editor)
# <p>Compare the DLN user with the corresponding user in JEDI to make sure that all
# of the data for the user is the same.</p>
#
# @input $self The DLN2DTS object that called the function.
# @input $contact_short_name The identifier used by the DLN database to identify a
# user, which should be the jedi.contact_short_name.
# @input $person_name The display name of the user.
# @input $email The email address of the user.
# @input $active_editor The flag that marks the user as an active editor of the DLN.
# @output $success_flag 1 if all of the values match between the DLN and JEDI, 0 if 1 or
# more values do not match.
##----------------------------------------------------------------------------------
sub user_jedi_test {
    my ($self,$contact_short_name,$person_name,$email,$active_editor) = @_;
    my $SELECT = "SELECT contact_id,person_name,email,active_editor FROM jedi7.contact WHERE contact_short_name='$contact_short_name'";

    my $data = $self->{"DTS"}->selectall_arrayref($SELECT);

    if (@$data == 1) {
	my $success_flag = 1;

	# Test the person name between DLN and JEDI
	if ($person_name ne $data->[0]->[1]) {
	    printf("User %s (%s) has a different display name that JEDI (%s)!\n",
		   $person_name,$contact_short_name,$data->[0]->[1]);
	    $success_flag = 0;
	}

	# Test the email addresses between DLN and JEDI
	if ((!defined($email) && defined($data->[0]->[2])) || 
	    (defined($email) && !defined($data->[0]->[2])) || 
	    (defined($email) && defined($data->[0]->[2]) && $email ne $data->[0]->[2])) {
	    printf("User %s (%s) email address %s does not match %s from JEDI!\n",$person_name,
		   $contact_short_name,defined($email) ? $email : "undef",
		   defined($data->[0]->[2]) ? $data->[0]->[2] : "undef");
	    $success_flag = 0;
	}

	# Test the active editor flag between DLN and JEDI
	if ($active_editor != $data->[0]->[3]) {
	    printf("User %s (%s) is %s in DLN and %s in JEDI!\n",$person_name,$contact_short_name,
		   $active_editor ? "active" : "not active",
		   $data->[0]->[3] ? "active" : "not active");
	    $success_flag = 0;
	}

	# Assign the DLN uid to the jedi.contact_id for mapping during dataset porting.
	$self->{"users"}->{$contact_short_name} = $data->[0]->[0];

	return $success_flag;
    } elsif (@$data > 1) {
	printf("Multiple entries in JEDI for contact_short_name='%s'\n",$contact_short_name);
	return 0;
    } elsif ($contact_short_name eq "unassigned") {
	# Make all assigned entries be NULL
	$self->{"users"}->{$contact_short_name} = undef();
	return 1;
    } elsif ($contact_short_name eq "other") {
	# Make all "unknown/other" people be the generic local user.
	$self->{"users"}->{$contact_short_name} = 1;
	return 1;
    } else {
	printf("JEDI does not contain contact_short_name='%s'\n",$contact_short_name);
	return 0;
    }
}
