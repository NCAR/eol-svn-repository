#!/usr/bin/perl -w

#***********************************************************************
# make_sql_updates_for_ML_datasets.pl
#
# Read list of Master List datasets with dataset_ids that match ML.xxxx
# and urls that are actual EOL dataset URLs. Change ML dataset_id to 
# the archive_ident for the dataset, and update the url to our current
# URL syntax for dataset links.
#
# input: MLxxxx_datasets-input.list
#   created with MySQL command from the ML dmg_merged_ml database
#
# output:  MLxxxx_datasets-changes_pt1.sql and MLxxxx_datasets-changes_pt2.sql
#   MySQL statements to update database 
#
# SELECT
#      CONCAT_WS(';',
#        dataset.dataset_id,
#        dataset.url,
#        dataset.doc_url,
#        classification.name,
#        dataset_project.project_id)
# FROM
#      dataset
#      INNER JOIN dataset_classification
#       ON dataset.dataset_id = dataset_classification.dataset_id
#      INNER JOIN classification
#       ON dataset_classification.class_id = classification.class_id
#      INNER JOIN dataset_project
#       ON dataset.dataset_id = dataset_project.dataset_id
# WHERE
#      dataset.dataset_id LIKE "ML%"
#      AND dataset.url LIKE "%data.eol.ucar.edu/%"
#      AND dataset.url NOT LIKE "%/master_list/%"
#      AND dataset.url NOT LIKE "%codiac/projs%"
#      AND dataset.url NOT LIKE "%/codiac_data/%"
# ORDER BY
#      dataset.url ASC;
#
#   And then the leading "| " was stripped from each line, the 
#      trailing "|", and then used this series of keystrokes 
#      in vile to delete spaces at the end of each line: 
#        :%s/\s\+$//g
#        
# October 26, 2018
#  updated Nov 29
#     "    Feb 11
#  -ds
#***********************************************************************

my $infile = "../docs/MLxxxx_datasets-input.list";
my $outfile1 = $infile;
my $outfile2 = $infile;
$outfile1 =~ s/input\.list$/changes_pt1\.sql/;
$outfile2 =~ s/input\.list$/changes_pt2\.sql/;
my $logfile = "logs/MLxxxx_sql_updates.log";

# Open the files for reading.
open(INFILE, $infile) || die "could not open $infile";
open(OUTFILE_1, ">$outfile1") || die "Can't open $outfile1: $!\n";
open(OUTFILE_2, ">$outfile2") || die "Can't open $outfile2: $!\n";
open(LOGFILE, ">$logfile") || die "Can't open $logfile: $!\n";

#***********************************************************************
# Read line
# split line at ';' separator
# match archive_ident at end of URL and save to variable
# update dataset_id: use archive_ident
# update url: use new syntax for it
# connect new dataset_id to classification, project, doc_url
#  that were connected to the MLxxxx dataset_id
# remove MLxxxx connections once read and used
# tally how many EOL and MLxxxx datasets were processed
#***********************************************************************

$missing = "999.999";
$last_archive_ident = $missing;
$doc_url="";

print "Now reading $infile\n";
print OUTFILE_1 "begin;\n";
print OUTFILE_2 "begin;\n";
while ($this_line = <INFILE>) {
    print LOGFILE "\n$this_line";
    chomp($this_line);
    ($MLxxxx, $ds_url, $ds_doc_url, $ds_classification, $ds_project) = split(/;/, $this_line);   
    if ($ds_url =~ /\d{1,3}\.\d{1,4}$/) {
        $archive_ident=$&;
        print LOGFILE "$MLxxxx has archive ident = $archive_ident\n";
    } else {
        print LOGFILE "no archive ident found for ML dataset_id=$MLxxxx!";
        print LOGFILE "\n$this_line\n\n";
        next:
    }
     
    if ($last_archive_ident eq $missing) {				# first time through
    	print OUTFILE_1 "update dataset set dataset_id=\"$archive_ident\" where dataset_id=\"$MLxxxx\";\n";
    	print OUTFILE_1 "update dataset set url=\"https:\/\/data.eol.ucar.edu\/dataset\/$archive_ident\" where dataset_id=\"$archive_ident\";\n";
		$category{$archive_ident}{$ds_classification} = 1;
		$project{$archive_ident}{$ds_project} = 1;
        $dataset{$archive_ident} = 1;
        $MLx{$MLxxxx} = 1;
		if ($ds_doc_url) {                                  # don't take incomplete doc urls (with no doc at end)
			$doc_url{$archive_ident} = $ds_doc_url unless ($ds_doc_url =~ /\/nph-get\/$archive_ident\/$/);
		}	
		print LOGFILE "project: $ds_project\n";
		$ds_MLxxxx = $MLxxxx;
		$last_archive_ident = $archive_ident;
	} elsif ($archive_ident eq $last_archive_ident) {	# if same EOL dataset
		$category{$archive_ident}{$ds_classification} = 1;
		$project{$archive_ident}{$ds_project} = 1;
		if ($ds_doc_url) {                                  # don't take incomplete doc urls (with no doc at end)
			$doc_url{$archive_ident} = $ds_doc_url unless ($ds_doc_url =~ /\/nph-get\/$archive_ident\/$/);
		}
		if ($MLxxxx ne $ds_MLxxxx) {
            $MLx{$MLxxxx} = 1;
			print OUTFILE_1 "delete from dataset_classification where dataset_id=\"$ds_MLxxxx\";\n";
			print OUTFILE_1 "delete from dataset_project where dataset_id=\"$ds_MLxxxx\";\n";
            print OUTFILE_1 "delete from dataset where dataset_id=\"$ds_MLxxxx\";\n";
			$ds_MLxxxx = $MLxxxx;
		}							
		print LOGFILE "project: $ds_project\n";
	} else {												# if a new archive_ident in the dataset url = a new EOL dataset
        if (!exists($dataset{"$archive_ident"})) {
    	    print OUTFILE_1 "update dataset set dataset_id=\"$archive_ident\" where dataset_id=\"$MLxxxx\";\n";
    	    print OUTFILE_1 "update dataset set url=\"https:\/\/data.eol.ucar.edu\/dataset\/$archive_ident\" where dataset_id=\"$archive_ident\";\n";
        } 
		$category{$archive_ident}{$ds_classification} = 1;
		$project{$archive_ident}{$ds_project} = 1;
        $dataset{$archive_ident} = 1;
		if ($ds_doc_url) {                                  # don't take incomplete doc urls (with no doc at end)
			$doc_url{$archive_ident} = $ds_doc_url unless ($ds_doc_url =~ /\/nph-get\/$archive_ident\/$/);
		}
		if ($MLxxxx ne $ds_MLxxxx) {
            $MLx{$MLxxxx} = 1;
			print OUTFILE_1 "delete from dataset_classification where dataset_id=\"$ds_MLxxxx\";\n";
			print OUTFILE_1 "delete from dataset_project where dataset_id=\"$ds_MLxxxx\";\n";
            print OUTFILE_1 "delete from dataset where dataset_id=\"$ds_MLxxxx\";\n";
		    $ds_MLxxxx = $MLxxxx;
		}							
		print LOGFILE "project: $ds_project\n";
		$last_archive_ident = $archive_ident;
	}	
}  # end while infile
 # cleanup
print OUTFILE_1 "delete from dataset_classification where dataset_id=\"$ds_MLxxxx\";\n";
print OUTFILE_1 "delete from dataset_project where dataset_id=\"$ds_MLxxxx\";\n";
print OUTFILE_1 "delete from dataset where dataset_id=\"$ds_MLxxxx\";\n";

#---------------------------------------------------------------------
# go through hashes and print out formatted sql statements to
#   add new links from datasets to classifications, projects, and docs
#---------------------------------------------------------------------
for $archive_ident (sort keys %doc_url) {
    print OUTFILE_2 "update dataset set doc_url=\"$doc_url{$archive_ident}\" where dataset_id=\"$archive_ident\";\n";
}
    
for $archive_ident (sort keys %category) {     
    for $classification (keys %{$category{$archive_ident}})  {
		print OUTFILE_2 "insert into dataset_classification (dataset_id, class_id) values (\'$archive_ident\', (SELECT class_id FROM classification WHERE name=\'$classification\'));\n";
    }
}

for $archive_ident (sort keys %project) {     
    for $proj (keys %{$project{$archive_ident}})  {
		print OUTFILE_2 "insert into dataset_project (dataset_id, project_id) values (\'$archive_ident\', \'$proj\');\n";
    }
}

$num_ds = keys %dataset;
print LOGFILE "\nnumber of unique EOL datasets handled: $num_ds\n";
print "number of unique EOL datasets handled: $num_ds\n";
$num_ml = keys %MLx;
print "number of unique MLxxxx dataset_ids handled: $num_ml\n";
print LOGFILE "number of unique MLxxxx dataset_ids handled: $num_ml\n";
print "wrote $outfile1 and $outfile2\n";

close(INFILE);
close(OUTFILE_1);
close(OUTFILE_2);
close(LOGFILE);
