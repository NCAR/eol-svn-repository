#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------------------
# <p>This script updates the dmg_ml database for the general master list to change all JOSS
# URLs in the dataset link and the documentation files to their replacements at EOL.  It
# searches for all URLs that reference http://www.joss.ucar.edu or that use a relative path
# that begins with a / and replaces them with the equivalent URL on http:://data.ucar.edu.</p>
# <p>The script also calls mysqldump before any processing to create a state file of the
# master list database before any changes for a reference and a backup file if something
# goes wrong.</p>
##Module-------------------------------------------------------------------------------------
use strict;
use DBI;

&main();

##-------------------------------------------------------------------------------------------
# @signature void main()
# <p>Run the script.</p>
##-------------------------------------------------------------------------------------------
sub main {

    # Store the current contents of the database into a file.
    dump_database();

    # Define the database connection.
    my $connection = DBI->connect("DBI:mysql:database=dmg_ml;host=data.eol.ucar.edu","dts-full","l\@micbc");

    # Get the list of projects in the database.
    my @projects = get_projects($connection);

    # Change all of the datasets for each project.
    foreach my $project (@projects) {
	printf("Processing project: %s...\n",$project);
	
	my @datasets = get_datasets($connection,$project);
	foreach my $dataset (@datasets) {
            # Update the dataset URL link in the database.
	    my $dataset_id = update_url($connection,$project,$dataset);
            # Update the dataset documentation URL link in the database.
            update_doc($connection,$project,$dataset,$dataset_id);
	}
    }

    # Properly close the database connection
    $connection->disconnect();
}

##--------------------------------------------------------------------------------------------
# @signature void dump_database()
# <p>Copy the entire database into a dump file to use as a save state for reference purposes
# or a backup file if something goes wrong.</p>
# @warning This requires the user of the script to enter the dmg user password for the 
# database.
##--------------------------------------------------------------------------------------------
sub dump_database {
    my $filename = "/export/web/dmg/html/tmp/dmg_ml_joss_url_dump.sql";
    unlink($filename) if (-e $filename);

    printf("\n\nEnter dmg password for user dmg:\n");
    system("mysqldump dmg_ml -u dmg -p -c > $filename");
}

##--------------------------------------------------------------------------------------------
# @signature int[] get_datasets(*connection, String project)
# <p>Get the master list ids for all of the data sets for the specified project.</p>
# @input $connection The reference to the database connection to be used.
# @input $project The project name as defined as part of the master list table name.
# @output @datasets The list of data set ids for the project.
##--------------------------------------------------------------------------------------------
sub get_datasets {
    my ($connection,$project) = @_;
    my @datasets = ();

    my $sql = "SELECT id FROM ds$project";
    my $stmt = $connection->prepare($sql);
    $stmt->execute();
    while (my @row = $stmt->fetchrow_array()) {
       push(@datasets,$row[0]);
    }

    return @datasets;
}

##---------------------------------------------------------------------------------------------
# @signature String[] get_projects(*connection)
# <p>Get the projects in the master list defined in the names of the tables.</p>
# @input $connection The reference to the database connection to be used.
# @output @projects The list of projects in the database.
##---------------------------------------------------------------------------------------------
sub get_projects {
    my ($connection) = @_;
    my @projects = ();

    my $sql = "SHOW TABLES LIKE 'ds%'";
    my $stmt = $connection->prepare($sql);
    $stmt->execute();
    while (my @row = $stmt->fetchrow_array()) {
	$row[0] =~ /^ds(.+)/;
	push(@projects,$1) if ($1 ne "TEST");
    }

    return @projects;
}

##---------------------------------------------------------------------------------------------
# @signature void update_doc(*connection, String project, int $dataset, String $dataset_id)
# <p>Update the documentation URL for the specified project's dataset in the database.</p>
# @input $connection The reference to the database connection to be used.
# @input $project The name of the project that the dataset to be updated is assigned.
# @input $dataset The master list id of the dataset to be updated.
# @input $dataset_id The commonly known dataset id for the dataset to be updated.
##---------------------------------------------------------------------------------------------
sub update_doc {
    my ($connection,$project,$dataset,$dataset_id) = @_;

    my $sql = "SELECT document,data_set FROM ds$project WHERE id=?";
    my $stmt = $connection->prepare($sql);
    $stmt->execute($dataset);

    while (my @row = $stmt->fetchrow_array()) {

        # Don't care if the document field is not defined or empty.        
        return if (!defined($row[0]) || $row[0] =~ /^\s*$/);

        # Remove any leading or trailing white space.
        $row[0] =~ s/^\s*//;
        $row[0] =~ s/\s*$//;

        # Add the full path to the URL if it used a relative path.
        my $doc = $row[0];
        if ($doc =~ /$\//) { $doc = sprintf("http://www.joss.ucar.edu%s",$doc); }

        # Handle the cases when the file was located in /web/data
        if ($doc =~ /^http:\/\/www\.joss\.ucar\.edu\/data\/(.+)\/([^\/]+)$/) {
            my $filename = $2;

            # Use the best URL for the doc if we know the common dataset id
            if (defined($dataset_id)) {
                $doc = sprintf("http://data.eol.ucar.edu/datafile/nph-get/%s/%s",$dataset_id,$filename);
            }
            # or use the link set up by John so there is still a way to directly access a file
            else {
                $doc = sprintf("http://data.eol.ucar.edu/codiac_data/%s/%s",$1,$filename);
            }
        # Handle special cases for docs that were on the system that were not in /web/data
        } elsif ($doc =~ /^http:\/\/www\.joss\.ucar\.edu\/gapp\/satellite\/noaa_list.html(.+)$/) {
            $doc = sprintf("http://www.eol.ucar.edu/projects/gapp/dm/satellite/noaa_list.html%s",$1);
        } elsif ($doc =~ /^http:\/\/www\.joss\.ucar\.edu\/name\/documentation\/(.+)$/) {
            $doc = sprintf("http://www.eol.ucar.edu/projects/name/documentation/%s",$1);
        } elsif ($doc =~ /^http:\/\/www\.joss\.ucar\.edu\/indoex\/dm\/(.+)$/) {
            $doc = sprintf("http://www.eol.ucar.edu/projects/indoex/dm/%s",$1);
        } elsif ($doc =~ /http:\/\/www\.joss\.ucar\.edu\/pacs\/tepps_doc\/(.+)/) {
            $doc = sprintf("http://www.eol.ucar.edu/projects/pacs/tepps_doc/%s",$1);
        } elsif ($doc =~ /^http:\/\/www\.joss\.ucar\.edu/) {
            printf("Found a www.joss URL: %s\n",$doc);
        }

        printf("Changed Doc URL: %s to %s\n",$row[0],$doc) if ($row[0] ne $doc);

        # Update the document URL in the database.
        my $update = "UPDATE ds$project SET document=? WHERE id=?";
        my $update_stmt = $connection->prepare($update);
        $update_stmt->execute($doc,$dataset);
    }
}

##--------------------------------------------------------------------------------------------
# @signature String update_url(*connection, String project, int dataset)
# <p>Update the dataset URL in the master list when it points to the CODIAC system for the
# specified project's dataset.</p>
# @input $connection The reference to the database connection to be used.
# @input $project The name of the project in the master list that is assigned to the dataset.
# @input $dataset The master list's dataset id of the dataset to be updated.
# @output $dataset_id The commonly known dataset id of the dataset that was updated.
##--------------------------------------------------------------------------------------------
sub update_url {
    my ($connection,$project,$dataset) = @_;
    my $dataset_id;

    my $sql = "SELECT link FROM ds$project WHERE id=?";
    my $stmt = $connection->prepare($sql);
    $stmt->execute($dataset);

    while (my @row = $stmt->fetchrow_array()) {

        # Don't care if there is not a link for the dataset.
	return if (!defined($row[0]));

        # Change all relative URLs to the full URL.
        my $url = $row[0];
        if ($url =~ /^\//) { $url = sprintf("http://www.joss.ucar.edu%s",$url); }

        # Change all URLs that point to a CODIAC dataset to the EOL URL and parse out the dataset id
        # to be used with the document link later.
        if ($url =~ /^http:\/\/www\.joss\.ucar\.edu\/cgi\-bin\/codiac\/dss.?\?(\d[\d\.]+)$/) {
            $dataset_id = $1;
            $url = sprintf("http://data.eol.ucar.edu/codiac/dss/id=%s",$dataset_id);
            printf("Changed URL: %s to %s\n",$row[0],$url);
        }

        # Update the dataset URL in the database for the dataset.
        my $update = "UPDATE ds$project SET link=? WHERE id=?";
        my $update_stmt = $connection->prepare($update);
        $update_stmt->execute($url,$dataset);
    }

    return $dataset_id;
}
