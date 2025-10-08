#! /usr/bin/perl -w

##Module-------------------------------------------------------------------------------------
# <p>This script updates the dmg_arctic database for the arctic master list to change all JOSS
# URLs in the dataset link and the documentation files to their replacements at EOL.  It
# searches for all URLs that reference http://www.joss.ucar.edu or that use a relative path
# that begins with a / and replaces them with the equivalent URL on http://data.eol.ucar.edu.</p>
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
    my $connection = DBI->connect("DBI:mysql:database=dmg_arctic;host=data.eol.ucar.edu","dts-full","l\@micbc");

    my @datasets = get_datasets($connection);
    foreach my $dataset (@datasets) {
	# Update the dataset URL link in the database.
	my $dataset_id = update_url($connection,$dataset);
	# Update the dataset documentation URL link in the database.
	update_doc($connection,$dataset,$dataset_id);
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
    my $filename = "/export/web/dmg/html/tmp/dmg_arctic_joss_url_dump.sql";
    unlink($filename) if (-e $filename);

    printf("\n\nEnter dmg password for user dmg:\n");
    system("mysqldump dmg_ml -u dmg -p -c > $filename");
}

##--------------------------------------------------------------------------------------------
# @signature int[] get_datasets(*connection)
# <p>Get the master list ids for all of the data sets.</p>
# @input $connection The reference to the database connection to be used.
# @output @datasets The list of data set ids for the project.
##--------------------------------------------------------------------------------------------
sub get_datasets {
    my ($connection) = @_;
    my @datasets = ();

    my $sql = "SELECT id FROM List";
    my $stmt = $connection->prepare($sql);
    $stmt->execute();
    while (my @row = $stmt->fetchrow_array()) {
       push(@datasets,$row[0]);
    }

    return @datasets;
}

##---------------------------------------------------------------------------------------------
# @signature void update_doc(*connection, int $dataset, String $dataset_id)
# <p>Update the documentation URL for the specified dataset in the database.</p>
# @input $connection The reference to the database connection to be used.
# @input $dataset The master list id of the dataset to be updated.
# @input $dataset_id The commonly known dataset id for the dataset to be updated.
##---------------------------------------------------------------------------------------------
sub update_doc {
    my ($connection,$dataset,$dataset_id) = @_;

    my $sql = "SELECT doc_url FROM List WHERE id=?";
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
        if ($doc =~ /^\//) { $doc = sprintf("http://www.joss.ucar.edu%s",$doc); }

        # Handle the cases when the file was located in /web/data
        if ($doc =~ /^http:\/\/www\.joss\.ucar\.edu\/data\/(.+)\/([^\/]+)$/) {
            $doc = sprintf("http://data.eol.ucar.edu/codiac_data/%s/%s",$1,$2);

	} elsif ($doc =~ /^http:\/\/www\.joss\.ucar\.edu\/sbi\/(.+)$/) {
	    $doc = sprintf("http://www.eol.ucar.edu/projects/sbi/%s",$1);

        } elsif ($doc =~ /^http:\/\/www\.joss\.ucar\.edu/) {
            printf("Found a www.joss URL: %s\n",$doc);
        }

        printf("Changed Doc URL: %s to %s\n",$row[0],$doc) if ($row[0] ne $doc);

        # Update the document URL in the database.
        my $update = "UPDATE List SET doc_url=? WHERE id=?";
        my $update_stmt = $connection->prepare($update);
        $update_stmt->execute($doc,$dataset);
    }
}

##--------------------------------------------------------------------------------------------
# @signature String update_url(*connection, int dataset)
# <p>Update the dataset URL in the master list when it points to the CODIAC system for the
# specified dataset.</p>
# @input $connection The reference to the database connection to be used.
# @input $dataset The master list's dataset id of the dataset to be updated.
# @output $dataset_id The commonly known dataset id of the dataset that was updated.
##--------------------------------------------------------------------------------------------
sub update_url {
    my ($connection,,$dataset) = @_;
    my $dataset_id;

    my $sql = "SELECT url,storm_id FROM List WHERE id=?";
    my $stmt = $connection->prepare($sql);
    $stmt->execute($dataset);

    while (my @row = $stmt->fetchrow_array()) {

        # Don't care if there is not a link for the dataset.
	return if (!defined($row[0]));

        # Change all relative URLs to the full URL.
        my $url = $row[0];
        if ($url =~ /^\//) { $url = sprintf("http://www.joss.ucar.edu%s",$url); }

	$dataset_id = $row[1];

        # Change all URLs that point to a CODIAC dataset to the EOL URL and parse out the dataset id
        # to be used with the document link later.
        if ($url =~ /^http:\/\/www\.joss\.ucar\.edu\/cgi\-bin\/codiac\/dss.?\?(\d[\d\.]+)$/) {
            $url = sprintf("http://data.eol.ucar.edu/codiac/dss/id=%s",$1);
            printf("Changed URL: %s to %s\n",$row[0],$url);
        }

        # Update the dataset URL in the database for the dataset.
        my $update = "UPDATE List SET url=? WHERE id=?";
        my $update_stmt = $connection->prepare($update);
        $update_stmt->execute($url,$dataset);
    }

    return $dataset_id;
}
