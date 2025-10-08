#! /usr/bin/perl -w

##Module---------------------------------------------------------------------------------
# <p>The deploy script copies the files in the source directory to the deploy directory
# and changes the permissions and groups so they can be executed correctly.</p>
#
# @author Joel Clawson
##Module---------------------------------------------------------------------------------
use strict;
use File::Copy;

# Constants where the scripts can be found and where they should go.
my $DEPLOY_DIR = "/net/work/bin/scripts/arctic_data_scripts";
my $SRC_DIR = "src/new_arctic_scripts";

&main();

##---------------------------------------------------------------------------------------
# @signature void main()
# <p>Deploy the scripts in the source directory to the deploy directory and change the
# permissions and groups accordingly.</p>
##---------------------------------------------------------------------------------------
sub main {
    # Read the files in the source directory to see what needs to be deployed.
    opendir(my $SRC, $SRC_DIR) or die("Can't open $SRC_DIR\n");
    my @files = readdir($SRC);
    closedir($SRC);

    # Loop through the found files in the SRC_DIR.
    foreach my $file (@files) {
        # Ignore the . and .. directories, and any temp/backup files (*.swp or *~)
        unless ($file =~ /^[\.]+$/ || $file =~ /^\..|\.swp$/ || $file =~ /\~$/) {
            # Change the permissions of the previously deployed file.
            if (-e sprintf("%s/%s", $DEPLOY_DIR, $file)) {
                chmod(0774, sprintf("%s/%s", $DEPLOY_DIR, $file));
            }

            # Copy the SRC file to the DEPLOY directory.
            copy(sprintf("%s/%s", $SRC_DIR, $file), sprintf("%s/%s", $DEPLOY_DIR, $file)) or
                die("Can't copy file $file to $DEPLOY_DIR\n");

            # Change the permissions of the deployed file.
            chmod(0554, sprintf("%s/%s", $DEPLOY_DIR, $file));

            # Change the group of the file to be "cds".
            system(sprintf("chgrp cds %s/%s", $DEPLOY_DIR, $file));
        }
    }
}
