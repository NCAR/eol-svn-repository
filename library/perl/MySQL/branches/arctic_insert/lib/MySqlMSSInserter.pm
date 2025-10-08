#! /usr/bin/perl -w

##Module--------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDatabase.pm>Link to MySqlDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDataset.pm>Link to MySqlDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
# <p>The MySqlMSSInserter modules is a specialized MySqlInserter that 
# inserts files on the mass store system into the database.</p>
# <p>The only difference from the MySqlInserter modules is the 
# <code>create_files</code> function.  This creates MySqlMSSFile instances
# instead of MySqlFile instances along with changed commands to read the
# data from the mass store instead of reading files locally.</p>
#
# @author Joel Clawson
# @version 1.0 Original Creation.
##Module--------------------------------------------------------------------

package MySqlMSSInserter;

use strict;

use lib "/net/work/lib/perl/Utilities";
use lib "/net/work/lib/perl/hpss";

use HPSS;
use DpgDate qw(:DEFAULT);
use MySqlMSSFile;
use MySqlInserter;
our @ISA = ("MySqlInserter");

##--------------------------------------------------------------------------
# @signature void create_files(int index, String directory, String[]* files)
# <p>Create a new MySqlFile for each file that is to be inserted into
# the database.</p>
# @warning This function is recursive.  It will read all of the directories
# in the directory it was given (except for . and ..) and try to find more
# files that match the pattern.
#
# @input $index The index of the data to use from the config file.
# @input $directory The directory to be read.
# @input $files A reference to an array that contains the MySqlFile instances.
##--------------------------------------------------------------------------
sub create_files {
    my ($self,$index,$directory,$files) = @_;

    printf("Processing directory: %s\n",$directory);

    # Read in the file lines.
    my @dir_files = grep(/^(\-|d)/,HPSS:ls($directory, "-l"));

    my $pattern = $self->{"cfg"}->{$index}->{"pattern"};

    # Loop through the files
    foreach my $file (@dir_files) {
	my @file_data = split(' ',$file);

	# Process directories under this directory
	if ($file_data[0] =~ /^d/ && $file_data[8] !~ /^\.+$/) {
	    $self->create_files($index,sprintf("%s/%s",$directory,$file_data[8]),$files);
	} elsif ($file_data[8] =~ /^$pattern$/) {
	    # Process files that match the pattern.
	    my $mysql = MySqlMSSFile->new();
	    
	    $self->set_dates($mysql,$file_data[8],$index);
	    $mysql->setDatasetArchiveIdent($self->{"cfg"}->{$index}->{"dataset_id"});
	    $mysql->setFile($directory,$file_data[8]);
	    $mysql->setFormatId($self->{"cfg"}->{$index}->{"format"});

	    # Add the new file to the list.

	    # Get the acceptable range of dates to be inserted.
	    my @range = ();
	    if (defined($self->{"cfg"}->{$index}->{"insertrange"})) {
		@range = split(/:/,$self->{"cfg"}->{$index}->{"insertrange"});
	    } else {
		@range = ("0000-00-00","9999-99-99");
	    }
	    
	    # Get the file date range
	    my @date = ((split(' ',$mysql->getBeginDate()))[0],
			(split(' ',$mysql->getEndDate()))[0]);

	    # Only insert the files in the defined date range
	    if (compareDates($range[0],"YYYY-MM-DD",$date[0],"YYYY-MM-DD") >= 0 &&
		compareDates($date[1],"YYYY-MM-DD",$range[1],"YYYY-MM-DD") >= 0) {
		push(@{ $files},$mysql);
	    }
	}
    }
}

1;
