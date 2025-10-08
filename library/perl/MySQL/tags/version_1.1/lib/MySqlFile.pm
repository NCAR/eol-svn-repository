#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
# <p>The MySqlFile.pm module creates file objects that represent entries
# in the file table of the MySQL database.</p>
#
# @author Joel Clawson
##Module-----------------------------------------------------------------------
package MySqlFile;
use strict;

##-----------------------------------------------------------------------------
# @signature String getBeginDate()
# <p>Get the begin date for the file.</p>
#
# @output $date The begin date in YYYY-MM-DD HH:MM:SS format.
##-----------------------------------------------------------------------------
sub getBeginDate {
    my $self = shift;
    return $self->{"begin_date"};
}

##-----------------------------------------------------------------------------
# @signature String getDatasetId()
# <p>Get the dataset id the file is associated with.</p>
#
# @output $id The dataset id.
##-----------------------------------------------------------------------------
sub getDatasetId {
    my $self = shift;
    return $self->{"dataset_id_fk"};
}

##-----------------------------------------------------------------------------
# @signature String getDirectory()
# <p>Get the directory where the file is stored.</p>
#
# @output $dir The directory path.
##-----------------------------------------------------------------------------
sub getDirectory {
    my $self = shift;
    return $self->{"directory"};
}

##-----------------------------------------------------------------------------
# @signature String getEndDate()
# <p>Get the end date for the file.
#
# @output $date The end date in YYYY-MM-DD HH:MM:SS format.
##-----------------------------------------------------------------------------
sub getEndDate {
    my $self = shift;
    return $self->{"end_date"};
}

##-----------------------------------------------------------------------------
# @signature String getFilename()
# <p>Get the name of the file.</p>
#
# @output $name The name of the file.
##-----------------------------------------------------------------------------
sub getFilename {
    my $self = shift;
    return $self->{"filename"};
}

##-----------------------------------------------------------------------------
# @signature int getFormatId()
# <p>Get the id for the format type of the file.</p>
#
# @output $id The format id.
##-----------------------------------------------------------------------------
sub getFormatId {
    my $self = shift;
    return $self->{"format_id_fk"};
}

##-----------------------------------------------------------------------------
# @signature String getHost()
# <p>Get the host who stores this file.</p>
#
# @output $host The host of the file.
##-----------------------------------------------------------------------------
sub getHost {
    my $self = shift;
    return $self->{"host"};
}

##-----------------------------------------------------------------------------
# @signature String getPurpose()
# <p>Get the purpose of this file.</p>
#
# @output $purpose The file purpose.
##-----------------------------------------------------------------------------
sub getPurpose {
    my $self = shift;
    return $self->{"purpose"};
}

##-----------------------------------------------------------------------------
# @signature int getSize()
# <p>Get the size of the file in kilobytes.</p>
#
# @output $size The size of file in KB.
##-----------------------------------------------------------------------------
sub getSize {
    my $self = shift;
    return defined($self->{"size_kb"}) ? $self->{"size_kb"} : 0;
}

##-----------------------------------------------------------------------------
# @signature String insert(MySqlDatabase db)
# <p>Insert this file into the specified database.</p>
#
# @input $db An open database connection where the file should be inserted.
# @output $err An error if the file could not be entered or the empty string
# if the file was inserted okay.
##-----------------------------------------------------------------------------
sub insert {
    my $self = shift;
    my $db = shift;
    my $attrs = "";
    my $values = "";

    if (!defined($self->getFilename())) {
	return "File name not set.";
    } elsif (!defined($self->getDirectory())) {
	return "Directory not set.";
    } elsif (!defined($self->getFormatId())) {
	return "Format Id not set.";
    } elsif (!defined($self->getDatasetId())) {
	return "Dataset Id not set.";
    } elsif (!defined($self->getBeginDate())) {
	return "Begin Date not set.";
    } elsif (!defined($self->getEndDate())) {
	return "End Date not set.";
    } elsif ($self->getSize() == 0) {
	return sprintf("File %s/%s has 0 size.  Not inserting into the database.",
		       $self->getDirectory(),$self->getFilename());
    }
    
    foreach my $key (keys(%{$self})) {
	if ($attrs eq "") {
	    $attrs = $key;
	   $values = sprintf("'%s'",$self->{$key});
	} else {
	    $attrs .= sprintf(",%s",$key);
	    $values .= sprintf(",'%s'",$self->{$key});
	}
    }

    $attrs .= ",data_archive_date";
    $values .= ",NOW()";

    return $db->insert("file",$attrs,$values);
}

##-----------------------------------------------------------------------------
# @signature MySqlFile new()
# <p>Create a new MySqlFile.</p>
##-----------------------------------------------------------------------------
sub new {
    my $invocant = shift;
    my $self = {};
    my $class = ref($invocant) || $invocant;
    bless($self,$class);

    $self->setHost("localhost");
    $self->setPurpose("data");

    return $self;
}

##-----------------------------------------------------------------------------
# @signature void setBeginDate(int year, int month, int day, int hour, int minute, int second)
# <p>Set the begin date for the file.</p>
#
# @input $year The year of the data.
# @input $month The month of the year.
# @input $day The day of the month.
# @input $hour The hour of the day.
# @input $minute The minute of the hour.
# @input $second The second of the minute.
##-----------------------------------------------------------------------------
sub setBeginDate {
    my $self = shift;
    $self->{"begin_date"} = sprintf("%04d-%02d-%02d %02d:%02d:%02d",@_);
}

##-----------------------------------------------------------------------------
# @signature void setDatasetId(String id)
# <p>Set the id of the dataset this file belongs to.</p>
#
# @input $id The dataset id.
##-----------------------------------------------------------------------------
sub setDatasetId {
    my $self = shift;
    $self->{"dataset_id_fk"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setEndDate(int year, int month, int day, int hour, int minute, int second)
# <p>Set the end date for the file.</p>
#
# @input $year The year of the data.
# @input $month The month of the year.
# @input $day The day of the month.
# @input $hour The hour of the day.
# @input $minute The minute of the hour.
# @input $second The second of the minute.
##-----------------------------------------------------------------------------
sub setEndDate {
    my $self = shift;
    $self->{"end_date"} = sprintf("%04d-%02d-%02d %02d:%02d:%02d",@_);
}

##-----------------------------------------------------------------------------
# @signature void setFile(String dir, String file)
# <p>Set the location and name of the file.</p>
#
# @input $dir The directory where the file is stored.
# @input $file The name of the file.
##-----------------------------------------------------------------------------
sub setFile {
    my $self = shift;
    $self->{"directory"} = $_[0];
    $self->{"filename"} = $_[1];
    $self->{"size_kb"} = int((-s sprintf("%s/%s",$_[0],$_[1]))/1024)+1 
	if ((-s sprintf("%s/%s",$_[0],$_[1])) > 0);
}

##-----------------------------------------------------------------------------
# @signature void setFormatId(int id)
# <p>Set the id of the format type of the file.</p>
#
# @input $id The format id.
##-----------------------------------------------------------------------------
sub setFormatId {
    my $self = shift;
    $self->{"format_id_fk"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setHost(String host)
# <p>Set the host machine for the file.  It must be localhost or mass_store.</p>
#
# @input $host The host machine.
##-----------------------------------------------------------------------------
sub setHost {
    my $self = shift;
    if ($_[0] !~ /^(localhost|mass_store)$/) {
	die("Unknown host: $_[0]\n");
    }
    $self->{"host"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setPurpose(String purpose)
# <p>Set the purpose of the file.  It must be data, doc, or eula.</p>
#
# @input $purpose The purpose of the file.
##-----------------------------------------------------------------------------
sub setPurpose {
    my $self = shift;
    if ($_[0] !~ /^(data|doc|eula)$/) {
	die("Unknown purpose type: $_[0]\n");
    }
    $self->{"purpose"} = $_[0];
}

1;







