#! /usr/bin/perl -w

##Module-----------------------------------------------------------------------
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDatabase.pm>Link to MySqlDatabase.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlDataset.pm>Link to MySqlDataset.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlFile.pm>Link to MySqlFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSFile.pm>Link to MySqlMSSFile.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlInserter.pm>Link to MySqlInserter.pm</a><br />
#<a href=http://dmg.eol.ucar.edu/cgi-bin/docperl/docperl?file=/net/work/lib/perl/mysql/MySqlMSSInserter.pm>Link to MySqlMSSInserter.pm</a><br />
# <p>The MySqlFile.pm module creates file objects that represent entries
# in the file table of the MySQL database.</p>
#
# @author Joel Clawson
##Module-----------------------------------------------------------------------
package MySqlFile;
use lib "/net/work/lib/perl/hpss";

use HPSS;
use MySqlDatabase;
use MySqlDataset;
use strict;


# cache ID lookups
$MySqlFile::_last_arc_id = undef;
$MySqlFile::_last_ds_id  = undef;


##-----------------------------------------------------------------------------
# @signature String getFileId()
# <p>Get the id (primary key) for the file.</p>
#
# @output $id
##-----------------------------------------------------------------------------
sub getFileId {
    my $self = shift;
    return $self->{"id"};
}

##-----------------------------------------------------------------------------
# @signature void setFileId(int fileId)
# <p>Set the id (primary key) of the file.</p>
#
# @input $fileId.
##-----------------------------------------------------------------------------
sub setFileId {
        my $self = shift;
        $self->{"id"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature String getVisible()
# <p>Get the visible flag for the file.</p>
#
# @output $id
##-----------------------------------------------------------------------------
sub getVisible {
    my $self = shift;
    return $self->{'visible'};
}

##-----------------------------------------------------------------------------
# @signature void setVisible(boolean vis)
# <p>Set the visible flag of the file.</p>
#
# @input $vis.
##-----------------------------------------------------------------------------
sub setVisible {
        my $self = shift;
        if ($_[0] eq '0') {
          $self->{'visible'} = 0;
        } elsif ($_[0]) {
          $self->{'visible'} = 1;
        } else {
          $self->{'visible'} = 0;
          }
}

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
    return $self->{"dataset_id"};
}

##-----------------------------------------------------------------------------
# @signature String getDatasetArchiveIdent()
# <p>Get the dataset archive identifier with which the file is associated.</p>
#
# @output $id The dataset archive identifier.
##-----------------------------------------------------------------------------
sub getDatasetArchiveIdent {
    my $self = shift;
    return $self->{"archive_ident"};
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
# @signature int getVersion()
# <p>Get the version of the file.</p>
#
# @output $file_version. The file version.
##-----------------------------------------------------------------------------
sub getVersion {
    my $self = shift;
    return $self->{"file_version"};
}

##-----------------------------------------------------------------------------
# @signature int getDesiredDatasetVersion()
# <p>Get the desired/configured dataset version of the file for inserts.</p>
#
# @output $desired_dataset_version. The desired dataset version.
##-----------------------------------------------------------------------------
sub getDesiredDatasetVersion {
    my $self = shift;
    return $self->{"__desired_dataset_version"};
}

##-----------------------------------------------------------------------------
# @signature int getFormatId()
# <p>Get the id for the format type of the file.</p>
#
# @output $id The format id.
##-----------------------------------------------------------------------------
sub getFormatId {
    my $self = shift;
    return $self->{"format_id"};
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
# @signature Int getMinlat()
# <p>Get the minimum latitude of the file.</p>
#
# @output $minLat.
##-----------------------------------------------------------------------------
sub getMinlat {
	my $self = shift;
	return $self->{"minimum_latitude"}
}

##-----------------------------------------------------------------------------
# @signature Int getMaxlat()
# <p>Get the maximum latitude of the file.</p>
#
# @output $minLat.
##-----------------------------------------------------------------------------
sub getMaxlat {
	my $self = shift;
	return $self->{"maximum_latitude"}
}

##-----------------------------------------------------------------------------
# @signature Int getMinlon()
# <p>Get the minimum longitude of the file.</p>
#
# @output $minLon.
##-----------------------------------------------------------------------------
sub getMinlon {
	my $self = shift;
	return $self->{"minimum_longitude"}
}

##-----------------------------------------------------------------------------
# @signature Int getMaxlon()
# <p>Get the maximum longitude of the file.</p>
#
# @output $maxLon.
##-----------------------------------------------------------------------------
sub getMaxlon {
	my $self = shift;
	return $self->{"maximum_longitude"}
}

##-----------------------------------------------------------------------------
# @signature Int getMinalt()
# <p>Get the minimum altitude of the file.</p>
#
# @output $minAlt.
##-----------------------------------------------------------------------------
sub getMinalt {
        my $self = shift;
        warnings::warnif("deprecated", "getMinalt is deprecated, use getMinVert instead (returning minVert for backwards-compatibility)");
        return $self->getMinVert();
}

##-----------------------------------------------------------------------------
# @signature Int getMinVert()
# <p>Get the minimum vertical value of the file.</p>
#
# @output $minVert.
##-----------------------------------------------------------------------------
sub getMinVert {
        my $self = shift;
        return $self->{"minimum_vertical"}
}

##-----------------------------------------------------------------------------
# @signature Int getMaxalt()
# <p>Get the maximum altitude of the file.</p>
#
# @output $minAlt.
##-----------------------------------------------------------------------------
sub getMaxalt {
        my $self = shift;
        warnings::warnif("deprecated", "getMaxalt is deprecated, use getMaxVert instead (returning maxVert for backwards-compatibility)");
        return $self->getMaxVert();
}

##-----------------------------------------------------------------------------
# @signature Int getMaxVert()
# <p>Get the maximum vertical value of the file.</p>
#
# @output $minVert.
##-----------------------------------------------------------------------------
sub getMaxVert {
        my $self = shift;
        return $self->{"maximum_vertical"}
}

##-----------------------------------------------------------------------------
# @signature String getAltUnits()
# <p>Get the units of altitude.</p>
#
# @output $altUnits.
##-----------------------------------------------------------------------------
sub getAltUnits {
    my $self = shift;
    warnings::warnif("deprecated", "getAltUnits is deprecated, use getVertUnits instead (returning vertUnits for backwards-compatibility)");
    return $self->getVertUnits();
}

##-----------------------------------------------------------------------------
# @signature String getVertUnits()
# <p>Get the units of the vertical value.</p>
#
# @output $vertUnits.
##-----------------------------------------------------------------------------
sub getVertUnits {
    my $self = shift;
    return $self->{"vertical_units"};
}

##-----------------------------------------------------------------------------
# @signature String getVertType()
# <p>Get the type of the vertical measurement.</p>
#
# @output $vertType.
##-----------------------------------------------------------------------------
sub getVertType {
    my $self = shift;
    return $self->{"vertical_type"};
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
# @signature String getEvent()
# <p>Get the event string</p>
#
# @output $event The size of file in KB.
##-----------------------------------------------------------------------------
sub getEvent {
    my $self = shift;
    return $self->{"event"};
}


##-----------------------------------------------------------------------------
# @signature String insert(MySqlDatabase db)
# <p>Insert this file into the specified database. This function will automatically
# expand the parent dataset date range if needed.</p>
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
    my $msg = "";

    if (!defined($self->getFilename())) {
	return "File name not set.";
    } elsif (!defined($self->getDirectory())) {
	return "Directory not set.";
    } elsif (!defined($self->getFormatId())) {
	return "Format Id not set.";
    } elsif ( (!defined($self->getDatasetId())) && (!defined($self->getDatasetArchiveIdent())) ) {
	return "Neither Dataset Id (database primary key) nor Archive Ident (natural key) is set for file.";
    } elsif (!defined($self->getBeginDate())) {
	return "Begin Date not set.";
    } elsif (!defined($self->getEndDate())) {
	return "End Date not set.";
    } elsif ($self->getSize() <= 0) {
	return sprintf("File %s/%s has <= 0 size.  Not inserting into the database.",
		       $self->getDirectory(),$self->getFilename());
    }

    # make sure we have a good integer dataset_id
    $self->{'dataset_id'} = $self->fetch_dataset_id($self->getDatasetArchiveIdent(),$db)
      if ( (!defined($self->{'dataset_id'})) || ($self->{'dataset_id'} !~ m/^\d+$/) );
    return "Cannot find/do not have the integer dataset_id for file.\n"
      if ( (!defined($self->{'dataset_id'})) || ($self->{'dataset_id'} !~ m/^\d+$/) );

    foreach my $key (keys(%{$self})) {
      next if ($key eq 'archive_ident');
      next if ($key eq 'id');
      next if ($key =~ m/^__/); # non-serialized keys
         $attrs .= sprintf("%s,",$key);
         $values .= sprintf("'%s',",$self->{$key});
    }

    $attrs .= "data_archive_date";
    $values .= "NOW()";

    #insert the file
    $msg = $db->insert("file",$attrs,$values);

    #if we failed return message
    if ($msg ne "") {return $msg;}

    # open dataset to get the begin/end dates
    my $dataset = MySqlDataset->new($self->getDatasetId());
    $msg = $dataset->selectDataset($db);
    # if we failed then return the error message
    if ($msg ne '') {return $msg;}
    
    my $file_begin = $self->getBeginDate(); $file_begin =~ s/[\s:-]//g;
    my $file_end = $self->getEndDate(); $file_end =~ s/[\s:-]//g;
    my $dataset_begin = $dataset->getBeginDate(); $dataset_begin =~ s/[\s:-]//g;
    my $dataset_end = $dataset->getEndDate(); $dataset_end =~ s/[\s:-]//g;
    
    if ($file_begin < $dataset_begin) {$dataset->setBeginDate(split(/[\s:-]/,$self->getBeginDate())); $msg .= $dataset->updateDataset($db);}
    if ($file_end > $dataset_end) {$dataset->setEndDate(split(/[\s:-]/,$self->getEndDate())); $msg .= $dataset->updateDataset($db);}
    return $msg;
}

##-----------------------------------------------------------------------------
# @signature Int[] increment_day(String date)
# <p>Increments a supplied date by one day. Returns array (Year,Month,Day,hour,minute,second) </p>
##-----------------------------------------------------------------------------
sub increment_day { #Added by Sean Stroble
     my $self = shift;
	my @date = split(/[\s:-]/,shift);
	
	#increment day
	$date[2]++;
	
	#Check to see if the month/year needs to be incremented

	my @days_in_month = (-1,31,28,31,30,31,30,31,31,30,31,30,31);
	#Correct Feb for leap year
	if ($date[0]%400 == 0) { $days_in_month[2]++; }
	elsif ($date[0]%4 == 0 && $date[0]%100 != 0) { $days_in_month[2]++; }

	#if the day is past the last day of the month inrement the month
	if ($date[2] > $days_in_month[$date[1]])
	{
		$date[1]++;
		
		#if month is past 12 increment year
		if ($date[1] > 12) {
			$date[0]++;
			$date[1] = 1;
		}
		
		$date[2] = 1;
	}

	return @date;
}

# internal - don't export doc
sub fetch_dataset_id {
	my $self = shift;
	my $arc_id = shift;
	my $database = shift;
    
    return(undef) if (!(defined($arc_id)));

    return $MySqlFile::_last_ds_id if (defined($MySqlFile::_last_arc_id) and ($arc_id eq $MySqlFile::_last_arc_id));

	my @results = $database->select('dataset', 'id', "archive_ident='${arc_id}'");
	unless ($results[0] eq '' && $database->getRows() <= 1) {
        die "Error retrieving dataset information for file!\n$results[0]\n";
        }

    $MySqlFile::_last_arc_id = $arc_id;
    $MySqlFile::_last_ds_id = $results[1];
	return $results[1];
}

# internal - don't export doc
sub fetch_archive_ident {
	my $self = shift;
	my $ds_id = shift;
	my $database = shift;

    return(undef) if (!(defined($ds_id)));


    return $MySqlFile::_last_arc_id if (defined($MySqlFile::_last_ds_id) and ($ds_id eq $MySqlFile::_last_ds_id));

	my @results = $database->select('dataset', 'archive_ident', "id=${ds_id}");
	unless ($results[0] eq '' && $database->getRows() <= 1) {
        die "Error retrieving dataset information for file!\n$results[0]\n";
        }

    $MySqlFile::_last_ds_id = $ds_id;
    $MySqlFile::_last_arc_id = $results[1];
	return $results[1];
}

##-----------------------------------------------------------------------------
# @signature bool load(long/string dataset_id, string directory, string filename, MySqlDatabase database)
##-----------------------------------------------------------------------------
sub load { #Added by Sean Stroble
	my $self = shift;
	my $dataset_id = shift;
	my $directory = shift;
	my $filename = shift;
	my $database = shift;

    $dataset_id = $self->fetch_dataset_id($dataset_id,$database) if ($dataset_id !~ m/^\d+$/);

	my @results = $database->select('file',
     'id,host,begin_date,end_date,'.
       'minimum_latitude,minimum_longitude,maximum_latitude,maximum_longitude,'.
       'minimum_vertical,maximum_vertical,vertical_units,'.
       'event,format_id,size_kb,purpose,vertical_type,dataset_id,filename,directory,visible',
     "dataset_id=" . $dataset_id . # INTEGER!
       " AND filename='" . $filename .
       "' AND directory='" . $directory. "'"
     );

	unless ($results[0] eq '' && $database->getRows() <= 1) {
        die "Error retrieving file information!\n$results[0]\n";
        }

	if ($database->getRows() == 0) { return 0; }
	elsif ($#results != 15) { die "Error retrieving file information!\n"; }

	$self->{"id"} = $results[1];
	$self->{"host"} = $results[2];
	$self->{"begin_date"} = $results[3];
	$self->{"end_date"} = $results[4];
	$self->{"minimum_latitude"} = $results[5];
	$self->{"minimum_longitude"} = $results[6];
	$self->{"maximum_latitude"} = $results[7];
	$self->{"maximum_longitude"} = $results[8];
	$self->{"minimum_vertical"} = $results[9];
	$self->{"maximum_vertical"} = $results[10];
	$self->{"vertical_units"} = $results[11];

    if ($results[12] =~ m/\S/) {
	  $self->{"event"} = $results[12];
	} else {
      $self->{"event"} = undef;
      }

	$self->{"format_id"} = $results[13];
	$self->{"size_kb"} = $results[14];
	$self->{"purpose"} = $results[15];
	$self->{"vertical_type"} = $results[16];

	$self->{"dataset_id"} = $results[17];

     # don't re-use these from above: database = is case-insensitive so get the official DB values
	$self->{"filename"} = $results[18];
	$self->{"directory"} = $results[19];

	$self->{"visible"} = ($results[20] eq '1' ? 1 : 0);

	$self->{"archive_ident"} = $self->fetch_archive_ident($self->{'dataset_id'},$database);

	return 1;	
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

    $self->setFileId(undef);
    $self->{'dataset_id'} = undef;
    $self->{'archive_ident'} = undef;

    $self->setHost('localhost');
    $self->setPurpose('data');
    $self->setVertType('geoidal');

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
	#End date correction added by Sean Stroble
	if (defined($self->getEndDate())){	
	    my $file_begin = $self->getBeginDate(); $file_begin =~ s/[\s:-]//g;
	    my $file_end = $self->getEndDate(); $file_end =~ s/[\s:-]//g;
	    
	    if ($file_begin > $file_end && $file_begin < ($file_end+1000000)) {
		$self->setEndDate($self->increment_day($self->getEndDate()));
	    }
	    elsif ($file_begin > $file_end)
	    {
		print "\nWARNING: File end date is more than 1 day before the file begin date!\n";
	    }
	}
}

##-----------------------------------------------------------------------------
# @signature void setDatasetId(ulong/String id)
# <p>Set the id of the dataset this file belongs to.
# Should be the dataset database ID (primary key), an unsigned long;
#  but accepts a string as the dataset archive identifier (natural key)
#  for backwards compatibility.</p>
#
# @input $id The dataset id.
##-----------------------------------------------------------------------------
sub setDatasetId {
    my $self = shift;
    my $new_id = $_[0];
    if (defined($new_id) && ($new_id =~ m/^\d+$/) ) {
      $self->{'dataset_id'} = $new_id;
    } else {
      warnings::warnif('deprecated',
         'setDatasetId should be given an unsigned long not the archive ID (setting the archive_ident for backwards-compatibility).');
      $self->{'dataset_id'} = undef;
      $self->setDatasetArchiveIdent($new_id);
    }
}

##-----------------------------------------------------------------------------
# @signature void setDatasetArchiveIdent(String id)
# <p>Set the archive identifier (natural key) of the dataset this file belongs to.</p>
#
# @input $id The dataset archive identifier (natural key).
##-----------------------------------------------------------------------------
sub setDatasetArchiveIdent {
    my $self = shift;
    $self->{"archive_ident"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setDirectory(String dir)
# <p>Set the directory that contains the file.</p>
#
# @input $dir The directory containing the file
##-----------------------------------------------------------------------------
sub setDirectory {
    my $self = shift;
    $self->{"directory"} = $_[0];
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
	#End date correction added by Sean Stroble
	if (defined($self->getBeginDate())){	
	    my $file_begin = $self->getBeginDate(); $file_begin =~ s/[\s:-]//g;
	    my $file_end = $self->getEndDate(); $file_end =~ s/[\s:-]//g;
	    
	    if ($file_begin > $file_end && $file_begin < ($file_end+1000000)) {
		$self->setEndDate($self->increment_day($self->getEndDate()));
	    }
	    elsif ($file_begin > $file_end)
	    {
		print "\nWARNING: File end date is more than 1 day before the file begin date!\n";
	    }
	}
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

    if (defined $_[2] && $_[2] > 0) { $self->{"size_kb"} = $_[2]; }
    elsif ($self->getHost() eq "hpss") {

	    my @result = HPSS::ls("$_[0]/$_[1]", "-l");
	    my @file_data = split(' ', $result[-1]);

	    $self->{"size_kb"} = int($file_data[4] / 1024.0 + 0.5) if ($file_data[4] != 0);


    }
    else {
        my $szkb = -s ( $_[0] . '/' . $_[1] );
		if ($szkb > 0) {
	        $self->{"size_kb"} = int( $szkb / 1024.0 + 0.5)
        } else {
	        $self->{"size_kb"} = 0
        }
    }

}

##-----------------------------------------------------------------------------
# @signature void setMinlat(int minLat)
# <p>Set the minimum latitude of the file.</p>
#
# @input $minLat.
##-----------------------------------------------------------------------------
sub setMinlat {
        my $self = shift;
        $self->{"minimum_latitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setMaxlat(int maxLat)
# <p>Set the maximum latitude of the file.</p>
#
# @input $maxLat.
##-----------------------------------------------------------------------------
sub setMaxlat {
        my $self = shift;
        $self->{"maximum_latitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setMinlon(int minLon)
# <p>Set the minimum longitude of the file.</p>
#
# @input $minLon.
##-----------------------------------------------------------------------------
sub setMinlon {
        my $self = shift;
        $self->{"minimum_longitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setMaxlon(int maxLon)
# <p>Set the maximum longitude of the file.</p>
#
# @input $maxLon.
##-----------------------------------------------------------------------------
sub setMaxlon {
        my $self = shift;
        $self->{"maximum_longitude"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setMinalt(int minAlt)
# <p>Set the minimum altitude of the file.</p>
#
# @input $minAlt.
##-----------------------------------------------------------------------------
sub setMinalt {
        my $self = shift;
        warnings::warnif("deprecated", "setMinalt is deprecated, use setMinVert instead (setting minVert for backwards-compatibility)");
        $self->setMinVert($_[0]);
}

##-----------------------------------------------------------------------------
# @signature void setMinVert(int minVert)
# <p>Set the minimum vertical value of the file.</p>
#
# @input $minVert.
##-----------------------------------------------------------------------------
sub setMinVert {
        my $self = shift;
        $self->{"minimum_vertical"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setMaxalt(int maxAlt)
# <p>Set the maximum altitude of the file.</p>
#
# @input $maxAlt.
##-----------------------------------------------------------------------------
sub setMaxalt {
        my $self = shift;
        warnings::warnif("deprecated", "setMaxalt is deprecated, use setMaxVert instead (setting maxVert for backwards-compatibility)");
        $self->setMaxVert($_[0]);
}

##-----------------------------------------------------------------------------
# @signature void setMaxVert(int maxVert)
# <p>Set the maximum vertical value of the file.</p>
#
# @input $maxVert.
##-----------------------------------------------------------------------------
sub setMaxVert {
        my $self = shift;
        $self->{"maximum_vertical"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setAltUnits(String units)
# <p>Set the units of altitude. It must be meters or millibars.</p>
#
# @input $altUnits.
##-----------------------------------------------------------------------------
sub setAltUnits {
    my $self = shift;
    warnings::warnif("deprecated", "setAltUnits is deprecated, use setVertUnits instead (setting vertUnits for backwards-compatibility)");
    $self->setVertUnits($_[0]);
}

##-----------------------------------------------------------------------------
# @signature void setVertUnits(String units)
# <p>Set the units of the vertical value. It must be meters or millibars.</p>
#
# @input $vertUnits.
##-----------------------------------------------------------------------------
sub setVertUnits {
    my $self = shift;
    if ($_[0] !~ /^(meters|millibars)$/) {
        die("Unknown altitude units: $_[0]\n");
    }
    $self->{"vertical_units"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setVertType(String type)
# <p>Set the type of the vertical measurement. It must be ISO-19115 compliant.
# Currently: 'geoidal','depth','barometric','other'.</p>
#
# @input $vertUnits.
##-----------------------------------------------------------------------------
sub setVertType {
    my $self = shift;
    if ($_[0] !~ /^(geoidal|depth|barometric|other)$/) {
        die("Unknown vertical type: $_[0]\n");
    }
    $self->{"vertical_type"} = $_[0];
}


##-----------------------------------------------------------------------------
# @signature void setVersion(int version)
# <p>Set the version of the file.</p>
#
# @input $file_version The file version.
##-----------------------------------------------------------------------------
sub setVersion {
    my $self = shift;
    $self->{"file_version"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setDesiredDatasetVersion(int version)
# <p>Set the desired dataset_version of the file (cache for later use by insert).</p>
#
# @input $desired_dataset_version The desired/configured dataset version.
##-----------------------------------------------------------------------------
sub setDesiredDatasetVersion {
    my $self = shift;
    $self->{"__desired_dataset_version"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setFormatId(int id)
# <p>Set the id of the format type of the file.</p>
#
# @input $id The format id.
##-----------------------------------------------------------------------------
sub setFormatId {
    my $self = shift;
    $self->{"format_id"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setHost(String host)
# <p>Set the host machine for the file.  It must be localhost or hpss.</p>
#
# @input $host The host machine.
##-----------------------------------------------------------------------------
sub setHost {
    my $self = shift;
    my $new_host = $_[0];
    if ($new_host eq 'mass_store') {
        warnings::warnif("deprecated", "mass_store is deprecated, use hpss instead (changing for you)");
        $new_host='hpss';
        }
	die("Unknown host: $new_host\n") if ($new_host !~ /^(localhost|hpss)$/);
    $self->{"host"} = $new_host;
}

##-----------------------------------------------------------------------------
# @signature void setPurpose(String purpose)
# <p>Set the purpose of the file.  It must be data, doc, preview, or eula.</p>
#
# @input $purpose The purpose of the file.
##-----------------------------------------------------------------------------
sub setPurpose {
    my $self = shift;
    if ($_[0] !~ /^(data|doc|eula|preview)$/) {
	die("Unknown purpose type: $_[0]\n");
    }
    $self->{"purpose"} = $_[0];
}

##-----------------------------------------------------------------------------
# @signature void setEvent(String event)
# <p>Set the event string for the file.</p>
#
# @input $event The event string of the file.
##-----------------------------------------------------------------------------
sub setEvent {
	my $self = shift;
    if ($_[0] =~ m/\S/) {
      $self->{"event"} = $_[0];
    } else {
      warnings::warnif("deprecated", "event cannot be blank or all whitespace, using null/undef");
      $self->{"event"} = undef;
    }
}

1;

